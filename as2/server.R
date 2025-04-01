# Define server logic required to draw a histogram
function(input, output, session) {
  
  output$summary_output <- renderPrint({
    # 获取变量和观测值的数量
    num_variables <- ncol(data)
    num_observations <- nrow(data)
    
    # 创建一个包含信息的字符串
    info_string <- paste("number of variables : ", num_variables, "\n")
    info_string <- paste(info_string, "number of observation: ", num_observations)
    
    cat(info_string)
  })
  
  output$summary <- renderPrint({
    if (is.null(data)) {
      print("null")
      return(NULL)
    }
    summary(data)
  })
  
  
  output$mytable <- renderDT({
    datatable(data)
  })
  
  output$corrgram <- renderPlot({
    selectedColumns <- c(input$corrgramplot)
    selectedData <- data[selectedColumns]
    method <- input$correlationMethod
    abs_value <- input$absCheckbox
    corrgram::corrgram(selectedData, abs = abs_value, cor.method = method, main = "correlation", order = input$ordervar)
  })
  
  output$Miss <- renderPlot({
    vis_dat(data) +
      ggtitle("Missing Value Distribution")
  })
  
  output$Miss_2 <- renderPlot({
    naniar::gg_miss_upset(data = data, nsets = 6) 
  })
  
  output$GGally <- renderPlot({
    req(!is.null(input$GGallyvar) && length(input$GGallyvar) > 0)
    
    # 确保输入是字符向量
    selected_columns <- data %>%
      dplyr::select(one_of(c(input$colorVar, input$GGallyvar)))
    
    # 设置一些默认列，以防用户未选择任何列
    if (ncol(selected_columns) == 1) {
      selected_columns <- data %>%
        dplyr::select(one_of(c(as.character(input$colorVar), "默认列名")))
    }
    
    ggplot_data <- GGally::ggpairs(data = selected_columns, 
                                   title = "Variable relationship matrix", 
                                   progress = FALSE, 
                                   mapping = aes(color = !!as.name(input$colorVar)), 
                                   cardinality_threshold = 500)
    
    print(ggplot_data)
  })
  
  output$Boxplot <- renderPlot({
    selected_data <- data[ ,input$boxplot]
    criterion <- input$outlierThreshold
    numData <- scale(selected_data, center = input$center_0, scale = input$scale_0)
    car::Boxplot(numData, col = "blue", range = criterion, outline = input$showOutliers)
  })
  
  
  output$shadow <- renderPlot({
    data_shadow <- data
    data_shadow$cost_shadow <- as.numeric(is.na(data_shadow$HEALTHCARE_COST))
    # 创建分组的直方图
    ggplot(data_shadow, aes(x = factor(cost_shadow), fill = factor(cost_shadow))) +
      geom_bar() +
      labs(x = "Cost Shadow", y = "Frequency",
           title = "red is missing") +
      facet_wrap(~ HEALTHCARE_BASIS, scales = "free") +
      scale_fill_manual(values = c("blue", "red")) +
      theme_minimal()
    
  })
  
  output$tree <- renderPlot({
    
    processed_data <- getCleanData2()
    processed_data <- processed_data[order(processed_data$CODE), ]
    processed_data$CODE <- 1:nrow(processed_data)
    processed_data$missingness <- rowSums(is.na(processed_data))
    tree <- caret::train(missingness ~ ., 
                         data = processed_data, 
                         method = "rpart", 
                         na.action = na.rpart) # na.rpart means "rpart will deal with missing predictors intrinsically"
    # step 4
    rpart.plot(tree$finalModel, 
               main = "Predicting the number of missing variables in an observation",
               sub = "Check whether the outcome variable is an important variable",
               roundint = TRUE, 
               clip.facs = TRUE)
  })
  
  
  
  getData <- reactive({
    data$HEALTHCARE_BASIS <- as.character(data$HEALTHCARE_BASIS)
    data$HEALTHCARE_COST[data$HEALTHCARE_BASIS == "FREE"] <- 0
    data$HEALTHCARE_BASIS <- as.factor(data$HEALTHCARE_BASIS)
    numeric_cols <- sapply(data, is.numeric)
    numeric_data <- data[, numeric_cols]
    if (input$center == "yes" && input$scale == "no") {
      
      
      means <- colMeans(numeric_data, na.rm = TRUE)  # 计算数值型列的均值，忽略缺失值
      
      centered_data <- scale(numeric_data, center = means, scale = FALSE)  # 中心化数值型列
      data_scaled <- data
      data_scaled[, numeric_cols] <- centered_data
    } else if (input$center == "no" && input$scale == "yes") {
      # 中心化和缩放数据
      scaled_data <- scale(numeric_data)
      data_scaled <- data
      data_scaled[, numeric_cols] <- scaled_data
    } else if (input$center == "yes" && input$scale == "yes") {
      numeric_data <- data[, sapply(data, is.numeric)]  # 选择数据中的数值型列
      
      means <- colMeans(numeric_data, na.rm = TRUE)  # 计算数值型列的均值，忽略缺失值
      
      centered_data <- scale(numeric_data, center = means, scale = FALSE)  # 中心化数值型列
      scaled_data <- scale(centered_data)
      data_scaled <- data
      data_scaled[, numeric_cols] <- scaled_data
    } else {
      data_scaled <- data
    }
    return(data_scaled)
    
  })
  
  getCleanData1 <- reactive({
    # remove excessively missing Vars
    data <- getData()
    vRatio <- apply(X = data, MARGIN = 2, FUN = pMiss)
    data[, vRatio < input$VarThreshold]
  })  
  
  getCleanData2 <- reactive({
    # remove excessively missing Obs
    data <- getCleanData1()
    oRatio <- apply(X = data, MARGIN = 1, FUN = pMiss)
    data[oRatio < input$ObsThreshold, ]
    
  })  
  
  
  
  
  output$Missing <- renderPlot({
    visdat::vis_dat(getCleanData2()) +
      labs(title = paste("Thresholds VarMiss:", input$VarThreshold, "ObsMiss:", input$ObsThreshold))
  }, width = 500)
  
  getFolds <- reactive({
    if (input$folds == "5") {
      folds = 5
    }else if (input$folds == "10"){
      folds = 10
    }
  })
  
  getRecipe <- reactive({
    rec <- recipe(DEATH_RATE ~ ., getCleanData2())
    if (input$ImpMethod == "KNN") {
      rec <- rec %>%
        update_role("CODE", new_role = "id") %>%
        update_role("OBS_TYPE", new_role = "split") %>%
        step_impute_knn(all_predictors(), neighbors = getFolds())
    } else if (input$ImpMethod == "Median") {
      rec <- rec %>%
        update_role("CODE", new_role = "id") %>%
        update_role("OBS_TYPE", new_role = "split") %>%
        step_impute_mode(all_nominal_predictors()) %>%
        step_impute_median(all_numeric_predictors())
    } else if (input$ImpMethod == "Partial Del") {
      rec <- rec %>%
        update_role("CODE", new_role = "id") %>%
        update_role("OBS_TYPE", new_role = "split") %>%
        step_naomit(all_predictors(), skip = FALSE)
    }
    rec <- prep(rec)
    rec
  })  
  
  
  
  # 定义训练数据集和测试数据集
  trainData <- reactive({
    rec <- getRecipe() # 获取处理后的数据
    data <- bake(rec, new_data = getCleanData2()) # 应用预处理器到原始数据
    
    # 将 OBS_TYPE 为 "Train" 的行标记为训练集
    train <- data[data$OBS_TYPE == "Train", ]
    #train <- train[, !names(train) %in% c("OBS_TYPE", "CODE")]
  })
  
  testData <- reactive({
    rec <- getRecipe() # 获取处理后的数据
    data <- bake(rec, new_data = getCleanData2()) # 应用预处理器到原始数据
    
    # 将 OBS_TYPE 为 "Test" 的行标记为测试集
    test <- data[data$OBS_TYPE == "Test", ]
    #test <- test[, !names(test) %in% c("OBS_TYPE", "CODE")]
  })
  
  
  getModel <- reactive({
    req(input$Go)
    isolate({
      # 定义参数网格
      param_grid <- expand.grid(
        alpha = seq(0, 1, by = 0.1),  # alpha 参数范围
        lambda = seq(0.01, 1, by = 0.01)  # lambda 参数范围
      )
      
      # 使用交叉验证选择最佳超参数
      fit_control <- trainControl(method = "cv", number = getFolds())  # 5 折交叉验证
      
      model <- caret::train(DEATH_RATE ~ .,
                   data = trainData(), 
                   method = "glmnet",
                   trControl = fit_control,
                   tuneGrid = param_grid
      )
      print(model$bestTune)
    })
    return(model)
  })
  
  output$Summary <- renderPrint({
    print(getModel())
  })
  
  
  

  
  
  
  # Display RMSE
  output$TestRMSE <- renderPrint({
    # Calculate RMSE
    # Generate predictions for test cases
    test_predictions <- predict(getModel(), newdata = testData())
    
    # Create a data frame to store actual and predicted values
    predictions_df <- data.frame(
      Actual = testData()$DEATH_RATE,
      Predicted = test_predictions
    )
    test_rmse <- caret::RMSE(predictions_df$Predicted, predictions_df$Actual)

    paste("Test RMSE:", round(test_rmse, 2))
  })
  
  output$prediction_vs_actual <- renderPlot({
    predictions <- predict(getModel(), newdata = testData())
    rang <- range(c(testData()$DEATH_RATE, predictions))
    ggplot(data = testData()) +
      geom_point(mapping = aes(x = predictions, y = testData()$DEATH_RATE)) +
      geom_abline(slope = 1, col = "blue") +
      xlim(rang) +
      ylim(rang) +
      labs(title = "Actual vs Predicted", y = "actual", x = "predicted")
  })
  
  

  
  output$Residualtrain <- renderPlot({
    train_predictions <- predict(getModel(), newdata = trainData())
    train_residuals <- trainData()$DEATH_RATE - train_predictions
    # 使用 reactive 的 coef
    names(train_residuals) <- rownames(trainData())
    coef <- input$outlierThreshold_2
    limits <- boxplot.stats(train_residuals, coef = coef)$stats
    
    # 标记异常值的行为 NA
    label <- ifelse(train_residuals < limits[1] | train_residuals > limits[5], as.character(rownames(train_residuals)), NA)
    
    
    ggplot(data = data.frame(train_residuals = train_residuals, label = label), mapping = aes(x = train_residuals, y = 0)) +
      geom_boxplot(coef = coef, outlier.colour = "red") +
      ggrepel::geom_text_repel(mapping = aes(label = label), max.overlaps = 50, na.rm=TRUE) +
      labs(title = paste("Boxplot using", coef, "as IQR Multplier")) +
      theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
    
  })
  
  output$Residualtest <- renderPlot({
    test_predictions <- predict(getModel(), newdata = testData())
    test_residuals <- testData()$DEATH_RATE - test_predictions
    names(test_residuals) <- rownames(testData())
    # 使用 reactive 的 coef
    coef <- input$outlierThreshold_3
    limits <- boxplot.stats(test_residuals, coef = coef)$stats
    
    # 标记异常值的行为 NA
    label <- ifelse(test_residuals < limits[1] | test_residuals > limits[5], as.character(rownames(test_residuals)), NA)
    
    
    ggplot(data = data.frame(test_residuals = test_residuals, label = label), mapping = aes(x = test_residuals, y = 0)) +
      geom_boxplot(coef = coef, outlier.colour = "red") +
      ggrepel::geom_text_repel(mapping = aes(label = label), max.overlaps = 50, na.rm=TRUE) +
      labs(title = paste("Boxplot using", coef, "as IQR Multplier")) +
      theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
  })
  
  output$ResidualBoxPlot <- renderPlot({
    # 训练数据集
    train_predictions <- predict(getModel(), newdata = trainData())
    train_residuals <- trainData()$DEATH_RATE - train_predictions

    train_limits <- boxplot.stats(train_residuals)$stats
    train_label <- ifelse(train_residuals < train_limits[1] | train_residuals > train_limits[5], as.character(rownames(train_residuals)), NA)
    
    train_data <- data.frame(residuals = train_residuals, dataset = "Train", label = train_label)
    
    # 测试数据集
    test_predictions <- predict(getModel(), newdata = testData())
    test_residuals <- testData()$DEATH_RATE - test_predictions

    test_limits <- boxplot.stats(test_residuals)$stats
    test_label <- ifelse(test_residuals < test_limits[1] | test_residuals > test_limits[5], as.character(rownames(test_residuals)), NA)
    
    test_data <- data.frame(residuals = test_residuals, dataset = "Test", label = test_label)
    
    # 合并数据集
    combined_data <- rbind(train_data, test_data)
    
    # 绘制残差图
    ggplot(data = combined_data, mapping = aes(x = residuals, y = 0, color = dataset)) +
      geom_boxplot(outlier.colour = "red") +
      ggrepel::geom_text_repel(mapping = aes(label = label), max.overlaps = 50, na.rm = TRUE) +
      labs(title = "Residuals Boxplot") +
      theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
      scale_color_manual(values = c("Train" = "blue", "Test" = "green"))  # 设置颜色
  })
  
  

  
  
  
  
  
  
  
  
  
  
  
  
  
 

  
  

}
