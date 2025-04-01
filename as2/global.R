library(shiny)
library(rpart.plot)
library(rpart)
library(DT)
library(recipes)
library(visdat)
library(ggplot2)
library(caret)
library(glmnet)
library(randomForest)
library(naniar)
library(shinycssloaders)
library(dplyr)


data <- read.csv("Ass2Data.csv", header = TRUE, na.strings = c("NA","N/A"))
                 
factor_columns <- c("CODE", "GOVERN_TYPE", "HEALTHCARE_BASIS", "OBS_TYPE")
numeric_columns <- c("POPULATION", "AGE25_PROPTN", "AGE_MEDIAN", "AGE50_PROPTN",
                       "POP_DENSITY", "GDP", "INFANT_MORT", "DOCS", "VAX_RATE",
                       "HEALTHCARE_COST", "DEATH_RATE")

for (col in factor_columns) {
  data[[col]] <- factor(data[[col]], ordered = FALSE)
}

for (i in numeric_columns) {
  data[[i]][data[[i]]<0] <- NA
}

# convert away from factor
data$GOVERN_TYPE <- as.character(data$GOVERN_TYPE)
data$GOVERN_TYPE[data$GOVERN_TYPE == "--"] <- NA
# convert back to factor
data$GOVERN_TYPE <- as.factor(data$GOVERN_TYPE)

pMiss <- function(x){ sum(is.na(x))/length(x)*100 }
