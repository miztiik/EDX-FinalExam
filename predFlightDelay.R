## EDX Final Exam
## First problem is to predict Flight Delays  - input dataset - "FinalExam_AirlineDelay.csv"

## GitHub : https://github.com/miztiik/EDX-FinalExam
## 20May2015

## Cleaning up the environment variables
rm(list = ls(all = TRUE))

readData <- function() {
  require("readr")
  envir = globalenv()

  print("## Begin importing & preparing the data ##")
  ## Importing & Preparing the data ( using readr package faster than read.csv)
  dfTrain = read_csv("./input/.csv")
  dfTest = read_csv("./input/test.csv")

  train_median_relevance = dfTrain$median_relevance
  train_relevance_variance = dfTrain$relevance_variance
  testID = dfTest$id

  ## Removing columns which are not necessary for the predictions
  dfTrain$median_relevance = NULL
  dfTrain$relevance_variance = NULL
  dfTrain$id = NULL
  dfTest$id = NULL

  ##Combining the dataset to make them ready for text analysis
  df = rbind(dfTrain,dfTest)

  assign("dfTrain", dfTrain, envir)
  assign("dfTest", dfTest, envir)
  assign("df",df,envir)
  assign("train_median_relevance",train_median_relevance,envir)
  assign("train_relevance_variance",train_relevance_variance,envir)


  print("## Finished importing & preparing the data ##")
}
readData()