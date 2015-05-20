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
  df = read_csv("./input/FinalExam_AirlineDelay.csv")
  #dfTest = read_csv("./input/test.csv")


  ## Removing columns which are not necessary for the predictions

  ##Changing the Character columns into Factors
  df$Flight = as.factor(df$Flight)
  df$Carrier = as.factor(df$Carrier)
  df$Month = as.factor(df$Month)
  df$DayOfWeek = as.factor(df$DayOfWeek)
  df$InsufficientHistory = as.factor(df$InsufficientHistory)
  df$OriginFog = as.factor(df$OriginFog)
  df$OriginThunder = as.factor(df$OriginThunder)
  df$DestFog = as.factor(df$DestFog)
  df$DestThunder = as.factor(df$DestThunder)

  #Splitting them into train & test sets
  set.seed(15071)
  spl = sample(nrow(df), 0.7*nrow(df))

  dfTrain = df[spl,]
  dfTest = df[-spl,]

  ##Combining the dataset to make them ready for text analysis


  assign("df", df, envir)
  assign("dfTrain", dfTrain, envir)
  assign("dfTest", dfTest, envir)

  print("## Finished importing & preparing the data ##")
}
readData()

#Creating linear regression
regModel = lm(TotalDelay ~ . , data = dfTrain)
summary(regModel)

#Correlation between variables
cor(dfTrain$NumPrevFlights,dfTrain$PrevFlightGap)
cor(dfTrain$OriginAvgWind,dfTrain$OriginWindGust)

#Making predictions on the test set
predregModel = predict(regModel, newdata = dfTest)


#Calculating sse,sst, confusion Matrix
sseLM = sum((dfTest$TotalDelay - predregModel)^2)
sseLM

sstLM = sum((dfTest$TotalDelay - mean(dfTrain$TotalDelay))^2)
sstLM

rmseLM = 1 - (sseLM/sstLM)
rmseLM

#creating new model with new independant factor variable
df1 = df

df1$DelayClass = factor(ifelse(df1$TotalDelay == 0, "No Delay", ifelse(df1$TotalDelay >= 30, "Major Delay", "Minor Delay")))

#remove the original delay variable as that will add noise to the model when using all other parameters
df1$TotalDelay = NULL

#Splitting them into train & test sets
set.seed(15071)
require("caTools")

split = sample.split(df1$DelayClass, SplitRatio = 0.70)

df1Train = subset(df1, split == TRUE)
df1Test = subset(df1, split == FALSE)

#Building a cart model
require("rpart")
require("rpart.plot")
treeModel = rpart(DelayClass ~ . , data = df1Train, method = "class")
prp(treeModel)

#Predicting the tree model
treePred = predict(treeModel, newdata = df1Test, type = "class")

#Confusion Matrix for CART
table(df1Test$DelayClass,treePred)

require("caret")
confusionMatrix(treePred, df1Test$DelayClass)
