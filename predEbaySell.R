## EDX Final Exam
## Second Exam - Predicting whether a shoe will be sold or not at a given price enabling the seller to choose the mode of sale in eBay


## GitHub : https://github.com/miztiik/EDX-FinalExam
## 20May2015

## Cleaning up the environment variables
rm(list = ls(all = TRUE))

readData <- function() {
  require("readr")
  envir = globalenv()

  print("## Begin importing & preparing the data ##")
  ## Importing & Preparing the data ( using readr package faster than read.csv)
  df = read_csv("./input/FinalExam_ebay.csv")


  ## Removing columns which are not necessary for the predictions

  ##Changing the Character columns into Factors
  df$condition = as.factor(df$condition)
  df$heel = as.factor(df$heel)
  df$style = as.factor(df$style)
  df$color = as.factor(df$color)
  df$material = as.factor(df$material)
  df$sold = as.factor(df$sold)

  ##Combining the dataset to make them ready for text analysis
  assign("df", df, envir)

  print("## Finished importing & preparing the data ##")
}
readData()

## Splitting the data
require("caTools")
set.seed(144)
spl = sample.split(df$sold, 0.7)

dfTrain = subset(df,spl==TRUE)
dfTest = subset(df,spl==FALSE)


#Creating Logistic Regression
m1 = glm(sold ~ biddable + startprice + condition + heel + style + color + material , data = dfTrain, family = "binomial")
summary(m1)

#Predicting on the test sets
p1 = predict(m1, newdata = dfTest,type = "response")

##Baseline for test set
table(dfTest$sold)
table(p1>0.5)

require("ROCR")
predROCR = prediction(p1,dfTest$sold)
as.numeric(performance(predROCR,"auc")@y.values)


# Performance function
perfROCR = performance(predROCR, "tpr", "fpr")

# Plot ROC curve with colors & threshold labels
plot(perfROCR, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
