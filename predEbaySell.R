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

#Lets train the CART model using caret function for the best cp parameter, 10 fold cross validation
require("rpart")
require("rpart.plot")
require("caret")
require("e1071")

rpartControl = trainControl( method ="cv" , number = 10)
rpartGrid = expand.grid( .cp = seq(0.001,0.05,0.00098))

print("= 10 Fold Cross Validation begins")
tr = train(sold ~ biddable + startprice + condition + heel + style + color + material , data = dfTrain, method = "rpart", trControl = rpartControl, tuneGrid = rpartGrid)

print("### 10 Fold Cross Validation ends & plotting the best model ###")

prp(tr$finalModel)

#Building a model with cp "0.005" found after CV
m2 = rpart(sold ~ biddable + startprice + condition + heel + style + color + material , data = dfTrain, method ="class" , cp = 0.005)
prp(m2)

#Building a corpus to peform text analytics to see if the description of shoes improves the prediction
require("tm")
require("SnowballC")
require("ggvis")

corpus = Corpus(VectorSource(df$description))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
dtm = DocumentTermMatrix(corpus)

#Total number of tersm in the document
dtm

#Remove all terms that don't appear in at least 10% of documents,
#i,e Limit dtm to contain terms appearing in atleast 10% of document - then set sparse to 0.9 i,e (100 -10)/100
#Sparse value in "removeSparseTerms" = remove all terms who are NOT appearing in "X" of the documents, i,e they are (100-X%)/100 empty/sparse

spdtm = removeSparseTerms(dtm, 0.9)

#Creating a data frame from the sparsed Cropus
descriptionText = as.data.frame(as.matrix(spdtm))

#Find the word stem that shows up most frequently across all the documents
which.max(colSums(descriptionText))

#Add a prefix "D" for description in the column names to make it R-Friendly
names(descriptionText) = paste0("D", names(descriptionText))

#Adding the other variables to this df
descriptionText$sold=df$sold
descriptionText$biddable=df$biddable
descriptionText$startprice=df$startprice
descriptionText$condition=df$condition
descriptionText$heel=df$heel
descriptionText$style=df$style
descriptionText$color=df$color
descriptionText$material=df$material

trainText = subset(descriptionText,spl==TRUE)
testText = subset(descriptionText,spl==FALSE)

#Logistic Regression using text analytics

m3 = glm(sold ~ . , data = trainText, family = "binomial")
summary(m3)

#Predict using the new model with description field
#Predicting within the Train set
p3Train = predict(m3,type="response")
#Calculating ROCR for Train set
p3TrainROCR = prediction(p3Train,trainText$sold)
p3TrainAUC = as.numeric(performance(p3TrainROCR,"auc")@y.values)
p3TrainAUC

#Predicting the test set
p3 = predict(m3,newdata=testText,type="response")
#Calculating ROCR for test set
p3ROCR = prediction(p3,testText$sold)
p3AUC = as.numeric(performance(p3ROCR,"auc")@y.values)
p3AUC

#Even with the additional variables in the model with "descriptions text" the AUC didn't improve compared to the previous glm model "m1" hence it is over fitted.
#Removing variables might improve this model