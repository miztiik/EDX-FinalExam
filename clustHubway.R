# EDX Final Exam
## Third Exam - Clustering - Segmentation - Classification
## Hubway - Bicycle Renting company rents out its bike for a membership fee. Unlimited number of fides less 30 minutes are free.
## Longer rides will cost a "overtime" fees.

## GitHub : https://github.com/miztiik/EDX-FinalExam
## 21May2015

## Cleaning up the environment variables
rm(list = ls(all = TRUE))
gc()

readData <- function() {
  require("readr")
  envir = globalenv()

  print("## Begin importing & preparing the data ##")
  ## Importing & Preparing the data ( using readr package faster than read.csv)
  df = read_csv("./input/FinalExam_HubwayTrips.csv")
  df1 = df

  ##Changing the Character columns into Factors
  df$Morning = as.factor(df$Morning)
  df$Afternoon = as.factor(df$Afternoon)
  df$Evening = as.factor(df$Evening)
  df$Weekday = as.factor(df$Weekday)
  df$Male = as.factor(df$Male)

  ##Combining the dataset to make them ready for text analysis
  assign("df", df, envir)
  assign("df1", df1, envir)

  print("## Finished importing & preparing the data ##")
}
readData()

#Average duration over the weekday
tapply(df$Duration,df$Weekday,mean)

#Proportion of Male among bikers
mean(df$Male==1)

#Normalizing the data to prepare them for clustering

require("caret")
preProc = preProcess(df1)
#str(preProc)
df1Norm = predict(preProc,df1)
str(df1Norm)

#Max duration after normalization
max(df1Norm$Duration)

#Lets just "attempt" a hierarchial cluster even though it is not useful. "Just too  many of them!!"
#dist = dist(df1Norm, method = "euclidean" )
#clus = hclust(dist, method = "ward" )
#plot(clus)

#Since Hierachial wont work, lets start building K-Means Cluster
set.seed(5000)
## Number of clusters (k)
k = 10
KMC = kmeans(df1Norm, centers = k , iter.max=1000)
str(KMC)

#Min & Max observations in a cluster
min(table(KMC$cluster))
max(table(KMC$cluster))

#Lets split the clusters
splKMC = split(df1Norm,KMC$cluster)
#str(splKMC[[1]])

#Listing the cluster centroids/means to understand hwo they are grouped
KMC$centers

#setting higher clusternumber
set.seed(8000)
k1 = 20
KMC1 = kmeans(df1Norm, centers = k1 , iter.max=1000)
str(KMC1)
table(KMC1$cluster)

#Use the centroids value & the mean from(summary function) to decide what each cluster group represents.
# You can also use the tapply function, i haven't really figured out how!!! :)