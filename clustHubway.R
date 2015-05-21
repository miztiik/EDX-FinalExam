## EDX Final Exam
## Third Exam - Clustering - Segmentation - Classification
## Hubway - Bicycle Renting company rents out its bike for a membership fee. Unlimited number of fides less 30 minutes are free.
## Longer rides will cost a "overtime" fees.


## GitHub : https://github.com/miztiik/EDX-FinalExam
## 21May2015

## Cleaning up the environment variables
rm(list = ls(all = TRUE))

readData <- function() {
  require("readr")
  envir = globalenv()

  print("## Begin importing & preparing the data ##")
  ## Importing & Preparing the data ( using readr package faster than read.csv)
  df = read_csv("./input/FinalExam_HubwayTrips.csv")


  ## Removing columns which are not necessary for the predictions

  ##Changing the Character columns into Factors

  ##Combining the dataset to make them ready for text analysis
  assign("df", df, envir)

  print("## Finished importing & preparing the data ##")
}
readData()