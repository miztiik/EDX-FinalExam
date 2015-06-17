## Unit 02 - Linear Regresttion
## 17Jun2015
## Assisgnment - Climate change

setwd("./Unit02")
climate <- read.csv("./Unit02_climate_change.csv")

train <- subset( climate , climate$Year <= 2006 )
test <- subset( climate , climate$Year > 2006 )

#Lets build the model and call it m1
m1 <- lm( Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols , data = train)

#R-Squared
summary(m1)

#Correlation between training set variables
cor(train)

#Lets build model 2
m2 <- lm( Temp ~ MEI + TSI + Aerosols + N2O , data = train)
summary(m2)

#Using the step function to try out different models
stepM1 <- step(m1)
summary(stepM1)

#Lets begin the predictions
p1 <- predict(stepM1,newdata =  test)

#R-Squared
SSE <- sum((test$Temp - p1)^2)
SST <- sum((test$Temp - mean(train$Temp))^2)

R2 <- ( 1 - (SSE/SST) )
R2