ProcterGamble= read.csv("Unit01_ProcterGambleStock.csv")
IBM = read.csv("Unit01_IBMStock.csv")
GE = read.csv("Unit01_GEStock.csv")
CocaCola = read.csv("Unit01_CocaColaStock.csv")
Boeing = read.csv("Unit01_BoeingStock.csv")

IBM$Date = as.Date(IBM$Date, "%m/%d/%y")

GE$Date = as.Date(GE$Date, "%m/%d/%y")

CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")

ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")

Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")

#SD for Procter & Gamble
sd(ProcterGamble$StockPrice)

#Plotting for Cocacola
plot(CocaCola$Date,CocaCola$StockPrice,type="l",col="red")
#For adding one more stock to the same plot
lines(ProcterGamble$Date, ProcterGamble$StockPrice,col="blue",lty=2)
# For adding a vertical line
abline(v=as.Date(c("2000-03-01")), lwd=2)

#Plotting only for a time period
plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210),col="red")
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432],col="blue",lty=2)
lines(IBM$Date[301:432], IBM$StockPrice[301:432],col="green",lty=3)
lines(GE$Date[301:432], GE$StockPrice[301:432],col="black",lty=4)
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432],col="yellow",lty=5)

#Now lets plot a line for the dot com crash in 2000
abline(v=as.Date(c("1997-10-01")), lwd=4)

# mean price over the months
sort(tapply(IBM$StockPrice,months(IBM$Date),mean))

#https://github.com/TarekDib03/Analytics/blob/master/Week1/Data%26Codes/Employment.Rmd
#Other people have documented it already!!!!

