# Forecast per bill code for Parkwater data set

library(forecast)
library(car)
library(MASS)
library(TTR)
library(MVN)
library(boot)
library(data.table)
library(lubridate)
library(fpp)
library(Hmisc)
require(fpp)
require(forecast)
library(fBasics)
library(data.table)
library(timeSeries)

df2 <- fread("per_bill_code.csv", sep = ",", header = TRUE,stringsAsFactors = FALSE)
df1 <- df2
# https://www.statmethods.net/management/sorting.html sorting
df1.billcode <- df1[order(Billcode),]
Billcodes <- unique(df1.billcode$Billcode)

df1.year <- df1.year[order(Year),]
year.list <- unique(df1.year$Year)

#Arrange data per bill code for time series analysis.
#Steps
#Get the rolled up data from CA
#Sort based on Billcodes
#Subset by each year per billcode
#Sort by month per year per billcode

      #initialize a matrix
      df.collect = matrix(NA, nrow = 0, ncol = 5)
      colnames(df.collect) <- c("Month","Year","Billcode","Revenue","Consumption")
      for(k in 1:length(Billcodes)){
        df1.billcode.c01 <- df1.billcode[df1.billcode$Billcode==Billcodes[k],]
        df1.billcode.c01 <- df1.billcode.c01[order(Year),]
        j = 2004
        for(i in 1:length(year.list)){
          
          df1.billcode.c01.2004 <- df1.billcode.c01[df1.billcode.c01$Year==j,]
          df1.billcode.c01.2004 <- df1.billcode.c01.2004[order(Month),]
          df.collect <- rbind(df.collect,df1.billcode.c01.2004, fill = TRUE) 
          j = j + 1
        }
        k = k + 1
      }

#deleting rows with bill codes b01 and b15
#https://stackoverflow.com/questions/22655060/remove-rows-conditionally-from-a-data-table-in-r

setkey(df.collect,Billcode) #need to set the key, look up for more
df.collect <- df.collect[!"B01"]
df.collect <- df.collect[!"B15"]
df.collect <- df.collect[!"IC7"]
df.collect <- df.collect[!"C02"]

#new billcode list aft4er deleting some problamatic ones
df.collect.billcode <- df.collect[order(Billcode),]
Billcodes <- unique(df.collect.billcode$Billcode)

write.csv(df.collect,"per_bill_code_mod_.csv")

#Read the altered file: C02 bill code has average Jan and feb data
df3 <- fread("per_bill_code_mod_.csv", sep = ",", header = TRUE,stringsAsFactors = FALSE)
          
          #One Time series model per bill code

          #initialize a matrix
          df.collect.sum = matrix(NA, nrow = 0, ncol = 2)
          colnames(df.collect.sum) <- c("Billcode","Forecasted_Revenue_2013")
          k=1
          for(k in 1:length(Billcodes)){
            df.per.billcode <- df3[df3$Billcode==Billcodes[k],]
            ut <- ts(df.per.billcode$Revenue, frequency = 12, start=c(2004,3))
            # plot(ut)
            #adf.test(ut, alternative = "stationary") #not being used in the loop
            diffs <- ndiffs(ut) #not being used in the loop
            fit.aa <- auto.arima(ut,seasonal=TRUE, max.order=9, stepwise = TRUE, allowdrift = FALSE) #arima model
            plot(forecast(fit.aa,h=12)) #prints one forecast plot per bill code
            fcast <- forecast(fit.aa,h=12) #capture forecasted values
            fcast.mean <- as.data.frame(fcast$mean) #get the mean per month
            fcast.rev.sum <- sum(fcast.mean$x) #take sum of 12 forecasted months - from june 2012 - May 2013
            sum.fcast.mean <- cbind((Billcodes[k]),(fcast.rev.sum)) #arrange the forecasted values with the respective billcode
            df.collect.sum <- rbind(df.collect.sum,sum.fcast.mean) #collect all the forecasts into one matrix
            k = k + 1
          }
          df.collect.sum <- as.data.frame(df.collect.sum) #convert the matric to data frame for export

        
#write the mean forecast per billcode table
write.csv(df.collect.sum, "per_billcode_forecast.csv")
