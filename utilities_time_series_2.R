# Forecast per bill code for FullSimData.csv

#Hardcode file read and aggregate for testing. Both steps are repeated inside the function - need to uncommen
df <- fread("FullSimData.csv", sep = ",", header = TRUE,stringsAsFactors = FALSE)
ts.data <- df

fcast <-  function(ts.data, Revenue){

library(forecast)
library(MASS)
library(data.table)
library(fpp)
library(Hmisc)
library(fBasics)
library(timeSeries)

# df <- fread("FullSimData.csv", sep = ",", header = TRUE,stringsAsFactors = FALSE)
df <- ts.data

tic <- Sys.time()
df1<- setNames(aggregate(df$Revenue, by=list(df$Month,df$Year,df$Billcode), FUN=sum),
               c('Month', 'Year','Billcode','Revenue'))
toc <- Sys.time()
time <- toc-tic
time
# Time difference of 10.80245 secs

# https://www.statmethods.net/management/sorting.html sorting
df1.billcode <- df1[order(df1$Billcode),]
Billcodes <- unique(df1.billcode$Billcode)

df1.year <- df1[order(df1$Year),]
year.list <- unique(df1.year$Year)

#Arrange data per bill code for time series analysis.
#Steps
#Get the rolled up data from CA
#Sort based on Billcodes
#Subset by each year per billcode
#Sort by month per year per billcode

      #initialize a matrix
      df.collect = matrix(NA, nrow = 0, ncol = 4)
      colnames(df.collect) <- c("Month","Year","Billcode","Revenue")
      for(k in 1:length(Billcodes)){
        df1.billcode.c01 <- df1.billcode[df1.billcode$Billcode==Billcodes[k],]
        df1.billcode.c01 <- df1.billcode.c01[order(df1.billcode.c01$Year),]
        j = 2004
        for(i in 1:length(year.list)){
          
          df1.billcode.c01.2004 <- df1.billcode.c01[df1.billcode.c01$Year==j,]
          df1.billcode.c01.2004 <- df1.billcode.c01.2004[order(df1.billcode.c01.2004$Month),]
          df.collect <- rbind(df.collect,df1.billcode.c01.2004) 
          j = j + 1
        }
        k = k + 1
      }

              #Forecaste using forecast function for bill codes b01, b15, IC7 and C02. This cannot be generalized 
              #if not done in the "One Time series model per bill code" loop below
              billcodes.problem <- c("B01", "B15", "IC7", "C02") #vector of problematic bill codes
              l=1
              m=1
              lst <- vector()
              for(l in 1:length(billcodes.problem)){
                billcode <- df.collect[df.collect$Billcode == billcodes.problem[l],]
                strt <-(as.numeric(min(billcode$Year)))
                nd <-(as.numeric(max(billcode$Year)))
                billcode.ts <- ts(billcode$Revenue, start = strt,end=nd)
                billcode.fcast <- forecast(billcode.ts,h=1)
                f.cast <- billcode.fcast$mean[1]
                lst[[m]] <- f.cast
                l <- l + 1
                m <- m + 1
                }
      
#deleting rows with bill codes b01, b15, IC7 and C02 as they dont form regular time series 
#https://stackoverflow.com/questions/22655060/remove-rows-conditionally-from-a-data-table-in-r
df.collect <- as.data.table(df.collect)
setkey(df.collect,Billcode) #need to set the key, look up for more
df.collect <- df.collect[!"B01"]
df.collect <- df.collect[!"B15"]
df.collect <- df.collect[!"IC7"]
df.collect <- df.collect[!"C02"]

#new billcode list aft4er deleting some problamatic ones
df.collect.billcode <- df.collect[order(Billcode),]
Billcodes <- unique(df.collect.billcode$Billcode)

                  #One Time series model per bill code

      df3 <- df.collect
        
                  #initialize a matrix
                  df.collect.sum = matrix(NA, nrow = 0, ncol = 2)
                  colnames(df.collect.sum) <- c("Billcode","12_Month_Forecast")
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
                   
                        #Avoiding zero as forecast
                        if(fcast.rev.sum == 0){
                          fcast.rev.sum = df.per.billcode$Revenue
                        }
                    
                    sum.fcast.mean <- cbind((Billcodes[k]),(fcast.rev.sum)) #arrange the forecasted values with the respective billcode
                    df.collect.sum <- rbind(df.collect.sum,sum.fcast.mean) #collect all the forecasts into one matrix
                    k = k + 1
                  }
                  df.collect.sum <- as.data.frame(df.collect.sum) #convert the matric to data frame for export

    #Put back the problematic billcodes with forecast to the main data frame
    df.collect.sum$`12_Month_Forecast` <- as.numeric(as.character((df.collect.sum$`12_Month_Forecast`)))
    billcodes.problem <- as.data.frame(billcodes.problem)
    lst <- (as.data.frame(lst))
    billcodes.problem <- cbind(billcodes.problem, lst)
    colnames(billcodes.problem) <- c("Billcode","12_Month_Forecast")
    df.collect.sum <- rbind(df.collect.sum, billcodes.problem)  
    
    #remove duplicates mand return the final data frame
    return(unique(setDT(df.collect.sum), by = c("Billcode")))
}



