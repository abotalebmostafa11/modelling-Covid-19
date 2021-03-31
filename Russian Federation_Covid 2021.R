
# **New techniques For Forecasting Covid 19 In top 10 countries infected ( Russian Federation)**

## **By**

### *Makarovskikh Tatyana Anatolyevna "Макаровских Татьяна Анатольевна"*

### *Abotaleb mostafa "Аботалеб Мостафа"*

### *Department of Electrical Engineering and Computer Science*

### *South ural state university, Chelyabinsk, Russian federation*

# Imports
library(fpp2)
library(forecast)
library(ggplot2)
library("readxl")
library(moments)
library(forecast)
require(forecast)  
require(tseries)
require(markovchain)
require(data.table)



#population in  Russian Federation = 145967957
#WHO COVID-19 global table data January 11th 2021 at 11.53.00 AM.csv
Full_original_data<-read.csv("F:/Phd/COVID 19 in 2021/WHO_data.csv")
View(Full_original_data)
y_lab<- "Covid 19 Infection cases in Russian Federation "   # input name of data
Actual_date_interval <- c("2020/01/03","2021/01/10")
Forecast_date_interval <- c("2021/01/11","2021/01/17")
validation_data_days <-7
frequency <-"days"
Population <-145967957 # population in Russia



# Data Preparation & calculate some of statistics measures
Covid_data<-Full_original_data[Full_original_data$Country == "Russian Federation", ]
original_data<-Covid_data$Cumulative_cases
View(original_data)
summary(original_data)
sd(original_data)  # calculate standard deviation
skewness(original_data)  # calculate Cofficient of skewness
kurtosis(original_data)   # calculate Cofficient of kurtosis

rows <- NROW(original_data)
training_data<-original_data[1:(rows-validation_data_days)]
testing_data<-original_data[(rows-validation_data_days+1):rows]
AD<-fulldate<-seq(as.Date(Actual_date_interval[1]),as.Date(Actual_date_interval[2]), frequency)  #input range for actual date
FD<-seq(as.Date(Forecast_date_interval[1]),as.Date(Forecast_date_interval[2]), frequency)  #input range forecasting date
N_forecasting_days<-nrow(data.frame(FD)) 
validation_dates<-tail(AD,validation_data_days)
validation_data_by_name<-weekdays(validation_dates)
forecasting_data_by_name<-weekdays(FD)




##bats model
# Data Modeling
data_series<-ts(training_data)
autoplot(data_series ,xlab=paste ("Time in  ", frequency, sep=" "), ylab = y_lab, main=paste ("Actual Data :", y_lab, sep=" "))

model_bats<-bats(data_series)
accuracy(model_bats)  # accuracy on training data
# Print Model Parameters
model_bats
plot(model_bats,xlab = paste ("Time in  ", frequency ,y_lab , sep=" "),  col.main="black", col.lab="black", col.sub="black", cex.main=1, cex.lab=1, cex.sub=1,font.main=4, font.lab=4)



# Testing Data Evaluation
forecasting_bats <- predict(model_bats, h=N_forecasting_days+validation_data_days)
validation_forecast<-head(forecasting_bats$mean,validation_data_days)
MAPE_Per_Day<-round(  abs(((testing_data-validation_forecast)/testing_data)*100)  ,3)
paste ("MAPE % For ",validation_data_days,frequency,"by using bats Model for  ==> ",y_lab, sep=" ")
MAPE_Mean_All<-paste(round(mean(MAPE_Per_Day),3),"% MAPE ",validation_data_days,frequency,y_lab,sep=" ")
MAPE_bats<-paste(round(MAPE_Per_Day,3),"%")
MAPE_bats_Model<-paste(MAPE_Per_Day ,"%")
paste (" MAPE that's Error of Forecasting for ",validation_data_days," days in bats Model for  ==> ",y_lab, sep=" ")
paste(MAPE_Mean_All,"%")
paste ("MAPE that's Error of Forecasting day by day for ",validation_data_days," days in bats Model for  ==> ",y_lab, sep=" ")
data.frame(date_bats=validation_dates,validation_data_by_name,actual_data=testing_data,forecasting_bats=validation_forecast,MAPE_bats_Model)
data.frame(FD,forecating_date=forecasting_data_by_name,forecasting_by_bats=tail(forecasting_bats$mean,N_forecasting_days))
plot(forecasting_bats)
x1_test <- ts(testing_data, start =(rows-validation_data_days+1) )
lines(x1_test, col='red',lwd=2)
graph1<-autoplot(forecasting_bats,xlab = paste ("Time in  ", frequency ,y_lab , sep=" "),  col.main="black", col.lab="black", col.sub="black", cex.main=1, cex.lab=1, cex.sub=1,font.main=4, font.lab=4, ylab=y_lab)
graph1



## Error of forecasting
Error_bats<-abs(testing_data-validation_forecast)  # Absolute error of forecast (AEOF)
REOF_A_bats<-abs(((testing_data-validation_forecast)/testing_data)*100)  #Relative error of forecast (divided by actual)(REOF_A)
REOF_F_bats<-abs(((testing_data-validation_forecast)/validation_forecast)*100)  #Relative error of forecast (divided by forecast)(REOF_F)
correlation_bats<-cor(testing_data,validation_forecast, method = c("pearson"))     # correlation coefficient between predicted and actual values 
RMSE_bats<-sqrt(sum((Error_bats^2))/validation_data_days)   #  Root mean square forecast error
MSE_bats<-(sum((Error_bats^2))/validation_data_days)   #  Root mean square forecast error
MAD_bats<-abs((sum(testing_data-validation_forecast))/validation_data_days)   # average forecast accuracy
AEOF_bats<-c(Error_bats)
REOF_Abats<-c(paste(round(REOF_A_bats,3),"%"))
REOF_Fbats<-c(paste(round(REOF_F_bats,3),"%"))
data.frame(correlation_bats,MSE_bats,RMSE_bats,MAPE_Mean_All,MAD_bats) # analysis of Error  by using Bats Model shows result of correlation ,MSE ,MPER
data.frame(validation_dates,Validation_day_name=validation_data_by_name,AEOF_bats,REOF_Abats,REOF_Fbats)   # Analysis of error shows result AEOF,REOF_A,REOF_F





## TBATS Model

# Data Modeling
data_series<-ts(training_data)
model_TBATS<-forecast:::fitSpecificTBATS(data_series,use.box.cox=FALSE, use.beta=TRUE,  seasonal.periods=c(6),use.damping=FALSE,k.vector=c(2))
accuracy(model_TBATS)  # accuracy on training data
# Print Model Parameters
model_TBATS
plot(model_TBATS,xlab = paste ("Time in  ", frequency ,y_lab , sep=" "),  col.main="black", col.lab="black", col.sub="black", cex.main=1, cex.lab=1, cex.sub=1,font.main=4, font.lab=4, ylab=y_lab)




# Testing Data Evaluation
forecasting_tbats <- predict(model_TBATS, h=N_forecasting_days+validation_data_days)
validation_forecast<-head(forecasting_tbats$mean,validation_data_days)
MAPE_Per_Day<-round(  abs(((testing_data-validation_forecast)/testing_data)*100)  ,3)
paste ("MAPE % For ",validation_data_days,frequency,"by using TBATS Model for  ==> ",y_lab, sep=" ")
MAPE_Mean_All<-paste(round(mean(MAPE_Per_Day),3),"% MAPE ",validation_data_days,frequency,y_lab,sep=" ")
MAPE_TBATS<-paste(round(MAPE_Per_Day,3),"%")
MAPE_TBATS_Model<-paste(MAPE_Per_Day ,"%")
paste (" MAPE that's Error of Forecasting for ",validation_data_days," days in TBATS Model for  ==> ",y_lab, sep=" ")
paste(MAPE_Mean_All,"%")
paste ("MAPE that's Error of Forecasting day by day for ",validation_data_days," days in TBATS Model for  ==> ",y_lab, sep=" ")
data.frame(date_TBATS=validation_dates,validation_data_by_name,actual_data=testing_data,forecasting_TBATS=validation_forecast,MAPE_TBATS_Model)
data.frame(FD,forecating_date=forecasting_data_by_name,forecasting_by_TBATS=tail(forecasting_tbats$mean,N_forecasting_days))
plot(forecasting_tbats)
x1_test <- ts(testing_data, start =(rows-validation_data_days+1) )
lines(x1_test, col='red',lwd=2)
graph2<-autoplot(forecasting_tbats,xlab = paste ("Time in  ", frequency ,y_lab , sep=" "),  col.main="black", col.lab="black", col.sub="black", cex.main=1, cex.lab=1, cex.sub=1,font.main=4, font.lab=4, ylab=y_lab)
graph2




## Error of forecasting TBATS Model

Error_tbats<-abs(testing_data-validation_forecast)  # Absolute error of forecast (AEOF)
REOF_A_tbats1<-abs(((testing_data-validation_forecast)/testing_data)*100)  #Relative error of forecast (divided by actual)(REOF_A)
REOF_F_tbats<-abs(((testing_data-validation_forecast)/validation_forecast)*100)  #Relative error of forecast (divided by forecast)(REOF_F)
correlation_tbats<-cor(testing_data,validation_forecast, method = c("pearson"))     # correlation coefficient between predicted and actual values 
RMSE_tbats<-sqrt(sum((Error_tbats^2))/validation_data_days)   #  Root mean square forecast error
MSE_tbats<-(sum((Error_tbats^2))/validation_data_days)   #  Root mean square forecast error
MAD_tbats<-abs((sum(testing_data-validation_forecast))/validation_data_days)   # average forecast accuracy
AEOF_tbats<-c(Error_tbats)
REOF_A_tbats<-c(paste(round(REOF_A_tbats1,3),"%"))
REOF_F_tbats<-c(paste(round(REOF_F_tbats,3),"%"))
data.frame(correlation_tbats,MSE_tbats,RMSE_tbats,MAPE_Mean_All,MAD_tbats) # analysis of Error  by using Holt's linear model shows result of correlation ,MSE ,MPER
data.frame(validation_dates,Validation_day_name=validation_data_by_name,AEOF_tbats,REOF_A_tbats,REOF_F_tbats)   # Analysis of error shows result AEOF,REOF_A,REOF_F




## Holt's linear trend


# Data Modeling
data_series<-ts(training_data)
model_holt<-holt(data_series,h=N_forecasting_days+validation_data_days,lambda = "auto")
accuracy(model_holt)  # accuracy on training data

# Print Model Parameters
summary(model_holt$model)






# Testing Data Evaluation
forecasting_holt <- predict(model_holt, h=N_forecasting_days+validation_data_days,lambda = "auto")
validation_forecast<-head(forecasting_holt$mean,validation_data_days)
MAPE_Per_Day<-round(  abs(((testing_data-validation_forecast)/testing_data)*100)  ,3)
paste ("MAPE % For ",validation_data_days,frequency,"by using holt Model for  ==> ",y_lab, sep=" ")
MAPE_Mean_All<-paste(round(mean(MAPE_Per_Day),3),"% MAPE ",validation_data_days,frequency,y_lab,sep=" ")
MAPE_holt<-paste(round(MAPE_Per_Day,3),"%")
MAPE_holt_Model<-paste(MAPE_Per_Day ,"%")
paste (" MAPE that's Error of Forecasting for ",validation_data_days," days in holt Model for  ==> ",y_lab, sep=" ")
paste(MAPE_Mean_All,"%")
paste ("MAPE that's Error of Forecasting day by day for ",validation_data_days," days in holt Model for  ==> ",y_lab, sep=" ")
data.frame(date_holt=validation_dates,validation_data_by_name,actual_data=testing_data,forecasting_holt=validation_forecast,MAPE_holt_Model)
data.frame(FD,forecating_date=forecasting_data_by_name,forecasting_by_holt=tail(forecasting_holt$mean,N_forecasting_days))
plot(forecasting_holt)
x1_test <- ts(testing_data, start =(rows-validation_data_days+1) )
lines(x1_test, col='red',lwd=2)
graph3<-autoplot(forecasting_holt,xlab = paste ("Time in  ", frequency ,y_lab , sep=" "),  col.main="black", col.lab="black", col.sub="black", cex.main=1, cex.lab=1, cex.sub=1,font.main=4, font.lab=4, ylab=y_lab)
graph3




## Error of forecasting by using Holt's linear model
Error_Holt<-abs(testing_data-validation_forecast)  # Absolute error of forecast (AEOF)
REOF_A_Holt1<-abs(((testing_data-validation_forecast)/testing_data)*100)  #Relative error of forecast (divided by actual)(REOF_A)
REOF_F_Holt<-abs(((testing_data-validation_forecast)/validation_forecast)*100)  #Relative error of forecast (divided by forecast)(REOF_F)
correlation_Holt<-cor(testing_data,validation_forecast, method = c("pearson"))     # correlation coefficient between predicted and actual values 
RMSE_Holt<-sqrt(sum((Error_Holt^2))/validation_data_days)   #  Root mean square forecast error
MSE_Holt<-(sum((Error_Holt^2))/validation_data_days)   #  Root mean square forecast error
MAD_Holt<-abs((sum(testing_data-validation_forecast))/validation_data_days)   # average forecast accuracy
AEOF_Holt<-c(Error_Holt)
REOF_A_Holt<-c(paste(round(REOF_A_Holt1,3),"%"))
REOF_F_Holt<-c(paste(round(REOF_F_Holt,3),"%"))
REOF_A_Holt11<-mean(abs(((testing_data-validation_forecast)/testing_data)*100))
data.frame(correlation_Holt,MSE_Holt,RMSE_Holt,MAPE_Mean_All,MAD_Holt) # analysis of Error  by using Holt's linear model shows result of correlation ,MSE ,MPER
data.frame(validation_dates,Validation_day_name=validation_data_by_name,AEOF_Holt,REOF_A_Holt,REOF_F_Holt)   # Analysis of error shows result AEOF,REOF_A,REOF_F




#Auto arima model
##################

require(tseries) # need to install tseries tj test Stationarity in time series 
paste ("tests For Check Stationarity in series  ==> ",y_lab, sep=" ")
kpss.test(data_series) # applay kpss test
pp.test(data_series)   # applay pp test
adf.test(data_series)  # applay adf test
ndiffs(data_series)    # Doing first diffrencing on data



#Taking the first difference
diff1_x1<-diff(data_series)
autoplot(diff1_x1, xlab = paste ("Time in  ", frequency ,y_lab , sep=" "),  col.main="black", col.lab="black", col.sub="black", cex.main=1, cex.lab=1, cex.sub=1,font.main=4, font.lab=4, ylab=y_lab,main = "1nd differenced series")



##Testing the stationary of the first differenced series
paste ("tests For Check Stationarity in series after taking first differences in  ==> ",y_lab, sep=" ")
kpss.test(diff1_x1)   # applay kpss test after taking first differences
pp.test(diff1_x1)     # applay pp test after taking first differences
adf.test(diff1_x1)    # applay adf test after taking first differences



#Taking the second difference
diff2_x1=diff(diff1_x1)
autoplot(diff2_x1, xlab = paste ("Time in  ", frequency ,y_lab , sep=" "),  col.main="black", col.lab="black", col.sub="black", cex.main=1, cex.lab=1, cex.sub=1,font.main=4, font.lab=4, ylab=y_lab ,main = "2nd differenced series")



##Testing the stationary of the first differenced series
paste ("tests For Check Stationarity in series after taking Second differences in",y_lab, sep=" ")
kpss.test(diff2_x1)   # applay kpss test after taking Second differences
pp.test(diff2_x1)     # applay pp test after taking Second differences
adf.test(diff2_x1)    # applay adf test after taking Second differences



####Fitting an ARIMA Model
#1. Using auto arima function
model1 <- auto.arima(data_series,stepwise=FALSE, approximation=FALSE, trace=T, test = c("kpss", "adf", "pp"))  #applaying auto arima
model1 # show the result of autoarima 
#Make changes in the source of auto arima to run the best model
arima.string <- function (object, padding = FALSE) 
{
  order <- object$arma[c(1, 6, 2, 3, 7, 4, 5)]
  m <- order[7]
  result <- paste("ARIMA(", order[1], ",", order[2], ",", 
                  order[3], ")", sep = "")
  if (m > 1 && sum(order[4:6]) > 0) {
    result <- paste(result, "(", order[4], ",", order[5], 
                    ",", order[6], ")[", m, "]", sep = "")
  }
  if (padding && m > 1 && sum(order[4:6]) == 0) {
    result <- paste(result, "         ", sep = "")
    if (m <= 9) {
      result <- paste(result, " ", sep = "")
    }
    else if (m <= 99) {
      result <- paste(result, "  ", sep = "")
    }
    else {
      result <- paste(result, "   ", sep = "")
    }
  }
  if (!is.null(object$xreg)) {
    if (NCOL(object$xreg) == 1 && is.element("drift", names(object$coef))) {
      result <- paste(result, "with drift        ")
    }
    else {
      result <- paste("Regression with", result, "errors")
    }
  }
  else {
    if (is.element("constant", names(object$coef)) || is.element("intercept", 
                                                                 names(object$coef))) {
      result <- paste(result, "with non-zero mean")
    }
    else if (order[2] == 0 && order[5] == 0) {
      result <- paste(result, "with zero mean    ")
    }
    else {
      result <- paste(result, "                  ")
    }
  }
  if (!padding) {
    result <- gsub("[ ]*$", "", result)
  }
  return(result)
}






source("stringthearima.R")  
bestmodel <- arima.string(model1, padding = TRUE)
bestmodel <- substring(bestmodel,7,11)
bestmodel <- gsub(" ", "", bestmodel)
bestmodel <- gsub(")", "", bestmodel)
bestmodel <- strsplit(bestmodel, ",")[[1]]
bestmodel <- c(strtoi(bestmodel[1]),strtoi(bestmodel[2]),strtoi(bestmodel[3]))
bestmodel
strtoi(bestmodel[3])



#2. Using ACF and PACF Function
#par(mfrow=c(1,2))  # Code for making two plot in one graph 
acf(diff2_x1,xlab = paste ("Time in  ", frequency ,y_lab , sep=" "),  col.main="black", col.lab="black", col.sub="black", cex.main=1, cex.lab=1, cex.sub=1,font.main=4, font.lab=4, ylab=y_lab, main=paste("ACF-2nd differenced series ",y_lab, sep=" ",lag.max=20))    # plot ACF "auto correlation function after taking second diffrences
pacf(diff2_x1,xlab = paste ("Time in  ", frequency ,y_lab , sep=" "),  col.main="black", col.lab="black", col.sub="black", cex.main=1, cex.lab=1, cex.sub=1,font.main=4, font.lab=4, ylab=y_lab,main=paste("PACF-2nd differenced series ",y_lab, sep=" ",lag.max=20))   # plot PACF " Partial auto correlation function after taking second diffrences
library(forecast)   # install library forecast             
x1_model1= arima(data_series, order=c(bestmodel)) # Run Best model of auto arima  for forecasting
x1_model1  # Show result of best model of auto arima 
paste ("accuracy of autoarima Model For  ==> ",y_lab, sep=" ")
accuracy(x1_model1)  # aacuracy of best model from auto arima
x1_model1$x          # show result of best model from auto arima 
checkresiduals(x1_model1,xlab = paste ("Time in  ", frequency ,y_lab , sep=" "),  col.main="black", col.lab="black", col.sub="black", cex.main=1, cex.lab=1, cex.sub=1,font.main=4, font.lab=4, ylab=y_lab)  # checkresiduals from best model from using auto arima 




paste("Box-Ljung test , Ljung-Box test For Modelling for   ==> ",y_lab, sep=" ")
Box.test(x1_model1$residuals^2, lag=20, type="Ljung-Box")   # Do test for resdulas by using Box-Ljung test , Ljung-Box test For Modelling
library(tseries)
jarque.bera.test(x1_model1$residuals)  # Do test jarque.bera.test 
#Actual Vs Fitted
plot(data_series, col='red',lwd=2, main="Actual vs Fitted Plot", xlab='Time in (days)', ylab=y_lab) # plot actual and Fitted model 
lines(fitted(x1_model1), col='black')



#Test data

x1_test <- ts(testing_data, start =(rows-validation_data_days+1) ) # make testing data in time series and start from rows-6
forecasting_auto_arima <- forecast(x1_model1, h=N_forecasting_days+validation_data_days)
validation_forecast<-head(forecasting_auto_arima$mean,validation_data_days)
MAPE_Per_Day<-round(abs(((testing_data-validation_forecast)/testing_data)*100)  ,3)
paste ("MAPE % For ",validation_data_days,frequency,"by using bats Model for  ==> ",y_lab, sep=" ")
MAPE_Mean_All<-paste(round(mean(MAPE_Per_Day),3),"% MAPE ",validation_data_days,frequency,y_lab,sep=" ")
MAPE_auto_arima<-paste(round(MAPE_Per_Day,3),"%")
MAPE_auto.arima_Model<-paste(MAPE_Per_Day ,"%")
paste (" MAPE that's Error of Forecasting for ",validation_data_days," days in bats Model for  ==> ",y_lab, sep=" ")
paste(MAPE_Mean_All,"%")
paste ("MAPE that's Error of Forecasting day by day for ",validation_data_days," days in bats Model for  ==> ",y_lab, sep=" ")
data.frame(date_auto.arima=validation_dates,validation_data_by_name,actual_data=testing_data,forecasting_auto.arima=validation_forecast,MAPE_auto.arima_Model)
data.frame(FD,forecating_date=forecasting_data_by_name,forecasting_by_auto.arima=tail(forecasting_auto_arima$mean,N_forecasting_days))
plot(forecasting_auto_arima)
x1_test <- ts(testing_data, start =(rows-validation_data_days+1) )
lines(x1_test, col='red',lwd=2)
graph4<-autoplot(forecasting_auto_arima,xlab = paste ("Time in  ", frequency ,y_lab , sep=" "),  col.main="black", col.lab="black", col.sub="black", cex.main=1, cex.lab=1, cex.sub=1,font.main=4, font.lab=4, ylab=y_lab)
graph4



## Error of forecasting
Error_auto.arima<-abs(testing_data-validation_forecast)  # Absolute error of forecast (AEOF)
REOF_A_auto.arima<-abs(((testing_data-validation_forecast)/testing_data)*100)  #Relative error of forecast (divided by actual)(REOF_A)
REOF_F_auto.arima<-abs(((testing_data-validation_forecast)/validation_forecast)*100)  #Relative error of forecast (divided by forecast)(REOF_F)
correlation_auto.arima<-cor(testing_data,validation_forecast, method = c("pearson"))     # correlation coefficient between predicted and actual values 
RMSE_auto.arima<-sqrt(sum((Error_auto.arima^2))/validation_data_days)   #  Root mean square forecast error
MSE_auto.arima<-(sum((Error_auto.arima^2))/validation_data_days)   #  Root mean square forecast error
MAD_auto.arima<-abs((sum(testing_data-validation_forecast))/validation_data_days)   # average forecast accuracy
AEOF_auto.arima<-c(Error_auto.arima)
REOF_auto.arima1<-c(paste(round(REOF_A_auto.arima,3),"%"))
REOF_auto.arima2<-c(paste(round(REOF_F_auto.arima,3),"%"))
data.frame(correlation_auto.arima,MSE_auto.arima,RMSE_auto.arima,MAPE_Mean_All,MAD_auto.arima) # analysis of Error  by using Holt's linear model shows result of correlation ,MSE ,MPER
data.frame(validation_dates,Validation_day_name=validation_data_by_name,AEOF_auto.arima,REOF_A_auto.arima=REOF_auto.arima1,REOF_F_auto.arima=REOF_auto.arima2)   # Analysis of error shows result AEOF,REOF_A,REOF_F




# SIR Model 
#install.packages("dplyr")
library(deSolve)
first<-rows-13
secondr<-rows-7
vector_SIR<-original_data[first:secondr]
Infected <- c(vector_SIR)
Day <- 1:(length(Infected))
N <- Population # population of the us
SIR <- function(time, state, parameters) {
  par <- as.list(c(state, parameters))
  with(par, {
    dS <- -beta/N * I * S
    dI <- beta/N * I * S - gamma * I
    dR <- gamma * I
    list(c(dS, dI, dR))
  })
}

init <- c(S = N-Infected[1], I = Infected[1], R = 0)
RSS <- function(parameters) {
  names(parameters) <- c("beta", "gamma")
  out <- ode(y = init, times = Day, func = SIR, parms = parameters)
  fit <- out[ , 3]
  sum((Infected - fit)^2)
}

# optimize with some sensible conditions
Opt <- optim(c(0.5, 0.5), RSS, method = "L-BFGS-B", 
             lower = c(0, 0), upper = c(10, 10))
Opt$message

Opt_par <- setNames(Opt$par, c("beta", "gamma"))
Opt_par
# beta     gamma 
# 0.6512503 0.4920399 

out <- ode(y = init, times = Day, func = SIR, parms = Opt_par)

plot(out)
plot(out, obs=data.frame(time=Day, I=Infected))
result_SIR<-data.frame(out)
validation_forecast<-result_SIR$I





## Error of forecasting
Error_SIR<-abs(testing_data-validation_forecast)  # Absolute error of forecast (AEOF)
REOF_A_SIR<-abs(((testing_data-validation_forecast)/testing_data)*100)  #Relative error of forecast (divided by actual)(REOF_A)
REOF_F_SIR<-abs(((testing_data-validation_forecast)/validation_forecast)*100)  #Relative error of forecast (divided by forecast)(REOF_F)
correlation_SIR<-cor(testing_data,validation_forecast, method = c("pearson"))     # correlation coefficient between predicted and actual values 
RMSE_SIR<-sqrt(sum((Error_SIR^2))/validation_data_days)   #  Root mean square forecast error
MSE_SIR<-(sum((Error_SIR^2))/validation_data_days)   #  Root mean square forecast error
MAD_SIR<-abs((sum(testing_data-validation_forecast))/validation_data_days)   # average forecast accuracy
AEOF_SIR<-c(Error_SIR)
REOF_A_SIR<-c(paste(round(REOF_A_SIR,3),"%"))
REOF_A_SIR1<-mean(abs(((testing_data-validation_forecast)/testing_data)*100))

REOF_F_SIR<-c(paste(round(REOF_F_SIR,3),"%"))
MAPE_Mean_All<-paste(round(mean(abs(((testing_data-validation_forecast)/testing_data)*100)),3),"% MAPE ",validation_data_days,frequency,y_lab,sep=" ")
data.frame(correlation_SIR,MSE_SIR,RMSE_SIR,MAPE_Mean_All,MAD_SIR) # analysis of Error  by using SIR's linear model shows result of correlation ,MSE ,MPER
data.frame(validation_dates,Validation_day_name=validation_data_by_name,AEOF_SIR,REOF_A_SIR,REOF_F_SIR,validation_forecast,testing_data)   # Analysis of error shows result AEOF,REOF_A,REOF_F



## forecasting by SIR model

Infected <- c(tail(original_data,validation_data_days))
Day <- 1:(length(Infected))
N <- Population # population of the us

SIR <- function(time, state, parameters) {
  par <- as.list(c(state, parameters))
  with(par, {
    dS <- -beta/N * I * S
    dI <- beta/N * I * S - gamma * I
    dR <- gamma * I
    list(c(dS, dI, dR))
  })
}

init <- c(S = N-Infected[1], I = Infected[1], R = 0)
RSS <- function(parameters) {
  names(parameters) <- c("beta", "gamma")
  out <- ode(y = init, times = Day, func = SIR, parms = parameters)
  fit <- out[ , 3]
  sum((Infected - fit)^2)
}

# optimize with some sensible conditions
Opt <- optim(c(0.5, 0.5), RSS, method = "L-BFGS-B", 
             lower = c(0, 0), upper = c(10, 10))
Opt$message

Opt_par <- setNames(Opt$par, c("beta", "gamma"))
Opt_par
# beta     gamma 
# 0.6512503 0.4920399 

out <- ode(y = init, times = Day, func = SIR, parms = Opt_par)

plot(out)
plot(out, obs=data.frame(time=Day, I=Infected))
result_SIR <-data.frame(out)
data.frame(FD,forecating_date=forecasting_data_by_name,forecasting_by_SIR=result_SIR$I)






# Choose Best model by least error

paste("System Summarizes  Error ==> ( MAPE ) of Forecasting  by using bats model and BATS Model, Holt's Linear Models , and autoarima for  ==> ", y_lab , sep=" ")
M1<-mean(REOF_A_bats)

paste("System Summarizes  Error ==> ( MAPE ) of Forecasting  by using TBATS  Model For ==> ", y_lab , sep=" ")
M2<-mean(REOF_A_tbats1)

paste("System Summarizes  Error ==> ( MAPE ) of Forecasting  by using Holt's Linear << Exponential Smoothing >>  For ==> ", y_lab , sep=" ")
M3<-REOF_A_Holt11

paste("System Summarizes  Error ==> ( MAPE ) of Forecasting  by using auto arima  Model For ==> ", y_lab , sep=" ")
M4<-mean(REOF_A_auto.arima)
paste("System Summarizes  Error ==> ( MAPE ) of Forecasting  by using SIR Model For ==> ", y_lab , sep=" ")
M5<-REOF_A_SIR1

paste("System Summarizes  Error ==> ( MAPE ) of Forecasting  by using autoarima  Model For ==> ", y_lab , sep=" ")
data.frame(validation_dates,forecating_date=forecasting_data_by_name,MAPE_bats_error=REOF_A_bats,MAPE_TBATS_error=REOF_A_tbats1,MAPE_Holt_error=REOF_A_Holt1,MAPE_autoarima_error = REOF_A_auto.arima)
recommend_Model<-c(M1,M2,M3,M4,M5)
best_recommended_model<-min(recommend_Model)
paste ("lodaing .....   ... . .Select Minimum MAPE from Models for select best Model ==> ", y_lab , sep=" ")
best_recommended_model
paste ("Best Model For Forecasting  ==> ",y_lab, sep=" ")
if(best_recommended_model >= M1) {paste("System Recommend Bats Model That's better  For forecasting==> ",y_lab, sep=" ")}
if(best_recommended_model >= M2) {paste("System Recommend  That's better TBATS  For forecasting ==> ",y_lab, sep=" ")}
if(best_recommended_model >= M3) {paste("System Recommend Holt's Linear Model < Exponential Smoothing Model >   That's better  For forecasting ==> ",y_lab, sep=" ")}
if(best_recommended_model >= M4) {paste("System Recommend auto arima Model  That's better  For forecasting ==> ",y_lab, sep=" ")}
if(best_recommended_model >= M5) {paste("System Recommend SIR Model  That's better  For forecasting ==> ",y_lab, sep=" ")}



message("System finished Forecasting  by using autoarima and Holt's ,TBATS, and SIR  Model ==>",y_lab, sep=" ")
message(" Thank you for using our System For Modelling  ==> ",y_lab, sep=" ")
