library(forecast)
em_house_prices <- read_csv("Desktop/20353100-MATH4022-CW/em_house_prices.csv")
avg_house<-ts(em_house_prices$average_price_gbp,start=2010,frequency=12)
tsdisplay(avg_house,main="Time plot of monthly average house prices in the East Midlands (2010-2019)")

#Removing stationary
s1<-diff(avg_house)
tsdisplay(s1)

#Removing seasonally
s12<-diff(s1,12)
tsdisplay(s12)

#First difference of the seasonally
avg_house_diff1<-diff(avg_house_diff12)
tsdisplay(avg_house_diff1)

#LB SARIMA test
LB_test_SARIMA<-function(resid,max.k,p,q,P,Q){
  lb_result<-list()
  df<-list()
  p_value<-list()
  for(i in (p+q+P+Q+1):max.k){
    lb_result[[i]]<-Box.test(resid,lag=i,type=c("Ljung-Box"),fitdf=(p+q+P+Q))
    df[[i]]<-lb_result[[i]]$parameter
    p_value[[i]]<-lb_result[[i]]$p.value
  }
  df<-as.vector(unlist(df))
  p_value<-as.vector(unlist(p_value))
  test_output<-data.frame(df,p_value)
  names(test_output)<-c("deg_freedom","LB_p_value")
  return(test_output)
}

# ARIMA(1, 1, 1) × (1,1,0) 12  
t1<-arima(avg_house,order=c(1,1,1), seasonal=list(order=c(1,1,0),period=12), method="ML")
arima(x = avg_house, order=c(1,1,1), seasonal=list(order=c(1,1,0),period=12), method = "ML")
tsdiag(t1)

# ARIMA(1, 1, 1) × (1,1,1) 12  
t2<-arima(avg_house,order=c(1,1,1), seasonal=list(order=c(1,1,1),period=12), method="ML")
arima(x = avg_house, order=c(1,1,1), seasonal=list(order=c(1,1,1),period=12), method = "ML")
tsdiag(t2)

# ARIMA(1, 1, 1) × (0, 1, 0) 12 
t3<-arima(avg_house,order=c(1,1,1), seasonal=list(order=c(0,1,0),period=12), method="ML")
arima(x = avg_house, order=c(1,1,1), seasonal=list(order=c(0,1,0),period=12), method = "ML")
tsdiag(t3)

# ARIMA(1, 1, 1) × (0, 1, 1) 12 
t4<-arima(avg_house,order=c(1,1,1), seasonal=list(order=c(0,1,1),period=12), method="ML")
arima(x = avg_house, order=c(1,1,1), seasonal=list(order=c(0,1,1),period=12), method = "ML")
tsdiag(t4)

# ARIMA(1, 1, 2) × (0, 1, 1) 12  best model
modelbest<-arima(avg_house,order=c(1,1,2), seasonal=list(order=c(0,1,1),period=12), method="ML")
arima(x = avg_house, order=c(1,1,2), seasonal=list(order=c(0,1,1),period=12), method = "ML")
resid<-residuals(modelbest)
par(mfrow=c(3,1))
ts.plot(resid)
acf(resid,main="Series resid")
modelbest.LB<-LB_test_SARIMA(resid,max.k=12,p=1,q=2,P=0,Q=1)
modelbest.LB
plot(modelbest.LB$deg_freedom,modelbest.LB$LB_p_value,xlab="Degrees of freedom",ylab="P-value",main="Ljung-Box test P-values",ylim=c(0,1))
abline(h=0.05,col="blue",lty=2)

# ARIMA(1, 1, 2) × (0,1,0) 12  
t5<-arima(avg_house,order=c(1,1,2), seasonal=list(order=c(0,1,0),period=12), method="ML")
arima(x = avg_house, order=c(1,1,2), seasonal=list(order=c(0,1,0),period=12), method = "ML")
tsdiag(t5)

# ARIMA(1, 1, 2) × (1,1,1) 12  
t6<-arima(avg_house,order=c(1,1,2), seasonal=list(order=c(1,1,1),period=12), method="ML")
arima(x = avg_house, order=c(1,1,2), seasonal=list(order=c(1,1,1),period=12), method = "ML")
tsdiag(t6)

# ARIMA(1, 1, 2) × (1，1，0) 12  
t7<-arima(avg_house,order=c(1,1,2), seasonal=list(order=c(1,1,0),period=12), method="ML")
arima(x = avg_house, order=c(1,1,2), seasonal=list(order=c(1,1,0),period=12), method = "ML")
tsdiag(t7)

# ARIMA(2, 1, 1) × (1，1，0) 12  
t8<-arima(avg_house,order=c(2,1,1), seasonal=list(order=c(1,1,0),period=12), method="ML")
arima(x = avg_house, order=c(2,1,1), seasonal=list(order=c(1,1,0),period=12), method = "ML")
tsdiag(t8)

# ARIMA(2, 1, 1) × (0，1，1) 12  
t9<-arima(avg_house,order=c(2,1,1), seasonal=list(order=c(0,1,1),period=12), method="ML")
arima(x = avg_house, order=c(2,1,1), seasonal=list(order=c(0,1,1),period=12), method = "ML")
tsdiag(t9)

# ARIMA(2, 1, 1) × (0，1，0) 12  
t10<-arima(avg_house,order=c(2,1,1), seasonal=list(order=c(0,1,0),period=12), method="ML")
arima(x = avg_house, order=c(2,1,1), seasonal=list(order=c(0,1,0),period=12), method = "ML")
tsdiag(t10)

# ARIMA(2, 1, 1) × (1，1，1) 12  
t11<-arima(avg_house,order=c(2,1,1), seasonal=list(order=c(1,1,1),period=12), method="ML")
arima(x = avg_house, order=c(2,1,1), seasonal=list(order=c(1,1,1),period=12), method = "ML")
tsdiag(t11)

#Forecast for the next 6 months (h=6) from the fitted
fc_6m<-forecast(avg_house,h=6,model=modelbest,level=95)
fc_6m
#Plot the forecasted values
autoplot(fc_6m)

#Assuming we don't know the data for 2019 to forcast
x<-ts(em_house_prices$average_price_gbp,start=2010,end=2019,frequency=12)
mo<-arima(x,order=c(1,1,2), seasonal=list(order=c(0,1,1),period=12), method="ML")
fc_2019<-forecast(x,h=6,model=mo,level=95)

#We extract the values of the series for 2017 and 2018
price17_18<-ts(avg_house[86:109],start=2017,frequency=12)

#We read in the true number of average_price in 2019 as a time series of length 6
Actual2019<-ts(c(191237,190990,190586,191571,191927,192029),frequency=12,start=2019)

#Using the model and forecasts fitted earlier combine all series into one object, for plotting
price_fit<-cbind(fc_2019$mean,fc_2019$upper,fc_2019$lower,price17_18,Actual2019)

#Plot the true values for 2017 and 2018 and the forecasted and true values for the first six months of 2019 (with 95% conf. int.)
autoplot(price_fit)