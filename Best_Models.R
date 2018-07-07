#Best Models




setwd(dirname(rstudioapi::getSourceEditorContext()$path))
library(astsa)
library(data.table)
library(car)
library(MASS)
library(imputeTS)
library(tseries)
library(forecast)


######################################
###########   IMPORT  DATA ###########
######################################



Houston_full<-read.csv('Houston_monthly_all.csv',header=TRUE)

Philly_full<-read.csv('Philadelphia_monthly_all.csv',header=TRUE)

Houston_limit<-read.csv('houston_limited_all.csv',header=TRUE)

Philly_limit<-read.csv('philly_limited_all.csv',header=TRUE)




#############################################
###########   PHILADELPHIA  MODEL ###########
#############################################

p<-Philly_limit$Pneumonia_and_Flu_Deaths
lp<-log(p)
dlp<-diff(lp)
sarima(lp,0,1,1, 0,1,1,12)
title('Philadelphia                                                             ')




########################################
###########   HOUSTON  MODEL ###########
########################################

h<-Houston_limit$Pneumonia_and_Flu_Deaths
boxcox(h~time(h))
hT<-h^.95
dhT<-diff(hT)
sarima(hT,0,1,1, 3,1,3,12)
title('Houston                                                             ')





################################################
###########   PHILADELPHIA  FORECAST ###########
################################################

sarima.for(lp,12,0,1,1, 0,1,1,12)
title('Philadelphia')



###########################################
###########   HOUSTON  FORECAST ###########
###########################################
houston.future<-Houston_full[600:nrow(Houston_full),]
hf<-houston.future$Pneumonia_and_Flu_Deaths

p1<-0
d<-1
q<-1
P<-3
D<-1
Q<- 3 

fit1<-arima(hT,order=c(p1,d,q),seasonal=list(order=c(P,D,Q),12))
Box.test(fit1$residuals)

sarima.for(hT,36,p1,d,q,P,D,Q,12)


points(264:299,(hf[1:36])^.95,col=4)
lines(264:299,(hf[1:36])^.95,col=4)
legend(180,0,legend=c('Predicted','Observed'),col=c(2,4),pch=20,cex=2)
title('Houston')










