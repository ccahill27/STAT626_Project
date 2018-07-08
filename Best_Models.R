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

Houston.limit<-read.csv('houston_91to12_all.csv',header=TRUE)

Philly.limit<-read.csv('philly_91to12_all.csv',header=TRUE)




#############################################
###########   PHILADELPHIA  MODEL ###########
#############################################

p<-Philly.limit$Pneumonia_and_Flu_Deaths
lp<-log(p)
dlp<-diff(lp)
sarima(lp,0,1,1, 0,1,1,12)
title('Philadelphia                                                             ')

fit.p<-arima(lp,order=c(0,1,1),seasonal=list(order=c(0,1,1),12))
Box.test(fit.p,type='Ljung-Box')

########################################
###########   HOUSTON  MODEL ###########
########################################

h<-Houston.limit$Pneumonia_and_Flu_Deaths
boxcox(h~time(h))
hT<-h^.95
dhT<-diff(hT)
sarima(hT,0,1,1, 3,1,3,12)
title('Houston                                                             ')




bcs <- boxcoxfit(x, hou, lambda2 = TRUE)
bcs$lambda
houb <- h^.95
dbhou=diff(houb,1)
sdbhou=diff(dbhou,12)
acf2(sdbhou, max.lag=80)
sarima(houb, 0,1,1, 3,1,3,12)






################################################
###########   PHILADELPHIA  FORECAST ###########
################################################

sarima.for(lp,12,0,1,1, 0,1,1,12)
title('Philadelphia')



###########################################
###########   HOUSTON  FORECAST ###########
###########################################
houston.future<-Houston_full[611:nrow(Houston_full),]
hf<-houston.future$Pneumonia_and_Flu_Deaths

p1<-0
d<-1
q<-1
P<-2
D<-1
Q<- 3 

fit1<-arima(hT,order=c(p1,d,q),seasonal=list(order=c(P,D,Q),12))
Box.test(fit1$residuals)

sarima.for(hT,36,p1,d,q,P,D,Q,12)


points(276:311,(hf[1:36])^.95,col=4)
lines(276:311,(hf[1:36])^.95,col=4)
legend(180,0,legend=c('Predicted','Observed'),col=c(2,4),pch=20,cex=2)
title('Houston')










