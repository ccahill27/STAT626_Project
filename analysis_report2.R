rm(list=ls())


setwd(dirname(rstudioapi::getSourceEditorContext()$path))
library(astsa)
library(data.table)
library(car)
library(MASS)
library(imputeTS)
library(tseries)
library(forecast)


##########################################
###########   IMPORT CITY DATA ###########
##########################################

Houston<-read.csv('Houston_weekly_full_data.csv',header=TRUE)

Philly<-read.csv('Philadelphia_weekly_full_data.csv',header=TRUE)


#############################################
###########   IMPUTE MISSING DATA ###########
#############################################

Philly[is.na(Philly$Pneumonia_and_Flu_Deaths),c(1,2,6,7,8,15:18)]
missing.philly<-Philly[is.na(Philly$Pneumonia_and_Flu_Deaths),1]
Philly$Pneumonia_and_Flu_Deaths<-na.interpolation(Philly$Pneumonia_and_Flu_Deaths)
Philly[missing.philly,c(1,2,6,7,8,15:18)]



Houston[is.na(Houston$Pneumonia_and_Flu_Deaths),c(1,2,6,7,8,15:18)]
missing.houston<-Houston[is.na(Houston$Pneumonia_and_Flu_Deaths),1]
Houston$Pneumonia_and_Flu_Deaths<-na.interpolation((Houston$Pneumonia_and_Flu_Deaths))
Houston[missing.houston,c(1,2,6,7,8,15:18)]



#################################################
###########   CONVERT TO MONTHLY DATA ###########
#################################################

#Philly

Philly$MONTH<-substr(Philly$End_of_Week,1,7)

#Aggregate over week

Philly_monthly<-aggregate(PRCP ~ MONTH, FUN=sum, data=Philly, na.rm=FALSE)
Philly_monthly$TMAX<-aggregate(TMAX ~ MONTH, FUN=mean, data=Philly, na.rm=FALSE)[,2]
Philly_monthly$TMIN<-aggregate(TMIN ~ MONTH, FUN=mean, data=Philly, na.rm=FALSE)[,2]
Philly_monthly$TAVG<-aggregate(TAVG ~ MONTH, FUN=mean, data=Philly, na.rm=FALSE)[,2]
Philly_monthly$Pneumonia_and_Flu_Deaths<-aggregate(Pneumonia_and_Flu_Deaths ~ MONTH, FUN=sum, data=Philly, na.rm=FALSE)[,2]

#Houston

Houston$MONTH<-substr(Houston$End_of_Week,1,7)

#Aggregate over week

Houston_monthly<-aggregate(PRCP ~ MONTH, FUN=sum, data=Houston, na.rm=FALSE)
Houston_monthly$TMAX<-aggregate(TMAX ~ MONTH, FUN=mean, data=Houston, na.rm=FALSE)[,2]
Houston_monthly$TMIN<-aggregate(TMIN ~ MONTH, FUN=mean, data=Houston, na.rm=FALSE)[,2]
Houston_monthly$TAVG<-aggregate(TAVG ~ MONTH, FUN=mean, data=Houston, na.rm=FALSE)[,2]
Houston_monthly$Pneumonia_and_Flu_Deaths<-aggregate(Pneumonia_and_Flu_Deaths ~ MONTH, FUN=sum, data=Houston, na.rm=FALSE)[,2]


##############################################
###########   LIMIT THE DATE RANGE ###########
##############################################

#Break Month into 2 columns

Houston_monthly$YEAR<-as.numeric(substr(Houston_monthly$MONTH,1,4))
Houston_monthly$month_only<-as.numeric(substr(Houston_monthly$MONTH,6,7))

Philly_monthly$YEAR<-as.numeric(substr(Philly_monthly$MONTH,1,4))
Philly_monthly$month_only<-as.numeric(substr(Philly_monthly$MONTH,6,7))


#Limit range

begin.year=1991
end.year=2012  


Houston.limit<-Houston_monthly[Houston_monthly$YEAR >= begin.year & Houston_monthly$YEAR <= end.year,]

Philly.limit<-Philly_monthly[Philly_monthly$YEAR >= begin.year & Philly_monthly$YEAR <= end.year,]



#This is a manual step because the Philadelphia range only goes to 2012-11.

Houston.limit<-Houston.limit[-264,]



################################################
###########   CREATE STATIONARY DATA ###########
################################################


h<-Houston.limit$Pneumonia_and_Flu_Deaths


lh<-log(h)

tsplot(lh)
acf2(lh)


auto.arima(lh)

dlh<-diff(lh)
acf2(dlh)
ddlh<-diff(lh,12)
acf2(ddlh)


plot.ts(cbind(h,lh,dlh,ddlh),yax.flip=TRUE,main="")


sarima(lh,1,1,1, 1,1,0,12)


p<-Philly.limit$Pneumonia_and_Flu_Deaths

lp<-log(p)

tsplot(lp)
acf2(lp)


auto.arima(lp)

dlp<-diff(lp)
acf2(dlp)
ddlp<-diff(lp,12)
acf2(ddlp)


plot.ts(cbind(p,lp,dlp,ddlp),yax.flip=TRUE,main="")


sarima(lp,0,1,1, 0,1,1,12)








#######################################
###########   FORECAST DATA ###########
#######################################

sarima.for(lp,12,0,1,1, 0,1,1,12)








