<<<<<<< HEAD
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

#Flu

Philly[is.na(Philly$Pneumonia_and_Flu_Deaths),c(1,2,6,7,8,15:18)]
missing.philly<-Philly[is.na(Philly$Pneumonia_and_Flu_Deaths),1]
Philly$Pneumonia_and_Flu_Deaths<-na.interpolation(Philly$Pneumonia_and_Flu_Deaths)
Philly[missing.philly,c(1,2,6,7,8,15:18)]



Houston[is.na(Houston$Pneumonia_and_Flu_Deaths),c(1,2,6,7,8,15:18)]
missing.houston<-Houston[is.na(Houston$Pneumonia_and_Flu_Deaths),1]
Houston$Pneumonia_and_Flu_Deaths<-na.interpolation((Houston$Pneumonia_and_Flu_Deaths))
Houston[missing.houston,c(1,2,6,7,8,15:18)]




#Total Deaths
Philly[is.na(Philly$All_Deaths),c(1,2,6,7,8,9,15:18)]
missing.philly<-Philly[is.na(Philly$All_Deaths),1]
Philly$All_Deaths<-na.interpolation(Philly$All_Deaths)
Philly[missing.philly,c(1,2,6,7,8,9,15:18)]

Houston[is.na(Houston$All_Deaths),c(1,2,6,7,8,9,15:18)]
missing.houston<-Houston[is.na(Houston$All_Deaths),1]
Houston$All_Deaths<-na.interpolation(Houston$All_Deaths)
Houston[missing.houston,c(1,2,6,7,8,9,15:18)]






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
Philly_monthly$All_Deaths<-aggregate(All_Deaths ~ MONTH, FUN=sum, data=Philly, na.rm=FALSE)[,2]

#Houston

Houston$MONTH<-substr(Houston$End_of_Week,1,7)

#Aggregate over week

Houston_monthly<-aggregate(PRCP ~ MONTH, FUN=sum, data=Houston, na.rm=FALSE)
Houston_monthly$TMAX<-aggregate(TMAX ~ MONTH, FUN=mean, data=Houston, na.rm=FALSE)[,2]
Houston_monthly$TMIN<-aggregate(TMIN ~ MONTH, FUN=mean, data=Houston, na.rm=FALSE)[,2]
Houston_monthly$TAVG<-aggregate(TAVG ~ MONTH, FUN=mean, data=Houston, na.rm=FALSE)[,2]
Houston_monthly$Pneumonia_and_Flu_Deaths<-aggregate(Pneumonia_and_Flu_Deaths ~ MONTH, FUN=sum, data=Houston, na.rm=FALSE)[,2]
Houston_monthly$All_Deaths<-aggregate(All_Deaths ~ MONTH, FUN=sum, data=Houston, na.rm=FALSE)[,2]

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


v<-sarima(lp,0,1,1, 0,1,1,12)


write.csv(Philly.limit,'philly_limited_all.csv')
write.csv(Houston.limit,'houston_limited_all.csv')




#######################################
###########   FORECAST DATA ###########
#######################################

sarima.for(lp,12,0,1,1, 0,1,1,12)



########################################
###########   Write CSV DATA ###########
########################################


write.csv(Philly_monthly,'Philadelphia_monthly_all.csv')
write.csv(Houston_monthly,'Houston_monthly_all.csv')


Philly_monthly<-read.csv('Philadelphia_monthly.csv',header=TRUE)
Houston_monthly<-read.csv('Houston_monthly.csv',header=TRUE)
p<-H


#Find AIC
library(gtools)
library(googlesheets)

gs_auth()

gs_ls()
gs_houston<-gs_title('houston_models')
AIC.table<-gs_read(gs_houston,ws=1)



S=12
x<-c(0,1,2)
tab1<-permutations(n=3,r=6,v=x,repeats.allowed = T)
AIC.val<-rep(NA,length(tab1[,1]))
AICc.val<-rep(NA,length(tab1[,1]))
BIC.val<-rep(NA,length(tab1[,1]))
AIC.table<-data.frame(tab1,S,AIC.val,AICc.val,BIC.val)
colnames(AIC.table)<-c('p','d','q','P','D','Q','S','AIC','AICc','BIC')



for(i in x){
  for(j in x){
    for(k in x){
      for(l in x){
        for(m in x){
          for(n in x){
            tryCatch({temp<-sarima(sh,i,j,k,l,m,n,S)
            AIC.table[AIC.table$p==i & AIC.table$d==j & AIC.table$q==k & AIC.table$P==l & AIC.table$D==m & AIC.table$Q==n,8]<-temp$AIC
            AIC.table[AIC.table$p==i & AIC.table$d==j & AIC.table$q==k & AIC.table$P==l & AIC.table$D==m & AIC.table$Q==n,9]<-temp$AICc
            AIC.table[AIC.table$p==i & AIC.table$d==j & AIC.table$q==k & AIC.table$P==l & AIC.table$D==m & AIC.table$Q==n,10]<-temp$BIC
            },error=conditionMessage)
            }
        }
      }
    }
  }
}





AIC.table<-AIC.table[order(AIC.table$AIC),]
write.csv(AIC.table,'houston_models_log')

sarima(lp, 2,1,3, 3,1,1,12)



#Ljung-Box

S=12
x<-c(0,1,2)
tab1<-permutations(n=3,r=6,v=x,repeats.allowed = T)
LB.val<-rep(NA,length(tab1[,1]))
Box.table<-data.frame(tab1,S,LB.val)
colnames(Box.table)<-c('p','d','q','P','D','Q','S','Ljung-Box')



for(i in x){
  for(j in x){
    for(k in x){
      for(l in x){
        for(m in x){
          for(n in x){
            tryCatch({temp<-arima(lp,order=c(i,j,k),seasonal=list(order=c(l,m,n),period=S))
            Box.table[Box.table$p==i & Box.table$d==j & Box.table$q==k & Box.table$P==l & Box.table$D==m & Box.table$Q==n,8]<-Box.test(temp$residuals,type="Ljung-Box")[3]
            },error=conditionMessage)
          }
        }
      }
    }
  }
}


Box.table.sorted<-Box.table[order(Box.table$`Ljung-Box`,decreasing=TRUE),]

write.csv(Box.table.sorted,'philly_lb.csv')


sarima(lh,2,1,2,0,2,1,12)







################################
###########   LOWESS ###########
################################


tsplot(p)
lines(lowess(p,f=.05),lwd=2,col=4)
lines(lowess(p),lty=2,lwd=2,col=2)

tsplot(h)
lines(lowess(h,f=.05),lwd=2,col=4)
lines(lowess(h),lty=2,lwd=2,col=2)

tsplot(lp)
lines(lowess(lp,f=.05),lwd=2,col=4)
lines(lowess(lp),lty=2,lwd=2,col=2)

tsplot(lh)
lines(lowess(lh,f=.05),lwd=2,col=4)
lines(lowess(lh),lty=2,lwd=2,col=2)

tsplot(diff(lp))
lines(lowess(diff(lp),f=.05),lwd=2,col=4)
lines(lowess(diff(lp)),lty=2,lwd=2,col=2)

tsplot(diff(lh))
lines(lowess(diff(lh),f=.05),lwd=2,col=4)
lines(lowess(diff(lh)),lty=2,lwd=2,col=2)


plot(Philly.limit$TMAX,p)
lines(lowess(Philly.limit$TMAX,p),lwd=2,lty=2,col=2)




#####################################
###########   Prediction? ###########
#####################################
houston.future<-Houston_monthly[600:nrow(Houston_monthly),]
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



=======
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








>>>>>>> 6f1984e0025ed687d29d93165918658006473577
