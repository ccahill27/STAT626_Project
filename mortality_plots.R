setwd(dirname(rstudioapi::getSourceEditorContext()$path))
library(astsa)
library(data.table)

#Austin
#Houston
#Texas
#MN
#philly
#phoenix
#san_antonio
#san_diego

city='san_antonio'

death<-read.csv(paste(city,'.csv',sep=''),header=TRUE)

begin=1990
end=2018


#Subset Data
death<-death[death$Year>begin & death$Year<end,]


#Convert Week date to date format
death$Week.Ending.Date <- as.Date(death$Week.Ending.Date, "%m/%d/%Y")

#Remove unneeded variables
death<-death[,c(3,8)]

#Remove Day and Week
death$Month<-substr(death$Week.Ending.Date,1,7)
death<-death[,c(2,3)]

#Make NA values 0
#Yes, I know this is not the same
#But this will be easier to sum in next step

for(i in 1:nrow(death)){
  if(is.na(death$All.Deaths[i])){
    death$All.Deaths[i]=0
  }
}

#Merge months
death<-data.table(death)
death2<-death[, lapply(.SD, sum), by=list(death$Month)]


#Plot by Month and Week
par(mfrow=c(2,2))
val<-
  tsplot(death$All.Deaths,main=paste(city,'by Week,',begin,'to',end))
abline(lm(death$All.Deaths~c(1:nrow(death))),col='red',lty=2)
acf(death$All.Deaths)



tsplot(death2$All.Deaths,main=paste(city,'by Month,',begin,'to',end))
abline(lm(death2$All.Deaths~c(1:nrow(death2))),col='red',lty=2)
acf(death2$All.Deaths)




##Influenza

death<-read.csv(paste(city,'.csv',sep=''),header=TRUE)

begin=1990
end=2018


#Subset Data
death<-death[death$Year>begin & death$Year<end,]


#Convert Week date to date format
death$Week.Ending.Date <- as.Date(death$Week.Ending.Date, "%m/%d/%Y")

#Remove unneeded variables
death<-death[,c(3,7)]

#Remove Day and Week
death$Month<-substr(death$Week.Ending.Date,1,7)
death<-death[,c(2,3)]

#Make NA values 0
#Yes, I know this is not the same
#But this will be easier to sum in next step

for(i in 1:nrow(death)){
  if(is.na(death$Pneumonia.and.Influenza.Deaths[i])){
    death$Pneumonia.and.Influenza.Deaths[i]=0
  }
}

#Merge months
death<-data.table(death)
death2<-death[, lapply(.SD, sum), by=list(death$Month)]


#Plot by Month and Week
par(mfrow=c(2,2))
val<-
  tsplot(death$Pneumonia.and.Influenza.Deaths,main=paste(city,'Flu by Week,',begin,'to',end))
abline(lm(death$Pneumonia.and.Influenza.Deaths~c(1:nrow(death))),col='red',lty=2)
acf(death$Pneumonia.and.Influenza.Deaths)



tsplot(death2$Pneumonia.and.Influenza.Deaths,main=paste(city,'Flu by Month,',begin,'to',end))
abline(lm(death2$Pneumonia.and.Influenza.Deaths~c(1:nrow(death2))),col='red',lty=2)
acf(death2$Pneumonia.and.Influenza.Deaths)





