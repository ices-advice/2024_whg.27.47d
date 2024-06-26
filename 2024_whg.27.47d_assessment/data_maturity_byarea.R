##################################
# mat by area  ###################
##################################

library(DATRAS)
library(plyr)
library(dplyr)
library(boot)  
library(svglite) 
library(ggplot2)
library(grid)
library(gridExtra)
library(gam)

ii<-c("NORTH", "SOUTH")
fyear<-year

for( il in 1:2) {

set.seed(23456)

dataG<-read.table("data\\mat_dataG2.csv",h=T, sep=",")
dataG<-dataG[dataG$area==ii[il],]

ogive.mod2<-glm(mat~age*as.factor(year),weight=WT,family=binomial,data=dataG)

#issues a warning but the model seems to take the weights

A50.91<--(ogive.mod2$coef["(Intercept)"])/(ogive.mod2$coef["age"])
A50.92<--(ogive.mod2$coef["(Intercept)"]+ogive.mod2$coef["as.factor(year)1992"])/(ogive.mod2$coef["age"]+ogive.mod2$coef["age:as.factor(year)1992"])
A50.93<--(ogive.mod2$coef["(Intercept)"]+ogive.mod2$coef["as.factor(year)1993"])/(ogive.mod2$coef["age"]+ogive.mod2$coef["age:as.factor(year)1993"])
A50.94<--(ogive.mod2$coef["(Intercept)"]+ogive.mod2$coef["as.factor(year)1994"])/(ogive.mod2$coef["age"]+ogive.mod2$coef["age:as.factor(year)1994"])
A50.95<--(ogive.mod2$coef["(Intercept)"]+ogive.mod2$coef["as.factor(year)1995"])/(ogive.mod2$coef["age"]+ogive.mod2$coef["age:as.factor(year)1995"])
A50.96<--(ogive.mod2$coef["(Intercept)"]+ogive.mod2$coef["as.factor(year)1996"])/(ogive.mod2$coef["age"]+ogive.mod2$coef["age:as.factor(year)1996"])
A50.97<--(ogive.mod2$coef["(Intercept)"]+ogive.mod2$coef["as.factor(year)1997"])/(ogive.mod2$coef["age"]+ogive.mod2$coef["age:as.factor(year)1997"])
A50.98<--(ogive.mod2$coef["(Intercept)"]+ogive.mod2$coef["as.factor(year)1998"])/(ogive.mod2$coef["age"]+ogive.mod2$coef["age:as.factor(year)1998"])
A50.99<--(ogive.mod2$coef["(Intercept)"]+ogive.mod2$coef["as.factor(year)1999"])/(ogive.mod2$coef["age"]+ogive.mod2$coef["age:as.factor(year)1999"])
A50.00<--(ogive.mod2$coef["(Intercept)"]+ogive.mod2$coef["as.factor(year)2000"])/(ogive.mod2$coef["age"]+ogive.mod2$coef["age:as.factor(year)2000"])
A50.01<--(ogive.mod2$coef["(Intercept)"]+ogive.mod2$coef["as.factor(year)2001"])/(ogive.mod2$coef["age"]+ogive.mod2$coef["age:as.factor(year)2001"])
A50.02<--(ogive.mod2$coef["(Intercept)"]+ogive.mod2$coef["as.factor(year)2002"])/(ogive.mod2$coef["age"]+ogive.mod2$coef["age:as.factor(year)2002"])
A50.03<--(ogive.mod2$coef["(Intercept)"]+ogive.mod2$coef["as.factor(year)2003"])/(ogive.mod2$coef["age"]+ogive.mod2$coef["age:as.factor(year)2003"])
A50.04<--(ogive.mod2$coef["(Intercept)"]+ogive.mod2$coef["as.factor(year)2004"])/(ogive.mod2$coef["age"]+ogive.mod2$coef["age:as.factor(year)2004"])
A50.05<--(ogive.mod2$coef["(Intercept)"]+ogive.mod2$coef["as.factor(year)2005"])/(ogive.mod2$coef["age"]+ogive.mod2$coef["age:as.factor(year)2005"])
A50.06<--(ogive.mod2$coef["(Intercept)"]+ogive.mod2$coef["as.factor(year)2006"])/(ogive.mod2$coef["age"]+ogive.mod2$coef["age:as.factor(year)2006"])
A50.07<--(ogive.mod2$coef["(Intercept)"]+ogive.mod2$coef["as.factor(year)2007"])/(ogive.mod2$coef["age"]+ogive.mod2$coef["age:as.factor(year)2007"])
A50.08<--(ogive.mod2$coef["(Intercept)"]+ogive.mod2$coef["as.factor(year)2008"])/(ogive.mod2$coef["age"]+ogive.mod2$coef["age:as.factor(year)2008"])
A50.09<--(ogive.mod2$coef["(Intercept)"]+ogive.mod2$coef["as.factor(year)2009"])/(ogive.mod2$coef["age"]+ogive.mod2$coef["age:as.factor(year)2009"])
A50.10<--(ogive.mod2$coef["(Intercept)"]+ogive.mod2$coef["as.factor(year)2010"])/(ogive.mod2$coef["age"]+ogive.mod2$coef["age:as.factor(year)2010"])
A50.11<--(ogive.mod2$coef["(Intercept)"]+ogive.mod2$coef["as.factor(year)2011"])/(ogive.mod2$coef["age"]+ogive.mod2$coef["age:as.factor(year)2011"])
A50.12<--(ogive.mod2$coef["(Intercept)"]+ogive.mod2$coef["as.factor(year)2012"])/(ogive.mod2$coef["age"]+ogive.mod2$coef["age:as.factor(year)2012"])
A50.13<--(ogive.mod2$coef["(Intercept)"]+ogive.mod2$coef["as.factor(year)2013"])/(ogive.mod2$coef["age"]+ogive.mod2$coef["age:as.factor(year)2013"])
A50.14<--(ogive.mod2$coef["(Intercept)"]+ogive.mod2$coef["as.factor(year)2014"])/(ogive.mod2$coef["age"]+ogive.mod2$coef["age:as.factor(year)2014"])
A50.15<--(ogive.mod2$coef["(Intercept)"]+ogive.mod2$coef["as.factor(year)2015"])/(ogive.mod2$coef["age"]+ogive.mod2$coef["age:as.factor(year)2015"])
A50.16<--(ogive.mod2$coef["(Intercept)"]+ogive.mod2$coef["as.factor(year)2016"])/(ogive.mod2$coef["age"]+ogive.mod2$coef["age:as.factor(year)2016"])
A50.17<--(ogive.mod2$coef["(Intercept)"]+ogive.mod2$coef["as.factor(year)2017"])/(ogive.mod2$coef["age"]+ogive.mod2$coef["age:as.factor(year)2017"])
A50.18<--(ogive.mod2$coef["(Intercept)"]+ogive.mod2$coef["as.factor(year)2018"])/(ogive.mod2$coef["age"]+ogive.mod2$coef["age:as.factor(year)2018"])
A50.19<--(ogive.mod2$coef["(Intercept)"]+ogive.mod2$coef["as.factor(year)2019"])/(ogive.mod2$coef["age"]+ogive.mod2$coef["age:as.factor(year)2019"])
A50.20<--(ogive.mod2$coef["(Intercept)"]+ogive.mod2$coef["as.factor(year)2020"])/(ogive.mod2$coef["age"]+ogive.mod2$coef["age:as.factor(year)2020"])
A50.21<--(ogive.mod2$coef["(Intercept)"]+ogive.mod2$coef["as.factor(year)2021"])/(ogive.mod2$coef["age"]+ogive.mod2$coef["age:as.factor(year)2021"])
A50.22<--(ogive.mod2$coef["(Intercept)"]+ogive.mod2$coef["as.factor(year)2022"])/(ogive.mod2$coef["age"]+ogive.mod2$coef["age:as.factor(year)2022"])
A50.23<--(ogive.mod2$coef["(Intercept)"]+ogive.mod2$coef["as.factor(year)2023"])/(ogive.mod2$coef["age"]+ogive.mod2$coef["age:as.factor(year)2023"])
A50.24<--(ogive.mod2$coef["(Intercept)"]+ogive.mod2$coef["as.factor(year)2024"])/(ogive.mod2$coef["age"]+ogive.mod2$coef["age:as.factor(year)2024"])

res<-c(A50.91,A50.92,A50.93,A50.94,A50.95,A50.96,A50.97,A50.98,A50.99,A50.00,
       A50.01,A50.02,A50.03,A50.04,A50.05,A50.06,A50.07,A50.08,A50.09,A50.10,
       A50.11,A50.12,A50.13,A50.14,A50.15,A50.16,A50.17,A50.18,A50.19,A50.20,
       A50.21,A50.22,A50.23,A50.24)

#remove 1989 and 1990
a50<-data.frame(year=c(1991:fyear),res)


a50fun<-function(d,indices) {
  d <- d[indices,]
  ogive.mod2<-glm(mat~age*as.factor(year),family=binomial,weight=WT,data=d)
  A50.91<--(ogive.mod2$coef["(Intercept)"])/(ogive.mod2$coef["age"])
  A50.92<--(ogive.mod2$coef["(Intercept)"]+ogive.mod2$coef["as.factor(year)1992"])/(ogive.mod2$coef["age"]+ogive.mod2$coef["age:as.factor(year)1992"])
  A50.93<--(ogive.mod2$coef["(Intercept)"]+ogive.mod2$coef["as.factor(year)1993"])/(ogive.mod2$coef["age"]+ogive.mod2$coef["age:as.factor(year)1993"])
  A50.94<--(ogive.mod2$coef["(Intercept)"]+ogive.mod2$coef["as.factor(year)1994"])/(ogive.mod2$coef["age"]+ogive.mod2$coef["age:as.factor(year)1994"])
  A50.95<--(ogive.mod2$coef["(Intercept)"]+ogive.mod2$coef["as.factor(year)1995"])/(ogive.mod2$coef["age"]+ogive.mod2$coef["age:as.factor(year)1995"])
  A50.96<--(ogive.mod2$coef["(Intercept)"]+ogive.mod2$coef["as.factor(year)1996"])/(ogive.mod2$coef["age"]+ogive.mod2$coef["age:as.factor(year)1996"])
  A50.97<--(ogive.mod2$coef["(Intercept)"]+ogive.mod2$coef["as.factor(year)1997"])/(ogive.mod2$coef["age"]+ogive.mod2$coef["age:as.factor(year)1997"])
  A50.98<--(ogive.mod2$coef["(Intercept)"]+ogive.mod2$coef["as.factor(year)1998"])/(ogive.mod2$coef["age"]+ogive.mod2$coef["age:as.factor(year)1998"])
  A50.99<--(ogive.mod2$coef["(Intercept)"]+ogive.mod2$coef["as.factor(year)1999"])/(ogive.mod2$coef["age"]+ogive.mod2$coef["age:as.factor(year)1999"])
  A50.00<--(ogive.mod2$coef["(Intercept)"]+ogive.mod2$coef["as.factor(year)2000"])/(ogive.mod2$coef["age"]+ogive.mod2$coef["age:as.factor(year)2000"])
  A50.01<--(ogive.mod2$coef["(Intercept)"]+ogive.mod2$coef["as.factor(year)2001"])/(ogive.mod2$coef["age"]+ogive.mod2$coef["age:as.factor(year)2001"])
  A50.02<--(ogive.mod2$coef["(Intercept)"]+ogive.mod2$coef["as.factor(year)2002"])/(ogive.mod2$coef["age"]+ogive.mod2$coef["age:as.factor(year)2002"])
  A50.03<--(ogive.mod2$coef["(Intercept)"]+ogive.mod2$coef["as.factor(year)2003"])/(ogive.mod2$coef["age"]+ogive.mod2$coef["age:as.factor(year)2003"])
  A50.04<--(ogive.mod2$coef["(Intercept)"]+ogive.mod2$coef["as.factor(year)2004"])/(ogive.mod2$coef["age"]+ogive.mod2$coef["age:as.factor(year)2004"])
  A50.05<--(ogive.mod2$coef["(Intercept)"]+ogive.mod2$coef["as.factor(year)2005"])/(ogive.mod2$coef["age"]+ogive.mod2$coef["age:as.factor(year)2005"])
  A50.06<--(ogive.mod2$coef["(Intercept)"]+ogive.mod2$coef["as.factor(year)2006"])/(ogive.mod2$coef["age"]+ogive.mod2$coef["age:as.factor(year)2006"])
  A50.07<--(ogive.mod2$coef["(Intercept)"]+ogive.mod2$coef["as.factor(year)2007"])/(ogive.mod2$coef["age"]+ogive.mod2$coef["age:as.factor(year)2007"])
  A50.08<--(ogive.mod2$coef["(Intercept)"]+ogive.mod2$coef["as.factor(year)2008"])/(ogive.mod2$coef["age"]+ogive.mod2$coef["age:as.factor(year)2008"])
  A50.09<--(ogive.mod2$coef["(Intercept)"]+ogive.mod2$coef["as.factor(year)2009"])/(ogive.mod2$coef["age"]+ogive.mod2$coef["age:as.factor(year)2009"])
  A50.10<--(ogive.mod2$coef["(Intercept)"]+ogive.mod2$coef["as.factor(year)2010"])/(ogive.mod2$coef["age"]+ogive.mod2$coef["age:as.factor(year)2010"])
  A50.11<--(ogive.mod2$coef["(Intercept)"]+ogive.mod2$coef["as.factor(year)2011"])/(ogive.mod2$coef["age"]+ogive.mod2$coef["age:as.factor(year)2011"])
  A50.12<--(ogive.mod2$coef["(Intercept)"]+ogive.mod2$coef["as.factor(year)2012"])/(ogive.mod2$coef["age"]+ogive.mod2$coef["age:as.factor(year)2012"])
  A50.13<--(ogive.mod2$coef["(Intercept)"]+ogive.mod2$coef["as.factor(year)2013"])/(ogive.mod2$coef["age"]+ogive.mod2$coef["age:as.factor(year)2013"])
  A50.14<--(ogive.mod2$coef["(Intercept)"]+ogive.mod2$coef["as.factor(year)2014"])/(ogive.mod2$coef["age"]+ogive.mod2$coef["age:as.factor(year)2014"])
  A50.15<--(ogive.mod2$coef["(Intercept)"]+ogive.mod2$coef["as.factor(year)2015"])/(ogive.mod2$coef["age"]+ogive.mod2$coef["age:as.factor(year)2015"])
  A50.16<--(ogive.mod2$coef["(Intercept)"]+ogive.mod2$coef["as.factor(year)2016"])/(ogive.mod2$coef["age"]+ogive.mod2$coef["age:as.factor(year)2016"])
  A50.17<--(ogive.mod2$coef["(Intercept)"]+ogive.mod2$coef["as.factor(year)2017"])/(ogive.mod2$coef["age"]+ogive.mod2$coef["age:as.factor(year)2017"])
  A50.18<--(ogive.mod2$coef["(Intercept)"]+ogive.mod2$coef["as.factor(year)2018"])/(ogive.mod2$coef["age"]+ogive.mod2$coef["age:as.factor(year)2018"])
  A50.19<--(ogive.mod2$coef["(Intercept)"]+ogive.mod2$coef["as.factor(year)2019"])/(ogive.mod2$coef["age"]+ogive.mod2$coef["age:as.factor(year)2019"])
  A50.20<--(ogive.mod2$coef["(Intercept)"]+ogive.mod2$coef["as.factor(year)2020"])/(ogive.mod2$coef["age"]+ogive.mod2$coef["age:as.factor(year)2020"])
  A50.21<--(ogive.mod2$coef["(Intercept)"]+ogive.mod2$coef["as.factor(year)2021"])/(ogive.mod2$coef["age"]+ogive.mod2$coef["age:as.factor(year)2021"])
  A50.22<--(ogive.mod2$coef["(Intercept)"]+ogive.mod2$coef["as.factor(year)2022"])/(ogive.mod2$coef["age"]+ogive.mod2$coef["age:as.factor(year)2022"])
  A50.23<--(ogive.mod2$coef["(Intercept)"]+ogive.mod2$coef["as.factor(year)2023"])/(ogive.mod2$coef["age"]+ogive.mod2$coef["age:as.factor(year)2023"])
  A50.24<--(ogive.mod2$coef["(Intercept)"]+ogive.mod2$coef["as.factor(year)2024"])/(ogive.mod2$coef["age"]+ogive.mod2$coef["age:as.factor(year)2024"])
  
  res<-c(A50.91,A50.92,A50.93,A50.94,A50.95,A50.96,A50.97,A50.98,A50.99,A50.00,A50.01,A50.02,
         A50.03,A50.04,A50.05,A50.06,A50.07,A50.08,A50.09,A50.10,A50.11,A50.12,A50.13,A50.14,
         A50.15,A50.16,A50.17,A50.18,A50.19,A50.20,A50.21,A50.22,A50.23,A50.24)
  return(res)
}

#Bootstrap, TAKES A LONG TIME
boot_fit <- boot(  
  data = dataG,
  strata=dataG$year,
  statistic = a50fun, 
  R = 999
) 

L<-length(c(1991:fyear))

cis<-do.call(rbind,lapply(1:L,getCI,x=boot_fit))
rezboot<-data.frame(year=c(1991:fyear),A50=boot_fit$t0,cis[,3:4])

write.taf(rezboot,paste0("A50CI_Weight_year2_",ii[il],".csv"), dir="data")


#produce ogives
ny<-(fyear-1991)+1

a50fun<-function(d,indices) {
  d <- d[indices,]
  ogive.mod<-glm(mat~age*as.factor(year),weight=WT,family=binomial,data=d)
  new<- data.frame(age = rep(c(1:8),ny),year=rep(c(1991:fyear),each=8))
  mat<-predict(ogive.mod,new,type="response")
  return(mat)
}


#Bootstrap, TAKES A LONG TIME
boot_fit <- boot(  
  data = dataG,
  strata=dataG$year,
  statistic = a50fun, 
  R = 999
) 

L<-length(boot_fit$t0)
rezboot<-data.frame(year=rep(c(1991:fyear),each=8),age=rep(c(1:8),ny),mat=round(boot_fit$t0,2),lwr=rep(0,L),upr=rep(0,L))

rounded<-round(boot_fit$t,4)
for(i in 1:L){
  if(length(levels(as.factor(rounded[,i]))) > 1) {
    a<-boot.ci(boot_fit, type="basic", index=i)
    rezboot[i,4]<-round(a$basic[4],2)
    rezboot[i,5]<-round(a$basic[5],2)
  } else {
    rezboot[i,4]<-round(boot_fit$t0[i],2)
    rezboot[i,5]<-round(boot_fit$t0[i],2)
  }
}


ogive<-rezboot
write.taf(ogive,paste0("ogives.year.whgt2_",ii[il],".csv"), dir="data")
}

# combine areas
North<- read.table("data\\ogives.year.whgt2_NORTH.csv",sep=",", h=T) 
South<- read.table("data\\ogives.year.whgt2_SOUTH.csv",sep=",", h=T) 

new1<-cbind(North,"NORTH")
colnames(new1)<-c(colnames(North),"area")
new2<-cbind(South,"SOUTH")
colnames(new2)<-colnames(new1)
new<-rbind(new1,new2)
write.taf(new,"ogives.year.AREA.csv",dir="data")


North<- read.table("data\\A50CI_Weight_year2_NORTH.csv",sep=",", h=T) 
South<- read.table("data\\A50CI_Weight_year2_SOUTH.csv",sep=",", h=T) 

new1<-cbind(North,"NORTH")
colnames(new1)<-c(colnames(North),"area")
new2<-cbind(South,"SOUTH")
colnames(new2)<-colnames(new1)
new<-rbind(new1,new2)
write.taf(new,"A50CI_yearAREA.csv",dir="data")



# smooth maturity index North
#######################################################
# smooth age 6

Mat0<-read.table("data\\ogives.year.AREA.csv",sep=",",header=T)    
Mat0<-subset(Mat0, year<=fyear & year>1990 & area=="NORTH") 

Mat0<-Mat0[,c(1,2,3)]

raw_mat<-reshape(Mat0,idvar=c("year"),timevar="age",direction="wide") 

write.taf(raw_mat,"raw_matogive_North.csv",dir="data",row.names=F)


# with plus group
library(gam)

Mat0$age[Mat0$age>6]<-6 

for (i in 1:6) {                             
  M<-subset(Mat0, age==i)
  
  g<-gam::gam(mat~s(year,4),data=M,family=gaussian)
  s<-predict.Gam(g)
  s<-cbind(M,s)
  s<-subset(s,select=c(year,age,s))
  if (i==1) {
    bericht<-s
    aic<-(summary(g))$aic}
  else {
    bericht<-rbind(bericht,s)
    aic<-rbind(aic,(summary(g))$aic)}
}


matog<-subset(bericht,select=c(year,age,s))
matog<-reshape(matog,idvar=c("year"),timevar="age",direction="wide")   

matog$s.7<-matog$s.6
matog$s.8<-matog$s.6

#values larger than 1 set to 1
matog[matog>1 & matog<2]<-1
colnames(matog)<-c("Year", "Age1", "Age2","Age3", "Age4","Age5", "Age6", "Age7", "Age8+")

write.taf(matog,"smoothed_matogive_plusg_North.csv", dir="data", row.names=F)

matogfull<-matog
matogfull$Age9<-matogfull$Age8
matogfull$Age10<-matogfull$Age8
matogfull$Age11<-matogfull$Age8
matogfull$Age12<-matogfull$Age8
matogfull$Age13<-matogfull$Age8
matogfull$Age14<-matogfull$Age8
matogfull$Age15<-matogfull$Age8

ex<-matogfull[1,]
maturityplus<-rbind(ex,ex,ex,ex,ex,ex,ex,ex,ex,ex,ex,ex,ex, matogfull)
maturityplus<-cbind(c(1978:year),0,maturityplus[,-1])

colnames(maturityplus)<-c("Year","age0","age1","age2","age3","age4","age5","age6","age7","age8","age9","age10","age11","age12","age13","age14","age15")

write.taf(maturityplus,"smoothed_matogive_plusg15_north_full.csv", dir="data")



# smooth maturity index Area South
#######################################################

Mat0<-read.table("data\\ogives.year.AREA.csv" ,sep=",",header=T)    
Mat0<-subset(Mat0, year<=fyear & year>1990 & area=="SOUTH") 

Mat0<-Mat0[,c(1,2,3)]

raw_mat<-reshape(Mat0,idvar=c("year"),timevar="age",direction="wide") 

for (i in 1:8) {                             
  M<-subset(Mat0, age==i)
  
  g<-gam::gam(mat~s(year,6),data=M,family=gaussian)
  s<-predict.Gam(g)
  s<-cbind(M,s)
  s<-subset(s,select=c(year,age,s))
  if (i==1) {
    bericht<-s
    aic<-(summary(g))$aic}
  else {
    bericht<-rbind(bericht,s)
    aic<-rbind(aic,(summary(g))$aic)}
}

matog<-subset(bericht,select=c(year,age,s))
matog<-reshape(matog,idvar=c("year"),timevar="age",direction="wide")    #####warning message, but simply takes the first time series for age "5", what is correct because all are the same. 
#values larger than 1 set to 1
matog[matog>1 & matog<2]<-1
colnames(matog)<-c("Year", "Age1", "Age2","Age3", "Age4","Age5", "Age6", "Age7", "Age8+")

write.taf(raw_mat,"raw_matogive_South.csv", dir="data",row.names=F)
write.taf(matog,"smoothed_matogive_plusg_South.csv",dir="data" ,row.names=F)

matogfull<-matog
matogfull$Age9<-matogfull$Age8
matogfull$Age10<-matogfull$Age8
matogfull$Age11<-matogfull$Age8
matogfull$Age12<-matogfull$Age8
matogfull$Age13<-matogfull$Age8
matogfull$Age14<-matogfull$Age8
matogfull$Age15<-matogfull$Age8

ex<-matogfull[1,]

maturityplus<-rbind(ex,ex,ex,ex,ex,ex,ex,ex,ex,ex,ex,ex,ex, matogfull)
maturityplus<-cbind(c(1978:year),0,maturityplus[,-1])

colnames(maturityplus)<-c("Year","age0","age1","age2","age3","age4","age5","age6","age7","age8","age9","age10","age11","age12","age13","age14","age15")

write.taf(maturityplus,"smoothed_matogive_plusg15_south_full.csv", dir="data")



