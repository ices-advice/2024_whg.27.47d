
library(DATRAS)
library(plyr)
library(dplyr)
library(boot)  
library(svglite) 
library(ggplot2)
library(grid)
library(gridExtra)
library(gam)


fyear<-year

#  1 get quarter 1 stock weights

#load DATRAS DATA

#downloaded exchange data since 1980 
load("bootstrap/data/exchange.Rdata")  #new

#keep whitings

# Species specific parameters
genus = "Merlangius"
bfamily = "merlangus"

# Q1 whiting data (in areas 1-7), valid hauls
whiting <- subset(whiting, Species==paste(genus,bfamily), Quarter==1, HaulVal=="V", !is.na(IndWgt))

#create an age 6+ group
whiting[["CA"]]$Age = revalue(as.factor(whiting[["CA"]]$Age), c("7"="6","8"="6","9"="6","10"="6","11"="6", "12"="6","13"="6","14"="6","15"="6","16"="6","17"="6","18"="6","19"="6","20"="6", "21"="6"))
whiting[["CA"]]$Age = as.numeric(as.character(whiting[["CA"]]$Age))
whiting[["CA"]]$NoAtALK <- whiting[["CA"]]$NoAtLngt

# Add raw number of observed individuals per length group to HH data and calculate CPUE (based on 60 mins effort)
whiting = addSpectrum(whiting,by=1)  # size groups

#add Age 0:7

whiting = addNage(whiting,0:6)
#tail(whiting[[2]]$Nage)


#add weight by haul
whiting=addWeightByHaul(whiting)

#standardise for an effort of 60 min
whiting[[2]]$N <- whiting[[2]]$N / whiting[[2]]$HaulDur * 60
#tail(whiting[[2]]$N)


#standardise for an effort of 60 min
whiting[[2]]$Nage <- whiting[[2]]$Nage / whiting[[2]]$HaulDur * 60
#tail(whiting[[2]]$Nage)

#standardise for an effort of 60 min
whiting[[2]]$HaulWgt <- whiting[[2]]$HaulWgt / whiting[[2]]$HaulDur * 60

# Storage
years <- 1990:fyear
ages<-c(0:6)

whiting<-subset(whiting,Year %in% years)

###CREATE SMALK FROM CA data
# total numbers per haul
whiting[[2]]$nbL<-rowSums(whiting[[2]]$N) 

smalk<-whiting[["CA"]]
#head(smalk)

#simplify the dataset and the maturity rating
smalk$year<-smalk$Year
smalk$length<-smalk$LngtCm
smalk$age<-smalk$Age
smalk$AREA<-smalk$AreaCode
smalk$sex<-smalk$Sex
smalk$mat<-smalk$Maturity
smalk$nb<-smalk$NoAtALK
smalk$indwgt<-smalk$IndWgt


#keep do North SOuth, channel 
area1<-c("52E6","52E7","52E8","52E9","52F0","52F1","52F2","52F3","51E6","51E7","51E8","51E9","51F0","51F1","51F2","51F3","50E6","50E7","50E8","50E9","50F0","50F1","50F2","50F3","49E6","49E7","49E8","49E9","49F0","49F1","49F2","49F3","48E6","48E7","48E8","48E9","48F0","48F1","48F2","48F3","47E8","47E9","47F0","47F1","47F2","47F3","46F0","46F1","46F2","46F3","45F0","45F1","45F2","45F3","45F4","44F0","44F1")
area2<-c("44F2","44F3","44F4","43F0","43F1","43F2","43F3","43F4","42F0","42F1","42F2","42F3","42F4","41F0","41F1","41F2","41F3","41F4","40F0","40F1","40F2","40F3","40F4","39F1","39F2")
area3<-c("47E6","47E7","46E6","46E7","46E8","46E9","45E6","45E7","45E8","45E9","44E6","44E7","44E8","44E9","43E6","43E7","43E8","43E9","42E6","42E7","42E8","42E9","41E6","41E7","41E8","41E9")
area4<-c("40E6","40E7","40E8","40E9","39E6","39E7","39E8","39E9","39F0","38E6","38E7","38E8","38E9","38F0","38F1","37E6","37E7","37E8","37E9","37F0","37F1","36E6","36E7","36E8","36E9","36F0")
area5<-c("36F1","36F2","35F0","35F1","35F2","34F0","34F1","34F2","33F0","33F1","33F2","32F0","32F1","31F0","31F1")
area6<-c("39F3","39F4","39F5","39F6","39F7","39F8","38F2","38F3","38F4","38F5","38F6","38F7","38F8","37F2","37F3","37F4","37F5","37F6","37F7","37F8","36F3","36F4","36F5","36F6","36F7","36F8","35F3","35F4","35F5","35F6","35F7","35F8","34F3","34F4","34F5","34F6","34F7","34F8","33F3","33F4","33F5","33F6","33F7","33F8","32F2","32F3","32F4","32F5","32F6","32F7","32F8","31F2","31F3","31F4","31F5","31F6","31F7","31F8")
area7<-c("44F5","43F5","43F6","43F7","42F5","42F6","42F7","42F8","41F5","41F6","41F7","41F8","40F5","40F6","40F7","40F8")
area8<-c("48F8","48F9","48G0","48G1","47F8","47F9","47G0","47G1","46F8","46F9","46G0","46G1","45F8","45F9","45G0","45G1","44F8","44F9","44G0","44G1","43F8","43F9","42F9")
area9<-c("43G0","43G1","43G2","42G0","42G1","42G2","41G0","41G1","41G2")
area10<-c("30F0","30F1","29F0","29F1","28F0","28F1")


NORTH<-c("35F0","35F1","36E9","36F0","36F1","37E9","37F0","37F1","38E8","38E9","38F0","38F1","38F2","39E8","39E9","39F0","39F1","39F2","40E6","40E7","40E8","40E9","40F0","40F1","40F2","41E6","41E7","41E8","41E9","41F0","41F1","41F2","41F3","42E7","42E8","42E9","42F0","42F1","42F2","42F3","43E7","43E8","43E9","43F0","43F1","43F2","43F3","44E5","44E6","44E7","44E8","44E9","44F0","44F1","44F2","44F3","44F4","45E6","45E7","45E8","45E9","45F0","45F1","45F2","45F3","45F4","46E6","46E7","46E8","46E9","46F0","46F1","46F2","46F3","47E6","47E7","47E8","47E9","47F0","47F1","47F2","47F3","48E6","48E7","48E8","48E9","48F0","48F1","48F2","48F3","49E6","49E7","49E8","49E9","49F0","49F1","49F2","49F3","50E7","50E8","50E9","50F0","50F1","50F2","50F3","51E8","51E9","51F0","51F1","51F2")

SOUTH<-c("31F0","31F1","31F2","31F3","31F4","32F0","32F1","32F2","32F3","32F4","33F1","33F2","33F3","33F4","34F0","34F1","34F2","34F3","34F4","35F2","35F3","35F4","35F5","35F6","35F7","35F8","36F2","36F3","36F4","36F5","36F6","36F7","36F8","36F9","37F2","37F3","37F4","37F5","37F6","37F7","37F8","38F3","38F4","38F5","38F6","38F7","38F8","39F3","39F4","39F5","39F6","39F7","39F8","40F3","40F4","40F5","40F6","40F7","40F8","41F4","41F5","41F6","41F7","41F8","42F4","42F5","42F6","42F7","43F4","43F5","43F6","43F7","44F5")

Chann<-c("30E8","30E9","30F0","30F1","29E8","29E9","29F0","29F1","28E8","28E9","28F0","28F1","27E8","27E9","27F0","26E8")


#KAT<-c("48F8","48F9","48G0","48G1","47F8","47F9","47G0","47G1","46F8","46F9","46G0","46G1","45F8","45F9","45G0","45G1","44F8","44F9","44G0","44G1","43F8","43F9","42F9","43G0","43G1","43G2","42G0","42G1","42G2","41G0","41G1","41G2")
KAT<-c("48F8","48F9","48G0","48G1","47F8","47F9","47G0","47G1","46F8","46F9","46G0","46G1","45F7","45F8","45F9","45G0","45G1","44F7","44F8","44F9","44G0","44G1","43F8","43F9","42F9","43G0","43G1","43G2","42G0","42G1","42G2","41G0","41G1","41G2")


smalk$area<-rep(0,dim(smalk)[1])
for(i in 1:dim(smalk)[1]){
  if(smalk$AREA[i] %in% area1)smalk$area[i]<-1
  if(smalk$AREA[i] %in% area2)smalk$area[i]<-2
  if(smalk$AREA[i] %in% area3)smalk$area[i]<-3
  if(smalk$AREA[i] %in% area4)smalk$area[i]<-4
  if(smalk$AREA[i] %in% area5)smalk$area[i]<-5
  if(smalk$AREA[i] %in% area6)smalk$area[i]<-6
  if(smalk$AREA[i] %in% area7)smalk$area[i]<-7
  if(smalk$AREA[i] %in% area8)smalk$area[i]<-8
  if(smalk$AREA[i] %in% area9)smalk$area[i]<-9
  if(smalk$AREA[i] %in% area10)smalk$area[i]<-10
}

smalk$area2<-rep(0,dim(smalk)[1])
for(i in 1:dim(smalk)[1]){
  if(smalk$AREA[i] %in% SOUTH)smalk$area2[i]<-"SOUTH"
  if(smalk$AREA[i] %in% NORTH)smalk$area2[i]<-"NORTH"
  if(smalk$AREA[i] %in% Chann)smalk$area2[i]<-"SOUTH"
  if(smalk$AREA[i] %in% KAT)smalk$area2[i]<-"KAT"
}

smalk<-droplevels(subset(smalk,!area==0))
smalk<-droplevels(subset(smalk,!area2==0))
smalk<-droplevels(subset(smalk,age>=0))

smalk$area2<-as.factor(smalk$area2)

#remove KAT

smalk<-droplevels(subset(smalk,!area2=="KAT"))

#summary(smalk)

smalk$length<-floor(smalk$length)

#get a catch rate per year North VS South
################################################################################

subHH<-droplevels(whiting[["HH"]])
#summary(subHH)


subHH$area2<-rep(0,dim(subHH)[1])
for(i in 1:dim(subHH)[1]){
  if(subHH$StatRec[i] %in% SOUTH)subHH$area2[i]<-"SOUTH"
  if(subHH$StatRec[i] %in% NORTH)subHH$area2[i]<-"NORTH"
  if(subHH$StatRec[i] %in% Chann)subHH$area2[i]<-"SOUTH"
  if(subHH$StatRec[i] %in% KAT)subHH$area2[i]<-"KAT"
}

subHH$area2<-as.factor(subHH$area2)

#head(subHH)

#remove KAT

subHH<-droplevels(subset(subHH,!area2=="KAT"))
subHH<-droplevels(subset(subHH,!area2=="0"))
#summary(subHH)

rez1<-ddply(subHH,.(Year,StatRec), summarize,nbL=mean(nbL),HaulWgt=mean(HaulWgt), area2=unique(area2) ) # mean values per rectangle first; new

rez<-ddply(rez1,.(Year,area2), summarize,nbL=mean(nbL),HaulWgt=mean(HaulWgt)) # mean values per area for all rectangles

##new
nrec<-length(unique(subHH[subHH$area2=="NORTH","StatRec"]))
srec<-length(unique(subHH[subHH$area2=="SOUTH","StatRec"]))

rez$proparea<-nrec/sum(nrec+srec)   
rez[rez$area2=="SOUTH", "proparea"]<-srec/sum(nrec+srec)

rez$nbL_2<-rez$proparea*rez$nbL  
rez$HaulWgt_2<-rez$proparea*rez$HaulWgt

rezT<-ddply(rez,.(Year), summarize,Tnb=sum(nbL_2),TWgt=sum(HaulWgt_2))            # total

rezF<-merge(rez,rezT)

rezF$wt<-rezF$nbL_2/rezF$Tnb

##

rezF$Year<-as.numeric(as.character(rez$Year))
write.taf(rezF,"rezF.csv",dir="data")

rezN<-subset(rezF,area2=="NORTH")
rezS<-subset(rezF,area2=="SOUTH")


rezF<-rezF[,c(1:2,6)] #new

rezFT<-ddply(rezF,.(Year), summarize,tot=sum(nbL_2)) #new

rezF<-merge(rezF,rezFT)

rezF$wtC<-rezF$nbL_2/rezF$tot  #new

rezF<-rezF[,c(1,2,5)]  #weighting factors by catch rate for each area

################################################################################

#generate the final dataset we will be working with 

dataG<-ddply(smalk,.(haul.id,age,length), summarise,nb = sum(nb),weight=mean(indwgt),year=unique(year),area=unique(area2))

#head(dataG)
#dataG<-dataG[!is.na(dataG$weight),]

#USE CEFAS weighting FACTOR as wi = ma x rg/Ra

#generate intermediate data set for Length
wL<-as.data.frame(whiting[[2]]$N)
wL$haul.id<-whiting[[2]]$haul.id
rownames(wL)<-c()
#summary(wL)

#produce the rg raising factor each fish
dataG$rg<-rep(-1,dim(dataG)[1])
#summary(dataG)

wL1<-droplevels(subset(wL,haul.id %in% dataG$haul.id))
wL1$length<-as.numeric(sub('.([^,]+),.*', '\\1', wL1$sizeGroup)) 

for(i in 1:dim(dataG)[1]){
  #dataG$rg[i]<-max(dataG$nb[i],subset(wL1,haul.id==dataG$haul.id[i])[,dataG$length[i]])/dataG$nb[i]
  ww<-subset(subset(wL1,haul.id==dataG$haul.id[i]),length==dataG$length[i])
  dataG$rg[i]<-max(dataG$nb[i],ww$Freq)/dataG$nb[i]
}

#head(dataG)
#summary(dataG)

#produce the Ra raising factor
dataG$Ra<-rep(-1,dim(dataG)[1])

#summary(dataG)

for(i in 1:dim(dataG)[1]){
  dataG$Ra[i]<-sum(subset(subset(dataG,haul.id==dataG$haul.id[i]),age==dataG$age[i])$rg)
}

#produce the wi raising factor
dataG$wi<-rep(-1,dim(dataG)[1])
#summary(dataG)

for(i in 1:dim(dataG)[1]){
  dataG$wi[i]<-sum(subset(subset(dataG,haul.id==dataG$haul.id[i]),age==dataG$age[i])$nb)*dataG$rg[i]/dataG$Ra[i]
}

colnames(rezF)<-c("year","area","wtC")
dataG<-merge(dataG,rezF)
dataG$WT<-dataG$wi*dataG$wtC

write.taf(dataG,"weights_dataG2.csv", dir="data")

##############################################
#combined

dataG<-read.table("data\\weights_dataG2.csv",sep=","  ,header=T)
dataG$year<-as.numeric(as.character(dataG$year))
d <- dataG[!is.na(dataG$weight) & dataG$year>=2000,]

d$new_nb<-d$WT # new weighted numbers
d$prod<-d$weight*d$new_nb 

dd<-ddply(d,.(year, age), summarise, nb = sum(new_nb), prod=sum(prod))

dd$weight<-dd$prod/dd$nb # get averag weight

write.taf(dd[,c(1:2,5)],"weight_comb.csv", dir="data", row.names=T)


#####################
#combined length at age

dataG<-read.table("data\\weights_dataG2.csv", sep=",", header=T)
d <- dataG[!is.na(dataG$length) & dataG$year>=2000,]

d$new_nb<-d$WT # new weighted numbers
d$prod<-d$length*d$new_nb 

dd<-ddply(d,.(year, age), summarise, nb = sum(new_nb), prod=sum(prod))

dd$length<-dd$prod/dd$nb # get average weight

write.taf(dd[,c(1:2,5)],"length_comb.csv", dir="data")

###################################################################
# length freq

dataG<-read.table("data\\weights_dataG2.csv", sep=",", header=T)

d <- dataG[!is.na(dataG$length) & dataG$year>=2000,]

d$new_nb<-d$WT # new weighted numbers

dd<-ddply(d,.(year, age, length), summarise, nb = sum(new_nb))

write.taf(dd[,],"length_comb_freq.csv", dir="data")


## 2  smooth weights IBTS

library(gam)

if("package:surveyIndex" %in% search()){ detach("package:surveyIndex",
                                            unload=TRUE) }
if("package:mgcv" %in% search()){ detach("package:mgcv", unload=TRUE) }


Mat0<-read.table("data\\weight_comb.csv",header=T, sep=",")
Mat0<-subset(Mat0, year<=fyear & year>1990 & age>0) 

raw_mat<-reshape(Mat0,idvar=c("year"),timevar="age",direction="wide") 

#for quarter 1 no age 0
#per age

for (i in 1:6) {
  
  M<-subset(Mat0, age==i)
  
  g<-gam::gam(weight~s(year,3),data=M,family=gaussian)
  s<-predict.Gam(g)
  s<-cbind(M,s)
  s<-subset(s,select=c(year,age,s))
  if (i==1) {
    bericht<-s
    aic<-(summary(g))$aic}else{
      bericht<-rbind(bericht,s)
      aic<-rbind(aic,(summary(g))$aic)}
  #aic 
}

matog<-subset(bericht,select=c(year,age,s))
matog<-reshape(matog,idvar=c("year"),timevar="age",direction="wide")    
matog<-round(matog,3) 

write.taf(matog,"smoothed_weight.csv",dir="data", row.names=F)


## 3  catch- stock weights- conversion factor
##############################################################################################

sweights<-read.csv("data\\smoothed_weight.csv",header=TRUE)

cweights<-read.csv("data\\wcatch.csv", header=TRUE) #uncorrected catch weights at age

#minimize MSE

p<-NULL
for(i in 1:6){
  c<- cweights[23:dim(cweights)[1],(2+i)]*1000  #adapt last line for final year
  s<- sweights[sweights$year%in%c(2000:fyear-1),(1+i)]
  n<-length(s)
  
  fr<-  function(p) sum((s-p*c)^2)/n
  
  opt<- optimize(fr,lower=0, upper=1.5, tol=0.000001)  # minimize mean squared error
  print(round(opt$minimum,3))
  p<-cbind(p,round(opt$minimum,3))
}

p<-cbind(p[,1],p)


cweights$X0<-cweights$X0*p[,1]*1000
cweights$X1<-cweights$X1*p[,2]*1000
cweights$X2<-cweights$X2*p[,3]*1000
cweights$X3<-cweights$X3*p[,4]*1000
cweights$X4<-cweights$X4*p[,5]*1000
cweights$X5<-cweights$X5*p[,6]*1000
cweights$X6<-cweights$X6*p[,7]*1000
#cweights$X7<-cweights$X7*p[,8]*1000
#cweights$X8<-cweights$X8*p[,9]*1000


write.csv(cweights,'data\\cw_converted_raw.csv',row.names=F)
write.csv(p,'data\\cw_conversion_factor.csv',row.names=F)


c<-cweights
c<-cbind(cweights,cweights$X6,cweights$X6,cweights$X6,cweights$X6,cweights$X6,cweights$X6,cweights$X6,cweights$X6,cweights$X6)
colnames(c)<-c("Year","age0","age1","age2","age3","age4","age5","age6","age7","age8","age9","age10","age11","age12","age13","age14","age15")
c[,2:17]<-c[,2:17]/1000

write.csv(c,'data\\cw_converted_raw_kg.csv',row.names=F)
write.csv(p,'data\\cw_conversion_factors.csv',row.names=F)

c<-cweights
colnames(c)<-c("","0","1","2","3","4","5","6")
c[,2:8]<-c[,2:8]/1000
write.csv(c,'data\\wstock.csv',row.names=F)



