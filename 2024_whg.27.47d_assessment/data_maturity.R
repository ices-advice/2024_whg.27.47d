#####
## maturity ogive estimation combined area 
###########################################################################################################

set.seed(23456)
fyear<-year

library(DATRAS)
library(plyr)
library(dplyr)
library(boot)  
library(svglite) 
library(ggplot2)
library(grid)
library(gridExtra)
library(gam)
library(maps) 
library(mapdata)
library(maptools)
library(surveyIndex)

#old maturity ogive from the 80's
OLD<-data.frame(age=c(1:8),mat=c(0.11,0.92,1.00,1.00,1.00,1.00,1.00,1.00))
#with(OLD,plot(age,mat,type="l"))

#calculate A50.old
old.mod<-glm(mat~age,family=quasibinomial,data=OLD)
A50.old<--coef(old.mod)[1]/coef(old.mod)[2]

#downloaded exchange data between 1980 and 2017
load("bootstrap/data/exchange.Rdata")  #new

#keep whiting
# Species specific parameters
genus = "Merlangius"
bfamily = "merlangus"

# Q1 whiting data (in areas 1-7), valid hauls
whiting <- subset(whiting, Species==paste(genus,bfamily), Quarter==1  , HaulVal=="V", !is.na(Maturity))

whiting[["CA"]]$Maturity2 = as.character(whiting[["CA"]]$Maturity)

# Recode Maturity
whiting[["CA"]]$Maturity = as.character(whiting[["CA"]]$Maturity)
whiting[["CA"]]$Maturity = revalue(whiting[["CA"]]$Maturity, c("A" = "0", "B" = "1", "Ba" = "0", "Bb" = "1", "C" = "1",  "D" = "1", "E" = "1", "F" = "66", "61"="0", "62"="1", "63"="1", "64"="1", "65"="1", "1"="0", "2"="1", "3"="1", "4"="1", "5"="1", "I"="0", "M"="1"))
#whiting[["CA"]]$Maturity = revalue(whiting[["CA"]]$Maturity, c("61"="0", "62"="1", "63"="1", "64"="1", "65"="1", "1"="0", "2"="1", "3"="1", "4"="1", "5"="1", "I"="0", "M"="1"))
whiting[["CA"]]$Maturity = as.numeric(whiting[["CA"]]$Maturity)
whiting[["CA"]] = filter(whiting[["CA"]], Maturity!=66 & Maturity!=6 & Maturity!=-9) # Remove abnormal

#create an age 8+ group
whiting[["CA"]]$Age = revalue(as.factor(whiting[["CA"]]$Age), c("9"="8","10"="8","11"="8", "12"="8","13"="8","14"="8","15"="8","16"="8","17"="8","18"="8","19"="8","20"="8", "21"="8"))
whiting[["CA"]]$Age = as.numeric(as.character(whiting[["CA"]]$Age))
whiting[["CA"]]$NoAtALK <- whiting[["CA"]]$NoAtLngt

# Add raw number of observed individuals per length group to HH data and calculate CPUE (based on 60 mins effort)
whiting = addSpectrum(whiting,by=1)  # size groups
#tail(whiting[[2]]$N)

#add Age 1:8
whiting = addNage(whiting,1:8)
#tail(whiting[[2]]$Nage)

#add weight by haul
whiting=addWeightByHaul(whiting)

#standardise for an effort of 60 min
whiting[[2]]$N <- whiting[[2]]$N / whiting[[2]]$HaulDur * 60

#standardise for an effort of 60 min
whiting[[2]]$Nage <- whiting[[2]]$Nage / whiting[[2]]$HaulDur * 60


#standardise for an effort of 60 min
whiting[[2]]$HaulWgt <- whiting[[2]]$HaulWgt / whiting[[2]]$HaulDur * 60

# Storage
years <- 1991:fyear
ages<-c(1:8)

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
smalk<-droplevels(subset(smalk,age>0))

smalk$area2<-as.factor(smalk$area2)

#remove KAT

smalk<-droplevels(subset(smalk,!area2=="KAT"))

smalk$length<-floor(smalk$length)



#get a catch rate per year North VS South
################################################################################

subHH<-droplevels(whiting[["HH"]])

subHH$area2<-rep(0,dim(subHH)[1])
for(i in 1:dim(subHH)[1]){
  if(subHH$StatRec[i] %in% SOUTH)subHH$area2[i]<-"SOUTH"
  if(subHH$StatRec[i] %in% NORTH)subHH$area2[i]<-"NORTH"
  if(subHH$StatRec[i] %in% Chann)subHH$area2[i]<-"SOUTH"
  if(subHH$StatRec[i] %in% KAT)subHH$area2[i]<-"KAT"
}

subHH$area2<-as.factor(subHH$area2)

#remove KAT

subHH<-droplevels(subset(subHH,!area2=="KAT"))
subHH<-droplevels(subset(subHH,!area2=="0"))
#summary(subHH)

rez1<-ddply(subHH,.(Year,StatRec), summarize,nbL=mean(nbL),HaulWgt=mean(HaulWgt), area2=unique(area2) ) # mean values per rectangle first; new
rez<-ddply(rez1,.(Year,area2), summarize,nbL=mean(nbL),HaulWgt=mean(HaulWgt))                           # mean values per area for all rectangles

##new section below####
nrec<-length(unique(subHH[subHH$area2=="NORTH","StatRec"]))
srec<-length(unique(subHH[subHH$area2=="SOUTH","StatRec"]))

rez$proparea<-NULL
rez[rez$area2=="NORTH", "proparea"]<-nrec/sum(nrec+srec)   # new relative area size  using sampled rectangles
rez[rez$area2=="SOUTH", "proparea"]<-srec/sum(nrec+srec)

rez$nbL_2<-rez$proparea*rez$nbL           # new standardized by area size
rez$HaulWgt_2<-rez$proparea*rez$HaulWgt   #  new

rezT<-ddply(rez,.(Year), summarize,Tnb=sum(nbL_2),TWgt=sum(HaulWgt_2))            # new total

rezF<-merge(rez,rezT)

rezF$wt<-rezF$nbL_2/rezF$Tnb  #new weight

###

rezF$Year<-as.numeric(as.character(rez$Year))

rezN<-subset(rezF,area2=="NORTH")
rezS<-subset(rezF,area2=="SOUTH")

write.taf(rezF,"rezF_mat.csv",dir="data")


#rezF<-rezF[,c(1:3)]
rezF<-rezF[,c(1:2,6)] #new

#rezFT<-ddply(rezF,.(Year), summarize,tot=sum(nbL))
rezFT<-ddply(rezF,.(Year), summarize,tot=sum(nbL_2)) #new

rezF<-merge(rezF,rezFT)

#rezF$wtC<-rezF$nbL/rezF$tot
rezF$wtC<-rezF$nbL_2/rezF$tot  #new

rezF<-rezF[,c(1,2,5)]  #weighting factors by catch rate for each area

################################################################################
#print out sampling levels, check for Dutch sampling in 2024

xt1<-xtabs(NoAtALK~Year+Age,data=smalk)
write.taf(xt1,file="IBTSQ1nrages sampled.csv",dir="output", row.names=T)


xt2<-xtabs(NoAtALK~Year+Age+area2,data=smalk)
write.taf(xt2[,,1],file="IBTSQ1nr ind mat sampled_AREAnorth.csv",dir="output", row.names=T)
write.taf(xt2[,,2],file="IBTSQ1nr ind mat sampled_AREAsouth.csv",dir="output", row.names=T)

xt1_NL<-xtabs(NoAtALK~Year+Age,data=smalk[smalk$Country%in%c("NL","NED"),])
write.taf(xt1_NL,file="IBTSQ1nrages sampled_NL.csv",dir="output", row.names=T)


xt2_NL<-xtabs(NoAtALK~Year+Age+area2,data=smalk[smalk$Country%in%c("NL","NED"),])
write.taf(xt2_NL[,,1],file="IBTSQ1nr ind mat sampled_AREAnorth_NL.csv",dir="output", row.names=T)
write.taf(xt2[,,2],file="IBTSQ1nr ind mat sampled_AREAsouth_NL.csv",dir="output", row.names=T)



mati<-xtabs(NoAtALK~Year+Maturity2,data=smalk[smalk$Age==1,])
write.taf(mati,"Mat_age1_Year.csv",dir="data", row.names=TRUE)

mati2<-xtabs(NoAtALK~Country+Maturity2,data=smalk[smalk$Age==1 & smalk$year==2024,] )
write.taf(mati2,"Mat_2024_age1_Country.csv",dir="data", row.names=TRUE)

mati2<-xtabs(NoAtALK~Country+Maturity2,data=smalk[smalk$Age==1 & smalk$year==2023,] )
write.taf(mati2,"Mat_2023_age1_Country.csv",dir="data", row.names=TRUE)

mati3<-xtabs(NoAtALK~Country+Maturity2,data=smalk[smalk$Age==1 & smalk$year==2022,] )
write.taf(mati3,"Mat_2022_age1_Country.csv",dir="data", row.names=TRUE)


mybubblePlot<-function (d, response = "HaulWgt", scale = NULL, col.zero = "red", 
                        pch.zero = "+", ...) 
{
  d[[2]]$resp.var <- d[[2]][[response]]
  if (is.null(scale)) 
    scale = mean(d[[2]]$resp.var, na.rm = TRUE)/max(d[[2]]$resp.var, 
                                                    na.rm = TRUE)
  plot(d$lon, d$lat, type = "n", xlab = "Longitude", ylab = "Latitude",...)
  map("worldHires", fill = TRUE, plot = TRUE, add = TRUE, col = grey(0.8))
  points(d$lon, d$lat, pch = 16, cex = scale * sqrt(d[[2]]$resp.var), 
         ...)
  zero = subset(d, resp.var == 0)
  points(zero$lon, zero$lat, pch = pch.zero, col = col.zero)
}

# Dutch age reading data missing
w_test<-subset(whiting,Country %in% c("NED","NL"))
w_test1<-subset(w_test,Year %in% c("2024"))

#subset dutch fleet no ages read
png("output\\bubbles-NL_whitingQ1_2024.png")
mybubblePlot(w_test1,scale=1/50,ylim=c(51,62),xlim=c(-2,10),main="IBTS Q1, NL 2024")
dev.off()

w_test2<-subset(w_test,Year %in% c("2023"))

png("output\\bubbles-NL_whitingQ1_2023.png")
mybubblePlot(w_test2,scale=1/50,ylim=c(51,62),xlim=c(-2,10),main="IBTS Q1, NL 2023")
dev.off()

w_test4<-subset(whiting,Year %in% c("2024"))
png("output\\bubbles_whitingQ1_2024.png")

mybubblePlot(w_test4,scale=1/50,ylim=c(51,62),xlim=c(-2,10), main="IBTS Q1, all 2024")
dev.off() 


#generate the final dataset we will be working with (1)
dataG<-ddply(smalk,.(haul.id,age,length,mat), summarise,nb = sum(nb),year=unique(year),area=unique(area2))

#drop maturity status for getting weights
dataG1<-ddply(smalk,.(haul.id,age,length), summarise,nb = sum(nb),year=unique(year),area=unique(area2))   # new 2022

#USE CEFAS weighting FACTOR as wi = ma x rg/Ra
#generate intermediate data set for Length

wL<-as.data.frame(whiting[[2]]$N)
wL$haul.id<-whiting[[2]]$haul.id
rownames(wL)<-c()
#summary(wL)

#produce the rg raising factor
#CA by length group

#produce the rg raising factor each fish
dataG1$rg<-rep(-1,dim(dataG1)[1])
#summary(dataG1)

wL1<-droplevels(subset(wL,haul.id %in% dataG$haul.id))
wL1$length<-as.numeric(sub('.([^,]+),.*', '\\1', wL1$sizeGroup)) 

for(i in 1:dim(dataG1)[1]){
  ww<-subset(subset(wL1,haul.id==dataG1$haul.id[i]),length==dataG1$length[i]) 
  
  nn<-sum(subset(subset(dataG1,haul.id==dataG1$haul.id[i]),length==dataG1$length[i])$nb) # new 2022, same weight irrespective of maturity status
  
  dataG1$rg[i]<-max(nn,ww$Freq)/nn  #new 2022
  
}



#produce the Ra raising factor
dataG1$Ra<-rep(-1,dim(dataG1)[1])

#summary(dataG1)

for(i in 1:dim(dataG1)[1]){
  dataG1$Ra[i]<-sum(subset(subset(dataG1,haul.id==dataG1$haul.id[i]),age==dataG1$age[i])$rg)
}


#produce the wi raising factor
dataG1$wi<-rep(-1,dim(dataG1)[1])

#summary(dataG1)

for(i in 1:dim(dataG1)[1]){
  
  dataG1$wi[i]<-sum(subset(subset(dataG1,haul.id==dataG1$haul.id[i]),age==dataG1$age[i])$nb)*dataG1$rg[i]/dataG1$Ra[i]
  
}

#merge dataG and dataG1 , same weight for different maturity states, the assign proportion so they sum
dataGfin<- merge(dataG, dataG1, by=c( "year","area","haul.id", "age", "length"), all=TRUE)  # new 2022
dataGfin$wi_prop<-dataGfin$nb.x/dataGfin$nb.y*dataGfin$wi   #new 2022


#head(rezF)
colnames(rezF)<-c("year","area","wtC")     
dataGfin<-merge(dataGfin,rezF)
dataGfin$WT<-dataGfin$wi_prop*dataGfin$wtC


write.taf(dataGfin,"mat_dataG2.csv", dir="data")


##################################

set.seed(23456)

dataG<-read.table("data\\mat_dataG2.csv",h=T, sep=",")

ogive.mod2<-glm(mat~age*as.factor(year),weight=WT,family=binomial,data=dataG)

#summary(ogive.mod2)

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
A50.24<--(ogive.mod2$coef["(Intercept)"]+ogive.mod2$coef["as.factor(year)2024"])/(ogive.mod2$coef["age"]+ogive.mod2$coef["age:as.factor(year)2024"]) #update with new year

res<-c(A50.91,A50.92,A50.93,A50.94,A50.95,A50.96,A50.97,A50.98,A50.99,A50.00,
       A50.01,A50.02,A50.03,A50.04,A50.05,A50.06,A50.07,A50.08,A50.09,A50.10,
       A50.11,A50.12,A50.13,A50.14,A50.15,A50.16,A50.17,A50.18,A50.19,A50.20,
       A50.21,A50.22,A50.23,A50.24)  # update with new year

#remove 1989 and 1990
a50<-data.frame(year=c(1991:fyear),res)


#update from here

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
         A50.15,A50.16,A50.17,A50.18,A50.19,A50.20,A50.21,A50.22,A50.23, A50.24)
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

#rezboot

png("data\\A50_est.png")
with(rezboot,plot(year,A50,pch=16,ylim=c(0,2),xlim=c(1980,fyear),xaxt="n",xlab="",ylab="Age at 50% Maturity"))
with(rezboot,segments(year,upr,year,lwr))
points(1980,A50.old,pch=16,col="red")
axis(1,at=seq(1980,fyear,1),labels=seq(1980,fyear,1))
dev.off()

write.taf(rezboot,"A50CI_Weight_year2.csv", dir="data")

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

write.taf(ogive,"ogives.year.whgt2.csv", dir="data")

##  smoothing

Mat0<-read.table("data\\ogives.year.whgt2.csv", sep=",", header=T) 
Mat0<-subset(Mat0, year<=fyear & year>1990) 

Mat0<-Mat0[,c(1,2,3)]
raw_mat<-reshape(Mat0,idvar=c("year"),timevar="age",direction="wide") 

write.taf(raw_mat,"raw_matogive.csv",dir="data")


# with plus group, run multiple knots 6,10
library(gam)
if("package:surveyIndex" %in% search()){ detach("package:surveyIndex",
                                                unload=TRUE) }
if("package:mgcv" %in% search()){ detach("package:mgcv", unload=TRUE) }

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

matog<-data.frame(matog[,1], 0,matog[,2:7])
matog$s.7<-matog$s.6
matog$s.8<-matog$s.6

#values larger than 1 set to 1
matog[matog>1 & matog<2]<-1

colnames(matog)<-c("Year", "Age0","Age1", "Age2","Age3", "Age4","Age5", "Age6", "Age7", "Age8")

matogfull<-matog
matogfull$Age9<-matogfull$Age6
matogfull$Age10<-matogfull$Age6
matogfull$Age11<-matogfull$Age6
matogfull$Age12<-matogfull$Age6
matogfull$Age13<-matogfull$Age6
matogfull$Age14<-matogfull$Age6
matogfull$Age15<-matogfull$Age6

write.taf(matogfull,"smoothed_matogive_plusg15.csv", dir="data")
write.taf(matog,"smoothed_matogive_plusg8.csv", dir="data")


natmor<-matogfull[,-1]
ex<-natmor[1,]
natmor<-rbind(ex,ex,ex,ex,ex,ex,ex,ex,ex,ex,ex,ex,ex, natmor)

maturityplus<-cbind(c(1978:year),natmor)
colnames(maturityplus)<-c("Year","age0","age1","age2","age3","age4","age5","age6","age7","age8","age9","age10","age11","age12","age13","age14","age15")

write.taf(maturityplus,"smoothed_matogive_plusg15_full.csv", dir="data")


