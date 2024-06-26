## Preprocess data, write data tables

## Before: dat files (cn, ln, dn, ibn, cw, lw, dw, lf, mo_old, sw_old, nm, pm, pf, survey), csv (CPUE, indices, Exchange), RData (gmt3)
## After: csv files (catage, ibcfrac, landfrac,maturity_old,natmort, wcatch, wibc, wdiscards, wlandings, wstock, wstock_old, wstock_age15, cw_conversion_factors, cw_converted_raw, cw_converted_raw_kg, weight_comb, weights_dataG2, length_comb, length_comb_freq,rezF, smoothed_weight, survey_q1, survey_q3,  ), dat(survey) to udate stockassessmment SAM

rm(list=ls())
#setwd("N:\\1_Tanja\\2_MARLAB\\ICES\\WGNSSK2024\\WHITING_2024\\TAF_2024_whg.27.47d_assessment-master") # Changed the directory


library(icesTAF)
library(stockassessment)
library(FLCore)


#memory.limit(4000)
#devtools::install_github("fishfollower/SAM/stockassessment")
#devtools::install('N:/STOCK_ASSESSMENT/TAF_2019_whg.27.47d_assessment-master/bootstrap/initial/software/SAM-master/stockassessment') # Added this to get stockassessment
#install.packages("FLCore", repos="http://R-Forge.R-project.org") # Added this to get FLCore
#devtools::install('N:\\1_Tanja\\Rpackages\\SAM-master (3)\\SAM-master\\stockassessment')

year<-2024   # assessment, last survey year
datayear<-2023  # catch data
season<-"spring"

taf.boot(clean = FALSE, data = TRUE, software = FALSE) 

mkdir("data")
mkdir("model")
mkdir("report")
mkdir("output")


source("utilities_data.R")


## 1 Update survey indices


indices<-read.csv("bootstrap/data/Indices.csv")

indices1<-indices[indices$Species=="Merlangius merlangus" & indices$Quarter==1 & indices$Year>1982 & indices$Survey=="NS-IBTS", c("Year", "Age_1", "Age_2", "Age_3","Age_4", "Age_5")]
survey_q1<-indices1
survey_q1[,-1]<-round(survey_q1[,-1]/100,3)

indices3<-indices[indices$Species=="Merlangius merlangus" & indices$Quarter==3 & indices$Year>1990 & indices$Survey=="NS-IBTS", c("Year","Age_0", "Age_1", "Age_2", "Age_3","Age_4", "Age_5")]
survey_q3<-indices3
survey_q3[,-1]<-round(survey_q3[,-1]/100,3)

x.idx<-readFLIndices("bootstrap/data/whg47d_survey_old.dat")

# extend time series
x.idx[[1]]<-window(x.idx[[1]],end=year)
x.idx[[2]]<-window(x.idx[[2]],end=datayear)

# IBTS-Q1 - ages 1-5, update all years
tmp<-indices1
yrs<-range(x.idx[[1]])["minyear"]:year
x.idx[[1]]@index<-FLQuant(t(tmp[tmp$Year %in% yrs,ac(paste0("Age_",1:5))]),dimnames=list(ages=1:5,year=yrs))
x.idx[[1]]@catch.n<-x.idx[[1]]@index
x.idx[[1]]@index<-x.idx[[1]]@index/100 # divide by effort
x.idx[[1]]@effort[,ac(yrs)]<-100 # effort

# IBTS-Q3 - ages 0-5, update all years
tmp<-indices3
yrs<-range(x.idx[[2]])["minyear"]:datayear
x.idx[[2]]@index<-FLQuant(t(tmp[tmp$Year %in% yrs,ac(paste0("Age_",0:5))]),dimnames=list(ages=0:5,year=yrs))
x.idx[[2]]@catch.n<-x.idx[[2]]@index
x.idx[[2]]@index<-x.idx[[2]]@index/100 # divide by effort
x.idx[[2]]@effort[,ac(yrs)]<-100 # effort

writeIndicesVPA(x.idx,"data/survey.dat")
write.taf(survey_q1,file="survey_q1.csv",dir="data", row.names=F)
write.taf(survey_q3,file="survey_q3.csv",dir="data", row.names=F)

## 2 read original data

#cn<-read.ices("bootstrap/data/whg47d_cn.dat")
dn<-read.ices("bootstrap/data/whg47d_dn.dat")
ln<-read.ices("bootstrap/data/whg47d_ln.dat")
ibn<-read.ices("bootstrap/data/whg47d_ibn.dat")
#cw<-read.ices("bootstrap/data/whg47d_cw.dat")
sw<-read.ices("bootstrap/data/whg47d_sw_old.dat")  # old file 
dw<-read.ices("bootstrap/data/whg47d_dw.dat")
ibw<-read.ices("bootstrap/data/whg47d_ibw.dat")
#lf<-read.ices("bootstrap/data/whg47d_lf.dat")
lw<-read.ices("bootstrap/data/whg47d_lw.dat")
mo<-read.ices("bootstrap/data/whg47d_mo_old.dat")
nm<-read.ices("bootstrap/data/whg47d_nm.dat")
pf<-read.ices("bootstrap/data/whg47d_pf.dat")
pm<-read.ices("bootstrap/data/whg47d_pm.dat")
#whg47d_ibf<-read.ices("bootstrap/data/whg47d_ibcf.dat")

cn <- ln+dn+ibn
cw <- lw*ln/cn + dw*dn/cn + ibw*ibn/cn
cw[is.nan(cw)] <- lw[is.nan(cw)]      #replace NAs
write.taf(cn,dir="bootstrap/data")
write.taf(cw,dir="bootstrap/data")

cw1<-cw
sw1<-sw
lw1<-lw
dw1<-dw
ibw1<-ibw
cn1<-cn
ln1<-ln
dn1<-dn
ibn1<-ibn
mo1<-mo
nm1<-nm

cutage<-6
low<-0
GE<-which(as.numeric(colnames(cn))>=cutage)
E<-which(as.numeric(colnames(cn))==cutage)
wex<-cn[,GE]/rowSums(cn[,GE])


cnn<-rbind(cn, cn[dim(cn)[1],])
wex1<-cnn[,GE]/rowSums(cnn[,GE])

wD<-dn[,GE]/ifelse(rowSums(dn[,GE])>0,rowSums(dn[,GE]),1)
wI<-ibn[,GE]/ifelse(rowSums(ibn[,GE])>0,rowSums(ibn[,GE]),1)
wL<-ln[,GE]/rowSums(ln[,GE])
cn[,E]<-rowSums(cn[,GE])
cn<-cn[,low:E]
ln[,E]<-rowSums(ln[,GE])
ln<-ln[,low:E]
dn[,E]<-rowSums(dn[,GE])
dn<-dn[,low:E]
ibn[,E]<-rowSums(ibn[,GE])
ibn<-ibn[,low:E]

mo[,E]<-rowSums(mo[,GE]*wex)
mo<-mo[,low:E]
sw[,E]<-rowSums(sw[,GE]*wex)
sw<-sw[,low:E]
cw[,E]<-rowSums(cw[,GE]*wex)
cw<-cw[,low:E]
dw[,E]<-rowSums(dw[,GE]*wD)
dw<-dw[,low:E]
lw[,E]<-rowSums(lw[,GE]*wL)
lw<-lw[,low:E]
ibw[,E]<-rowSums(ibw[,GE]*wI)
ibw<-ibw[,low:E]
nm[,E]<-rowSums(nm[,GE]*wex)
nm<-nm[,low:E]

lf<-ifelse(cn>0,ln/(dn+ln),1) # HC catches
ibf<-ifelse(cn>0,ibn/(cn),1)  # catches
df<-ifelse(cn>0,dn/(cn),1)   #  catches

cw<-round(cw,3)
lw<-round(lw,3)
dw<-round(dw,3)
ibw<-round(ibw,3)

catage<-xtab2taf(cn)
datage<-xtab2taf(dn)
latage<-xtab2taf(ln)
ibcatage<-xtab2taf(ibn)
wcatch<-xtab2taf(cw)
wstock_old<-xtab2taf(sw)
wdiscards<-xtab2taf(dw)
landfrac<-xtab2taf(lf)
dfrac<-xtab2taf(df)
wlandings<-xtab2taf(lw)
wibc<-xtab2taf(ibw)
maturity_old<-xtab2taf(mo)
natmort<-xtab2taf(nm)
propf<-xtab2taf(pf)
propm<-xtab2taf(pm)
ibcfrac<-xtab2taf(ibf)

## 3 Write data tables, 8+

write.taf(catage,dir="data")
write.taf(latage,dir="data")
write.taf(wcatch,dir="data")
write.taf(ibcfrac,dir="data")
write.taf(landfrac,dir="data")
write.taf(wlandings,dir="data")
write.taf(maturity_old,dir="data")
write.taf(natmort,dir="data")
write.taf(wstock_old,dir="data")
write.taf(wdiscards,dir="data")
write.taf(wibc,dir="data")
write.taf(df,dir="data")


## save all data 
save.image( file="data/data.RData")



### 4 update stock weights at age

source("data_stockweights.R")

sw<-read.table("data/wstock.csv", header=T,sep=",")

rep.col<-function(x,n){
  matrix(rep(x,each=n), ncol=n, byrow=TRUE)
}

xtra_col<-rep.col(sw[,ncol(sw)],(15-cutage))

sw<-cbind(sw,xtra_col)

sw<-xtab2taf(sw)
sw<-sw[,-1]

colnames(sw)<-c("Year","age0","age1","age2","age3","age4","age5","age6","age7","age8","age9","age10","age11","age12","age13","age14","age15")
#sw$Year<-c(1978:year)
sw<-rbind(sw,colMeans(sw[(dim(sw)[1]-2):dim(sw)[1],]))
sw[length(sw$Year),1]<-2024


write.taf(sw,"data\\wstock_age15.csv")

## 5 update maturities

source("data_maturity.R")


## 6 update maturities by area

source("data_maturity_byarea.R")  


