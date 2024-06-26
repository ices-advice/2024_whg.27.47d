## Prepare plots and tables for report

## Before: SAM results (RES, RESP, RETRO,LO, CPUE), SURBAR results (surbar_results1), DATRAS CPUE
## After: Tables 17-22, Figures  CPUE maps (for all years in output, final Q1, Q3 in report), SAM, SAM-SURBAR comparison

rm(list=ls())
#setwd("N:\\1_Tanja\\2_MARLAB\\ICES\\WGNSSK2024\\WHITING_2024\\TAF_2024_whg.27.47d_assessment-master") # Changed the directory



library(icesTAF)
library(stockassessment)
library(grid)
library(minpack.lm)
library(lattice)
library(FLCore)

source("utilities_output.R")
source("utilities_report.R")
source("utilities_model.R")

load("data/data.Rdata")
load("model/allmodel.Rdata")
load("model/model.Rdata")
load("model/surbar_results1.Rdata")  # startage 1 SURBAR results


datayear<-max(fit$data$years)-1
startyear<-min(fit$data$years)


source("report_CPUEmaps.R")
source("report_SAMSURBAR_compare.R")

fyear<-year

## 1  Report Plots ##

#IC plots
WtData<-read.table("output\\WtData.csv",sep=",", header=T)

par(mfrow=c(1,1))

### overview of the total amount of landings with discards provided or not

png("report\\Fig_1_DiscardProvisionCountryFleets.png", width=3000, height=1600,res=200)
plotStockOverview(WtData,plotType="DiscProvided",byFleet=TRUE,byCountry=TRUE,bySampled=TRUE,bySeason=FALSE,
                  byArea=FALSE,countryColours=TRUE,set.mar=TRUE,markSampled=TRUE,individualTotals=FALSE)
dev.off()

png("report\\Fig_2_landingsPercCountryFleets_all.png", width=3000, height=1300,res=200)
plotStockOverview(WtData,byFleet=TRUE,byCountry=TRUE,bySampled=TRUE,bySeason=FALSE,
                  byArea=FALSE,countryColours=TRUE,set.mar=TRUE,markSampled=TRUE,individualTotals=FALSE)
dev.off()

png("report\\Fig_2_discardsCountryFleets_all.png", width=2200, height=1300,res=200)
plotStockOverview(WtData,plotType="DisWt",byFleet=TRUE,byCountry=TRUE,bySampled=TRUE,bySeason=FALSE,
                  byArea=FALSE,countryColours=TRUE,set.mar=TRUE,markSampled=TRUE,individualTotals=FALSE)

dev.off()


d<-read.table("bootstrap/data/whg47d_di.dat",skip=5)
l<-read.table("bootstrap/data/whg47d_la.dat",skip=5)
ib<-read.table("bootstrap/data/whg47d_iby.dat",skip=5)
c<-d+ib+l

taf.png("Fig_3_Yield")  
par(mfrow = c(1,1),cex=1.3, mar = c(4,4,2,1) + 0.1)
plot(c(1978:datayear),t(l)/1000,lwd=3, lty="solid", col="black", type="l",bty="l", ylim=c(0,120),ylab="Yield (1000 tonnes)" ,xlab="Year")
lines(c(1978:datayear),t(d)/1000,lwd=3, lty="solid", col="purple", type="l")
lines(c(1978:datayear),t(ib)/1000,lwd=3, lty="solid", col="darkorange", type="l")
legend("topright", legend=c("Landings","Discards + BMS","IBC"),  col=c("black","purple","darkorange"),lwd=3, bty="n" )
dev.off()


# prop discarded of total catches, does not work with taf.png
df<-read.taf("data/df.csv")

png('report\\Fig_4_prop_discarded.png',  width=2800, height=2600, res=300)
par(mfrow = c(3,3),cex=1, mar = c(3,2,1,1))
for(i in 1:(cutage+1)){plot(c(1978:datayear),df[,i],lwd=1.8, lty="solid" ,col="black", type="l",ylab="", ylim=c(0,1.1),xlab="Year")
  legend("topleft",paste("Age",i-1,sep=" "), col="black", bty="n")
}
dev.off()

# plot mean weights at age

sweights<-read.csv("data\\smoothed_weight.csv",header=TRUE)

cweights<-read.csv("data\\wcatch.csv", header=TRUE) 
lweights<-read.csv("data\\wlandings.csv", header=TRUE) 
dweights<-read.csv("data\\wdiscards.csv", header=TRUE) 
iweights<-read.csv("data\\wibc.csv", header=TRUE) 

lweights[lweights==0]<-NA
dweights[dweights==0]<-NA
iweights[iweights==0]<-NA


png('report\\Fig_5_catchmeanweights.png',  width=3400, height=3000, res=400)

par(mfrow = c(2,2),cex=1, mar = c(4,4,2,1) + 0.1)

plot(cweights$Year, cweights$X0*1000, col="magenta", ylim=c(0,850), lwd=1.6, xlab="", ylab="Mean Weight (g)", bty="l",type="l",main="Total catch")
lines(cweights$Year, cweights$X1*1000, col="red", pch=20, lwd=1.6)
lines(cweights$Year, cweights$X2*1000, col="green",pch=20, lwd=1.6)
lines(cweights$Year, cweights$X3*1000, col="blue", lwd=1.6,pch=20)
lines(cweights$Year, cweights$X4*1000, col="cyan", pch=20, lwd=1.6)
lines(cweights$Year, cweights$X5*1000, col="black", pch=20, lwd=1.6)
lines(cweights$Year, cweights$X6*1000, col="purple", pch=20, lwd=1.6)

legend("topright", legend=c("0","1","2","3","4","5","6+"),  col=c("magenta","red","green","blue","cyan","black","purple"),lwd=1.6, bty="n",ncol=2 )

plot(lweights$Year, lweights$X0*1000, col="magenta", ylim=c(0,850), lwd=1.6, xlab="", ylab="Mean Weight (g)", bty="l",type="l", main="Landings")
lines(lweights$Year, lweights$X1*1000, col="red", pch=20, lwd=1.6)
lines(lweights$Year, lweights$X2*1000, col="green",pch=20, lwd=1.6)
lines(lweights$Year, lweights$X3*1000, col="blue", lwd=1.6,pch=20)
lines(lweights$Year, lweights$X4*1000, col="cyan", pch=20, lwd=1.6)
lines(lweights$Year, lweights$X5*1000, col="black", pch=20, lwd=1.6)
lines(lweights$Year, lweights$X6*1000, col="purple", pch=20, lwd=1.6)

plot(dweights$Year, dweights$X0*1000, col="magenta", ylim=c(0,850), lwd=1.6, xlab="Year", ylab="Mean Weight (g)", bty="l",type="l", main="Discards + BMS")
lines(dweights$Year, dweights$X1*1000, col="red", pch=20, lwd=1.6)
lines(dweights$Year, dweights$X2*1000, col="green",pch=20, lwd=1.6)
lines(dweights$Year, dweights$X3*1000, col="blue", lwd=1.6,pch=20)
lines(dweights$Year, dweights$X4*1000, col="cyan", pch=20, lwd=1.6)
lines(dweights$Year, dweights$X5*1000, col="black", pch=20, lwd=1.6)
lines(dweights$Year, dweights$X6*1000, col="purple", pch=20, lwd=1.6)

plot(iweights$Year, iweights$X0*1000, col="magenta", ylim=c(0,850), lwd=1.6, xlab="Year", ylab="Mean Weight (g)", bty="l",type="l", main="IBC")
lines(iweights$Year, iweights$X1*1000, col="red", pch=20, lwd=1.6)
lines(iweights$Year, iweights$X2*1000, col="green",pch=20, lwd=1.6)
lines(iweights$Year, iweights$X3*1000, col="blue", lwd=1.6,pch=20)
lines(iweights$Year, iweights$X4*1000, col="cyan", pch=20, lwd=1.6)
lines(iweights$Year, iweights$X5*1000, col="black", pch=20, lwd=1.6)
lines(iweights$Year, iweights$X6*1000, col="purple", pch=20, lwd=1.6)

dev.off()

Mat0<-read.table("output\\sw_plotdata.csv",sep=",", header=T)
png('report\\Fig_6_sw_corrected_weights.png',  width=2600, height=2000, res=400)

par(mfrow = c(1,1),cex=1, mar = c(4,4,2,1) + 0.1)
plot(Mat0[Mat0$age==0,"year"], Mat0[Mat0$age==0,"weight"], col="transparent", ylim=c(0,850), xlab="Year", ylab="Weight (g)", bty="l")
axis(2,at=c(1:9)*100)
lines(Mat0[Mat0$age==0,"year"], Mat0[Mat0$age==0,"weight"], col="magenta", lwd=1.6)
lines(Mat0[Mat0$age==1,"year"], Mat0[Mat0$age==1,"weight"], col="red", lwd=1.6)
lines(Mat0[Mat0$age==2,"year"], Mat0[Mat0$age==2,"weight"], col="green", lwd=1.6)
lines(Mat0[Mat0$age==3,"year"], Mat0[Mat0$age==3,"weight"], col="blue", lwd=1.6)
lines(Mat0[Mat0$age==4,"year"], Mat0[Mat0$age==4,"weight"], col="cyan",  lwd=1.6)
lines(Mat0[Mat0$age==5,"year"], Mat0[Mat0$age==5,"weight"], col="black",  lwd=1.6)
lines(Mat0[Mat0$age==6,"year"], Mat0[Mat0$age==6,"weight"] ,col="purple",  lwd=1.6)

legend("topright", legend=c("0","1","2","3","4","5","6+"),  col=c("magenta","red","green","blue","cyan","black","purple"),lwd=1.6, bty="n",ncol=2 )

dev.off()



# Fig mort, mat
#maturity plot and table
natmor<-read.table("data\\smoothed_matogive_plusg15_full.csv", sep=",", h=T)

png('report\\Fig_7_Maturity.png',  width=2600, height=2000, res=400)
par(mfrow = c(1,1),cex=1, mar = c(4,4,2,1) + 0.1)
plot(c(1978:year), natmor[,2], col="magenta", ylim=c(0,1.15), xlab="Year", ylab="Proportion mature",lwd=2,type="l", bty="l")
lines(c(1978:year), natmor[,3], col="red", lwd=2)
lines(c(1978:year), natmor[,4], col="green", lwd=2)
lines(c(1978:year), natmor[,5], col="blue", lwd=2)
lines(c(1978:year), natmor[,6], col="cyan", lwd=2)
lines(c(1978:year), natmor[,7], col="black", lwd=2)
lines(c(1978:year), natmor[,8], col="grey", lwd=2)

legend("topright", legend=c("0","1","2","3","4","5","6+"),  col=c("magenta","red","green","blue","cyan","black","grey"),lwd=rep(2,7),ncol=4, bty="n" )

dev.off()


natmor<-read.table("data/natmort.csv",sep=",", header=T)
natmor<-natmor[,-1]

png('report\\Fig_8_NaturalMortality.png',  width=2600, height=2000, res=400)
par(mfrow = c(1,1),cex=1, mar = c(4,4,2,1) + 0.1)
plot(c(1978:datayear), natmor[,1], col="magenta", ylim=c(0,3), xlab="Year", ylab="Natural mortality (M)",lwd=2,type="l", bty="l")
lines(c(1978:datayear), natmor[,2], col="red", lwd=2)
lines(c(1978:datayear), natmor[,3], col="green", lwd=2)
lines(c(1978:datayear), natmor[,4], col="blue", lwd=2)
lines(c(1978:datayear), natmor[,5], col="cyan", lwd=2)
lines(c(1978:datayear), natmor[,6], col="black", lwd=2)
lines(c(1978:datayear), natmor[,7], col="blueviolet", lwd=2)

legend("topleft", legend=c("0","1","2","3","4","5","6+"),  col=c("magenta","red","green","blue","cyan","black","blueviolet"),lwd=rep(2,9),ncol=2, bty="n" )

dev.off()



# Survey/SURBAR plots
###########################################################################################################################
load("model/surbar_index1.Rdata")  # startage 1 SURBAR results
load("model/surbar_results1.Rdata")  # startage 1 SURBAR results

#Figure : Survey log CPUE at age
png('report\\Fig_11_LogCpue_cohort.png', width=1200, height=1000,res=200)
plot.log.cpue(wk.x = s.index, wk.do.legend = TRUE, wk.ages = 1:5, wk.lty = c(1,1), wk.col = c(2,3), wk.lwd=c(2,2))
dev.off()

# Figure : Survey catch curves
png('report\\Fig_12_surveyCatchcurveQ1.png', width=1400, height=1400,res=200)
plot.surbar(s.results, "catch.curve", nums=1)
dev.off()

#Figure  : Within-survey correlations for IBTS Q1
png('report\\Fig_13_withinsurvey_correlationsQ1.png', width=1200, height=1200,res=200)
  plot.index.corr(s.results$s.idx[1], wk.type = "SURBAR")
  dev.off()
  

png('report\\Fig_15_log_by_cohort.png', width=1200, height=1200,res=200)
plot.surbar(s.results, "log.by.cohort")
dev.off()

# SURBAR stock summary
png('report\\Fig_16_surbarSummary.png', width=1400, height=1100,res=200)
plot.surbar(s.results, "sum.line")
dev.off()

# SURBAR log survey residuals
png('report\\Fig_17_surbarresiduals.png', width=1400, height=1400,res=200)
plot.surbar(s.results, "res.smooth")
dev.off()

# SURBAR parameter estimates
png('report\\Fig_18_surbarparameterEstimates.png', width=1700, height=1000,res=200)
plot.surbar(s.results, "params")
dev.off()



#Figure : Commercial catch curves
png('report\\Fig_19_logcommercialcc.png', width=1400, height=1200,res=200)
plot.catch.curve.and.grads(f.stock, wk.ptype = "c", wk.ages = mean.f.range, 
                           wk.main = "Commercial Catch Data", wk.yrs = f.stock@range["minyear"]:f.stock@range["maxyear"])
dev.off()

#Figure : Commercial catch curve gradients
png('report\\Fig_20_commercialccgradients.png', width=1400, height=1200,res=200)
par(mfrow = c(1,1), mar = c(5,5,4,3))
plot.catch.curve.and.grads(f.stock, wk.ptype = "g", wk.ages = mean.f.range, 
                           wk.main = "Commercial Catch Data", wk.yrs = f.stock@range["minyear"]:f.stock@range["maxyear"])
dev.off()


#Figure : Commercial catch correlations
png('report\\Fig_21_commercialccorrelations.png', width=1200, height=1200,res=200)
plot.index.corr(wk.object=list(FLIndex(catch.n = f.stock@catch.n[,-(dim(f.stock)[2]),,,,], name = "Catch numbers at age")),
                wk.type = "FLR")
dev.off()

#####################################################################################################
# Figures SAM

taf.png("Fig_22_SSB")
ssbplot(fit,addCI=T,xlab="Year", las=0)
dev.off()

taf.png("Fig_22_Fbar")
fbarplot(fit,xlab="Year",partial=F,addCI=TRUE)
dev.off()

taf.png("Fig_22_Rec")
recplot(fit,xlab="Year",las=0,drop=1,addCI=TRUE)
dev.off()


taf.png("Fig_22_Catches")
catchplot(fit,xlab="Year",las=0)
dev.off()

taf.png("Fig_23_Est_correlations")
corplot(fit)
dev.off()


attr(RESP, 'fleetNames')[[2]]<- c("Joint sample residuals log(F)") 
taf.png("Fig_24_Process Residuals",width=1600,height=1500)
plot(RESP)
dev.off()

taf.png("Fig_25_Residuals",width=1300,height=1300)
plot(RES)
dev.off()


fig<-26:28

for(f in 1:fit$data$noFleets){
  taf.png(paste("Fig_",fig[f],"_observed_predicted_fleet_",f,sep=""))
  stockassessment::fitplot(fit,log=F, fleets=f)
  dev.off()
}


taf.png("Fig_29_Leaveoneout_SSB")
ssbplot(LO,xlab="Year")
dev.off()

taf.png("Fig_29_Leaveoneout_Fbar")
fbarplot(LO, xlab="Year")
dev.off()

taf.png("Fig_29_Leaveoneout_Rec")
recplot(LO, xlab="Year", drop=1)
dev.off()

taf.png("Fig_29_Leaveoneout_Catch")
catchplot(LO, xlab="Year")
dev.off()

taf.png("Fig_30_Retro_SSB")
ssbplot(RETRO,xlab="Year", las=0, drop=0)
dev.off()

taf.png("Fig_30_Retro_Fbar")  
fbarplot(RETRO, las=0, drop=1, xlab="Year")
dev.off()

taf.png("Fig_30_Retro_Rec")
recplot(RETRO,xlab="Year", las=0, drop=1)
dev.off()

taf.png("Fig_30_Retro_Catch")
catchplot(RETRO,xlab="Year",las=0)
dev.off()

taf.png("Fig_31_SR")
srplot(fit)
dev.off()


# plot F at age

# forecastF<-read.table("output\\forecastF.csv",h=T, sep=",")
# Fs       <-read.table("output\\recentF.csv",h=T, sep=",")
# 
# taf.png("Fig_33_Fs")
# plot(0:8, Fs[1,], type = "n", lty = 1,bty="L", xlab = "Age", ylab = "F", ylim = c(0,0.5))
# 
# lines(0:8, Fs[1,], lty = 1, col = "royalblue", lwd = 4)
# lines(0:8, Fs[2,], lty = 1, col = "darkorange", lwd = 4)
# lines(0:8, Fs[3,], lty = 1, col = "green", lwd = 4)
# lines(0:8, forecastF[,], lty = 1, col = "purple", lwd =8)
# 
# legend(legend = c(as.character(year-3), as.character(year-2),as.character(year-1), "Forecast F"), x = "topleft",
#        lty = c(1,1,1,1), lwd = c(4,4,4,8), bty = "n", col = c("royalblue", "darkorange", "green","purple"))
# dev.off()


# maturity ogive by area

data<-read.table("data\\ogives.year.whgt2.csv",h=T, sep=",")
data_area<-read.table("data\\ogives.year.AREA.csv",sep=",",h=T)

data1<-subset(data,age==1)
data1_areaN<-data_area[data_area$age==1 & data_area$area=="NORTH",]
data1_areaS<-data_area[data_area$age==1 & data_area$area=="SOUTH",]


png('report\\Fig_37_mat_age_compare.png',  width=2600, height=2000, res=400)

with(data,plot(year,mat,type="n",lwd=2,xlim=c(1990,fyear),ylim=c(0,0.9),xaxt="n",yaxt="n",xlab="",ylab=""))
axis(1,at=seq(1990,fyear,1),labels=seq(1990,fyear,1), mgp=c(3, .9, 0))
mtext("Year", side=1, line=3, cex.lab=2)
axis(2,at=seq(0,1,0.1),labels=seq(0,1,0.1), mgp=c(3, .9, 0))
mtext("Proportion mature age 1", side=2, line=3,cex.lab=2,las=3)

polygon(c(data1$year, rev(data1$year)), c(data1$upr, rev(data1$lwr)),col =adjustcolor("darkgrey",alpha.f=0.2), border = NA)
with(data1,points(year,mat,type="l",lwd=2,col="black"))

polygon(c(data1_areaN$year, rev(data1_areaN$year)), c(data1_areaN$upr, rev(data1_areaN$lwr)),col =adjustcolor("purple",alpha.f=0.2), border = NA)
with(data1_areaN,points(year,mat,type="l",lwd=2,col="purple"))

polygon(c(data1_areaS$year, rev(data1_areaS$year)), c(data1_areaS$upr, rev(data1_areaS$lwr)),col =adjustcolor("orange",alpha.f=0.2), border = NA)
with(data1_areaS,points(year,mat,type="l",lwd=2,col="orange"))

legend("bottomright", c("NORTH", "SOUTH", "combined"), lty = 1.5,bty="n", col =c("purple","orange","black"))


dev.off()



##  4 write report tables 15, 8+

#cn<-read.ices("bootstrap/data/whg47d_cn.dat")
dn<-read.ices("bootstrap/data/whg47d_dn.dat")
ln<-read.ices("bootstrap/data/whg47d_ln.dat")
ibn<-read.ices("bootstrap/data/whg47d_ibn.dat")
#cw<-read.ices("bootstrap/data/whg47d_cw.dat")
dw<-read.ices("bootstrap/data/whg47d_dw.dat")
ibw<-read.ices("bootstrap/data/whg47d_ibw.dat")
#lf<-read.ices("bootstrap/data/whg47d_lf.dat")
lw<-read.ices("bootstrap/data/whg47d_lw.dat")
nm<-read.ices("bootstrap/data/whg47d_nm.dat")
pf<-read.ices("bootstrap/data/whg47d_pf.dat")
pm<-read.ices("bootstrap/data/whg47d_pm.dat")
#whg47d_ibf<-read.ices("bootstrap/data/whg47d_ibcf.dat")

cn <- ln+dn+ibn
cw <- lw*ln/cn + dw*dn/cn + ibw*ibn/cn
cw[is.nan(cw)] <- lw[is.nan(cw)]        # replace NAs


cw1<-cw
lw1<-lw
dw1<-dw
ibw1<-ibw
cn1<-cn
ln1<-ln
dn1<-dn
ibn1<-ibn
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
dbf<-ifelse(cn>0,dn/(cn),1)   # HC catches

cw<-round(cw,3)
lw<-round(lw,3)
dw<-round(dw,3)
ibw<-round(ibw,3)

cw1<-round(cbind(cw1,cw[,E]),3)
lw1<-round(cbind(lw1,lw[,E]),3)
dw1<-round(cbind(dw1,dw[,E]),3)
ibw1<-round(cbind(ibw1,ibw[,E]),3)
cn1<-round(cbind(cn1,cn[,E]),3)
ln1<-round(cbind(ln1,ln[,E]),3)
dn1<-round(cbind(dn1,dn[,E]),3)
ibn1<-round(cbind(ibn1,ibn[,E]),3)
nm1<-round(cbind(nm1,nm[,E]),3)

catageplus<-xtab2taf(cn1)
datageplus<-xtab2taf(dn1)
latageplus<-xtab2taf(ln1)
ibcatageplus<-xtab2taf(ibn1)
wcatchplus<-xtab2taf(cw1)
wdiscardsplus<-xtab2taf(dw1)
wlandingsplus<-xtab2taf(lw1)
wibcplus<-xtab2taf(ibw1)
natmortplus_<-xtab2taf(nm1)

natmortplus<-cbind(natmortplus_[,1:(cutage+1)],natmortplus_[,dim(natmortplus_)[2]])
colnames(natmortplus)<-c("Year","age0","age1","age2","age3","age4","age5","age6+")

sw<-read.table("data/cw_converted_raw_kg.csv",header=T, sep=",")
sw[,-1]<-round(sw[,-1],4)
wstockplus<-cbind(sw[,1:(cutage+1)],sw[,dim(sw)[2]])
colnames(wstockplus)<-c("Year","age0","age1","age2","age3","age4","age5","age6+")

colnames(catageplus)<-c("Year","age0","age1","age2","age3","age4","age5","age6","age7","age8","age9","age10","age11","age12","age13","age14","age15","6+")
colnames(latageplus)<-c("Year","age0","age1","age2","age3","age4","age5","age6","age7","age8","age9","age10","age11","age12","age13","age14","age15","6+")
colnames(datageplus)<-c("Year","age0","age1","age2","age3","age4","age5","age6","age7","age8","age9","age10","age11","age12","age13","age14","age15","6+")
colnames(ibcatageplus)<-c("Year","age0","age1","age2","age3","age4","age5","age6","age7","age8","age9","age10","age11","age12","age13","age14","age15","6+")
colnames(wcatchplus)<-c("Year","age0","age1","age2","age3","age4","age5","age6","age7","age8","age9","age10","age11","age12","age13","age14","age15","6+")
colnames(wlandingsplus)<-c("Year","age0","age1","age2","age3","age4","age5","age6","age7","age8","age9","age10","age11","age12","age13","age14","age15","6+")
colnames(wdiscardsplus)<-c("Year","age0","age1","age2","age3","age4","age5","age6","age7","age8","age9","age10","age11","age12","age13","age14","age15","6+")
colnames(wibcplus)<-c("Year","age0","age1","age2","age3","age4","age5","age6","age7","age8","age9","age10","age11","age12","age13","age14","age15","6+")


#maturity plot and table
mo<-read.table("data\\smoothed_matogive_plusg15_full.csv", sep=",", h=T)
mo[,-1]<-round(mo[,-1],3)
maturityplus<-cbind(mo[,1:(cutage+1)],mo[,dim(mo)[2]])
colnames(maturityplus)<-c("Year","age0","age1","age2","age3","age4","age5","age6+")


## 3 Write report tables

write.taf(catageplus,file="Table_4_catage.csv", dir="report", row.names=T)
write.taf(latageplus,file="Table_5_latage.csv",dir="report", row.names=T)
write.taf(datageplus,file="Table_6_datage.csv",dir="report", row.names=T)
write.taf(ibcatageplus,file="Table_7_ibcatage.csv",dir="report", row.names=T)
write.taf(wcatchplus,file="Table_8_cweights.csv",dir="report", row.names=T)
write.taf(wlandingsplus,file="Table_9_lweights.csv",dir="report", row.names=T)
write.taf(wdiscardsplus,file="Table_10_dweights.csv",dir="report", row.names=T)
write.taf(wibcplus,file="Table_11_iweights.csv",dir="report", row.names=T)
write.taf(wstockplus,file="Table_13_stweights.csv",dir="report", row.names=T)
write.taf(maturityplus,file="Table_14_mat.csv",dir="report", row.names=T)
write.taf(natmortplus,file="Table_15_natmor.csv",dir="report", row.names=T)

# ICES catches

uw<-read.table("bootstrap/data/whg47d_di.dat",skip=5)
w<-read.table("bootstrap/data/whg47d_la.dat",skip=5)
i<-read.table("bootstrap/data/whg47d_iby.dat",skip=5)
c<-w+uw+i
cc<-cbind(c(1978:datayear),c,w,uw,i)
cc[,-1]<-round(cc[,-1])
colnames(cc)<-c("Year", "Catch", "Landings","Discards + BMS", "IBC")
write.taf(cc,"Table_12_ICES_catches.csv", dir="report",row.names=T)


# surveys

s1<-read.table("data\\survey_q1.csv",sep=",", h=T)
s3<-read.table("data\\survey_q3.csv",sep=",", h=T)
write.taf(s1,file="Table_16_survey_q1.csv",dir="report", row.names=F)
write.taf(s3,file="Table_16_survey_q3.csv",dir="report", row.names=F)


#Tables SAM
#
ftab<-read.taf("output/Fatage.csv")
ftab[,2:(cutage+2)]<-round(ftab[,2:(cutage+2)],3)
write.taf(ftab,"Table_17_F_SAM.csv", dir="report")

ntab<-read.taf("output/natage.csv")
ntab[,2:(cutage+2)]<-round(ntab[,2:(cutage+2)])
write.taf(ntab,"Table_18_N_SAM.csv", dir="report")

summary<-read.taf("output/summary.csv")
write.taf(summary,"Table_19_Summary_SAM_I.csv", dir="report")

ctab<-read.taf("output/catchtab.csv")
ctab[,2:4]<-round(ctab[,2:4])
write.taf(ctab,"Table_20_Summary_SAM_II.csv", dir="report")



ptab <- partable(fit)
ptab<-xtab2taf(ptab)
ptab[,-1]<-round(ptab[,-1],3)
colnames(ptab)[1]<-" "
write.taf(ptab,"Table_21_SAM_model_parameters.csv",dir="report")


mohn<-round(stockassessment::mohn(RETRO, what = NULL, lag=0),4)
mohn<-as.data.frame(mohn)
write.taf(mohn,"Table_22_mohnsrho_lag0_SSB.csv", dir="report", row.names=T)

mohn<-round(stockassessment::mohn(RETRO, what = NULL,lag=1),4)
mohn<-as.data.frame(mohn)
write.taf(mohn,"Table_22_mohnsrho_lag1_Rec_F.csv", dir="report", row.names=T)


#forcast input
# rec_for<-read.taf(paste0("output/Rec_forecast_", season,".csv"))
# colnames(rec_for)<-c("Year","forcast R")
# write.taf(rec_for,paste0("Table_24_Rec_forecast_",season,".csv"), dir="report", row.names=T)


ave.years<-max(fit$data$years) +(-3:-1)

doAve  <- function(x,y)colMeans(x[rownames(x)%in%ave.years,,drop=FALSE]) 
doAve1  <- function(x,y)colMeans(x[rownames(x)%in%ave.years,,]) 

ave.sw <- doAve(fit$data$stockMeanWeight)
ave.cw <- doAve1(fit$data$catchMeanWeight)
ave.mo <- doAve(fit$data$propMat)
ave.nm <- doAve(fit$data$natMor)
ave.lw <- doAve1(fit$data$landMeanWeight)

# to calculate partial Fibc########################
ibcf<-read.taf("data/ibcfrac.csv")
# use recent 3 years of fishing selectivity
selYears<-max(fit$data$years) +(-3:-1)  
# mean recent 3 years of proportion ibc
ibc_mean<-apply(ibcf[length(ibcf[,1])+(-2:0),-1],2,mean) 

#estimated total F at age
summary_F<-summary(fit)
f_est    <-summary_F[,7]
#total selectivity
fsel <- faytable(fit)/f_est
# mean recent 3 years of F, scaled to final catch data year
catch_mean<-apply(fsel[length(fsel[,1])+(-3:-1),],2,mean)*f_est[length(f_est)-1] 
IBC_f <-ibc_mean*catch_mean
F_ibc <-c(IBC_f[[1]],IBC_f[[2]],IBC_f[[3]],IBC_f[[4]],IBC_f[[5]],IBC_f[[6]],IBC_f[[7]])


# fortab<-cbind(c(0:8), round(forecastF,4),round(t(ntab[dim(ntab)[[1]],2:10]),4) ,round(ave.nm,4),round(ave.mo,4), 0,0, round(ave.sw,4), round(ave.cw,4),round(ave.lw,4), round(ave.cw,4), round(F_ibc,4) )
# colnames(fortab)<-c("Age","F","N","M", "Mat", "PF","PM","SWt","CWt","LWt", "IBCWt","FIBC" )
# fortab["Age 0","N"]<-rec_for[rec_for$Year==year,2]
# 
# write.taf(fortab,"Table_25_forecastinput.csv", dir="report", row.names=F)

# advice tables for report
# ad_spring<-read.taf(paste0("output/Advice_Table_intermediate_year_", season,".csv"), sep=",")
# write.taf(ad_spring,"Table_26a_forecast_intermediate.csv", dir="report", row.names=F)
# 
# ad_spring2<-read.taf(paste0("output/Advice_Table_", season,".csv"), sep=",")
# write.taf(ad_spring2,"Table_26b_forecast.csv", dir="report", row.names=F)



matN<-read.table("data\\smoothed_matogive_plusg_North.csv", sep=",", h=T)
matN[,-1]<-round(matN[,-1],3)
matN<-cbind(matN[,1],0,matN[-1])
colnames(matN)<-c("Year","Age0", "Age1", "Age2", "Age3","Age4","Age5", "Age6+")
write.taf(matN,"Table_29_matN.csv", dir="report")

matS<-read.table("data\\smoothed_matogive_plusg_South.csv", sep=",", h=T)
matS[,-1]<-round(matS[,-1],3)
matS<-cbind(matS[,1],0,matS[-1])
colnames(matS)<-c("Year","Age0", "Age1", "Age2", "Age3","Age4","Age5", "Age6+")
write.taf(matS,"Table_30_matS.csv", dir="report")


#######################################################################################################################

load("model\\surbar_results_north.Rdata")
s.north<-s.results
load("model\\surbar_results_south.Rdata")
s.south<-s.results
load("model\\surbar_results1.Rdata")
s.all<-s.results

for(i in 1:3){
  
  if(i==1) s.results<-s.north
  if(i==2) s.results<-s.south
  if(i==3) s.results<-s.all

  wk.stock<-s.results$s.stock
  wk.psim<-s.results$s.psim
  wk.y1 <- s.results$s.y1
  wk.y2 <- s.results$s.y2
  
  # meanZ
  wk.stock.meanz <- do.call(rbind, lapply(wk.psim, function(wk){wk$meanz}))
  wk.stock.meanz.quantile <- array(NA, dim = c(dim(wk.stock.meanz)[2],5))
  wk.stock.meanz.mean <- rep(NA, dim(wk.stock.meanz)[2])
  rownames(wk.stock.meanz.quantile) <- wk.y1:wk.y2
  colnames(wk.stock.meanz.quantile) <- c("5%","25%","50%","75%","95%")
  
  for (wk.i in 1:dim(wk.stock.meanz)[2])
  {
    wk.stock.meanz.quantile[wk.i,] <- quantile(wk.stock.meanz[,wk.i], c(0.05, 0.25, 0.5, 0.75, 0.95))
    wk.stock.meanz.mean[wk.i] <- mean(wk.stock.meanz[,wk.i])
  }
  
  # SSB
  wk.stock.ssb <- do.call(rbind, lapply(wk.psim, function(wk){wk$ssb}))
  wk.stock.ssb.quantile <- array(NA, dim = c(dim(wk.stock.ssb)[2],5))
  wk.stock.ssb.mean <- rep(NA, dim(wk.stock.ssb)[2])
  rownames(wk.stock.ssb.quantile) <- wk.y1:wk.y2
  colnames(wk.stock.ssb.quantile) <- c("5%","25%","50%","75%","95%")
  for (wk.i in 1:dim(wk.stock.ssb)[2])
  {
    wk.stock.ssb.quantile[wk.i,] <- quantile(wk.stock.ssb[,wk.i], c(0.05, 0.25, 0.5, 0.75, 0.95))
    wk.stock.ssb.mean[wk.i] <- mean(wk.stock.ssb[,wk.i])
  }
  
  # TSB
  wk.stock.tsb <- do.call(rbind, lapply(wk.psim, function(wk){wk$tsb}))
  wk.stock.tsb.quantile <- array(NA, dim = c(dim(wk.stock.tsb)[2],5))
  wk.stock.tsb.mean <- rep(NA, dim(wk.stock.tsb)[2])
  rownames(wk.stock.tsb.quantile) <- wk.y1:wk.y2
  colnames(wk.stock.tsb.quantile) <- c("5%","25%","50%","75%","95%")
  for (wk.i in 1:dim(wk.stock.tsb)[2])
  {
    wk.stock.tsb.quantile[wk.i,] <- quantile(wk.stock.tsb[,wk.i], c(0.05, 0.25, 0.5, 0.75, 0.95))
    wk.stock.tsb.mean[wk.i] <- mean(wk.stock.tsb[,wk.i])
  }
  
  # Recruitment
  
  wk.stock.rec <- do.call(rbind, lapply(wk.psim, function(wk){wk$rec}))
  wk.stock.rec.quantile <- array(NA, dim = c(dim(wk.stock.rec)[2],5))
  wk.stock.rec.mean <- rep(NA, dim(wk.stock.rec)[2])
  rownames(wk.stock.rec.quantile) <- wk.y1:wk.y2
  colnames(wk.stock.rec.quantile) <- c("5%","25%","50%","75%","95%")
  for (wk.i in 1:dim(wk.stock.rec)[2])
  {
    wk.stock.rec.quantile[wk.i,] <- quantile(wk.stock.rec[,wk.i], c(0.05, 0.25, 0.5, 0.75, 0.95))
    wk.stock.rec.mean[wk.i] <- mean(wk.stock.rec[,wk.i])
  }
  
  
  if(i==1){wk.stock.meanz.quantile1<-wk.stock.meanz.quantile
  wk.stock.ssb.quantile1<-wk.stock.ssb.quantile
  wk.stock.tsb.quantile1<-wk.stock.tsb.quantile
  wk.stock.rec.quantile1<-wk.stock.rec.quantile
  }
  
  if(i==2){wk.stock.meanz.quantile2<-wk.stock.meanz.quantile
  wk.stock.ssb.quantile2<-wk.stock.ssb.quantile
  wk.stock.tsb.quantile2<-wk.stock.tsb.quantile
  wk.stock.rec.quantile2<-wk.stock.rec.quantile
  }
  if(i==3){wk.stock.meanz.quantile3<-wk.stock.meanz.quantile
  wk.stock.ssb.quantile3<-wk.stock.ssb.quantile
  wk.stock.tsb.quantile3<-wk.stock.tsb.quantile
  wk.stock.rec.quantile3<-wk.stock.rec.quantile
  }

}


png('report\\Fig_36_plot_Z.png', width=1200, height=1000,res=200)
plot(wk.y1:wk.y2, wk.stock.meanz.quantile2[,3], type = "n", lty = 1,bty="L",
     xlab = "Year", ylab = "Mean Z", 
     ylim = c(min(0, wk.stock.meanz.quantile2), max(wk.stock.meanz.quantile2)))

lines(wk.y1:wk.y2, wk.stock.meanz.quantile1[,3], lty = 1, col = "darkgrey", lwd = 2)
lines(wk.y1:wk.y2, wk.stock.meanz.quantile2[,3], lty = 1, col = "black", lwd = 2)
lines(wk.y1:wk.y2, wk.stock.meanz.quantile3[,3], lty = 1, col = "green", lwd = 2)

legend(legend = c("North", "South","combined"), x = "bottomleft",
       lty = c(1,1,1), lwd = c(2,2,2), bty = "n", col = c("darkgrey", "black", "green"))
dev.off()

png('report\\Fig_36_plot_SSB.png', width=1200, height=1000,res=200)
plot(wk.y1:wk.y2, wk.stock.ssb.quantile2[,3], type = "n", lty = 1,bty="L",
     xlab = "Year", ylab = "SSB", 
     ylim = c(min(0, wk.stock.ssb.quantile2), max(wk.stock.ssb.quantile2)))

lines(wk.y1:wk.y2, wk.stock.ssb.quantile1[,3], lty = 1, col = "darkgrey", lwd = 2)
lines(wk.y1:wk.y2, wk.stock.ssb.quantile2[,3], lty = 1, col = "black", lwd = 2)
lines(wk.y1:wk.y2, wk.stock.ssb.quantile3[,3], lty = 1, col = "green", lwd = 2)

legend(legend = c("North", "South","combined"),x="topleft",
       lty = c(1,1,1), lwd = c(2,2,2), bty = "n", col = c("darkgrey", "black","green"))
dev.off()

png('report\\Fig_36_plot_TSB.png', width=1200, height=1000,res=200)
plot(wk.y1:wk.y2, wk.stock.tsb.quantile2[,3], type = "n", lty = 1,bty="L",
     xlab = "Year", ylab = "TSB", 
     ylim = c(min(0, wk.stock.tsb.quantile2), max(wk.stock.tsb.quantile2)))

lines(wk.y1:wk.y2, wk.stock.tsb.quantile1[,3], lty = 1, col = "darkgrey", lwd = 2)
lines(wk.y1:wk.y2, wk.stock.tsb.quantile2[,3], lty = 1, col = "black", lwd = 2)
lines(wk.y1:wk.y2, wk.stock.tsb.quantile3[,3], lty = 1, col = "green", lwd = 2)

legend(legend = c("North", "South","combined"), x = "topleft",
       lty = c(1,1,1), lwd = c(2,2,2), bty = "n", col = c("darkgrey", "black","green"))
dev.off()

png('report\\Fig_36_plot_Rec.png', width=1200, height=1000,res=200)
plot(wk.y1:wk.y2, wk.stock.rec.quantile2[,3], type = "n", lty = 1,bty="L",
     xlab = "Year", ylab = "Recruitment", 
     ylim = c(min(0, wk.stock.rec.quantile2), max(wk.stock.rec.quantile2[,4])))

lines(wk.y1:wk.y2, wk.stock.rec.quantile1[,3], lty = 1, col = "darkgrey", lwd = 2)
lines(wk.y1:wk.y2, wk.stock.rec.quantile2[,3], lty = 1, col = "black", lwd = 2)
lines(wk.y1:wk.y2, wk.stock.rec.quantile3[,3], lty = 1, col = "green", lwd = 2)

legend(legend = c("North", "South","combined"), x = "topright",
       lty = c(1,1,1), lwd = c(2,2,2), bty = "n", col = c("darkgrey", "black","green"))
dev.off()


#Figure: Within-survey correlations for IBTS Q3
load("model/surbar_results0.Rdata")  

# Within-survey correlations for IBTS Q3
png('report\\Fig_14_withinsurvey_correlationsQ3_age_zero.png', width=1200, height=1200,res=200)
plot.index.corr(s.results$s.idx[2], wk.type = "SURBAR")
dev.off() 


#new
# Figure : Survey catch curves
png('report\\Fig_12_surveyCatchcurve_Q3.png', width=1400, height=1400,res=200)
plot.surbar(s.results, "catch.curve", nums=2)
dev.off()



