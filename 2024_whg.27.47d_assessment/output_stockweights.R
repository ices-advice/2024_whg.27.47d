
# create outputs for stockweight results
fyear<-year

rezF<-read.table("data\\rezF.csv",sep="," ,h=T)

rezN<-subset(rezF,area2=="NORTH")
rezS<-subset(rezF,area2=="SOUTH")

png('output\\catchrat_area.png',  width=2600, height=2000, res=400)

with(rezF,plot(Year,nbL,type="n",ylab="Average Nb caught per hour per haul"))
with(rezN,points(Year,nbL,type="l",col="blue"))
with(rezS,points(Year,nbL,type="l",col="red"))
axis(side = 1, at = c(1990:fyear),labels=NA)
dev.off()

png(paste('output\\cweightfact_area.png',sep=""),  width=2600, height=2000, res=400)

with(rezF,plot(Year,wt,type="n", ylab="regional weighting factor"))
with(rezN,points(Year,wt,type="l",col="blue"))
with(rezS,points(Year,wt,type="l",col="red"))
axis(side = 1, at = c(1990:fyear),labels=NA)
dev.off()



cweights<-read.table("data\\cw_converted_raw.csv",sep="," ,h=T)
sweights<-read.table("data\\smoothed_weight.csv", sep=",", h=T)
cweights<-cweights[cweights$year%in% c(2000:year),]

png('output\\conversion_catch_weights_at_age.png',  width=2600, height=2000, res=400)
par(mfrow = c(1,1),cex=1, mar = c(4,4,2,1) + 0.1)

plot(sweights[,"year"], sweights$s.1, col="transparent",ylim=c(0,850),  xlab="Year", ylab="Weight (g)", bty="l",type="l")
points(cweights$X, cweights$X0, col="magenta", pch=20, lwd=1.6)

lines(sweights[,"year"], sweights$s.1, col="red" , type="l")
points(cweights$X, cweights$X1, col="red", pch=20, lwd=1.6)

lines(sweights[,"year"], sweights$s.2, col="blue", bty="l",type="l")
points(cweights$X, cweights$X2, col="blue",pch=20, lwd=1.6)

lines(sweights[,"year"], sweights$s.3, col="green", bty="l",type="l")
points(cweights$X, cweights$X3, col="green", lwd=1.6,pch=20)

lines(sweights[,"year"], sweights$s.4, col="cyan", bty="l",type="l")
points(cweights$X, cweights$X4, col="cyan", pch=20, lwd=1.6)

lines(sweights[,"year"], sweights$s.5, col="black", bty="l",type="l")
points(cweights$X, cweights$X5, col="black", pch=20, lwd=1.6)

lines(sweights[,"year"], sweights$s.6, col="grey", bty="l",type="l")
points(cweights$X, cweights$X6, col="grey", pch=20, lwd=1.6)


legend("topleft", legend=c("0","1","2","3","4","5","6","7","8+"),  col=c("magenta","red","blue","green","cyan","black","grey"),lwd=1.6, bty="n",ncol=2 )

dev.off()


matog<-read.table("data\\smoothed_weight.csv",sep=",", header=T)
Mat0<-read.table("data\\weight_comb.csv",sep=",",header=T)   
Mat0<-subset(Mat0, year<=fyear & year>1990 & age>0) 

png('output\\smoothed_weights.png',  width=2600, height=2000, res=400)

par(mfrow = c(1,1),cex=1, mar = c(4,4,2,1) + 0.1)
plot(Mat0[Mat0$age==1,"year"], Mat0[Mat0$age==1,"weight"], col="red", ylim=c(0,850),main="IBTS Q1", pch=20, xlab="Year", ylab="Weight (g)", bty="l")
axis(2, at=c(1:9)*100) 
lines(matog$year, matog$s.1, col="red", lwd=1.6)
points(Mat0[Mat0$age==2,"year"], Mat0[Mat0$age==2,"weight"], col="green", pch=20)
lines(matog$year, matog$s.2, col="green", lwd=1.6)
points(Mat0[Mat0$age==3,"year"], Mat0[Mat0$age==3,"weight"], col="blue", pch=20)
lines(matog$year, matog$s.3, col="blue")
points(Mat0[Mat0$age==4,"year"], Mat0[Mat0$age==4,"weight"], col="cyan", pch=20, lwd=1.6)
lines(matog$year, matog$s.4, col="cyan")
points(Mat0[Mat0$age==5,"year"], Mat0[Mat0$age==5,"weight"], col="black", pch=20, lwd=1.6)
lines(matog$year, matog$s.5, col="black")
points(Mat0[Mat0$age==6,"year"], Mat0[Mat0$age==6,"weight"] ,col="grey", pch=20, lwd=1.6)
lines(matog$year, matog$s.6, col="grey")


legend("topleft", legend=c("1","2","3","4","5","6+"),  col=c("red","green","blue","cyan","black", "grey"),lwd=1.6, bty="n",ncol=2 )

dev.off()


#combinedlength
##############################################################################################

library(gam)
Mat0<-read.table("data\\length_comb.csv", sep=",", header=T)    
Mat0<-subset(Mat0, year<=fyear & year>1990) 
colnames(Mat0)<-c("year","age","lengths")

raw_mat<-reshape(Mat0,idvar=c("year"),timevar="age",direction="wide") 


for (i in 1:cutage) {                             
  M<-subset(Mat0, age==i)
  
  g<-gam::gam(lengths~s(year,3),data=M,family=gaussian)
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

#sum(aic)


matog<-subset(bericht,select=c(year,age,s))
matog<-reshape(matog,idvar=c("year"),timevar="age",direction="wide")    
matog<-round(matog,3) 

png('output\\smoothed_lengths.png',  width=2600, height=2000, res=400)

par(mfrow = c(1,1),cex=1, mar = c(4,4,2,1) + 0.1)
plot(Mat0[Mat0$age==1,"year"], Mat0[Mat0$age==1,"lengths"], col="red", ylim=c(0,45),main="IBTS Q1", pch=20, xlab="Year", ylab="Length (cm)", bty="l")
lines(matog$year, matog$s.1, col="red", lwd=1.6)
points(Mat0[Mat0$age==2,"year"], Mat0[Mat0$age==2,"lengths"], col="green", pch=20)
lines(matog$year, matog$s.2, col="green", lwd=1.6)
points(Mat0[Mat0$age==3,"year"], Mat0[Mat0$age==3,"lengths"], col="blue", pch=20)
lines(matog$year, matog$s.3, col="blue")
points(Mat0[Mat0$age==4,"year"], Mat0[Mat0$age==4,"lengths"], col="cyan", pch=20, lwd=1.6)
lines(matog$year, matog$s.4, col="cyan")
points(Mat0[Mat0$age==5,"year"], Mat0[Mat0$age==5,"lengths"], col="black", pch=20, lwd=1.6)
lines(matog$year, matog$s.5, col="black")
points(Mat0[Mat0$age==6,"year"], Mat0[Mat0$age==6,"lengths"] ,col="grey", pch=20, lwd=1.6)
lines(matog$year, matog$s.6, col="grey")


legend("bottomright", legend=c("1","2","3","4","5","6+"),  col=c("red","green","blue","cyan","black","grey"),lwd=1.6, bty="n",ncol=2 )

dev.off()

#################################################################################################

Mat0<-read.table("data\\length_comb_freq.csv",sep=",",header=T)    
Mat0<-subset(Mat0, year<=fyear & year>1990) 

png('output\\length_freq_combined.png',  width=3200, height=3600, res=400)

par(mfrow = c(4,2),cex=1, mar = c(4,4,1,1))

for (i in 1:cutage){
  if(i%in%c(1:6))  plot(Mat0[Mat0$age==i & Mat0$year==year-20,"length"], Mat0[Mat0$age==i& Mat0$year==year-20,"nb"]/sum(Mat0[Mat0$age==i& Mat0$year==year-20,"nb"]), col="orange", xlim=c(0,65),ylim=c(0,0.3),main=paste("age",i), pch=20,type="l", xlab="", ylab="Proportion", bty="l")
  
  if(i%in%c(7:8))  plot(Mat0[Mat0$age==i & Mat0$year==year-20,"length"], Mat0[Mat0$age==i& Mat0$year==year-20,"nb"]/sum(Mat0[Mat0$age==i& Mat0$year==year-20,"nb"]), col="orange", xlim=c(0,65),ylim=c(0,0.3),main=paste("age",i), pch=20,type="l", xlab="Length (cm)", ylab="Proportion", bty="l")
  lines(Mat0[Mat0$age==i & Mat0$year==year-16,"length"], Mat0[Mat0$age==i& Mat0$year==year-16,"nb"]/sum(Mat0[Mat0$age==i& Mat0$year==year-16,"nb"]), col="red", lwd=1.6,type="l")
  lines(Mat0[Mat0$age==i & Mat0$year==year-11,"length"], Mat0[Mat0$age==i& Mat0$year==year-11,"nb"]/sum(Mat0[Mat0$age==i& Mat0$year==year-11,"nb"]), col="purple", lwd=1.6,type="l")
  lines(Mat0[Mat0$age==i & Mat0$year==year-6,"length"], Mat0[Mat0$age==i& Mat0$year==year-6,"nb"]/sum(Mat0[Mat0$age==i& Mat0$year==year-6,"nb"]), col="blue", lwd=1.6,type="l")
  
  lines(Mat0[Mat0$age==i & Mat0$year==year-1,"length"], Mat0[Mat0$age==i& Mat0$year==year-1,"nb"]/sum(Mat0[Mat0$age==i& Mat0$year==year-1,"nb"]), col="black", lwd=1.6,type="l")
  if(i==2) legend("topright", legend=as.character(c(year-20,year-16,year-11,year-6,year-1)),  col=c("orange","red","purple","blue", "black"),lwd=1.6, bty="n",ncol=2, cex=1 )
  
}
dev.off()


##################################################################################################
# stock weights results

library(gam)
cw<-read.csv("data\\cw_converted_raw.csv", sep=",",header=T)

new1<-NULL

for(i in 0:cutage){
  
  new<-cbind(cw[,c(1,(i+2))],i)
  colnames(new)<-c("year","weight","age")
  
  new1<-rbind(new1, new)
}

Mat0<-new1

aic1<-NULL

sf<-c(5,2,10,10,10,10,10,10,10)

for (j in 0:cutage) {                             
  
  M<-subset(new1, age==j)
  
  g<-gam(weight~s(year,sf[j+1]),data=M,family=gaussian)  # 
  s<-predict.Gam(g)
  s<-cbind(M,s)
  s<-subset(s,select=c(year,age,s))
  if (j==0) {
    bericht<-s
    aic<-(summary(g))$aic}  else {
      bericht<-rbind(bericht,s)
      aic<-rbind(aic,(summary(g))$aic)}
  #aic
  
}

#sum(aic)
write.taf(Mat0,"output\\sw_plotdata.csv")

matog<-subset(bericht,select=c(year,age,s))
matog<-reshape(matog,idvar=c("year"),timevar="age",direction="wide")    
matog<-round(matog,3) 


png('output\\Fig_6_smoothed_corrected_weights.png',  width=2600, height=2000, res=400)

par(mfrow = c(1,1),cex=1, mar = c(4,4,2,1) + 0.1)
plot(Mat0[Mat0$age==0,"year"], Mat0[Mat0$age==0,"weight"], col="magenta",ylim=c(0,850), pch=20, xlab="Year", ylab="Weight (g)", bty="l")
axis(2,at=c(1:9)*100)
lines(matog$year, matog$s.0, col="magenta", lwd=1.6)
points(Mat0[Mat0$age==1,"year"], Mat0[Mat0$age==1,"weight"], col="red", pch=20)
lines(matog$year, matog$s.1, col="red", lwd=1.6)
points(Mat0[Mat0$age==2,"year"], Mat0[Mat0$age==2,"weight"], col="green", pch=20)
lines(matog$year, matog$s.2, col="green", lwd=1.6)
points(Mat0[Mat0$age==3,"year"], Mat0[Mat0$age==3,"weight"], col="blue", pch=20)
lines(matog$year, matog$s.3, col="blue")
points(Mat0[Mat0$age==4,"year"], Mat0[Mat0$age==4,"weight"], col="cyan", pch=20, lwd=1.6)
lines(matog$year, matog$s.4, col="cyan")
points(Mat0[Mat0$age==5,"year"], Mat0[Mat0$age==5,"weight"], col="black", pch=20, lwd=1.6)
lines(matog$year, matog$s.5, col="black")
points(Mat0[Mat0$age==6,"year"], Mat0[Mat0$age==6,"weight"] ,col="grey", pch=20, lwd=1.6)
lines(matog$year, matog$s.6, col="purple")


legend("topright", legend=c("0","1","2","3","4","5","6+"),  col=c("magenta","red","green","blue","cyan","black","grey"),lwd=1.6, bty="n",ncol=2 )

dev.off()


cw<-read.csv("data\\wstock_old.csv", sep=",",header=T)  #old data

new1<-NULL

for(i in 0:cutage){
  
  new<-cbind(cw[,c(1,(i+2))],i)
  colnames(new)<-c("year","weight","age")
  
  new1<-rbind(new1, new)
}

Mat0_old<-new1[!new1$year==datayear,]



png('output\\Fig_6_sw_corrected_weights_compare.png',  width=2600, height=2000, res=400)

par(mfrow = c(1,1),cex=1, mar = c(4,4,2,1) + 0.1)
plot(Mat0_old[Mat0_old$age==0,"year"], Mat0_old[Mat0_old$age==0,"weight"]*1000, col="transparent", ylim=c(0,850), xlab="Year", ylab="Weight (g)", bty="l")
axis(2,at=c(1:9)*100)


lines(Mat0[Mat0$age==0,"year"], Mat0[Mat0$age==0,"weight"], col="magenta", lwd=1.6)
lines(Mat0[Mat0$age==1,"year"], Mat0[Mat0$age==1,"weight"], col="red", lwd=1.6)
lines(Mat0[Mat0$age==2,"year"], Mat0[Mat0$age==2,"weight"], col="green", lwd=1.6)
lines(Mat0[Mat0$age==3,"year"], Mat0[Mat0$age==3,"weight"], col="blue", lwd=1.6)
lines(Mat0[Mat0$age==4,"year"], Mat0[Mat0$age==4,"weight"], col="cyan",  lwd=1.6)
lines(Mat0[Mat0$age==5,"year"], Mat0[Mat0$age==5,"weight"], col="black",  lwd=1.6)
lines(Mat0[Mat0$age==6,"year"], Mat0[Mat0$age==6,"weight"] ,col="grey",  lwd=1.6)

lines(Mat0_old[Mat0_old$age==0,"year"], Mat0_old[Mat0_old$age==0,"weight"]*1000, col="darkmagenta", lwd=1.6,lty="dashed")
lines(Mat0_old[Mat0_old$age==1,"year"], Mat0_old[Mat0_old$age==1,"weight"]*1000, col="darkred", lwd=1.6,lty="dashed")
lines(Mat0_old[Mat0_old$age==2,"year"], Mat0_old[Mat0_old$age==2,"weight"]*1000, col="darkgreen", lwd=1.6,lty="dashed")
lines(Mat0_old[Mat0_old$age==3,"year"], Mat0_old[Mat0_old$age==3,"weight"]*1000, col="darkblue", lwd=1.6,lty="dashed")
lines(Mat0_old[Mat0_old$age==4,"year"], Mat0_old[Mat0_old$age==4,"weight"]*1000, col="darkcyan",  lwd=1.6,lty="dashed")
lines(Mat0_old[Mat0_old$age==5,"year"], Mat0_old[Mat0_old$age==5,"weight"]*1000, col="black",  lwd=1.6,lty="dashed")
lines(Mat0_old[Mat0_old$age==6,"year"], Mat0_old[Mat0_old$age==6,"weight"]*1000 ,col="darkgrey",  lwd=1.6,lty="dashed")


legend("topright", legend=c("0","1","2","3","4","5","6+"),  col=c("magenta","red","green","blue","cyan","black","grey"),lwd=1.6, bty="n",ncol=2 )

dev.off()



