############################### Prepare CPUE maps #################################


library(icesTAF)
library(stockassessment)



load("data/data.Rdata")
source("utilities_report.R")

datayear<-as.numeric(datayear)

#Q1 Change species name for different species
load('bootstrap/data/gmt3.RData') 
cpue1<-read.csv("bootstrap/data/CPUE_per_age_per_subarea.csv")
cpue<-cpue1[cpue1$Species=="Merlangius merlangus" & cpue1$Quarter==1,]
lonlat<-xy(cpue$SubArea)
gscale=0.25


png(paste0("report/Fig_9 WHG Q1_",datayear-3,"-",datayear+1,".png"), width=3000, height=1900)
par(mar=c(0,3,3,3), lwd=2)
lay<-cbind(c(1,2,2),3:5,6:8,9:11,12:14,15:17)
layout(lay)

basemap(-4,12,50,62)
points(lonlat,pch='+', cex=2)
title('Survey Positions', cex.main=4)

plot(rep(.5,4),1:4/5, ylim=c(0,1), xlim=c(0,1), cex=gscale*sqrt(c(100,1000,2000,3000)), xlab='', ylab='', axes=FALSE)
text(rep(.7,4),1:4/5, c(100,1000,2000,3000), cex=2)

for(y in (datayear-3):(datayear+1)){
  for(a in c('1','2','3+')){
    plotone(a,y,1)
    title(paste('Q1 ',y,': whiting age ',a,sep=''), cex.main=4)
  }
}
dev.off()

png(paste('report/Fig XXa WHG Q1_', datayear-8,"-",datayear-4,'.png', sep=''), width=3000, height=1900)
par(mar=c(0,3,3,3), lwd=2)
lay<-cbind(c(1,2,2),3:5,6:8,9:11,12:14,15:17)
layout(lay)

basemap(-4,12,50,62)
points(lonlat,pch='+', cex=2)
title('Survey Positions', cex.main=4)

plot(rep(.5,4),1:4/5, ylim=c(0,1), xlim=c(0,1), cex=gscale*sqrt(c(100,1000,2000,3000)), xlab='', ylab='', axes=FALSE)
text(rep(.7,4),1:4/5, c(100,1000,2000,3000), cex=2)

for(y in (datayear-8):(datayear-4)){
  for(a in c('1','2','3+')){
    plotone(a,y,1)
    title(paste('Q1 ',y,': whiting age ',a,sep=''), cex.main=4)
  }
}
dev.off()


png(paste('report/Fig XXa WHG Q1_', datayear-13,'-',datayear-9,'.png', sep=''), width=3000, height=1900)
par(mar=c(0,3,3,3), lwd=2)
lay<-cbind(c(1,2,2),3:5,6:8,9:11,12:14,15:17)
layout(lay)

basemap(-4,12,50,62)
points(lonlat,pch='+', cex=2)
title('Survey Positions', cex.main=4)

plot(rep(.5,4),1:4/5, ylim=c(0,1), xlim=c(0,1), cex=gscale*sqrt(c(100,1000,2000,3000)), xlab='', ylab='', axes=FALSE)
text(rep(.7,4),1:4/5, c(100,1000,2000,3000), cex=2)

for(y in (datayear-13):(datayear-9)){
  for(a in c('1','2','3+')){
    plotone(a,y,1)
    title(paste('Q1 ',y,': whiting age ',a,sep=''), cex.main=4)
  }
}
dev.off()


png(paste('report/Fig XXa WHG Q1_', datayear-18,'-',datayear-14 ,'.png', sep=''), width=3000, height=1900)
par(mar=c(0,3,3,3), lwd=2)
lay<-cbind(c(1,2,2),3:5,6:8,9:11,12:14,15:17)
layout(lay)

basemap(-4,12,50,62)
points(lonlat,pch='+', cex=2)
title('Survey Positions', cex.main=4)

plot(rep(.5,4),1:4/5, ylim=c(0,1), xlim=c(0,1), cex=gscale*sqrt(c(100,1000,2000,3000)), xlab='', ylab='', axes=FALSE)
text(rep(.7,4),1:4/5, c(100,1000,2000,3000), cex=2)

for(y in (datayear-18):(datayear-14)){
  for(a in c('1','2','3+')){
    plotone(a,y,1)
    title(paste('Q1 ',y,': whiting age ', a, sep=''), cex.main=4)
  }
}
dev.off()



png(paste('report/Fig XXa WHG Q1_', datayear-23,'-',datayear-19 ,'.png', sep=''), width=3000, height=1900)
par(mar=c(0,3,3,3), lwd=2)
lay<-cbind(c(1,2,2),3:5,6:8,9:11,12:14,15:17)
layout(lay)

basemap(-4,12,50,62)
points(lonlat,pch='+', cex=2)
title('Survey Positions', cex.main=4)

plot(rep(.5,4),1:4/5, ylim=c(0,1), xlim=c(0,1), cex=gscale*sqrt(c(100,1000,2000,3000)), xlab='', ylab='', axes=FALSE)
text(rep(.7,4),1:4/5, c(100,1000,2000,3000), cex=2)

for(y in (datayear-23):(datayear-19)){
  for(a in c('1','2','3+')){
    plotone(a,y,1)
    title(paste('Q1 ',y,': whiting age ', a, sep=''), cex.main=4)
  }
}
dev.off()


png(paste('report/Fig XXa WHG Q1_', datayear-28,'-',datayear-24 ,'.png', sep=''), width=3000, height=1900)
par(mar=c(0,3,3,3), lwd=2)
lay<-cbind(c(1,2,2),3:5,6:8,9:11,12:14,15:17)
layout(lay)

basemap(-4,12,50,62)
points(lonlat,pch='+', cex=2)
title('Survey Positions', cex.main=4)

plot(rep(.5,4),1:4/5, ylim=c(0,1), xlim=c(0,1), cex=gscale*sqrt(c(100,1000,2000,3000)), xlab='', ylab='', axes=FALSE)
text(rep(.7,4),1:4/5, c(100,1000,2000,3000), cex=2)

for(y in (datayear-28):(datayear-24)){
  for(a in c('1','2','3+')){
    plotone(a,y,1)
    title(paste('Q1 ',y,': whiting age ', a, sep=''), cex.main=4)
  }
}
dev.off()



#Q3


cpue<-cpue1[cpue1$Species=="Merlangius merlangus" & cpue1$Quarter==3,]
lonlat<-xy(cpue$SubArea)
gscale=0.25

png(paste('report/Fig_10 WHG Q3_',datayear-3,"-",datayear,'.png', sep=''), width=3000, height=2600)
par(mar=c(4,1,7,1), lwd=2)
lay<-cbind(c(1,2,2,2),3:6,7:10,11:14,15:18,19:22)
layout(lay)

basemap(-4,12,50,62)
points(lonlat,pch='+', cex=2)
title('Survey Positions', cex.main=5)

plot(rep(.5,4),1:4/5, ylim=c(0,1), xlim=c(0,1), cex=gscale*sqrt(c(100,1000,2000,3000)), xlab='', ylab='', axes=FALSE)
text(rep(.7,4),1:4/5, c(100,1000,2000,3000), cex=2)

for(y in (datayear-3):datayear){
  for(a in c('0','1','2','3+')){
    plotone(a,y,3)
    title(paste('Q3 ',y,': whiting age ',a,sep=''), cex.main=5)
  }
}
dev.off()



png(paste('report/Fig XXb WHG Q3_',datayear-8,"-",datayear-4,'.png', sep=''), width=3000, height=2600)
par(mar=c(4,1,7,1), lwd=2)
lay<-cbind(c(1,2,2,2),3:6,7:10,11:14,15:18,19:22)
layout(lay)

basemap(-4,12,50,62)
points(lonlat,pch='+', cex=2)
title('Survey Positions', cex.main=5)

plot(rep(.5,4),1:4/5, ylim=c(0,1), xlim=c(0,1), cex=gscale*sqrt(c(100,1000,2000,3000)), xlab='', ylab='', axes=FALSE)
text(rep(.7,4),1:4/5, c(100,1000,2000,3000), cex=2)

for(y in (datayear-8):(datayear-4)){
  for(a in c('0','1','2','3+')){
    plotone(a,y,3)
    title(paste('Q3 ',y,': whiting age ',a,sep=''), cex.main=5)
  }
}
dev.off()

png(paste('report/Fig XXb WHG Q3_',datayear-13,"-",datayear-9,'.png', sep=''), width=3000, height=2600)
par(mar=c(4,1,7,1), lwd=2)
lay<-cbind(c(1,2,2,2),3:6,7:10,11:14,15:18,19:22)
layout(lay)

basemap(-4,12,50,62)
points(lonlat,pch='+', cex=2)
title('Survey Positions', cex.main=5)

plot(rep(.5,4),1:4/5, ylim=c(0,1), xlim=c(0,1), cex=gscale*sqrt(c(100,1000,2000,3000)), xlab='', ylab='', axes=FALSE)
text(rep(.7,4),1:4/5, c(100,1000,2000,3000), cex=2)

for(y in (datayear-13):(datayear-9)){
  for(a in c('0','1','2','3+')){
    plotone(a,y,3)
    title(paste('Q3 ',y,': whiting age ',a,sep=''), cex.main=5)
  }
}
dev.off()

png(paste('report/Fig XXb WHG Q3_',datayear-18,"-",datayear-14,'.png', sep=''), width=3000, height=2600)
par(mar=c(4,1,7,1), lwd=2)
lay<-cbind(c(1,2,2,2),3:6,7:10,11:14,15:18,19:22)
layout(lay)

basemap(-4,12,50,62)
points(lonlat,pch='+', cex=2)
title('Survey Positions', cex.main=5)

plot(rep(.5,4),1:4/5, ylim=c(0,1), xlim=c(0,1), cex=gscale*sqrt(c(100,1000,2000,3000)), xlab='', ylab='', axes=FALSE)
text(rep(.7,4),1:4/5, c(100,1000,2000,3000), cex=2)

for(y in (datayear-18):(datayear-14)){
  for(a in c('0','1','2','3+')){
    plotone(a,y,3)
    title(paste('Q3 ',y,': whiting age ',a,sep=''), cex.main=5)
  }
}
dev.off()

png(paste('report/Fig XXb WHG Q3_',datayear-23,"-",datayear-19,'.png', sep=''), width=3000, height=2600)
par(mar=c(4,1,7,1), lwd=2)
lay<-cbind(c(1,2,2,2),3:6,7:10,11:14,15:18,19:22)
layout(lay)

basemap(-4,12,50,62)
points(lonlat,pch='+', cex=2)
title('Survey Positions', cex.main=5)

plot(rep(.5,4),1:4/5, ylim=c(0,1), xlim=c(0,1), cex=gscale*sqrt(c(100,1000,2000,3000)), xlab='', ylab='', axes=FALSE)
text(rep(.7,4),1:4/5, c(100,1000,2000,3000), cex=2)

for(y in (datayear-23):(datayear-19)){
  for(a in c('0','1','2','3+')){
    plotone(a,y,3)
    title(paste('Q3 ',y,': whiting age ',a,sep=''), cex.main=5)
  }
}
dev.off()

png(paste('report/Fig XXb WHG Q3_',datayear-28,"-",datayear-24,'.png', sep=''), width=3000, height=2600)
par(mar=c(4,1,7,1), lwd=2)
lay<-cbind(c(1,2,2,2),3:6,7:10,11:14,15:18,19:22)
layout(lay)

basemap(-4,12,50,62)
points(lonlat,pch='+', cex=2)
title('Survey Positions', cex.main=5)

plot(rep(.5,4),1:4/5, ylim=c(0,1), xlim=c(0,1), cex=gscale*sqrt(c(100,1000,2000,3000)), xlab='', ylab='', axes=FALSE)
text(rep(.7,4),1:4/5, c(100,1000,2000,3000), cex=2)

for(y in (datayear-28):(datayear-24)){
  for(a in c('0','1','2','3+')){
    plotone(a,y,3)
    title(paste('Q3 ',y,': whiting age ',a,sep=''), cex.main=5)
  }
}
dev.off()