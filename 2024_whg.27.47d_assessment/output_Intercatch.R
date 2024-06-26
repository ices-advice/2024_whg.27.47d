
# Intercatch submission analysis  
##############################################################################################

#make plots for LandPercent sampled/unsampled, Diswt, by area

### import IC files (need to be in the input folder)

StockOverviewFile <- "bootstrap\\data\\StockOverview.txt"
NumbersAtAgeLengthFile <- "bootstrap\\data\\NumbersAtAgeLength.txt"


par(ask=TRUE)

WtData <- readStockOverview(StockOverviewFile,NumbersAtAgeLengthFile)
WtData<-WtData[WtData$D_Imported_Raised=="Imported",]

Ndata <- readNumbersAtAgeLength(NumbersAtAgeLengthFile)
Ndata$Area<-ifelse(Ndata$Area %in% c('27.4.a','27.4.b','27.4.c'),'27.4',
                   ifelse(Ndata$Area %in% c('27.6.a','27.6.b'),'27.6',as.character(Ndata$Area)))
WtData$Area<-ifelse(WtData$Area %in% c('27.4.a','27.4.b','27.4.c'),'27.4',
                    ifelse(WtData$Area %in% c('27.6.a','27.6.b'),'27.6',as.character(WtData$Area)))

W<-aggregate(CatchWt~CatchCat+Country+Area,WtData,function(x) round(sum(x),1))

# % UK landings

sum(W[W$CatchCat=="L" & W$Country %in% c("UK (England)","UK(Scotland)"),"CatchWt"])/sum(W[W$CatchCat=="L","CatchWt"])

# % UK landings in 4

sum(W[W$CatchCat=="L" & W$Area=="27.4" & W$Country %in% c("UK (England)","UK(Scotland)"),"CatchWt"])/sum(W[W$CatchCat=="L" & W$Area=="27.4","CatchWt"])

# % France landings in 7d

sum(W[W$CatchCat=="L" & W$Area=="27.7.d" & W$Country %in% c("France"),"CatchWt"])/sum(W[W$CatchCat=="L" & W$Area=="27.7.d","CatchWt"])

write.taf(W,"output_IC_catch_country.csv",dir="output")

#### Look at age distribution

pdf(paste("output\\AgeDistrib_",year,".pdf",sep=''))
plotAgeDistribution1(Ndata) 
dev.off()

na<-as.data.frame(aggregate(cbind(NumAgeMeasurement,NumSamplesAge)~CatchCat+Fleet+Country+Season+Area,Ndata,mean))

write.taf(na,paste("number age samples",year,".csv",sep=''), dir="output",row.names=F)



################################## READ IN INTERCATCH DATA ###############################

par(ask=TRUE)

WtData <- readStockOverview(StockOverviewFile,NumbersAtAgeLengthFile) 
WtData<-WtData[WtData$D_Imported_Raised=="Imported",]

Ndata <- readNumbersAtAgeLength(NumbersAtAgeLengthFile) 

levels(WtData$Area)[levels(WtData$Area)%in%c("27.4","27.4.a","27.4.b","27.4.c")]<-"27.4"
levels(Ndata$Area)[levels(Ndata$Area)%in%c("27.4","27.4.a","27.4.b","27.4.c")]<-"27.4"

write.taf(WtData,"output\\Wtdata.csv",quote=T)

################################## PLOT LANDINGS DATA ####################################

### Percentage of sampled stratas (LANDINGS)

W<-WtData[WtData$Area=="27.4",]

png("output\\landingsPercCountryFleets_4.png" ,width=3000, height=1300,res=200)
plotStockOverview(W,byFleet=TRUE,byCountry=TRUE,bySampled=TRUE,bySeason=FALSE,
                  byArea=TRUE,countryColours=TRUE,set.mar=TRUE,markSampled=TRUE,individualTotals=FALSE)
dev.off()

W<-WtData[WtData$Area=="27.7.d",]
png("output\\landingsPercCountryFleets_7d.png",width=2200, height=1300,res=200)
plotStockOverview(W,byFleet=TRUE,byCountry=TRUE,bySampled=TRUE,bySeason=FALSE,
                  byArea=TRUE,countryColours=TRUE,set.mar=TRUE,markSampled=TRUE,individualTotals=FALSE)
dev.off()

#perc discrads per area and country

W<-WtData[WtData$Area=="27.4",]

png("output\\discardsCountryFleets_4.png",width=2200, height=1200,res=200)
plotStockOverview(W,byFleet=TRUE,plotType="DisWt",byCountry=TRUE,bySampled=TRUE,bySeason=FALSE,
                  byArea=TRUE,countryColours=TRUE,set.mar=TRUE,markSampled=TRUE,individualTotals=FALSE)
dev.off()

W<-WtData[WtData$Area=="27.7.d",]
png("output\\discardsPercCountryFleets_7d.png" ,width=2200, height=1200,res=200)
plotStockOverview(W,byFleet=TRUE,plotType="DisWt",byCountry=TRUE,bySampled=TRUE,bySeason=FALSE,
                  byArea=TRUE,countryColours=TRUE,set.mar=TRUE,markSampled=TRUE,individualTotals=FALSE)
dev.off()



