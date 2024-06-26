
# SAM forecast 
##############################################################################################
library(icesTAF)
library(icesAdvice)

#make intermediate year forecast table, advice table, catch option table

if(season=="spring") xy<-3
if(season=="autumn") xy<-2

load("model\\FC.RData")

#status quo F at age
summary_F <-summary(fit)
f_est     <-summary_F[,7]
Fsq       <- f_est[length(f_est)-1]

# mean recent 3 years of F, scaled to final catch data year
fsel <- faytable(fit)/f_est
catch_mean<-apply(fsel[length(fsel[,1])+(-3:-1),],2,mean)*f_est[length(f_est)-1] 

F_past<-faytable(fit)[(length(summary(fit)[,1])-3):(length(summary(fit)[,1])-1),]


write.taf(F_past, file="output/recentF.csv", row.names=F)
write.taf(catch_mean, file="output/forecastF.csv", row.names=F)

 
res<-NULL
  
  for(i in 1:length(FC)){
    
    foc<-FC[[i]]

    tc <-(attributes(foc)$tab[,"catch:median"][[xy]])   # catch median 
    wc <-(attributes(foc)$tab[,"Land:median"][[xy]])
    uc <-(attributes(foc)$tab[,"Discard:median"][[xy]])
    ibc<-(attributes(foc)$tab[,"ibc:median"][[xy]])
    
    F_all<-icesRound(attributes(foc)$tab[,"fbar:median"][[xy]])
    F_wc <-icesRound(attributes(foc)$tab[,"fbarL:median"][[xy]])
    F_uwc<-icesRound(attributes(foc)$tab[,"fbarD:median"][[xy]])
    Fibc <-icesRound(attributes(foc)$tab[,"fbarI:median"][[xy]])
    SSB  <-(attributes(foc)$tab[,"ssb:median"][[xy]])
    TSB  <-(attributes(foc)$tab[,"tsb:median"][[xy]])
    rec  <-(attributes(foc)$tab[,"rec:median"][[xy]])

    TSB_all  <-attributes(foc)$tab[,"tsb:median"]
    SSB_all  <-attributes(foc)$tab[,"ssb:median"]
    SSBchange<-icesRound((SSB_all[xy+1]-SSB_all[xy])/SSB_all[xy]*100)
    
    if(attributes(foc)$label=="No HC fishery"){ # continue
      F_wc  <-0
      F_uwc <-0
      ibc   <-tc
      uc<-wc<-0
    }
    
    TACchange<-icesRound((((wc+uc)*propc4-TAC)/TAC)*100)
    Advice_change<-icesRound(((tc-advice)/advice)*100)
    
    res_table<-cbind(round(rec),round(TSB),round(SSB), round(tc), round(wc), round(uc), round(ibc), round(wc+uc), round(propc4*(wc+uc)), round((1-propc4)*(wc+uc)), F_all, F_wc, F_uwc, Fibc, round(SSB_all[xy+1]), SSBchange, TACchange, Advice_change, attributes(foc)$label)
    res_table<-as.data.frame(res_table)
    colnames(res_table)<-as.character(c("Rec",paste0("TSB ", names(TSB_all[xy])),paste0("SSB ", names(SSB_all[xy])),paste0("Total catch ", names(SSB_all[xy])),"Wanted catch","Unwanted catch","IBC","HC catch","HC catch 4","HC catch 7d","Total F", "F wanted","F unwanted","F IBC",paste0("SSB ", names(SSB_all[xy+1])),"%SSB change","%TAC change","%Advice change","Basis"))
    
    res<-rbind(res,res_table)
    
  }

  write.taf(res, file=paste0("output/Advice_Table_",season,".csv"), row.names=F)
  
  
  foc<-FC[[1]]
  
  tc <-(attributes(foc)$tab[,"catch:median"][[xy-1]])   # catch median in 2021
  wc <-(attributes(foc)$tab[,"Land:median"][[xy-1]])
  uc <-(attributes(foc)$tab[,"Discard:median"][[xy-1]])
  ibc<-(attributes(foc)$tab[,"ibc:median"][[xy-1]])
  
  F_all<-icesRound(attributes(foc)$tab[,"fbar:median"][[xy-1]])
  F_wc<-icesRound(attributes(foc)$tab[,"fbarL:median"][[xy-1]])
  F_uwc<-icesRound(attributes(foc)$tab[,"fbarD:median"][[xy-1]])
  Fibc<-icesRound(attributes(foc)$tab[,"fbarI:median"][[xy-1]])
  SSB<-(attributes(foc)$tab[,"ssb:median"][[xy-1]])
  TSB<-(attributes(foc)$tab[,"tsb:median"][[xy-1]])
  Rec<-(attributes(foc)$tab[,"rec:median"][[xy-1]])
  
  SSB_all  <-attributes(foc)$tab[,"ssb:median"]
  TSB_all  <-attributes(foc)$tab[,"tsb:median"]
  Rec_all  <-attributes(foc)$tab[,"rec:median"]
  
  res_table<-cbind(round(Rec),round(TSB), round(SSB), round(tc), round(wc), round(uc), round(ibc), round(wc+uc), round(propc4*(wc+uc)), round((1-propc4)*(wc+uc)), F_all, F_wc, F_uwc, Fibc)
  res_table<-as.data.frame(res_table)
  
  colnames(res_table)<-c("Rec",paste0("TSB ", names(TSB_all[xy-1])), paste0("SSB ", names(SSB_all[xy-1])),paste0("Total catch ", names(SSB_all[xy-1])),"Wanted catch","Unwanted catch","IBC","HC catch","HC catch 4","HC catch 7d","Total F", "F wanted","F unwanted","F IBC")
  
  write.taf(res_table, file=paste0("output/Advice_Table_intermediate_year_",season,".csv"), row.names=F)
  
  
  write.taf(Rec_all, file=paste0("output/Rec_forecast_",season,".csv"), row.names=T)
  
  
   
###############################################################################################################################
# catch option table

    
  load("model\\FC_options.RData")
  res<-NULL
  
  for(i in 1:length(FC)){
    
    foc<-FC[[i]]
    
    tc <-(attributes(foc)$tab[,"catch:median"][[xy]])   # catch median 
    wc <-(attributes(foc)$tab[,"Land:median"][[xy]])
    uc <-(attributes(foc)$tab[,"Discard:median"][[xy]])
    ibc<-(attributes(foc)$tab[,"ibc:median"][[xy]])
    
    F_all<-icesRound(attributes(foc)$tab[,"fbar:median"][[xy]])
    F_wc <-icesRound(attributes(foc)$tab[,"fbarL:median"][[xy]])
    F_uwc<-icesRound(attributes(foc)$tab[,"fbarD:median"][[xy]])
    Fibc <-icesRound(attributes(foc)$tab[,"fbarI:median"][[xy]])
    SSB  <-(attributes(foc)$tab[,"ssb:median"][[xy]])
    rec  <-(attributes(foc)$tab[,"rec:median"][[xy]])
    TSB  <-(attributes(foc)$tab[,"tsb:median"][[xy]])
    
    TSB_all  <-attributes(foc)$tab[,"tsb:median"]
    SSB_all  <-attributes(foc)$tab[,"ssb:median"]
    SSBchange<-icesRound((SSB_all[xy+1]-SSB_all[xy])/SSB_all[xy]*100)
    
    if(attributes(foc)$label=="No HC fishery"){ # continue
      F_wc  <-0
      F_uwc <-0
      ibc   <-tc
      uc<-wc<-0
    }
    
    TACchange<-icesRound((((wc+uc)*propc4-TAC)/TAC)*100)
    Advice_change<-icesRound(((tc-advice)/advice)*100)
    
    res_table<-cbind(round(rec), round(TSB), round(SSB), round(tc), round(wc), round(uc), round(ibc), round(wc+uc), round(propc4*(wc+uc)), round((1-propc4)*(wc+uc)), F_all, F_wc, F_uwc, Fibc, round(TSB_all[xy+1]), round(SSB_all[xy+1]), SSBchange, TACchange, Advice_change, attributes(foc)$label)
    res_table<-as.data.frame(res_table)
    
    colnames(res_table)<-c("Rec", paste0("TSB ", names(TSB_all[xy])), paste0("SSB ", names(SSB_all[xy])), paste0("Total catch", names(SSB_all[xy])),"Wanted catch", "Unwanted catch", "IBC", "HC catch", "HC catch 4", "HC catch 7d", "Total F", "F wanted", "F unwanted", "F IBC", paste0("TSB ", names(TSB_all[xy+1])), paste0("SSB ", names(SSB_all[xy+1])),"%SSB change","%TAC change","%Advice change","Basis")
    
    res<-rbind(res,res_table)
    
  }
  
  colnames(res)<-c("Rec",paste0("TSB ", names(TSB_all[xy])), paste0("SSB ", names(SSB_all[xy])),paste0("Total catch", names(SSB_all[xy])),"Wanted catch","Unwanted catch","IBC","HC catch","HC catch 4","HC catch 7d","Total F", "F wanted","F unwanted","F IBC",paste0("TSB ", names(TSB_all[xy+1])), paste0("SSB ", names(SSB_all[xy+1])),"%SSB change","%TAC change","%Advice change","Basis")
  
  write.taf(res, file=paste0("output/Advice_Table_",season,"_options.csv"), row.names=F)
  

  
  
 
  
  