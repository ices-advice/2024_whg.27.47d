## Run analysis, write model results

## Before: www.stockassessment.org current assessment, SURBAR input data
## After: RData (model, residuals, model, leaveout, retros, forecast, forecast_options, s.results0, s.results1), 

rm(list=ls())
#setwd("N:\\1_Tanja\\2_MARLAB\\ICES\\WGNSSK2024\\WHITING_2024\\TAF_2024_whg.27.47d_assessment-master") # Changed the directory


library(icesTAF)
library(stockassessment)
library(minpack.lm)
library(FLCore)
library(FLfse)

##  1 SURBAR

load("data/data.Rdata")
source("utilities_model.R")

#Define paths and load functions
data.path<-"bootstrap\\data\\"

set.seed(11111)

startage<-1
  
  plus.gp <- 6
  mean.f.range <- c(2,4)
  mean.z.range <- c(2,4) # Survey data only used up to age 5
  
  #Read in data for NS whiting
  
  now <- datayear  #last historic year data
  
  f.temp <- read.assessment.data(wk.data.path = data.path, wk.mean.f.range = mean.f.range, # add an extra line in data/wstock_age15 file
                                 wk.plus.gp = plus.gp, areas="all")
  
  s.stock <- f.temp$s.stock
  s.index <- f.temp$s.index
  f.stock <- f.temp$f.stock
  f.stock <- trim(f.stock, age = 0:f.stock@range["max"], year = 1978:now)  #new
  f.stock <- setPlusGroup(f.stock, plus.gp)                 
  
 
  

  # Set up and produce SURBAR run
  s.results <- surbar.wrapper(wk.stock = s.stock, wk.index = s.index,
                              wk.lambda = 5.0, wk.refage = 3, wk.zrange = mean.z.range, startyear=1983, startage=startage)  # adapt source code to age 0
  
  save(s.results, file=paste0("model\\surbar_results",startage,".Rdata"))
  save(s.index, file=paste0("model\\surbar_index",startage,".Rdata"))
  
 startage<-0 
  s.index[[1]] <- f.temp$s.index[[2]] # produce age 0 correlation overwrite Q1

  s.results <- surbar.wrapper(wk.stock = s.stock, wk.index = s.index,
                              wk.lambda = 5.0, wk.refage = 3, wk.zrange = mean.z.range, startyear=1983, startage=startage)  # adapt source code to age 0
  
  save(s.results, file=paste0("model\\surbar_results",startage,".Rdata"))
  

# by area

cc<-c("north", "south")

for (ii in 1:2) {
  
  a<-cc[ii]
  plus.gp <- 6
  mean.f.range <- c(2,4)
  mean.z.range <- c(2,4) # Survey data only used up to age 5
  ref.points <- data.frame(Blim = NA, Bpa = NA, Flim = NA, Fpa = NA, Fmsy = NA)
  
  #Read in data for NS whiting
  
  now <- datayear  #last historic year data
  f.temp <- read.assessment.data(wk.data.path = data.path, wk.mean.f.range = mean.f.range,
                                 wk.plus.gp = plus.gp, areas=a)
  s.stock <- f.temp$s.stock
  s.index <- f.temp$s.index
  x.index <- f.temp$f.index
  # Set up and produce SURBAR run
  s.results <- surbar.wrapper(wk.stock = s.stock, wk.index = s.index,
                              wk.lambda = 5.0, wk.refage = 3, wk.zrange = mean.z.range, startyear=1983,startage=1)
  
  save(s.results, file=paste("model\\surbar_results_",a,".Rdata", sep=""))
  
}


## 2.  download SAM results from stockassessment.org


#current run
stockname<-as.character(substitute(NSwhiting_2024))  # change to current run!

options(download.file.method = "wininet")

#load(url(sub("SN",stockname , "https://stockassessment.org/datadisk/stockassessment/userdirs/user3/SN/run/forecast.RData")))
download.file(url=sub("SN",stockname , "https://stockassessment.org/datadisk/stockassessment/userdirs/user3/SN/run/model.RData"),destfile="model/model.RData")
download.file(url=sub("SN",stockname , "https://stockassessment.org/datadisk/stockassessment/userdirs/user3/SN/run/data.RData"),destfile="model/data.RData")
download.file(url=sub("SN",stockname , "https://stockassessment.org/datadisk/stockassessment/userdirs/user3/SN/run/leaveout.RData"),destfile="model/leaveoneout.RData")
download.file(url=sub("SN",stockname , "https://stockassessment.org/datadisk/stockassessment/userdirs/user3/SN/run/residuals.RData"),destfile="model/residuals.RData")
download.file(url=sub("SN",stockname , "https://stockassessment.org/datadisk/stockassessment/userdirs/user3/SN/run/retro.RData"),destfile="model/retro.RData")
download.file(url=sub("SN",stockname , "https://stockassessment.org/datadisk/stockassessment/userdirs/user3/SN/run/forecast.RData"),destfile="model/FC.RData")


load("model/model.Rdata")
load("model/data.RData")
load("model/leaveoneout.RData")
load("model/residuals.RData")
load("model/retro.RData")
load("model/FC.Rdata")
save.image( file="model/allmodel.RData")


# FC<-NULL
# load(url(sub("SN",stockname , "https://stockassessment.org/datadisk/stockassessment/userdirs/user3/SN/run/forecast_options.RData")))
# save(FC, file="model/FC_options.RData")


# create output for MIXFISH
stk_fit<-FLfse::SAM2FLStock(fit, catch_estimate = T, mat_est = T, stock.wt_est = T, catch.wt_est = T, m_est = T)
stk_fit@desc<-"FLStock created from SAM model fit, model estimated values, catches include IBC"
save(stk_fit, file="model\\whg.27.47d_FLStock_model_estimates.Rdata")

stk_dat<-FLfse::SAM2FLStock(fit, catch_estimate = F, mat_est = F, stock.wt_est = F, catch.wt_est = F, m_est = F)
stk_dat@desc<-"FLStock created from SAM model fit, input data, catches include IBC"
save(stk_dat, file="model\\whg.27.47d_FLStock_input_data.Rdata")
