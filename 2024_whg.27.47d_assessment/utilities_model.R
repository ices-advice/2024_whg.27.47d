
############################## Read assessment data #############################################################


read.assessment.data <- function(wk.data.path, wk.mean.f.range, wk.plus.gp, areas="all", datayear)
{
  
  # Read objects
  wk.stock <- readFLStock(paste(wk.data.path, "whg47d_idx.txt", sep=""))
  
  if(areas=="all") wk.index <- readFLIndices("data\\survey.dat")
  if(areas=="north") wk.index <- readFLIndices("bootstrap\\data\\whg47d_survey_north.dat")
  if(areas=="south") wk.index <- readFLIndices("bootstrap\\data\\whg47d_survey_south.dat")
  
  if(areas=="all"){ wk.temp<- read.table("data\\smoothed_matogive_plusg15_full.csv", header=T,sep=",")
                   wk.temp<-wk.temp[,-1]
                   wk.stock@mat[,,,,] <- t(data.matrix(wk.temp))}
  
  if(areas=="north") { wk.temp<- read.table("data\\smoothed_matogive_plusg15_north_full.csv", header=T,sep=",")
                      wk.temp<-wk.temp[,-1]
                      wk.stock@mat[,,,,] <- t(data.matrix(wk.temp))}
  if(areas=="south") { wk.temp<- read.table("data\\smoothed_matogive_plusg15_south_full.csv", header=T,sep=",")
                      wk.temp<-wk.temp[,-1]
                      wk.stock@mat[,,,,] <- t(data.matrix(wk.temp))}
  
  
  # Set mean F ranges
  wk.stock@range["minfbar"] <- wk.mean.f.range[1]
  wk.stock@range["maxfbar"] <- wk.mean.f.range[2]
  
  # Read in separate data for landings and discards
  
  wk.temp <- read.table("data\\wstock_age15.csv", header=T,sep=",")
  wk.temp<-wk.temp[,-1]
  wk.stock@stock.wt[,,,,] <- t(data.matrix(wk.temp))
  
  
  # Replace negatives in objects
  wk.stock     	<- replaceNegative(wk.stock)
  wk.index 		<- replaceNegative(wk.index)
  
  # Set units for objects
  units(wk.stock)[1:17] <- as.list(c(rep(c("tonnes","thousands","kg"),4), "NA", "NA", "f", "NA", "NA"))
  
  for (i in 1:length(wk.index))
  {
    wk.index[[i]]@type <- "age-structured survey"
  }
  
  # Set up temporary datasets with specified plus-groups (for table printing)
  wk.stock.pg <- setPlusGroup(wk.stock, wk.plus.gp)
  
  
  # Read in separate data for landings and discards
  
  wk.temp <- read.csv(paste(wk.data.path, "cn.csv", sep = ""))
  wk.stock@catch.n[,-dim(wk.stock)[2],,,] <- t(data.matrix(wk.temp))
  wk.temp <- read.csv(paste(wk.data.path, "cw.csv", sep = ""))
  wk.stock@catch.wt[,-dim(wk.stock)[2],,,] <- t(data.matrix(wk.temp))
  wk.stock@catch <- apply(wk.stock@catch.n * wk.stock@catch.wt, 2, sum)

  wk.temp <- read.table("data\\wstock_age15.csv", header=T,sep=",")
  wk.temp<-wk.temp[,-1]
  wk.stock@stock.wt[,,,,] <- t(data.matrix(wk.temp))	
  
  
  # Replace negatives in objects
  wk.stock     	<- replaceNegative(wk.stock)
  wk.index 		<- replaceNegative(wk.index)
  
  # Set units for objects
  units(wk.stock)[1:17] <- as.list(c(rep(c("tonnes","thousands","kg"),4), "NA", "NA", "f", "NA", "NA"))
  
  for (i in 1:length(wk.index))
  {
    wk.index[[i]]@type <- "age-structured survey"
  }
  
  # Set up temporary datasets with specified plus-groups (for table printing)
  wk.stock.pg <- setPlusGroup(wk.stock, wk.plus.gp)
  
  # Return data
  list(f.stock = wk.stock,
       f.stock.pg = wk.stock.pg,
       f.index = wk.index, s.stock = wk.stock, s.index = wk.index)
  
}

######################################################################################

# Replace negatives in FLR objects with NAs - following Iago Mosqueira

setGeneric("replaceNegative", function(obj, ...)
{
  standardGeneric("replaceNegative")
})

setMethod("replaceNegative", signature(obj="FLQuant"), function(obj)
{
  return(FLQuant(ifelse(obj<0, NA, obj)))
})

setMethod("replaceNegative", signature(obj="FLIndices"), function(obj)
{
  res <- obj
  for (i in 1:length(obj)) res[[i]]@index <- replaceNegative(obj[[i]]@index)
  return(res)
})

setMethod("replaceNegative", signature(obj="FLStock"), function(obj)
{
  for (name in names(getSlots(class(obj))[getSlots(class(obj))=="FLQuant"]))
    slot(obj,name) <- replaceNegative(slot(obj,name) )
  return(obj)
})

################################# print.input.data ###################################

print.input.data <- function(wk.x, wk.x.pg, wk.pg, wk.dp, wk.file, wk.title)
{
  # wk.x:     data frame to be tabulated
  # wk.x.pg:  data frame reduced using plus-group
  # wk.pg:    plus-group
  # wk.dp:    number of decimal places
  # wk.title: table title
  
  wk.nrows <- dim(wk.x)[1]
  wk.ncols <- dim(wk.x)[2]
  wk.ages <- rownames(wk.x)
  wk.years <- colnames(wk.x)
  
  wk.mat <- matrix(wk.x, nrow = wk.nrows, ncol = wk.ncols)
  wk.mat <- t(wk.mat)
  
  wk.mat <- cbind(wk.mat, as.numeric(wk.x.pg[ch(wk.pg)]))
  rownames(wk.mat) <- wk.years
  colnames(wk.mat) <- c(wk.ages, paste(ch(wk.pg), "+", sep = ""))
  
  invisible(capture.output(eval(
    {cat(paste("\n", wk.title, "\n\n"))
      round(wk.mat, wk.dp)}
  ), file = wk.file))
  
}



##############################  ch  ######################################

# Shortcut utilities

ch <- function(x) as.character(x)



########################### empty ########################################

# Functions to plot bivariate correlations

empty <- function(x, y, groups, subscripts, panel.number, packet.number) 
{
  # Empty function
}

diag.panel.cm <- function(draw, axis.line.col, ...)
{
  diag.panel.splom(draw = F, axis.line.col = axis.line.col, ...)
}

main.panel.cm <- function(x, y, groups, subscripts, 
                          panel.number, packet.number, .tol = 0.05) 
{
  panel.xyplot(x, y, pch = 19, col = grey(0.35), cex = 0.2)
  
  # Fit a linear model to the data
  wk.lm1 <- lm(y ~ x)
  wk.x1 <- 0:20 / 20
  wk.fit <- suppressWarnings(predict.lm(wk.lm1, 
                                        newdata = data.frame(x = wk.x1), se.fit = T))
  wk.y1 <- wk.fit$fit
  wk.yu <- wk.y1 + 2*wk.fit$se
  wk.yl <- wk.y1 - 2*wk.fit$se
  
  wk.sig <- identical(anova(wk.lm1)$"Pr(>F)"[1] < .tol, T)
  
  if (wk.sig) 
  {
    wk.line.f <- list(lwd = 2, lty = 1, col = "#000000")
    wk.line.ci <- list(lwd = 1, lty = 1, col = "red4")
  } else 
  {
    wk.line.f <- list(lwd = 1, lty = 1, col = "#0000FF")
    wk.line.ci <- list(lwd = 1, lty = 2, col = "#0000FF")
  }
  
  panel.lines(wk.x1, wk.y1, lwd = wk.line.f$lwd, lty = wk.line.f$lty, col = wk.line.f$col)
  panel.lines(wk.x1, wk.yu, lwd = wk.line.ci$lwd, lty = wk.line.ci$lty, col = wk.line.ci$col)
  panel.lines(wk.x1, wk.yl, lwd = wk.line.ci$lwd, lty = wk.line.ci$lty, col = wk.line.ci$col)
  
  # Draw in axes
  grid.draw(linesGrob(x = c(-0.1,1,1), y = c(0,0,1.1),
                      gp = gpar(lwd = 2, col = grey(0.5)),
                      default.units = "npc"))
}

###################### panel.pairs.cm ########################################################

panel.pairs.cm <- function(z, subscripts, panel.subscripts) 
{
  panel.pairs(z,
              panel.subscripts = panel.subscripts,
              subscripts       = subscripts,
              lower.panel      = empty,
              diag.panel       = diag.panel.cm,
              upper.panel      = main.panel.cm,
              axis.line.col    = "#FFFFFF",
  )
  
}

##################################### centre.log ############################################

centre.log <- function(wk.mat) 
{
  wk.mat[wk.mat <= 0] <- NA
  wk.mat <- log(wk.mat)
  apply(wk.mat, 2, function(wk.x) (wk.x-min(wk.x, na.rm = TRUE))/diff(range(wk.x, na.rm = TRUE)))
}

################################## plot.index.corr ##########################################

plot.index.corr <- function(wk.object, wk.type) 
{
  # par(bty="n")
  # trellis.par.set(box.rectangle = list(col = "white"))
  for (wk.i in seq(length(wk.object))) 
  {
    # Select one tuning fleet
    if (wk.type == "FLR")
    {
      wk.tune.mat <- t(wk.object[[wk.i]]@catch.n@.Data[,,1,1,1,1])
      wk.main <- wk.object[[wk.i]]@name
    } else
    {
      wk.tune.mat <- wk.object[[wk.i]]
      wk.main <- names(wk.object)
    }
    # Make cohort matrix
    if(is.na(wk.tune.mat[1,1])) wk.tune.mat<-wk.tune.mat[,]
    wk.n <- dim(wk.tune.mat)[2]
    wk.cohort.mat <- matrix(NA, ncol = wk.n, nrow = dim(wk.tune.mat)[1] + wk.n - 1)
    colnames(wk.cohort.mat) <- colnames(wk.tune.mat)
    for (wk.j in 1:wk.n) 
    {
      wk.cohort.mat[,wk.j] <- c(rep(NA,wk.n-wk.j), 
                                wk.tune.mat[,wk.j], rep(NA,wk.j-1))
      
    }
    
    print(splom(~centre.log(wk.cohort.mat), superpanel = panel.pairs.cm, xlab = wk.main, col = "white"))
  }
}



####################################  surbar  ################################

# SURBAR optimisation function

surbar <- function(wk.p, wk.surbar.data)
{
  # Data wk.idx (list), wk.qval, wk.wt, wk.rho, wk.refage, wk.lambda, wk.numk, wk.na, wk.ny, wk.y1
  # are passed through the wk.surbar.data list object
  
  wk.idx = wk.surbar.data$wk.idx
  wk.qval = wk.surbar.data$wk.qval
  wk.wt = wk.surbar.data$wk.wt
  wk.rho = wk.surbar.data$wk.rho
  wk.refage = wk.surbar.data$wk.refage
  wk.lambda = wk.surbar.data$wk.lambda
  wk.numk = wk.surbar.data$wk.numk
  wk.na = wk.surbar.data$wk.na
  wk.ny = wk.surbar.data$wk.ny
  wk.y1 = wk.surbar.data$wk.y1
  
  wk.s <- rep(NA, length = wk.na)
  wk.s[1:(wk.refage-1)] <- wk.p[1:(wk.refage-1)]
  wk.s[wk.refage] <- 1.0
  wk.s[(wk.refage+1):(wk.na-1)] <- wk.p[wk.refage:(wk.na-2)]
  wk.s[wk.na] <- wk.s[wk.na-1]
  
  wk.f <- rep(NA, length = wk.ny)
  wk.f[1:(wk.ny-1)] <- wk.p[(wk.na-1):(wk.na+wk.ny-3)]
  wk.f[wk.ny] <- mean(wk.f[(wk.ny-3):(wk.ny-1)])
  
  wk.r <- rep(NA, length = wk.na + wk.ny - 1)
  wk.r[1:(wk.na+wk.ny-1)] <- wk.p[(wk.na+wk.ny-2):length(wk.p)]
  
  # Total mortality
  wk.z <- wk.f %o% wk.s
  
  # Abundance
  wk.n <- array(NA, dim = dim(wk.z))
  wk.n[1,] <- rev(wk.r[1:wk.na])
  wk.n[2:wk.ny,1] <- wk.r[(wk.na+1):length(wk.r)]
  
  vecs <- array(NA, dim = c(wk.na*wk.ny, 4))
  vecs[,1] <- matrix(data = wk.n, nrow = wk.na * wk.ny, ncol = 1)
  vecs[,2] <- rep(1:wk.na, each = wk.ny)
  vecs[,3] <- rep(wk.y1:(wk.y1 + wk.ny - 1), wk.na)
  vecs[,4] <- vecs[,3] - vecs[,2]
  
  cz.list <- tapply(wk.z, vecs[,4], cumsum)
  
  vecs.list <- lapply(levels(as.factor(vecs[,4])), function(wk){
    temp <- vecs[vecs[,4] == wk,]
    temp.rep <- dim(temp)[1]
    if (!is.null(temp.rep)) 
    {
      temp[,1] <- rep(temp[1], temp.rep)
    }
    temp
  })
  
  vecs.list <- lapply(vecs.list, function(wk){
    temp.rep <- dim(wk)[1]
    if (!is.null(temp.rep))
    {
      wk.zz <- unlist(cz.list[as.character(wk[1,4])])
      wk <- cbind(wk, wk.zz)
      wk.a <- dim(wk)[1]
      # lnN(a,y) <- lnN(a-1,y-1) - z(a-1,y-1)
      wk[2:wk.a,1] <- wk[1:(wk.a-1),1] - wk[1:(wk.a-1),5]
    }else
    {
      wk.zz <- unlist(cz.list[as.character(wk[4])])
      wk <- c(wk, wk.zz)
    }		
    wk
  })
  
  vecs.table <- do.call(rbind, vecs.list)
  vecs.table <- vecs.table[order(vecs.table[,3], vecs.table[,2]),]
  
  new.n <- matrix(vecs.table[,1], nrow = wk.ny, ncol = wk.na, byrow = TRUE)
  wk.n <- exp(new.n)
  
  # Fitted survey indices I.hat and
  # back-transformed observed survey indices I.dash.star
  i.hat <- vector("list", length = wk.numk)
  i.dash.star <- vector("list", length = wk.numk)
  for (wk.k in 1:wk.numk)
  {
    i.hat[[wk.k]] <- wk.n * array(unlist(wk.qval[[wk.k]]), dim = dim(wk.n))
    i.dash.star[[wk.k]] <- array(unlist(wk.idx[[wk.k]] * exp(wk.z * wk.rho[[wk.k]])), dim = dim(wk.n))
  }
  
  # Survey log residuals
  res1 <- vector("list", length = wk.numk)
  out0 <- vector("list", length = wk.numk)
  for (wk.k in 1:wk.numk)
  {
    res1[[wk.k]] <- sqrt(wk.wt[[wk.k]]) * (log(i.dash.star[[wk.k]]) - log(i.hat[[wk.k]]))
    out0[[wk.k]] <- array(NA, dim = c(wk.na * wk.ny, 1))
    out0[[wk.k]][1:(wk.na*wk.ny),1] <- unlist(res1[[wk.k]])
    out0[[wk.k]] <- as.vector(na.exclude(out0[[wk.k]]))
  }
  out1 <- as.vector(unlist(out0))
  
  # Lambda smoother
  f1 <- wk.f[1:(wk.ny-2)]
  f2 <- wk.f[2:(wk.ny-1)]
  res2 <- sqrt(wk.lambda) * (f1 - f2)
  out2 <- as.vector(res2)
  
  # Overall SSQ
  as.numeric(c(out1,out2))
}

##################################### surbar.wrapper #################################################################

# Wrapper for SURBAR function
# Limit time-series , and age 1 and up

surbar.wrapper <- function(wk.stock, wk.index, wk.lambda, wk.refage, wk.zrange, startyear,startage)
{
  
  # here change 1 to min to get age zero corr for Q3
  wk.stock <- trim(wk.stock, age = startage:wk.stock@range["max"], year = startyear:wk.stock@range["maxyear"])
  
  wk.index[[2]] <- trim(wk.index[[2]], age = startage:wk.index[[2]]@range["max"],
                        year = max(startyear, wk.index[[2]]@range["minyear"]):wk.index[[2]]@range["maxyear"])
  wk.index[[1]] <- trim(wk.index[[1]], age = 1:wk.index[[1]]@range["max"],
                        year = max(startyear, wk.index[[1]]@range["minyear"]):wk.index[[1]]@range["maxyear"])
  
  
  # Generate stock weight data object
  wk.sw <- data.frame(t(array(as.numeric(wk.stock@stock.wt), dim = dim(wk.stock@stock.wt)[1:2])))
  names(wk.sw) <- wk.stock@range["min"]:wk.stock@range["max"]
  rownames(wk.sw) <- wk.stock@range["minyear"]:wk.stock@range["maxyear"]
  
  # Generate maturity data object
  wk.mat <- data.frame(t(array(as.numeric(wk.stock@mat), dim = dim(wk.stock@mat)[1:2])))
  names(wk.mat) <- wk.stock@range["min"]:wk.stock@range["max"]
  rownames(wk.mat) <- wk.stock@range["minyear"]:wk.stock@range["maxyear"]
  
  # Generate survey index list object
  wk.numk <- length(wk.index)
  wk.idx <- wk.rho <- vector("list", length = wk.numk)
  names(wk.idx) <- names(wk.index)
  for (wk.k in 1:wk.numk)
  {
    wk.idx[[wk.k]] <- data.frame(t(array(as.numeric(wk.index[[wk.k]]@index), 
                                         dim = dim(wk.index[[wk.k]]@index)[1:2])))
    names(wk.idx[[wk.k]]) <- wk.index[[wk.k]]@range["min"]:wk.index[[wk.k]]@range["max"]
    rownames(wk.idx[[wk.k]]) <- wk.index[[wk.k]]@range["minyear"]:wk.index[[wk.k]]@range["maxyear"]
    wk.rho[[wk.k]] <- as.numeric((wk.index[[wk.k]]@range["startf"] + wk.index[[wk.k]]@range["endf"]) / 2.0)
  }
  
  # Set survey data to consistent dimensions
  wk.idx.tmp <- wk.idx
  wk.idx <- vector("list", length = wk.numk)
  names(wk.idx) <- names(wk.idx.tmp)
  wk.idx <- lapply(wk.idx, function(wk)
  {
    wk.tab <- data.frame(array(NA, dim = dim(wk.sw)))
    names(wk.tab) <- names(wk.sw)
    rownames(wk.tab) <- rownames(wk.sw)
    wk.tab
  })
  for (wk.k in 1:wk.numk)
  {
    wk.a <- wk.index[[wk.k]]@range["min"]:wk.index[[wk.k]]@range["max"]
    wk.y <- wk.index[[wk.k]]@range["minyear"]:wk.index[[wk.k]]@range["maxyear"]
    wk.idx[[wk.k]][ch(wk.y),ch(wk.a)] <- wk.idx.tmp[[wk.k]][ch(wk.y),ch(wk.a)]
  }
  
  # Trim SW, MAT and survey data to ranges specified by survey data only
  wk.a1 <- min(unlist(lapply(wk.index, function(wk){wk@range["min"]})))
  wk.a2 <- max(unlist(lapply(wk.index, function(wk){wk@range["max"]})))
  wk.y1 <- min(unlist(lapply(wk.index, function(wk){wk@range["minyear"]})))
  wk.y2 <- max(unlist(lapply(wk.index, function(wk){wk@range["maxyear"]})))
  
  wk.sw <- wk.sw[ch(wk.y1:wk.y2), ch(wk.a1:wk.a2)]
  names(wk.sw) <- wk.a1:wk.a2
  rownames(wk.sw) <- wk.y1:wk.y2
  wk.mat <- wk.mat[ch(wk.y1:wk.y2), ch(wk.a1:wk.a2)]
  names(wk.mat) <- wk.a1:wk.a2
  rownames(wk.mat) <- wk.y1:wk.y2
  for (wk.k in 1:wk.numk)
  {
    wk.idx[[wk.k]] <- wk.idx[[wk.k]][ch(wk.y1:wk.y2), ch(wk.a1:wk.a2)]
  }
  
  # If last year missing, apply three-year mean for SW and MAT
  if (wk.stock@range["maxyear"] < wk.y2)
  {
    wk.sw[length(rownames(wk.sw)),] <- apply(wk.sw[(length(rownames(wk.sw))-3):(length(rownames(wk.sw))-1),], 2, mean)
    wk.mat[length(rownames(wk.mat)),] <- apply(wk.mat[(length(rownames(wk.mat))-3):(length(rownames(wk.mat))-1),], 2, mean)
  }
  
  # Replace zeros with minimum value
  for (wk.k in 1:wk.numk)
  {
    wk.xx <- wk.idx[[wk.k]]
    wk.x.min1 <- min(wk.xx, na.rm = TRUE)
    if (wk.x.min1 == 0)
    {
      wk.xx[wk.xx == 0] <- 9999
      wk.x.min2 <- min(wk.xx, na.rm = TRUE)
      wk.xx[wk.xx == 9999.0] <- wk.x.min2
    }
    wk.idx[[wk.k]] <- wk.xx
  }
  
  # Mean-standardise survey data
  for (wk.k in 1:wk.numk)
  {
    w<-wk.idx[[wk.k]]
    wk.mean <- mean(colMeans(wk.idx[[wk.k]], na.rm = TRUE), na.rm = TRUE)  #corrected mean function
    wk.idx[[wk.k]] <- wk.idx[[wk.k]] / wk.mean
  }
  
  # Set up standard catchability and weightings arrays
  wk.qval <- vector("list", length = wk.numk)
  wk.wt <- vector("list", length = wk.numk)
  for (wk.k in 1:wk.numk)
  {
    wk.qval[[wk.k]] <- data.frame(array(1.0, dim = dim(wk.idx[[wk.k]])))
    rownames(wk.qval[[wk.k]]) <- rownames(wk.idx[[wk.k]])
    names(wk.qval[[wk.k]]) <- names(wk.idx[[wk.k]])
    wk.wt[[wk.k]] <- data.frame(array(1.0, dim = dim(wk.idx[[wk.k]])))
    rownames(wk.wt[[wk.k]]) <- rownames(wk.idx[[wk.k]])
    names(wk.wt[[wk.k]]) <- names(wk.idx[[wk.k]])
  }
  
  # General target vectors (leaving room for fixed values)
  
  wk.na <- length(wk.a1:wk.a2)
  wk.ny <- length(wk.y1:wk.y2)
  wk.s0.a <- seq(1.0, 1.0, length = wk.refage - wk.a1 + 1)
  wk.s0.b <- seq(1.0, 1.0, length = wk.a2 - wk.refage)
  wk.s0 <- c(wk.s0.a[1:(length(wk.s0.a) - 1)], wk.s0.b[2:length(wk.s0.b)])
  if (length(na.omit(wk.s0)) < length(wk.s0))
  {
    stop("\nCheck reference age.\n")
  }
  wk.f0 <- rep(1.0, wk.ny - 1)
  
  # Initial estimates for r are taken from the log of the average (across surveys)
  # of the survey index values at the appropriate years/ages
  wk.temp.r <- data.frame(array(NA, dim = dim(wk.idx[[1]])))
  names(wk.temp.r) <- names(wk.idx[[1]])
  rownames(wk.temp.r) <- rownames(wk.idx[[1]])
  for (wk.i in rownames(wk.temp.r))
  {
    for (wk.j in names(wk.temp.r))
    {
      wk.temp.r[wk.i,wk.j] <- mean(unlist(lapply(wk.idx, function(wk){wk[wk.i, wk.j]})), na.rm = TRUE)
    }
  }
  # Fill missing values with age-based averages
  for (wk.j in names(wk.temp.r))
  {
    wk.temp.mean <- mean(wk.temp.r[,wk.j], na.rm = TRUE)
    wk.temp.r[is.nan(wk.temp.r[,wk.j]),wk.j] <- wk.temp.mean
  }
  wk.r0 <- log(as.numeric(unlist(c(rev(wk.temp.r[1,]), wk.temp.r[2:dim(wk.temp.r)[1],1]))))
  wk.params0 <- c(wk.s0, wk.f0, wk.r0)
  # Test: if there are ages and years for which there are NO data in the initial estimates of r, stop
  if (length(is.nan(wk.r0)[is.nan(wk.r0)]) > 0)
  {
    cat("\nAcross-survey averages:\n")
    print(wk.temp.r)
    stop("One of the recruiting cohorts lacks information from any survey.")
  }
  
  # Set up control object
  wk.mqdt.control <- list(ftol = 0.00001, ptol = 0.00001, gtol = 0, 
                          diag = numeric(), factor = 100, maxfev = 100 * (length(wk.params0) + 1), 
                          nprint = 0, maxiter = 200)
  
  # Set up SURBAR data and setting object
  wk.surbar.data <- list(wk.idx = wk.idx, wk.qval = wk.qval, wk.wt = wk.wt, wk.rho = wk.rho, 
                         wk.refage = wk.refage, wk.lambda = wk.lambda, wk.numk = wk.numk, wk.na = wk.na, 
                         wk.ny = wk.ny, wk.y1 = wk.y1)
  
  # Run minimisation
  wk.surbar <- nls.lm(wk.params0, fn = surbar, wk.surbar.data = wk.surbar.data,
                      control = wk.mqdt.control)
  
  # Extract parameter values
  wk.s <- rep(NA, length = wk.na)
  wk.s[1:(wk.refage-1)] <- wk.surbar$par[1:(wk.refage-1)]
  wk.s[wk.refage] <- 1.0
  wk.s[(wk.refage+1):(wk.na-1)] <- wk.surbar$par[wk.refage:(wk.na-2)]
  wk.s[wk.na] <- wk.s[wk.na-1]
  wk.f <- rep(NA, length = wk.ny)
  wk.f[1:(wk.ny-1)] <- wk.surbar$par[(wk.na-1):(wk.na + wk.ny - 3)]
  wk.f[wk.ny] <- mean(wk.f[(wk.ny-3):(wk.ny-1)])
  wk.r <- wk.surbar$par[(wk.na + wk.ny - 2):length(wk.surbar$par)]
  
  # Extract log residuals
  wk.res <- vector("list", length = wk.numk)
  names(wk.res) <- names(wk.idx)
  wk.x.res <- surbar(wk.surbar$par, wk.surbar.data)
  wk.res.start <- 1
  for (wk.k in 1:wk.numk)
  {
    wk.x.temp <- wk.idx[[wk.k]][rownames(wk.idx[[wk.k]]),names(wk.idx[[wk.k]])]
    wk.x.temp <- na.omit(wk.x.temp)
    wk.res.end <- (wk.res.start - 1) + (dim(wk.x.temp)[1] * dim(wk.x.temp)[2])
    wk.res[[wk.k]] <- data.frame(array(wk.x.res[wk.res.start:wk.res.end], dim = dim(wk.x.temp)))
    names(wk.res[[wk.k]]) <- names(wk.x.temp)
    rownames(wk.res[[wk.k]]) <- rownames(wk.x.temp)
    wk.res.start <- wk.res.end + 1
  }
  
  # Construct population estimates
  
  wk.sumtab <- data.frame(array(NA, dim = c(wk.ny,5)))
  names(wk.sumtab) <- c("year", "rec", "ssb", "tsb", "meanz")
  
  wk.zmort <- wk.f %o% wk.s
  
  wk.lnn <- array(NA, dim = dim(wk.zmort))
  wk.lnn[1,] <- rev(wk.r[1:dim(wk.lnn)[2]])
  wk.lnn[2:dim(wk.lnn)[1],1] <- wk.r[(dim(wk.lnn)[2]+1):length(wk.r)]
  # Oldest true age
  wk.ota <- dim(wk.zmort)[1]
  for (wk.j in 2:dim(wk.zmort)[2])
  {
    for (wk.i in 2:dim(wk.zmort)[1])
    {
      wk.lnn[wk.i,wk.j] <- wk.lnn[wk.i-1,wk.j-1] - wk.zmort[wk.i-1,wk.j-1]
    }
  }
  wk.n <- exp(wk.lnn)	
  
  wk.sumtab$year <- wk.y1:wk.y2
  wk.sumtab$meanz <- apply(wk.zmort[,wk.zrange[1]:wk.zrange[2]], 1, mean)
  wk.sumtab$rec <- exp(wk.r[wk.na:length(wk.r)])
  wk.sumtab$ssb <- apply(wk.n * wk.sw * wk.mat, 1, sum)	
  wk.sumtab$tsb <- apply(wk.n * wk.sw, 1, sum)
  
  # Mean-standardise rec, ssb, tsb
  wk.mean.rec <- mean(wk.sumtab$rec)
  wk.mean.ssb <- mean(wk.sumtab$ssb)
  wk.mean.tsb <- mean(wk.sumtab$tsb)
  wk.sumtab$rec <- wk.sumtab$rec / wk.mean.rec
  wk.sumtab$ssb <- wk.sumtab$ssb / wk.mean.ssb
  wk.sumtab$tsb <- wk.sumtab$tsb / wk.mean.tsb
  
  # Bootstrap model fit using data simulation
  
  # Test for singularity
  wk.eigen <- eigen(wk.surbar$hessian, only.values = TRUE)$values
  if (length(wk.eigen[wk.eigen == 0]) > 0)
  {
    stop("At least one parameter cannot be estimated: \nCheck that there are data for each cohort.")
  }
  
  wk.n.psim <- 1000
  wk.psim <- mvrnorm(n = wk.n.psim, mu = wk.surbar$par, vcov(wk.surbar))
  
  wk.boot.stock <- data.frame(array(NA, dim = c(wk.ny, 5)))
  names(wk.boot.stock) <- c("year", "rec", "ssb", "tsb", "meanz")
  wk.psim.stock <- vector("list", wk.n.psim)
  wk.psim.stock <- lapply(wk.psim.stock, function(wk){wk <- wk.boot.stock})
  
  wk.psim.s <- array(NA, dim = c(1000, wk.na))
  wk.psim.s[,1:(wk.refage-1)] <- wk.psim[,1:(wk.refage-1)]
  wk.psim.s[,wk.refage] <- 1.0
  wk.psim.s[,(wk.refage+1):(wk.na-1)] <- wk.psim[,wk.refage:(wk.na-2)]
  wk.psim.s[,wk.na] <- wk.psim.s[,wk.na-1]
  
  wk.psim.f <- array(NA, dim = c(1000, wk.ny))
  wk.psim.f[,1:(wk.ny-1)] <- wk.psim[,(wk.na-1):(wk.na + wk.ny - 3)]
  wk.psim.f[,wk.ny] <- apply(wk.psim.f[,(wk.ny-3):(wk.ny-1)], 1, mean)
  
  wk.psim.r <- wk.psim[,(wk.na + wk.ny - 2):length(wk.surbar$par)]
  
  wk.temp.s <- rep(NA, length = wk.na)
  wk.temp.f <- rep(NA, length = wk.ny)
  
  for (wk.i in 1:wk.n.psim)
  {
    # Extract parameters
    
    wk.temp.s[1:(wk.refage-1)] <- wk.psim[wk.i,1:(wk.refage-1)]
    wk.temp.s[wk.refage] <- 1.0
    wk.temp.s[(wk.refage+1):(wk.na-1)] <- wk.psim[wk.i,wk.refage:(wk.na-2)]
    wk.temp.s[wk.na] <- wk.temp.s[wk.na-1]
    
    wk.temp.f[1:(wk.ny-1)] <- wk.psim[wk.i,(wk.na-1):(wk.na + wk.ny - 3)]
    wk.temp.f[wk.ny] <- mean(wk.temp.f[(wk.ny-3):(wk.ny-1)])
    
    wk.temp.r <- wk.psim[wk.i,(wk.na + wk.ny - 2):length(wk.surbar$par)]
    
    # Generate stock summaries
    
    wk.zmort <- wk.temp.f %o% wk.temp.s
    
    wk.lnn <- array(NA, dim = dim(wk.zmort))
    wk.lnn[1,] <- rev(wk.r[1:dim(wk.lnn)[2]])
    wk.lnn[2:dim(wk.lnn)[1],1] <- wk.r[(dim(wk.lnn)[2]+1):length(wk.r)]
    for (wk.jj in 2:dim(wk.zmort)[2])
    {
      for (wk.ii in 2:dim(wk.zmort)[1])
      {
        wk.lnn[wk.ii,wk.jj] <- wk.lnn[wk.ii-1,wk.jj-1] - wk.zmort[wk.ii-1,wk.jj-1]
      }
    }
    wk.n <- exp(wk.lnn)	
    
    wk.psim.stock[[wk.i]]$year <- wk.y1:wk.y2
    wk.psim.stock[[wk.i]]$meanz <- apply(wk.zmort[,wk.zrange[1]:wk.zrange[2]], 1, mean)
    wk.psim.stock[[wk.i]]$z <- wk.zmort
    wk.psim.stock[[wk.i]]$rec <- exp(wk.temp.r[wk.na:length(wk.temp.r)]) / wk.mean.rec
    wk.psim.stock[[wk.i]]$ssb <- apply(wk.n * wk.sw * wk.mat, 1, sum) / wk.mean.ssb
    wk.psim.stock[[wk.i]]$tsb <- apply(wk.n * wk.sw, 1, sum) / wk.mean.tsb
  }
  
  # Return all the data required to produce summary tables and plots
  # outwith this function (required for Sweave implementation)
  
  list(s.stock = wk.stock, s.idx = wk.idx, s.sumtab = wk.sumtab, 
       s.s = wk.s, s.f = wk.f, s.r = wk.r, 
       s.psim = wk.psim.stock, s.psim.s = wk.psim.s, 
       s.psim.f = wk.psim.f, s.psim.r = wk.psim.r, 
       s.res = wk.res, s.y1 = wk.y1, s.y2 = wk.y2, 
       s.a1 = wk.a1, s.a2 = wk.a2)
  
}



############################### plot.surbar  #####################################

# General function to plot SURBAR outputs

plot.surbar <- function(wk.list, wk.type, nums=NULL)
{
  # Transfer data from input list to local variables
  wk.stock <- wk.list$s.stock
  wk.idx <- wk.list$s.idx
  wk.sumtab <- wk.list$s.sumtab
  wk.s <- wk.list$s.s
  wk.f <- wk.list$s.f
  wk.r <- wk.list$s.r
  wk.psim <- wk.list$s.psim
  wk.psim.s <- wk.list$s.psim.s
  wk.psim.f <- wk.list$s.psim.f
  wk.psim.r <- wk.list$s.psim.r
  wk.res <- wk.list$s.res
  wk.y1 <- wk.list$s.y1
  wk.y2 <- wk.list$s.y2
  wk.a1 <- wk.list$s.a1
  wk.a2 <- wk.list$s.a2
  
  # Determine number of indices
  wk.numk <- length(wk.res)
  if (is.null(nums)) nums <-(1:wk.numk)
  wk.ny <- wk.y2 - wk.y1 + 1
  wk.na <- wk.a2 - wk.a1 + 1
  
  # Define window
  if (wk.type == "sum.line" | wk.type == "sum.boxplot")
  {
    par(mfrow = c(2,2), mar = c(5,4,1,1)+0.1)
  } else if (wk.type == "res.line" | wk.type == "res.smooth")
  {
    config <- switch(wk.numk,
                     c(1,1), c(2,1), c(2,2), c(2,2), c(2,3), c(2,3), c(3,3), c(3,3), c(3,3))
    par(mfrow = config, mar = c(5,4,1,1)+0.1)
  } else if (wk.type == "catch.curve" | wk.type == "log.by.cohort")
  {
    config <- switch(wk.numk,
                     c(1,1), c(2,1), c(2,2), c(2,2), c(2,3), c(2,3), c(3,3), c(3,3), c(3,3))
   
    
    par(mfrow = config, mar = c(5,4,3,1)+0.1)
  } else if (wk.type == "params")
  {
    par(mfrow = c(2,3), mar = c(5,4,1,1)+0.1)
  } else if (wk.type == "age.scatterplot")
  {
    par(mfrow = c(2,1), mar = c(5,4,1,1)+0.1)
  }
  
  # Generate plots
  if (wk.type == "sum.line")
  {
    # Mean Z
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
    plot(wk.y1:wk.y2, wk.stock.meanz.quantile[,3], type = "n", lty = 1,
         xlab = "Year", ylab = "Mean Z", 
         ylim = c(min(0, wk.stock.meanz.quantile), max(wk.stock.meanz.quantile)))
    polygon(x = c(wk.y1:wk.y2, rev(wk.y1:wk.y2)), 
            y = c(wk.stock.meanz.quantile[,1], rev(wk.stock.meanz.quantile[,5])), density = -1, col = "grey",
            lty = 0)
    lines(wk.y1:wk.y2, wk.stock.meanz.quantile[,3], lty = 1, col = "black", lwd = 2)
    points(wk.sumtab$year, wk.stock.meanz.mean, pch = 3, col = "red")
    points(wk.sumtab$year, wk.sumtab$meanz, pch = 16, col = "green")
    legend(legend = c("NLS estimate", "Bootstrap mean", "Bootstrap median", "90% CI"), x = "bottomleft",
           pch = c(16,3,-1,-1), lty = c(-1,-1,1,1), lwd = c(-1,-1,2,5), bty = "n", col = c("green","red","black", "grey"))
    
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
    plot(wk.y1:wk.y2, wk.stock.ssb.quantile[,3], type = "n", lty = 1,
         xlab = "Year", ylab = "SSB", ylim = c(min(0, wk.stock.ssb.quantile), max(wk.stock.ssb.quantile)))
    polygon(x = c(wk.y1:wk.y2, rev(wk.y1:wk.y2)), 
            y = c(wk.stock.ssb.quantile[,1], rev(wk.stock.ssb.quantile[,5])), density = -1, col = "grey",
            lty = 0)
    lines(wk.y1:wk.y2, wk.stock.ssb.quantile[,3], lty = 1, col = "black", lwd = 2)
    points(wk.sumtab$year, wk.stock.ssb.mean, pch = 3, col = "red")
    points(wk.sumtab$year, wk.sumtab$ssb, pch = 16, col = "green")
    
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
    plot(wk.y1:wk.y2, wk.stock.tsb.quantile[,3], type = "n", lty = 1,
         xlab = "Year", ylab = "Total biomass", ylim = c(min(0, wk.stock.tsb.quantile), max(wk.stock.tsb.quantile)))
    polygon(x = c(wk.y1:wk.y2, rev(wk.y1:wk.y2)), 
            y = c(wk.stock.tsb.quantile[,1], rev(wk.stock.tsb.quantile[,5])), density = -1, col = "grey",
            lty = 0)
    lines(wk.y1:wk.y2, wk.stock.tsb.quantile[,3], lty = 1, col = "black", lwd = 2)
    points(wk.sumtab$year, wk.stock.tsb.mean, pch = 3, col = "red")
    points(wk.sumtab$year, wk.sumtab$tsb, pch = 16, col = "green")
    
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
    plot(wk.y1:wk.y2, wk.stock.rec.quantile[,3], type = "n", lty = 1,
         xlab = "Year", ylab = "Recruitment", ylim = c(min(0, wk.stock.rec.quantile), max(wk.stock.rec.quantile)))
    polygon(x = c(wk.y1:wk.y2, rev(wk.y1:wk.y2)), 
            y = c(wk.stock.rec.quantile[,1], rev(wk.stock.rec.quantile[,5])), density = -1, col = "grey",
            lty = 0)
    lines(wk.y1:wk.y2, wk.stock.rec.quantile[,3], lty = 1, col = "black", lwd = 2)
    points(wk.sumtab$year, wk.stock.rec.mean, pch = 3, col = "red")
    points(wk.sumtab$year, wk.sumtab$rec, pch = 16, col = "green")
  } else if (wk.type == "sum.boxplot")
  {
    # Mean Z
    wk.stock.meanz <- do.call(rbind, lapply(wk.psim, function(wk){wk$meanz}))
    wk.bp.meanz <- data.frame(wk.stock.meanz)
    names(wk.bp.meanz) <- wk.y1:wk.y2
    wk.bp.meanz <- stack(wk.bp.meanz)
    boxplot(values ~ ind, data = wk.bp.meanz, xlab = "Year", ylab = "Mean Z")
    
    # SSB
    wk.stock.ssb <- do.call(rbind, lapply(wk.psim, function(wk){wk$ssb}))
    wk.bp.ssb <- data.frame(wk.stock.ssb)
    names(wk.bp.ssb) <- wk.y1:wk.y2
    wk.bp.ssb <- stack(wk.bp.ssb)
    boxplot(values ~ ind, data = wk.bp.ssb, xlab = "Year", ylab = "SSB")
    
    # TSB
    wk.stock.tsb <- do.call(rbind, lapply(wk.psim, function(wk){wk$tsb}))
    wk.bp.tsb <- data.frame(wk.stock.tsb)
    names(wk.bp.tsb) <- wk.y1:wk.y2
    wk.bp.tsb <- stack(wk.bp.tsb)
    boxplot(values ~ ind, data = wk.bp.tsb, xlab = "Year", ylab = "Total biomass")
    
    # Recruitment
    wk.stock.rec <- do.call(rbind, lapply(wk.psim, function(wk){wk$rec}))
    wk.bp.rec <- data.frame(wk.stock.rec)
    names(wk.bp.rec) <- wk.y1:wk.y2
    wk.bp.rec <- stack(wk.bp.rec)
    boxplot(values ~ ind, data = wk.bp.rec, xlab = "Year", ylab = "Recruitment")
  } else if (wk.type == "res.line")
  {
    wk.ymin <- min(as.numeric(unlist(wk.res)), na.rm = TRUE)
    wk.ymax <- max(as.numeric(unlist(wk.res)), na.rm = TRUE)
    
    for (wk.k in 1:wk.numk)
    {
      wk.y1.a <- as.numeric(rownames(wk.res[[wk.k]])[1])
      wk.y2.a <- as.numeric(rev(rownames(wk.res[[wk.k]]))[1])
      
      plot (wk.y1:wk.y2, rep(0, wk.ny), xlab = "Year", ylab = "Log residual",
            ylim = c(wk.ymin, wk.ymax), type = "n")
      for (wk.i in 1:dim(wk.res[[wk.k]])[2])
      {
        lines(wk.y1.a:wk.y2.a, wk.res[[wk.k]][,wk.i], col = wk.i)
      }
      abline(h = 0, col = 1)
      legend(x = "topleft", legend = names(wk.res)[wk.k], bty = "n", cex = 0.75)	
      legend(x = "bottomleft", legend = paste("Age", names(wk.res[[wk.k]])), lty = 1, col = 1:dim(wk.res[[wk.k]])[2],
             bty = "n", cex = 0.75, ncol = 2)
    }
  } else if (wk.type == "res.smooth")
  {
    wk.ymin <- min(as.numeric(unlist(wk.res)), na.rm = TRUE)
    wk.ymax <- max(as.numeric(unlist(wk.res)), na.rm = TRUE)
    
    for (wk.k in 1:wk.numk)
    {
      wk.y1.a <- as.numeric(rownames(wk.res[[wk.k]])[1])
      wk.y2.a <- as.numeric(rev(rownames(wk.res[[wk.k]]))[1])
      
      plot (wk.y1:wk.y2, rep(0, wk.ny), xlab = "Year", ylab = "Log residual",
            ylim = c(wk.ymin, wk.ymax), type = "n")
      for (wk.i in 1:dim(wk.res[[wk.k]])[2])
      {
        if (length(wk.res[[wk.k]][!is.na(wk.res[[wk.k]][,wk.i]),wk.i]) > 0)
        {
          points(wk.y1.a:wk.y2.a, wk.res[[wk.k]][,wk.i], pch = 3, col = wk.i)
          wk.data.loess <- data.frame(x = wk.y1.a:wk.y2.a, y = wk.res[[wk.k]][,wk.i])
          wk.res.loess <- loess(y ~ x, data = wk.data.loess, span = 1.0)
          wk.res.loess.pred.x <- seq(wk.y1.a, wk.y2.a, length = 100)
          wk.res.loess.pred <- predict(wk.res.loess, newdata = wk.res.loess.pred.x, se = TRUE)
          lines(wk.res.loess.pred.x, wk.res.loess.pred$fit, col = wk.i, lty = 1)
        }
      }
      abline(h = 0, col = 1)
      legend(x = "topleft", legend = names(wk.res)[wk.k], bty = "n", cex = 0.75)	
      legend(x = "bottomleft", legend = paste("Age", names(wk.res[[wk.k]])), lty = 1, col = 1:dim(wk.res[[wk.k]])[2],
             bty = "n", cex = 0.75, ncol = 2)
    }
  } else if (wk.type == "params")
  {
    # Boxplots: s
    wk.bp.s <- data.frame(wk.psim.s)
    names(wk.bp.s) <- wk.a1:wk.a2
    wk.bp.s <- stack(wk.bp.s)
    boxplot(values ~ ind, data = wk.bp.s, xlab = "Age", ylab = "s")
    
    # Boxplots: f
    wk.bp.f <- data.frame(wk.psim.f)
    names(wk.bp.f) <- wk.y1:wk.y2
    wk.bp.f <- stack(wk.bp.f)
    boxplot(values ~ ind, data = wk.bp.f, xlab = "Year", ylab = "f")
    
    # Boxplots: r
    wk.bp.r <- data.frame(wk.psim.r)
    names(wk.bp.r) <- (wk.y1 - wk.na + 1 - wk.a1):(wk.y2 - wk.a1)
    wk.bp.r <- stack(wk.bp.r)
    boxplot(values ~ ind, data = wk.bp.r, xlab = "Cohort", ylab = "r")
    
    # Lineplots: s
    wk.stock.s.quantile <- array(NA, dim = c(dim(wk.psim.s)[2],5))
    wk.stock.s.mean <- rep(NA, dim(wk.psim.s)[2])
    rownames(wk.stock.s.quantile) <- wk.a1:wk.a2
    colnames(wk.stock.s.quantile) <- c("5%","25%","50%","75%","95%")
    for (wk.i in 1:dim(wk.psim.s)[2])
    {
      wk.stock.s.quantile[wk.i,] <- quantile(wk.psim.s[,wk.i], c(0.05, 0.25, 0.5, 0.75, 0.95))
      wk.stock.s.mean[wk.i] <- mean(wk.psim.s[,wk.i])
    }
    plot(wk.a1:wk.a2, wk.stock.s.quantile[,3], type = "n", lty = 1,
         xlab = "Age", ylab = "s", ylim = c(min(0, wk.stock.s.quantile), max(wk.stock.s.quantile)))
    polygon(x = c(wk.a1:wk.a2, rev(wk.a1:wk.a2)), 
            y = c(wk.stock.s.quantile[,1], rev(wk.stock.s.quantile[,5])), density = -1, col = "grey",
            lty = 0)
    lines(wk.a1:wk.a2, wk.stock.s.quantile[,3], lty = 1, col = "black", lwd = 2)
    points(wk.a1:wk.a2, wk.stock.s.mean, pch = 3, col = "red")
    points(wk.a1:wk.a2, wk.s, pch = 16, col = "green")
    legend(legend = c("NLS estimate", "Bootstrap mean", "Bootstrap median", "90% CI"), x = "bottomright",
           pch = c(16,3,-1,-1), lty = c(-1,-1,1,1), lwd = c(-1,-1,2,5), bty = "n", col = c("green","red","black", "grey"))
    
    # Lineplots: f
    wk.stock.f.quantile <- array(NA, dim = c(dim(wk.psim.f)[2],5))
    wk.stock.f.mean <- rep(NA, dim(wk.psim.f)[2])
    rownames(wk.stock.f.quantile) <- wk.y1:wk.y2
    colnames(wk.stock.f.quantile) <- c("5%","25%","50%","75%","95%")
    for (wk.i in 1:dim(wk.psim.f)[2])
    {
      wk.stock.f.quantile[wk.i,] <- quantile(wk.psim.f[,wk.i], c(0.05, 0.25, 0.5, 0.75, 0.95))
      wk.stock.f.mean[wk.i] <- mean(wk.psim.f[,wk.i])
    }
    plot(wk.y1:wk.y2, wk.stock.f.quantile[,3], type = "n", lty = 1,
         xlab = "Year", ylab = "f", ylim = c(min(0, wk.stock.f.quantile), max(wk.stock.f.quantile)))
    polygon(x = c(wk.y1:wk.y2, rev(wk.y1:wk.y2)), 
            y = c(wk.stock.f.quantile[,1], rev(wk.stock.f.quantile[,5])), density = -1, col = "grey",
            lty = 0)
    lines(wk.y1:wk.y2, wk.stock.f.quantile[,3], lty = 1, col = "black", lwd = 2)
    points(wk.y1:wk.y2, wk.stock.f.mean, pch = 3, col = "red")
    points(wk.y1:wk.y2, wk.f, pch = 16, col = "green")
    
    # Lineplots: r
    wk.r1 <- wk.y1 - wk.na + 1 - wk.a1
    wk.r2 <- wk.y2 - wk.a1
    wk.stock.r.quantile <- array(NA, dim = c(dim(wk.psim.r)[2],5))
    wk.stock.r.mean <- rep(NA, dim(wk.psim.r)[2])
    rownames(wk.stock.r.quantile) <- wk.r1:wk.r2
    colnames(wk.stock.r.quantile) <- c("5%","25%","50%","75%","95%")
    for (wk.i in 1:dim(wk.psim.r)[2])
    {
      wk.stock.r.quantile[wk.i,] <- quantile(wk.psim.r[,wk.i], c(0.05, 0.25, 0.5, 0.75, 0.95))
      wk.stock.r.mean[wk.i] <- mean(wk.psim.r[,wk.i])
    }
    plot(wk.r1:wk.r2, wk.stock.r.quantile[,3], type = "n", lty = 1,
         xlab = "Cohort", ylab = "r", ylim = c(min(0, wk.stock.r.quantile), max(wk.stock.r.quantile)))
    polygon(x = c(wk.r1:wk.r2, rev(wk.r1:wk.r2)), 
            y = c(wk.stock.r.quantile[,1], rev(wk.stock.r.quantile[,5])), density = -1, col = "grey",
            lty = 0)
    lines(wk.r1:wk.r2, wk.stock.r.quantile[,3], lty = 1, col = "black", lwd = 2)
    points(wk.r1:wk.r2, wk.stock.r.mean, pch = 3, col = "red")
    points(wk.r1:wk.r2, wk.r, pch = 16, col = "green")
    abline(v = wk.y1 - wk.a1 - 0.5, lty = 8, col = "blue")
    
  } else if (wk.type == "catch.curve")
  {
    for (wk.k in nums)
    {
      
      wk.cc.idx <- na.omit(wk.idx[[wk.k]])
      wk.cc.years <- as.numeric(row.names(wk.cc.idx))
      wk.cc.ages <- as.numeric(names(wk.cc.idx))
      
      wk.cc <- stack(wk.cc.idx)
      wk.cc$ind <- as.numeric(as.character(wk.cc$ind))
      wk.cc <- data.frame(cbind(wk.cc, wk.cc.years))
      names(wk.cc) <- c("index","age","year")
      wk.cc <- data.frame(cbind(wk.cc, wk.cc$year - wk.cc$age))
      names(wk.cc) <- c("index","age","year","cohort")
      
      wk.cc.cohorts <- unique(wk.cc$cohort)[order(unique(wk.cc$cohort))]
      plot(x = 0, y = 0, 
           xlim = c(min(wk.cc.years, na.rm = TRUE), max(wk.cc.years, na.rm = TRUE)),
           ylim = c(min(log(wk.cc.idx), na.rm = TRUE), max(log(wk.cc.idx), na.rm = TRUE) + 0.25),
           type = "n", xlab = "Year", ylab = "Log survey index",
           main = names(wk.idx)[wk.k])
      
      wk.jj <- 0
      for (wk.j in wk.cc.cohorts)
      {
        wk.jj <- wk.jj + 1
        wk.cc0 <- wk.cc[wk.cc$cohort == wk.j,]
        lines(wk.cc0$year, log(wk.cc0$index), type = "l", lty = 1, pch = -1,
              col = rainbow(n = length(wk.cc.cohorts))[wk.jj])
        text(x = wk.cc0$year[1], y = log(wk.cc0$index)[1] + 0.25, wk.cc0$cohort[1], cex = 0.75)
      }
    }
  } else if (wk.type == "log.by.cohort")
  {
    
    # Set plot limits to cover all cohorts in all series
    
    wk.lbc.idx <- wk.idx
    wk.lbc.xmin <- 9999
    wk.lbc.xmax <- -9999
    for (wk.k in nums)
    {
      wk.lbc.idx[[wk.k]] <- na.omit(wk.idx[[wk.k]])
      wk.xmin <- min(as.numeric(rownames(wk.lbc.idx[[wk.k]]))) - max(as.numeric(names(wk.lbc.idx[[wk.k]])))
      wk.xmax <- max(as.numeric(rownames(wk.lbc.idx[[wk.k]])))
      if (wk.xmin < wk.lbc.xmin)
      {
        wk.lbc.xmin <- wk.xmin
      }
      if (wk.xmax > wk.lbc.xmax)
      {
        wk.lbc.xmax <- wk.xmax
      }
    }
    
    for (wk.k in nums)
    {
      
      wk.lbc <- wk.lbc.idx[[wk.k]]
      
      # Mean standardise by age
      for (wk.kk in 1:dim(wk.lbc)[2])
      {
        wk.lbc[,wk.kk] <- wk.lbc[,wk.kk] / mean(wk.lbc[,wk.kk])
      }
      
      # Take logs
      wk.lbc <- log(wk.lbc)
      
      wk.lbc.ages <- as.numeric(names(wk.lbc))
      wk.lbc.years <- as.numeric(rownames(wk.lbc))
      
      # Generate stacked dataframe
      wk.lbc <- stack(wk.lbc)
      wk.lbc$ind <- as.numeric(as.character(wk.lbc$ind))
      wk.lbc <- data.frame(cbind(wk.lbc, wk.lbc.years))
      names(wk.lbc) <- c("index","age","year")
      wk.lbc <- data.frame(cbind(wk.lbc, wk.lbc$year - wk.lbc$age))
      names(wk.lbc) <- c("index","age","year","cohort")
      
      wk.lbc.cohorts <- unique(wk.lbc$cohort)[order(unique(wk.lbc$cohort))]
      plot(x = 0, y = 0, 
           xlim = c(wk.lbc.xmin, wk.lbc.xmax),
           ylim = c(min(wk.lbc$index, na.rm = TRUE), max(wk.lbc$index, na.rm = TRUE)),
           type = "n", xlab = "Cohort", ylab = "Mean-std log survey index",
           main = names(wk.idx)[wk.k])
      
      wk.jj <- 0
      for (wk.j in wk.lbc.ages)
      {
        wk.jj <- wk.jj + 1
        wk.lbc0 <- wk.lbc[wk.lbc$age == wk.j,]
        lines(wk.lbc0$cohort, wk.lbc0$index, type = "l", lty = 1, 
              col = rainbow(n = length(wk.lbc.ages))[wk.jj])
      }
      wk.jj <- 0
      for (wk.j in wk.lbc.ages)
      {
        wk.jj <- wk.jj + 1
        wk.lbc0 <- wk.lbc[wk.lbc$age == wk.j,]
        points(x = wk.lbc0$cohort[1], y = wk.lbc0$index[1], pch = 16, cex = 2.5, col = "white")
        points(x = wk.lbc0$cohort[1], y = wk.lbc0$index[1], pch = 1, cex = 2.5, col = rainbow(n = length(wk.lbc.ages))[wk.jj])
        text(x = wk.lbc0$cohort[1], y = wk.lbc0$index[1], wk.lbc0$age[1], cex = 0.75)
      }
    }
  } else if (wk.type == "age.scatterplot")
  {
    for (wk.k in nums)
    {
      # windows()
      plot.index.corr(wk.idx[wk.k], wk.type = "SURBAR")
    }
  }
}

####################################################################
####################################################################

# Calculate SSB - required for NS whiting as the supplied FLR SSB function would 
# only generate NAs     

ssb.new <- function(wk.x)
{
  # wk.x = stock object
  apply(wk.x@stock.n * wk.x@stock.wt * wk.x@mat, 2, sum)
}

####################################################################

####################################################################

# Plot catch curves

plot.catch.curves <- function(wk.x, wk.ages = NULL, wk.yrs = NULL, wk.main = "", wk.ptype = "b") 
{
  wk.catch <- t(wk.x)
  wk.years <- as.numeric(rownames(wk.catch))
  wk.catch <- as.data.frame(cbind(wk.years, wk.catch))
  
  # Remove any zeros
  wk.catch[wk.catch <= 0] <- NA
  if (is.null(wk.yrs)) wk.yrs <- wk.catch$wk.years
  wk.catch <- wk.catch[wk.catch$wk.years %in% wk.yrs,]
  wk.xlim <- range(wk.catch$wk.years)
  wk.ylim <- range(log(wk.catch[,-1]), na.rm = TRUE)
  wk.plot.grad <-  TRUE
  
  # Make cohort matrix
  wk.n <- dim(wk.catch[,-1])[2]
  wk.cohort.mat <- wk.cohort.mat.yrs <- matrix(NA, ncol = wk.n, nrow = dim(wk.catch[,-1])[1]+wk.n-1)
  colnames(wk.cohort.mat) <- names(wk.catch)[-1]
  rownames(wk.cohort.mat) <- (wk.yrs[2] - wk.n):rev(wk.yrs)[1] - as.numeric(names(wk.catch)[2])
  wk.catch.yrs <- matrix(wk.yrs, nrow = length(wk.yrs), ncol = dim(wk.catch)[2]-1)
  for (wk.i in 1:wk.n) 
  {
    wk.cohort.mat[,wk.i] <- c(rep(NA, wk.n - wk.i), wk.catch[,wk.i+1],rep(NA,wk.i-1))
    wk.cohort.mat.yrs[,wk.i] <- c(rep(NA, wk.n - wk.i), wk.catch.yrs[,wk.i],rep(NA,wk.i-1))
  }
  
  par(bty = "l")
  if (wk.ptype %in% c("b","c")) 
  {
    plot(0, 0, type = "n", ylim = wk.ylim, xlim = wk.xlim, ylab = "log-catch", xlab = "year of catch",
         las = 1, main = wk.main)
    for (wk.i in 2:dim(wk.catch)[2]) 
    {
      text(wk.yrs, log(wk.catch[,wk.i]), names(wk.catch)[wk.i])
    }
  }
  
  wk.grad <- wk.cohort <- rep(NA, dim(wk.cohort.mat)[1])
  wk.m <- 0
  wk.cohort <- as.numeric(rownames(wk.cohort.mat))
  for (wk.i in 1:dim(wk.cohort.mat)[1]) 
  {
    if (wk.ptype %in% c("b","c")) lines(wk.cohort.mat.yrs[wk.i,], log(wk.cohort.mat[wk.i,]), col = "darkblue")
    if (!is.null(wk.ages) && !any(is.na(wk.cohort.mat[wk.i,paste(wk.ages)]))) 
    {
      wk.grad[wk.i] <- coef(lm(log(wk.cohort.mat[wk.i,paste(wk.ages)]) ~ wk.ages))[2]
    }
  }
  
  if (!is.null(wk.ages) && wk.ptype %in% c("b","g")) 
  {
    if (wk.ptype == "b") windows()
    plot(wk.cohort, -wk.grad, bty = "l", ylim = c(0, max(-wk.grad, na.rm = T)), 
         xlim = range(wk.cohort[!is.na(wk.grad)]), ylab = "negative gradient", xlab = "cohort", 
         main = paste(wk.main,"\nAges ", wk.ages[1]," to ",rev(wk.ages)[1], sep=""), las = 1)
    lines(wk.cohort, -wk.grad, lwd=2)
  }
}

####################################################################

# Wrapper function for plot.catch.curve

plot.catch.curve.and.grads <- function(wk.x, wk.yrs = NULL, wk.ptype = "b", wk.ages = 2:4, wk.main = NULL, wk.group = NULL) 
{
  if (is.FLStock(wk.x)) 
  {
    plot.catch.curves(wk.x@catch.n@.Data[,,1,1,1,1], wk.yrs = wk.yrs, 
                      wk.ptype = wk.ptype, wk.main = ifelse(is.null(wk.main),"",wk.main), wk.ages = wk.ages)
  }
  if (is(wk.x,"FLIndices"))   #replace is.FLindices
  {
    if (is.null(wk.group)) wk.group <- 1:length(wk.x)
    if (is.null(wk.main)) wk.main <- rep("",length(unique(wk.group)))
    for (wk.i in unique(wk.group)) 
    {
      wk.avail.ages <- wk.nages <- dims(wk.x[wk.group == wk.i][[1]])$min:dims(wk.x[wk.group == wk.i][[1]])$max
      if (!is.na(wk.x[wk.group == wk.i][[1]]@range["plusgroup"])) wk.avail.ages <- wk.avail.ages[-length(wk.avail.ages)]
      wk.ages <- wk.ages[wk.ages %in% wk.avail.ages]
      if (length(wk.ages) < 3) wk.ages <- NULL
      wk.data <- matrix(numeric(0), nrow=length(wk.nages))
      for (wk.j in 1:sum(wk.group == wk.i)) wk.data <- cbind(wk.data, wk.x[wk.group == wk.i][[wk.j]]@catch.n@.Data[,,1,1,1,1])
      plot.catch.curves(wk.data, wk.yrs = wk.yrs, wk.ptype = wk.ptype, wk.main = wk.main[wk.i], wk.ages = wk.ages)
    }
  }
}


