

################################## plotsurvey CPUE maps ###############################

basemap<-function (lon1, lon2, lat1, lat2, grid = FALSE, zoom = FALSE, 
                   landcolor = "springgreen3", seacolor = "white", data = gmt3) 
{
  xrange <- c(lon1, lon2)
  yrange <- c(lat1, lat2)
  aspect <- c(cos((mean(yrange) * pi)/180), 1)
  d <- c(diff(xrange), diff(yrange)) * (1 + 2 * 0) * aspect
  if (!par("new")) 
    plot.new()
  p <- par("fin") - as.vector(matrix(c(0, 1, 1, 0, 0, 1, 1, 
                                       0), nrow = 2) %*% par("mai"))
  par(pin = p)
  p <- par("pin")
  p <- d * min(p/d)
  par(pin = p)
  d <- d * 0 + ((p/min(p/d) - d)/2)/aspect
  realusr <- c(xrange, yrange) + rep(c(-1, 1), 2) * rep(d, 
                                                        c(2, 2))
  par(usr = realusr)
  rect(lon1, lat1, lon2, lat2, col = seacolor)
  if (grid) {
    axis(1, tck = 1)
    axis(2, tck = 1)
  }
  if (xrange[1] < 0) {
    par(usr = realusr + c(360, 360, 0, 0))
    polygon(data, border = landcolor, col = landcolor)
  }
  if (xrange[2] > 360) {
    par(usr = realusr - c(360, 360, 0, 0))
    polygon(data, border = landcolor, col = landcolor)
  }
  par(usr = realusr)
  polygon(data, border = landcolor, col = landcolor)
  rect(lon1, lat1, lon2, lat2, lwd = 1)
  #axis(1)
  #mtext("Longitude", side = 1, line = 3)
  #par(las = 1)
  #axis(2)
  #mtext("Latitude", side = 2, line = 3, las = 0)
  if (zoom) {
    ret <- locator(2)
    if (length(ret$x) < 2) {
      zoom <- FALSE
    }
    else {
      lon1 <- min(ret$x)
      lon2 <- max(ret$x)
      lat1 <- min(ret$y)
      lat2 <- max(ret$y)
    }
    basemap(lon1, lon2, lat1, lat2, grid, zoom, landcolor, seacolor, data)
  }
}

# xy<-function(subArea){
#   key<-c(E=-10,F=0,G=10,H=20)
#   txt<-as.character(cpue$SubArea)
#   x<-key[substr(txt,3,3)]+as.numeric(substr(txt,4,4))+.5
#   y<-(as.numeric(substr(txt,1,2))+71)/2+.25
#   cbind(x,y)
# }

xy<-function(subArea){ 
  key<-c(E=-10,F=0,G=10,H=20)
  txt<-as.character(cpue$SubArea)
  zz<-data.frame(array(NA,dim=c(length(txt),2)))
  colnames(zz) <- c("x","y")
  for(i in 1:length(txt)) { #i<-2
    if(nchar(txt[i])==4) {
      zz[i,"x"]<-key[substr(txt[i],3,3)]+as.numeric(substr(txt[i],4,4))+.5
      zz[i,"y"]<-(as.numeric(substr(txt[i],1,2))+71)/2+.25    
    }
    if(nchar(txt[i])==8) {
      zz[i,"x"]<-key[substr(txt[i],5,5)]+as.numeric(substr(txt[i],7,8))-.5
      zz[i,"y"]<-((as.numeric(substr(txt[i],1,3))*10)+71)/2+.25      
    }
  }
  zz
}

plotone<-function(age, year, quarter, lon1=-5, lon2=12, lat1=50, lat2=62, scale=gscale){
  basemap(lon1,lon2,lat1,lat2)
  idx<-which((cpue$Year==year)&(cpue$Quarter==quarter))
  if(age=='3+'){
    vec<-rowSums(cpue[idx,11:14])
  }else{
    vec<-cpue[idx,8+as.numeric(age)]
  }
  ll<-lonlat[idx,]
  points(ll,cex=sqrt(vec)*scale)
}

#######################################  plot.log.cpue  #############################

# Plot survey log catch per unit effort by age

plot.log.cpue <- function(wk.x, wk.do.legend = F, wk.ages = NULL, wk.lty = NULL, wk.col = NULL, wk.lwd = NULL) 
{
  
  wk.dim.x <- lapply(wk.x, dims)
  wk.y1 <- min(unlist(lapply(wk.dim.x, function(wk) wk$minyear)))
  wk.y2 <- max(unlist(lapply(wk.dim.x, function(wk) wk$maxyear)))
  wk.a1 <- min(unlist(lapply(wk.dim.x, function(wk) wk$min)))
  wk.a2 <- max(unlist(lapply(wk.dim.x, function(wk) wk$max)))
  
  if (is.null(wk.ages)) wk.ages <- wk.a1:wk.a2
  
  par(mfrow = c(3,2), bty = "l", mar = c(4,3,3,1), xpd = NA)
  
  for (wk.i in wk.ages)
  {
    wk.survs <- unlist(lapply(wk.x, function(wk, wk.i) wk.i %in% dims(wk)$min:dims(wk)$max, wk.i))
    wk.x.set <- wk.x[wk.survs]
    
    min.wk.x <- function(wk,wk.i) 
    {
      wk.y <- wk@index@.Data[paste(wk.i),,1,1,1,1]
      wk.y[wk.y == 0] <- NA
      min(wk.y, na.rm = T)
    }
    wk.min <- min(unlist(lapply(wk.x.set, min.wk.x ,wk.i)))
    wk.max <- max(unlist(lapply(wk.x.set, function(wk,wk.i) max(wk@index@.Data[paste(wk.i),,1,1,1,1], na.rm=T) ,wk.i)))
    wk.ylim <- log(c(wk.min, wk.max))
    
    if(is.null(wk.lty)) wk.lty <- c(1:6)
    if(is.null(wk.col)) wk.col <- c(2:7)
    if(is.null(wk.lwd)) wk.lwd <- 1
    
    plot(0, 0, type = "n", ylab = "", xlab = "", xlim = c(wk.y1,wk.y2),
         ylim = wk.ylim, main = paste("age",wk.i,sep=" "))
    if(wk.i>=4) title(xlab="Year")
    for (wk.j in 1:length(wk.x.set)) 
    {
      wk.y <- log(wk.x.set[[wk.j]]@index@.Data[paste(wk.i),,1,1,1,1])
      wk.xx <- as.numeric(names(wk.y))
      lines(wk.xx, wk.y, lwd = wk.lwd[wk.survs][wk.j], col = wk.col[wk.survs][wk.j], 
            lty = wk.lty[wk.survs][wk.j], type = "l", cex = 0.8)
    }
  }	
  
  if (wk.do.legend) 
  {
    plot(0, 0, xlab = "", ylab = "", xlim = c(0,1), ylim = c(0,1), axes = F, type = "n")
    legend(0, 1,
           legend = unlist(lapply(wk.x, function(wk) wk@name)),
           col = c(wk.col)[1:length(wk.x)], lwd = wk.lwd, 
           lty = c(wk.lty)[1:length(wk.x)], bty="n")
  }
}

####################################################################
