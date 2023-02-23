library(ks)
library(plot3D)
library(RColorBrewer)
library(scales)
library(MASS)
library(kernelboot)

###Got ths from: 
##http://biostatmatt.com/archives/2745

## multivariate extension of Scott's bandwidth rule
Scott <- function(data)
  cov(data) * (nrow(data) ^ (-1/(ncol(data)+4)))


###----------------------------------------CULTIVATED CROPS-----------------------------------------------#####
##---------------------------------------Use traffic data -----------------------
#Load coupled crop data
crop.data<-Crop.coupled[,c(13,5)]
## compute bivariate KDE and plot contours
crop.sub <- subset(crop.data, !is.na(tdr.cali) & !is.na(CI_20),
                   select=c("tdr.cali", "CI_20"))

crop.sub.mat<-as.matrix(crop.sub, rownames.force = FALSE)
##Estimate 2D KDE
crop.kde<-kde(crop.sub.mat,H = Scott(crop.sub),positive=TRUE, xmin = 0, xmax = c(0.6,5.0),gridsize = 500)
##What do they use as default bandwidth??? amise=TRUE?
con.col<-palette(colorRampPalette(brewer.pal(9,"OrRd"))(7))
ramp.col<-palette(colorRampPalette(brewer.pal(9,"YlOrRd"))(50))

plot(crop.kde, display = "image", col = ramp.col, xlim = c(0.0,0.60)) 
contour(x=crop.kde$eval.points[[1]],y=crop.kde$eval.points[[2]], z=crop.kde$estimate , ylim = c(0,2), xlim = c(0.0,0.60),col = "black",lwd = 2,
        labcex = 1.5,xlab=expression("Soil moisture at 5cm "~(cm^3~cm^{-3})), ylab="Ave. Cone Index over 20cm (MPa)", add=TRUE)

## multivariate extension of Scott's bandwidth rule
#Az <- matrix(rnorm(n*ncol(y)), n, ncol(y)) %*% chol(bw) - from code kernelbook github
#return(Az + mu)
#https://github.com/cran/kernelboot/blob/master/R/multivar-gaussian-kd.R
#ftp://cran.r-project.org/pub/R/web/packages/kernelboot/kernelboot.pdf

Scott <- function(data) #- SAMPLING with cholesky decomposition and lower triangle 
  t(chol(cov(data))) * nrow(data) ^ (-1/(ncol(data)+4))

## 1. sample the original data with replacement
n <- 500; p <- dim(crop.sub)[2]; set.seed(1234)
dens.samp.crop <- crop.sub[sample(1:nrow(crop.sub), size=n, replace=TRUE),]
## 2. add variability by sampling from kernel

dens.samp.crop <- dens.samp.crop + matrix(rnorm(n*p), n, p) %*% Scott(crop.sub)

dens.samp.crop<-dens.samp.crop[dens.samp.crop$tdr.cali>0,]
dens.samp.crop<-dens.samp.crop[dens.samp.crop$tdr.cali<0.51,]
dens.samp.crop<-dens.samp.crop[dens.samp.crop$CI_20 >0,]

#Add to plot
points(crop.sub$tdr, crop.sub$CI, pch=20, cex = 1,xlim = c(0,0.55))
points(dens.samp.crop$tdr, dens.samp.crop$CI, pch=4,
       cex=0.4, col=gray(0.4))


##Histogram
#truehist() is from the MASS package and scales the counts to give an estimate of the probability density. 
par(mfrow=c(1,2))
hist(crop.sub$tdr.cali, xlim = c(0,0.55), probability = TRUE, main = "TDR")
lines(density(crop.sub$tdr.cali, bw=bw.nrd0(crop.sub$tdr.cali)))
#lines(density(crop.sub$tdr.cali, bw =0.000249), col = "blue")

hist(dens.samp.crop$tdr.cali, xlim = c(0,0.55), probability=TRUE)
lines(density(dens.samp.crop$tdr.cali, bw =0.0226))
#lines(density(dens.samp.crop$tdr.cali, bw =0.000249), col = "blue")


hist(crop.sub$CI_20,probability = TRUE,main = "CI")
lines(density(crop.sub$CI_20, bw =0.138))
lines(density(crop.sub$CI_20, bw = 0.0121310264), col = "blue")

hist(dens.samp.crop$CI_20,probability = TRUE)
lines(density(dens.samp.crop$CI_20, bw =0.138))
lines(density(dens.samp.crop$CI_20, bw = 0.0121310264), col = "blue")

##-----------------------------------------------------CCDF using all the points-------------------------------------------------------
#Calculate the kcde using all points
CI.grid<-(5-0)/0.001
tdr.grid<-(1.1-0)/0.001 ## arbitrary. doesnt make fine grid with smaller interval. dunno why!

library(MASS)
library(ks)
Fhat_2d.crop<-kcde(dens.samp.crop,xmin=c(0,0), xmax=c(1.1,3), gridsize=c(tdr.grid,CI.grid))
hts <- seq(0, 1.1*max(Fhat_2d.crop$estimate), length=500)
plot(Fhat_2d.crop, display = "persp", axes=TRUE,scale=TRUE, box=TRUE, col=terrain.colors(length(hts)),
     nticks=5, ticktype="detailed", xlim = c(0,0.55))


##Get Marginal CDF for tdr 
eval.crop<-Fhat_2d.crop$eval.points[[1]]
Fhat_tdr_all.crop<-kcde(dens.samp.crop$tdr.cali,eval.points = eval.crop)
plot(Fhat_tdr_all.crop, xlim = c(0,0.5))

##Get Conditional cdf --- P(CI<=07.| TDR<=y) y = any soil moisture value
#Example TDR=25


#Get joint cdf  row = tdr, col = CI
jcdf.crop<-Fhat_2d.crop$estimate
colnames(jcdf.crop)<-round(Fhat_2d.crop$eval.points[[2]],3)
rownames(jcdf.crop)<-round(Fhat_2d.crop$eval.points[[1]],3)

jcdf.7<-jcdf.crop[,as.character(0.7)]


x<-0.25 ## testing using soil moisture of 0.25
jcdf.val<-jcdf.crop[as.character(x),as.character(0.7)]
jcdf.val

#Get marginal cdf for tdr value at 25
mcdf.crop<-cbind(round(Fhat_2d.crop$eval.points[[1]],3),Fhat_tdr_all.crop$estimate)
rownames(mcdf.crop)<-round(Fhat_2d.crop$eval.points[[1]],3)
mcdf.val<-mcdf.crop[as.character(x),2]
mcdf.val

ccdf<-jcdf.val/mcdf.val
ccdf

####-----------------------------------GRASS--------------------------------------------------
##---------------------------------------Use traffic data -----------------------
## multivariate extension of Scott's bandwidth rule
Scott <- function(data)
  cov(data) * (nrow(data) ^ (-1/(ncol(data)+4)))
#Load coupled gr data
gr.data<-Grass.coupled[,c(13,5)]
## compute bivariate KDE and plot contours
gras.sub <- subset(gr.data, !is.na(tdr.cali) & !is.na(CI_20),
                   select=c("tdr.cali", "CI_20"))
gras.sub.mat<-as.matrix(gras.sub, rownames.force = FALSE)

## multivariate extension of Scott's bandwidth rule
Scott <- function(data)
  cov(data) * (nrow(data) ^ (-1/(ncol(data)+4)))


##Estimate 2D KDE
#kde function has documentation on default bandwidth used
gras.kde<-kde(gras.sub.mat,H=Scott(gras.sub),xmin = c(0,0), xmax = c(0.6,6),gridsize = 500)

plot(gras.kde, display = "image", col = ramp.col, xlim = c(0,0.6)) 
contour(x=gras.kde$eval.points[[1]],y=gras.kde$eval.points[[2]], z=gras.kde$estimate , ylim = c(0,2), xlim = c(0.10,0.6),col = "black",lwd = 2,
        labcex = 1.5,xlab=expression("Soil moisture at 5cm "~(cm^3~cm^{-3})), ylab="Ave. Cone Index over 20cm (MPa)", add=TRUE)

## multivariate extension of Scott's bandwidth rule
#Az <- matrix(rnorm(n*ncol(y)), n, ncol(y)) %*% chol(bw) - from code kernelbook github
#return(Az + mu)
#https://github.com/cran/kernelboot/blob/master/R/multivar-gaussian-kd.R
#ftp://cran.r-project.org/pub/R/web/packages/kernelboot/kernelboot.pdf

Scott <- function(data) #- SAMPLING with cholesky decomposition and lower triangle 
  t(chol(cov(data))) * nrow(data) ^ (-1/(ncol(data)+4))


## 1. sample the original data with replacement
n <- 500; p <- dim(gras.sub)[2]; set.seed(234)
dens.samp.gras <- gras.sub[sample(1:nrow(gras.sub), size=n, replace=TRUE),]
## 2. add variability by sampling from kernel

dens.samp.gras <- dens.samp.gras + matrix(rnorm(n*p), n, p) %*% Scott(gras.sub)

dens.samp.gras<-dens.samp.gras[dens.samp.gras$tdr.cali>0,]
dens.samp.gras<-dens.samp.gras[dens.samp.gras$tdr.cali<0.51,]
dens.samp.gras<-dens.samp.gras[dens.samp.gras$CI_20 >0,]

#Add to plot
points(gras.sub$tdr, gras.sub$CI, pch=20, cex = 1,xlim = c(0,0.55))
points(dens.samp.gras$tdr, dens.samp.gras$CI, pch=4,
       cex=0.4, col=gray(0.4))


##Histogram
#truehist() is from the MASS package and scales the counts to give an estimate of the probability density. 
par(mfrow=c(1,2))
hist(gras.sub$tdr.cali, xlim = c(0,0.55), probability = TRUE, main = "TDR")
lines(density(gras.sub$tdr.cali, bw =0.02575132))
lines(density(gras.sub$tdr.cali, bw =0.0005923905), col = "blue")

hist(dens.samp.gras$tdr.cali, xlim = c(0,0.55), probability=TRUE)
lines(density(dens.samp.gras$tdr.cali, bw =0.02575132))
lines(density(dens.samp.gras$tdr.cali, bw =0.0005923905), col = "blue")


hist(gras.sub$CI_20,probability = TRUE,main = "CI")
lines(density(gras.sub$CI_20, bw =0.27032923))
lines(density(gras.sub$CI_20, bw = 0.054651136), col = "blue")

hist(dens.samp.gras$CI_20,probability = TRUE)
lines(density(dens.samp.gras$CI_20, bw =0.27032923))
lines(density(dens.samp.gras$CI_20, bw = 0.054651136), col = "blue")


##---------------------------------------CCDF using all the points------------------------
#Calculate the kcde using all points
CI.grid<-(5-0)/0.001
tdr.grid<-(1.1-0)/0.001

library(MASS)
library(ks)
Fhat_2d.grass<-kcde(dens.samp.gras,xmin=c(0,0), xmax=c(1.1,3), gridsize=c(tdr.grid,CI.grid))
hts <- seq(0, 1.1*max(Fhat_2d.grass$estimate), length=500)
plot(Fhat_2d.grass, display = "persp", axes=TRUE,scale=TRUE, box=TRUE, col=terrain.colors(length(hts)),
     nticks=5, ticktype="detailed")


##Get Marginal CDF for tdr 
eval.grass<-Fhat_2d.grass$eval.points[[1]]
Fhat_tdr_all.grass<-kcde(dens.samp.gras$tdr.cali,eval.points = eval.grass)
plot(Fhat_tdr_all.grass, xlim = c(0,0.5))

##Get Conditional cdf --- P(CI<=07.| TDR<=y) y = any soil moisture value
#Example TDR=25


#Get joint cdf  row = tdr, col = CI
jcdf.grass<-Fhat_2d.grass$estimate
rownames(jcdf.grass)<-round(Fhat_2d.grass$eval.points[[1]],3)
colnames(jcdf.grass)<-round(Fhat_2d.grass$eval.points[[2]],3)


jcdf.7<-jcdf.grass[,as.character(0.7)]


x<-0.25 ## testing using soil moisture of 0.25
jcdf.val<-jcdf.grass[as.character(x),as.character(0.7)]
jcdf.val

#Get marginal cdf for tdr value at 25
mcdf.grass<-cbind(round(Fhat_2d.grass$eval.points[[1]],3),Fhat_tdr_all.grass$estimate)
rownames(mcdf.grass)<-round(Fhat_2d.grass$eval.points[[1]],3)
mcdf.val<-mcdf.grass[as.character(x),2]
mcdf.val

ccdf<-jcdf.val/mcdf.val
ccdf


