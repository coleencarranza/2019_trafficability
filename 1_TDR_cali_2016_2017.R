#################TDR calibration@###########################
require(lubridate)
library(grDevices)
library(scales)
tdr16.cali<-read.csv("/media/coleen/DDrive/1_OWASIS/4_Papers/traffic/data/TDR_Cali/TDR_cali_R.csv",
                     header =T, stringsAsFactors = FALSE)
tdr17.cali<-read.csv("/media/coleen/DDrive/1_OWASIS/4_Papers/traffic/data/TDR_Cali/TDR_cali_2017.csv",
                     header =T, stringsAsFactors = FALSE)
tdr17lab.cali<-read.csv("/media/coleen/DDrive/1_OWASIS/4_Papers/traffic/data/TDR_Cali/TDR_cali_2017lab.csv",
                        header =T, stringsAsFactors = FALSE)
tdr17lab.cali<-tdr17lab.cali[,1:13]


tdr.cali<-rbind(tdr16.cali,tdr17.cali,tdr17lab.cali)
tdr.cali$Date<-as.Date(tdr.cali$Date, format = "%Y-%m-%d")
tdr.cali$Year<-year(tdr.cali$Date)

tdr.cali<-data.frame(tdr.cali)
tdr.cali$water<-tdr.cali$Wt.wet-tdr.cali$Wt.dry
tdr.cali$grav<-tdr.cali$water/(tdr.cali$Wt.dry-tdr.cali$Ring.wt)
tdr.cali$Bd<- (tdr.cali$Wt.dry-tdr.cali$Ring.wt)/100 ## 100cc rings volume
tdr.cali$theta<-tdr.cali$Bd*tdr.cali$grav
tdr.cali$theta_pct<-tdr.cali$theta*100

hist(tdr.cali$theta_pct)

####Cali with GRAV
tdr.cali$TDR_cor<-tdr.cali$TDR-(12.12*tdr.cali$Bd-17.05)### TRIME correction for different dry densities
tdr.cali<-tdr.cali[tdr.cali$TDR_cor>=0,]
tdr.cali$TDR_cor<-tdr.cali$TDR_cor/100
tdr.lm<-lm(tdr.cali$theta~tdr.cali$TDR_cor)

##--------------Accuracy Metrics - RMSE and Bias------------------------
tdr.cali$theta_pred<-(tdr.cali$theta*coefficients(tdr.lm)[2]) +coefficients(tdr.lm)[1]

library(SimDesign)
tdr.rmse<-round(sqrt(mean((tdr.cali$theta- tdr.cali$theta_pred)^2)),4)
tdr.bias<-round(bias(tdr.cali$theta,tdr.cali$theta_pred, type = "bias"),3)
# OR mean(tdr.cali$theta - tdr.cali$theta_pred)


##--------------------------------------------PLOT-----------------------
pdf(paste("/media/coleen/DDrive/1_OWASIS/2_Statistical_analysis/tdr_trime_cali_big.pdf", sep = ""),
height=5,width=5)
par(mfrow = c(1,1), mar = c(4,3,2.5,1) + 0.0,mgp=c(1.7,0.35,0), tck = -0.005)
yearcol<-c("sandybrown","steelblue3")
plot(tdr.cali$TDR_cor,tdr.cali$theta, pch=21,bg=alpha("grey55",0.7),col=alpha("black",0.5), cex = 1.75,xlim = c(0,0.50), ylim = c(0,0.50),
     xlab ="TRIME TDR", ylab ="undistrubed soil samples",cex.axis = 1.5, cex.lab = 1.5,cex.main = 1.75
     ,main="Volumetric soil moisture")
title(sub = expression("(all values in"~cm^3~cm^{-3}~")"), line=2.9, cex.sub = 0.85, col.sub = "grey38")
lines(x = c(-0.10,0.60), y = c(-0.10,0.60), col = "grey40", lty = 5, lwd =0.5)
#lm fit line
lines(tdr.lm$model$`tdr.cali$TDR_cor`,tdr.lm$fitted.values,col = "black", lwd = 6, lty=1)

#Texts
text(0.35,0.16,labels=paste0('y = ', round(coef(tdr.lm)[[2]],3), 'x', ' + ', round(coef(tdr.lm)[[1]],3)),cex = 1.75)
R2<-round(summary(tdr.lm)$adj.r.squared,3)
text(0.35,0.12,labels=bquote(R^2 == .(round(R2,3))) ,cex = 1.75)

text(0.35,0.05,labels=paste0("RMSE = ",tdr.rmse) ,cex = 1.35)
text(0.35,0.02,labels=paste0("bias = ",tdr.bias) ,cex = 1.35)
dev.off()




