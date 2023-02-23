###########RUN 3a_scatter_decouple code and use outputs
###compare the CI values from Grass and Cultivated crops

##This is from the vignette of BEST package
library(BEST)


#RIAL for comparing the CI values between grass and cultivated fields
####Similar if one performs a t-test
y1<-Grass$CI_20
y2<- Crop$CI_20

### Use the default uninformative prior from Kruschke (2013)--- by setting prio=NULL
###This takes some minutes to finish
BESTout <- BESTmcmc(y1, y2, priors=NULL, parallel=TRUE)

plot(BESTout)

meanDiff <- (BESTout$mu1 - BESTout$mu2)
meanDiffGTzero <- mean(meanDiff > 0)
# try also t-test for 2 groups
t.test(y1, y2)

# meanDiff2grpsMore
plot(BESTout, compVal=1, ROPE=c(-0.1,0.1))

#sd2grps
plot(BESTout, which="sd")


summary(BESTout)
summary(BESTout, credMass=0.8, ROPEm=c(-0.1,0.1), ROPEsd=c(-0.15,0.15),
        compValeff=1) 


class(BESTout)
print(BESTout)

### code chunk number 14: ppd2grps
plotPostPred(BESTout)


#PLOT all
plotAll(BESTout,showCurve = TRUE, col = "grey", lwd = 10)##graphical paramters ignored!

#plotAll2grps
pdf(paste("/media/coleen/DDrive/1_OWASIS/4_Papers/traffic/figures/Grass_vs_Culti_BEST_v3.pdf"),
    height=7,width=11)
par(mfrow = c(2,1), mar = c(4,4,2,4), mgp = c(2,0.7,0))
    plotPost(BESTout$mu1, showCurve = TRUE, col = "grey80", cex.axis =1.25, main = "Grass", lwd = 10, xlim= c(0.6,2.0),
             xlab = expression(mu[1]))#Grass
    plotPost(BESTout$mu2, showCurve = TRUE, col = "grey80", cex.axis =1.25 ,main = "Cultivated",
             lwd = 10, xlim= c(0.6,2.0), xlab = expression(mu[2]))#Cutivated
dev.off()



###----------------RUN 2b_5_20SM or 2_decouple code to get results of residuals analysis-------------------##
###-----------------USE BEST approach to get the distribution of the mean ratio between 5cm and 20cm--------------###
#coupled range
sandy.coupled<-sandy.vwc.df[sandy.vwc.df$VWC_5cm>=0.19,]
sandy.coupled$R5_20<-sandy.coupled$VWC_5cm/sandy.coupled$VWC_20cm
y0<-sandy.coupled$R5_20


#decoupled range
sandy.decoupled<-sandy.vwc.df[sandy.vwc.df$VWC_5cm<0.19,]
sandy.decoupled$R5_20<-sandy.decoupled$VWC_5cm/sandy.decoupled$VWC_20cm
y1<-sandy.decoupled$R5_20
# Use the default uninformative prior from Kruschke (2013)- by setting prio=NULL
#This takes some minutes to finish
BESTout_y0 <- BESTmcmc(y0, priors=NULL, parallel=TRUE)
BESTout_y1 <- BESTmcmc(y1, priors=NULL, parallel=TRUE)

plotAll(BESTout_y0, compValm = 1, showCurve = TRUE)
plotAll(BESTout_y1, compValm = 1, showCurve = TRUE)


### code chunk number 15: plotAll2grps
pdf(paste("/media/coleen/DDrive/1_OWASIS/4_Papers/traffic/figures/Ratio_5_20prob_coupled2.pdf"),
    height=7,width=4)
par(mfrow = c(2,1), mar = c(5,4,2,4), mgp = c(2,0.7,0))
plotPost(BESTout_y0$mu, showCurve = TRUE, col = "grey80", cex.axis =1.25, main = "Coupled Soil Moisture Range", xlab =" ",lwd = 10)
plotPost(BESTout_y1$mu, showCurve = TRUE, col = "grey80", cex.lab = 1.25, cex.axis =1.25 ,main = "Decoupled Soil Moisture Range",
         xlab = "Mean soil moisture",lwd = 10)
dev.off()


