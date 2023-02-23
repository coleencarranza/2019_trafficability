##RUN first 4_KDE_traffic_crop_grass_v3.R

##COMBINED PLOT FOR Cultivated and Grass Fields----------
con.col<-palette(colorRampPalette(brewer.pal(9,"OrRd"))(7))
ramp.col.br<-palette(colorRampPalette(brewer.pal(9,"YlOrBr"))(50))
ramp.col.gr<-palette(colorRampPalette(brewer.pal(9,"Greens"))(50))

cont.lev<-seq(1,12, by=1.5)
cont.lev2<-seq(1,10,by=0.5)


#pdf(paste("/media/coleen/DDrive/1_OWASIS/4_Papers/traffic/figures/kde2d_v7_big1.pdf", sep = ""),height = 7,width=11)
#oma = c(2,0,0,0)
layout(matrix(c(1,1,1,1,2,2,2,2,
                3,4,5,6,7,8,9,10,
                11,4,12,6,13,8,14,10), nrow=3, byrow=TRUE), heights  = c(7,1.5,1.5))
#, oma = c(3,3,1,1), mgp = c(1.5,0.5,0), tck = -0.01)
#KDE PLOTS
par(mar= c(5,4,3,1),mgp = c(1.95,0.75,0), tck = -0.01, lwd = 0.05)
plot(crop.kde, display = "image", col = alpha(ramp.col.br,0.7), ylim = c(0,3.75), xlim = c(0.0,0.55), xlab = "", ylab = "",cex.axis= 1.5,
     oldstyle = TRUE, useRaster = TRUE) 
title(main = "Cultivated", line=0.5)
points(crop.data$tdr.cali,crop.data$CI_20, pch=21, col=alpha("black",0.5),bg= alpha("peru", 0.75),cex=0.75,lwd = 0.01)
contour(x=crop.kde$eval.points[[1]],y=crop.kde$eval.points[[2]], z=crop.kde$estimate,col = "black",lwd = 1.5,levels = cont.lev,drawlabels = TRUE,
        labcex=0.75,  add=TRUE)
points(dens.samp.crop$tdr.cali, dens.samp.crop$CI_20, pch=20,cex=0.5)

plot(gras.kde, display = "image",col = alpha(ramp.col.gr,0.55),ylim = c(0,3.75), xlim = c(0.0,0.55),xlab = "", ylab = "", yaxt = "n",cex.axis=1.5,
     oldstyle = TRUE, useRaster = TRUE) 
title("Grass", line = 0.5)
points(gr.data$tdr,gr.data$CI_20,pch=21, col=alpha("black",0.5),bg=  alpha("green4", 0.9),cex=0.75,lwd = 0.01)
contour(x=gras.kde$eval.points[[1]],y=gras.kde$eval.points[[2]], z=gras.kde$estimate ,col = "black",lwd = 1.25,
        labcex = 1,add=TRUE)
points(dens.samp.gras$tdr.cali, dens.samp.gras$CI_20,  pch=20,cex=0.5)
mtext(side =1, expression("Soil moisture at 5 cm "~(cm^3~cm^{-3})), outer = TRUE, line=-18, cex = 1.25)
mtext(side=2, "Ave. Cone Index (CI) over 20 cm (MPa)", outer = TRUE, line=-1.7,cex = 1.25, adj = 0.79)




#Plot cdf
#HISTOGRAMS AND CDFS
#CRop TDR
par(mar = c(3,4,0,0), mgp = c(1.5,0.25,0), tck = -0.03, lwd = 0.05)
hist(crop.sub$tdr.cali,xlim = c(0,0.55), probability  = TRUE, main = "", col = alpha("peru", 0.5),xlab = "", ylim = c(0,8),
     cex.axis = 1, cex.lab=1.5, xaxt = "n", lwd = 0.1)
par(mar= c(3,3,0,0))
plot(ecdf(crop.sub$tdr.cali), xlim = c(0,0.55), col = "peru", pch =19, main = "", cex.axis = 1, cex.lab=1.5, bty = "n",ylim = c(0,1),
     xlab = "",ylab= "Cumulative Density")
lines(ecdf(dens.samp.crop$tdr.cali), col = "grey38", cex = 0.2)

#CROP CI
par(mar = c(3,4,0,0), mgp = c(1.5,0.25,0), tck = -0.03)
hist(crop.sub$CI_20,xlim = c(0,2.5),probability = TRUE, col = alpha("peru", 0.5), main = "",xaxt= "n",
     cex.axis = 1, cex.lab=1.5, xlab = "",ylab= "",lwd = 0.1)
par(mar= c(3,2,0,1))
plot(ecdf(crop.sub$CI_20), xlim = c(0,5),col = "peru", pch =19, main = "", ylab = "", yaxt = "n",cex.axis = 1, cex.lab=1,bty = "n",
     xlab= "")
axis(side=2,labels=F) 
lines(ecdf(dens.samp.crop$CI_20), yaxt = "n", col = "grey38", cex = 0.2)

#PLot labels here now!
mtext(side =1, expression("Soil moisture at 5 cm "~(cm^3~cm^{-3})), outer = TRUE, line=-1.2, cex = 0.8, adj = 0.07)
mtext(side =1, "Ave. Cone Index (CI) over 20 cm (MPa)", outer = TRUE, line=-1.3, cex = 0.8, adj = 0.37)

#GRAS TDR
par(mar = c(3,4,0,0), mgp = c(1.5,0.25,0), tck = -0.03)
hist(gras.sub$tdr.cali, breaks=5,xlim = c(0,0.55), probability = TRUE, main = "", col = alpha("darkgreen", 0.5),ylim = c(0,8),
     cex.axis = 1, cex.lab=1.5,xaxt = "n", xlab = "",ylab = "",lwd = 0.1)

par(mar= c(3,2,0,0))
plot(ecdf(gras.sub$tdr.cali), xlim = c(0,0.55),col = "darkgreen", pch =19, main = "", ylab = "", yaxt = "n",cex.axis = 1, cex.lab=1.5,bty = "n",
     xlab = "")
axis(side=2,labels=F) 
lines(ecdf(dens.samp.gras$tdr.cali), col = "grey38", cex = 0.2)

#GRAS CI
par(mar = c(3,4,0,0), mgp = c(1.5,0.25,0), tck = -0.03)
hist(gras.sub$CI_20,xlim = c(0,5),probability = TRUE,main = "", col = alpha("darkgreen", 0.5),ylim = c(0,0.8),
     cex.axis = 1, cex.lab=1.5,xaxt = "n", xlab = "",ylab = "",lwd = 0.1)

par(mar= c(3,1,0,1))
plot(ecdf(gras.sub$CI_20), xlim = c(0,5),col = "darkgreen", pch =19, main = "", ylab = "", yaxt = "n",cex.axis = 1, cex.lab=1.5,bty = "n",
     xlab="")
axis(side=2,labels=F) 
lines(ecdf(dens.samp.gras$CI_20), col = "grey38", cex = 0.2)

#PLot labels here now!
mtext(side =1, expression("Soil moisture at 5 cm "~(cm^3~cm^{-3})), outer = TRUE, line=-1.1, cex=0.8, adj = 0.67)
mtext(side =1, "Ave. Cone Index (CI) over 20 cm (MPa)", outer = TRUE, line=-1.3, cex =0.8, adj = 0.99)


par(mar = c(3,4,0,0),  mgp = c(1.5,0.25,0), tck = -0.03)
hist(dens.samp.crop$tdr.cali, xlim = c(0,0.55), probability=TRUE, col = alpha("grey38", 0.5),main = "",
     cex.axis = 1, cex.lab=1.5, xlab = "",lwd = 0.1)
hist(dens.samp.crop$CI_20,xlim = c(0,2.5),probability = TRUE, col = alpha("grey38", 0.5), main = "",
     cex.axis = 1, cex.lab=1.5, xlab = "",ylab= "",lwd = 0.1)
hist(dens.samp.gras$tdr.cali,xlim = c(0,0.55), probability=TRUE,col = alpha("grey38", 0.5), main = "",
     cex.axis = 1, cex.lab=1.5, xlab = "",ylab= "",lwd = 0.1)
hist(dens.samp.gras$CI_20,xlim = c(0,5),probability = TRUE,col = alpha("grey38", 0.5),main = "",
     cex.axis = 1, cex.lab=1.5, xlab = "",ylab= "",lwd = 0.1)



#dev.off()




###Plot histogram all data points Raam and Twente study area
#run first 2_decouple.R
#pdf(paste("/media/coleen/DDrive1/1_OWASIS/4_Papers/traffic/figures/hist_3panel.pdf", sep = ""),height = 7,width=11)
par(mfrow = c(1,3))
hist(sandy.vwc.df$VWC_5cm, probability= TRUE,
     main = "Histogram all data points study sites \nRaam and Twente network (9 fields)", xlab =expression("Soil moisture at 5cm "~(cm^3~cm^{-3})) )
hist(dens.samp.crop$tdr.cali, xlim = c(0,0.6), probability=TRUE, col = alpha("gold3", 0.5),main = "Random samples Cultivated",
      xlab = expression("Soil moisture at 5cm "~(cm^3~cm^{-3})),ylab = "",lwd = 0.5)
hist(dens.samp.gras$tdr.cali,xlim = c(0,0.6), probability=TRUE,col = alpha("darkgreen", 0.5),main = "Random samples Grass",
     xlab = expression("Soil moisture at 5cm "~(cm^3~cm^{-3})),ylab= "",lwd = 0.5)
#dev.off()