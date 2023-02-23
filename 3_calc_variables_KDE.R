##########FOR CREATING CI vs TDR plot############3
###ACCESS all .csv files created###############
setwd("/media/coleen/DDrive/1_OWASIS/4_Papers/traffic/data/CI_TDR/2016/CI_20cm")
temp = list.files(pattern="*_20.csv")###Change CI_20 or CI_15
RM_all16 = lapply(temp, function(x) read.csv(x, header=T,stringsAsFactors = F))

setwd("/media/coleen/DDrive/1_OWASIS/4_Papers/traffic/data/CI_TDR/2017/CI_20cm")
temp = list.files(pattern="*_20.csv")###Change CI_20 or CI_15
RM_all17 = lapply(temp, function(x) read.csv(x, header=T,stringsAsFactors = F))


##rbind all into a single data frame
RM_16<-do.call("rbind", RM_all16)
RM_17<-do.call("rbind", RM_all17)


##-----------------------------------------------------------------combine------------
RM_all<-rbind(RM_16,RM_17)
RM_all<-RM_all[complete.cases(RM_all),]
RM_all<-RM_all[!duplicated(RM_all$ID),]
RM_all$mon<-as.numeric(substr(RM_all$ID,3,4))


#Convert % values to  decimals
RM_all$TDR<-RM_all$TDR/100

#calculate CALIBRATED soil moisture value y = 0.797x +0.114
RM_all$tdr.cali<-(0.797*RM_all$TDR)+0.114
RM_all<-RM_all[!duplicated(RM_all$ID),]

#Remove some erroneous values
RM_all<- RM_all[RM_all$tdr.cali>0.1140,]


#Separate grass from non-grass
RM_all.sort<-RM_all[order(RM_all$X),] 




#---------------------------------------4. SEPARATE PER CROP----------------------------------------------------
Grass<-RM_all.sort[RM_all.sort$Crop == "Grass",]
Crop<-RM_all.sort[RM_all.sort$Crop != "Grass",]
Bare<-RM_all.sort[RM_all.sort$Crop == "Bare",]
Crop$Crop<-as.factor(Crop$Crop)
Grass$Crop<-as.factor(Grass$Crop)
Bare$Crop<-as.factor(Bare$Crop)

##PLOT all points
RM_all.sort$Crop<-as.factor(RM_all.sort$Crop)
crop<-as.factor(RM_all.sort$Crop)## vector need to be as factor


##########RUN 3a_scatter_decouple for all the files##########
##Subset only above 19%SM - values that are coupled -based on 2b_plot_5_20SM...
RM.coupled<-RM_all.sort[RM_all.sort$tdr.cali>=0.19,]
Grass.coupled<-Grass[Grass$tdr.cali>=0.19,]
Crop.coupled<-Crop[Crop$tdr.cali>=0.19,]
Bare.coupled<-Bare[Bare$tdr.cali>=0.19,]


RM_all_decoup<-RM_all.sort[RM_all.sort$tdr.cali<0.19,]
Bare_decoup<-Bare[Bare$tdr.cali<0.19,]


library(RColorBrewer)
require("graphics");library(RColorBrewer);library(scales);library(plyr);library(ecp);
library(lubridate)
library(RColorBrewer)
darkcols <-palette(colorRampPalette(brewer.pal(10,"Spectral"))(7))
col.grass = darkcols[4]
col.crop = darkcols[-4]

#pdf(paste("/media/coleen/DDrive/1_OWASIS/4_Papers/traffic/figures/scatter_v2_big.pdf", sep = ""),height =4,width=5)
par(mfrow = c(1,1), mar = c(2.5,2.5,1,1) + 0.0,mgp=c(1.35,0.25,0), tck = -0.01)
plot(RM_all_decoup$tdr.cali,RM_all_decoup$CI_20,pch = 19,  col = alpha("grey38",0.5),ylim = c(0,4), xlim = c(0,0.55),cex = 0.35, ylab = "Ave. Cone Index over 20 cm (MPa)",
     xlab = expression("Soil moisture at 5 cm "~(cm^3~cm^{-3})))
points(Grass.coupled$tdr.cali,Grass.coupled$CI_20,  bg=alpha("darkgreen",0.8), pch = 21, col= alpha("black", 0.5),cex=0.8)
points(Crop.coupled$tdr.cali,Crop.coupled$CI_20,  bg=alpha("peru",0.8), pch = 21, col= alpha("black", 0.5),cex=0.8)
legend(x=0.43,y=4.2,legend=c("Grass", "Cultivated"), y.intersp = 0.85, x.intersp = 0.6,bty = "n",
        pt.cex=1, cex=0.8, col = alpha("black",0.5), pt.bg=c("darkgreen",'peru'), pch=21, ncol=1)


#dev.off()


#OLD version plot
plot(RM_all_decoup$tdr.cali,RM_all_decoup$CI_20, pch = 19,  col = alpha("grey38",0.5), xlim = c(0,0.55),cex = 0.5, ylab = "Ave. Cone Index over 20cm (MPa)",
     xlab = expression("Soil moisture at 5cm "~(cm^3~cm^{-3})))
points(RM.coupled$tdr.cali,RM.coupled$CI_20,  bg=alpha(darkcols[RM.coupled$Crop],0.7), pch = 21, 
       col= alpha("black", 0.5),cex=1.25)
legend(x=0.45,y=4.2,legend=levels(RM_all.sort$Crop), y.intersp = 0.85, x.intersp = 0.6,bty = "n",
      pt.cex=1.25, cex=1, col = alpha("black",0.5), pt.bg=alpha(darkcols,0.7), pch=21, ncol=1)

