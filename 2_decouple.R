################PLOT 5cm vs 20cm soil moisture in RAAM###############
require("graphics");library(RColorBrewer);library(scales);library(plyr);library(ecp)
library(lubridate);library(gdata)

setwd("/media/coleen/DDrive/1_OWASIS/4_Papers/traffic/data/ts_select_couple")
rm_all_daily<-list.files(pattern = "_day.csv")

rm_all.list<-lapply(rm_all_daily,function(x) read.csv(x, header=T,stringsAsFactors = F)  )
rm.names<-substr(rm_all_daily,1,4)
rm_all.list<-setNames(rm_all.list,rm.names)
rm_all.list<-lapply(rm_all.list,function(x) {x[, "date"] <- as.Date(x[, "date"],
                       format = "%Y-%m-%d ",tz = "Europe/Amsterdam");x})
rm_all.list<-lapply(rm_all.list,function(x) {x$mon <- month(x[,"date"]);x})

###TWENTE has difference format for separate the list
tw.vwc.list<-rm_all.list[c(7:9)]
rm_all.list<-rm_all.list[c(1:6)]
##subset each dataframe for only 5,10,20 cm SM#######
rm.vwc.list<-lapply(rm_all.list, function(x) x[,c(1:6,19,13)])
tw.vwc.list<-lapply(tw.vwc.list, function(x) x[,c(1:2,4,6,8,10,12)])

##-------------------------------ADD KNMI rainfall to Twente stations-----------------------------------------------------
##TUBBERGEN 361 -ITC02
temp <- tempfile()
download.file("http://cdn.knmi.nl/knmi/map/page/klimatologie/gegevens/monv_reeksen/neerslaggeg_TUBBERGEN_361.zip",temp,mode="wb")
Tubb<- read.table(unz(temp,"neerslaggeg_TUBBERGEN_361.txt"), sep=",",skip=23, header=T)[ ,1:3]
head(Tubb)
Tubb$RDmm <-Tubb$RD/10
Tubb$YYYYMMDD<- strptime(Tubb$YYYYMMDD, "%Y%m%d")
Tubb$date <- as.Date(Tubb$YYYYMMDD,format = "%Y-%m-%d")
Tubb<-Tubb[match(tw.vwc.list$TW02$date,Tubb$date),]

##DENEKAMP -331 -ITC07
download.file("http://cdn.knmi.nl/knmi/map/page/klimatologie/gegevens/monv_reeksen/neerslaggeg_DENEKAMP_331.zip",temp,mode="wb")
Denekamp<- read.table(unz(temp,"neerslaggeg_DENEKAMP_331.txt"), sep=",",skip=23, header=T)[ ,1:3]
head(Denekamp)
Denekamp$RDmm <-Denekamp$RD/10
Denekamp$YYYYMMDD<- strptime(Denekamp$YYYYMMDD, "%Y%m%d")
Denekamp$date <- as.Date(Denekamp$YYYYMMDD,format = "%Y-%m-%d")
Denekamp<-Denekamp[match(tw.vwc.list$TW07$date,Denekamp$date),]


##MARKELO - ITC10
download.file("http://cdn.knmi.nl/knmi/map/page/klimatologie/gegevens/monv_reeksen/neerslaggeg_MARKELO_682.zip",temp,mode="wb")
Markelo<- read.table(unz(temp,"neerslaggeg_MARKELO_682.txt"), sep=",",skip=23, header=T)[ ,1:3]
head(Markelo)
Markelo$RDmm <-Markelo$RD/10
Markelo$YYYYMMDD<- strptime(Markelo$YYYYMMDD, "%Y%m%d")
Markelo$date <- as.Date(Markelo$YYYYMMDD,format = "%Y-%m-%d %H:%M:%S")
Markelo<-Markelo[match(tw.vwc.list$TW10$date,Markelo$date),]

tw.rd.list<-list(Tubb,Denekamp,Markelo)


for(i in seq_along(tw.vwc.list)){
  
  tw.vwc.list[[i]]$RDmm<-tw.rd.list[[i]]$RDmm
}


####Merge two lists to create groups based on sandy soil only!!!
##ITC10 recently changed position. Not confident on the continuity of trends there
##ITC07 is from different soil type..
##add only ITC02 to stations in the Raam
sandy.vwc.list<-do.call(c, list(rm.vwc.list,tw.vwc.list[1]))


#pdf(paste("/media/coleen/DDrive1/1_OWASIS/2_Statistical_analysis/penlog/figures/plots_twrm_5cm_vs_depths_SM.pdf"),
#   height=7,width =11)
##10cm##
plot(sandy.vwc.list$sandy01$VWC_5cm,sandy.vwc.list$sandy01$VWC_10cm, main = "5cm vs 10cm",
     xlim = c(0,0.5), ylim = c(0, 0.65), pch = 21, bg="#CAE1FF", cex = 1.25)
lapply(sandy.vwc.list, function(x) points(x$VWC_5cm, x$VWC_10cm,pch = 21, bg="#CAE1FF", cex = 1.25))
##20cm##
plot(sandy.vwc.list$sandy01$VWC_5cm,sandy.vwc.list$sandy01$VWC_20cm, main = "5cm vs 20cm",
     xlim = c(0,0.5), ylim = c(0, 0.65), pch = 21, bg ="#4F94CD", cex = 1.25)
lapply(sandy.vwc.list, function(x) points(x$VWC_5cm, x$VWC_20cm,pch = 21, bg = "#4F94CD", cex = 1.25))
##40cm##
plot(sandy.vwc.list$sandy01$VWC_5cm,sandy.vwc.list$sandy01$VWC_40cm, main = "5cm vs 40cm",
     xlim = c(0,0.5), ylim = c(0, 0.65), pch = 21, bg ="#4169E1", cex = 1.25)
lapply(sandy.vwc.list, function(x) points(x$VWC_5cm, x$VWC_40cm,pch = 21, bg = "#4169E1", cex = 1.25))
##80cm##
plot(sandy.vwc.list$sandy01$VWC_5cm,sandy.vwc.list$sandy01$VWC_80cm, main = "5cm vs 80cm",
     xlim = c(0,0.5), ylim = c(0, 0.65), pch = 21, bg ="#27408B", cex = 1.25)
lapply(sandy.vwc.list, function(x) points(x$VWC_5cm, x$VWC_80cm,pch = 21, bg = "#27408B", cex = 1.25))
#dev.off()


###conduct residuals analysis for 5cm vs 20cm SM####
#add column for station in eacf df --USE MAP
Station <-as.list(as.numeric(substr(names(sandy.vwc.list),3,4)))
sandy.vwc.list<-Map(cbind, sandy.vwc.list, Station = Station)
     
##combine list into one dataframe
sandy.vwc.df<-do.call(rbind,sandy.vwc.list)
sandy.vwc.df<-sandy.vwc.df[sandy.vwc.df$VWC_5cm<=0.6,]
sandy.vwc.df<-sandy.vwc.df[complete.cases(sandy.vwc.df$VWC_5cm),]
sandy.vwc.df<-sandy.vwc.df[complete.cases(sandy.vwc.df$VWC_20cm),]

#sandy.vwc.df<-sandy.vwc.df[order(sandy.vwc.df$RDmm),] 

#reg fit
lo<- loess(sandy.vwc.df$VWC_20cm ~ sandy.vwc.df$VWC_5cm, span=0.75,degree=2,se=TRUE)### span can be changed for smoothness
lm<-lm(sandy.vwc.df$VWC_20cm ~ sandy.vwc.df$VWC_5cm)

#exporting
#pdf(paste("/media/coleen/DDrive/1_OWASIS/4_Papers/traffic/figures/reg_residuals_grey1_big.pdf", sep = ""),
#height=5,width=11)
colB<-palette(colorRampPalette(brewer.pal(11,"Spectral"))(12))
par(oma = c(1,2.5,1,3))
layout(matrix(1:2,ncol=2,byrow =FALSE), widths = c(4,7),height = c(1,1))
##Scatterplot-------------------------
par(mar = c(3,1,2,0.7),mgp = c(1.7,0.6, 0), tck = -0.010)
plot(c(0,0.55), c(0.05,0.45), type="n",  cex.main= 1.5,ylab = "",xlab = "",cex.axis=1.4, main= "Soil moisture at stations")

#SM points -grey color
points(sandy.vwc.df$VWC_20cm ~ sandy.vwc.df$VWC_5cm,pch=21, bg=alpha("grey38",0.35),
       col=alpha("grey38",0.25),cex=0.25, lwd=0.25)

#fit loess line
j <- order(lo$x)
lines(lo$x[j],lo$fitted[j], lwd=4, col = "grey30")

#cond mean +var+ bars
mean.agg<-aggregate(sandy.vwc.df$VWC_20cm,by=list(round(sandy.vwc.df$VWC_5cm,2)),mean,na.rm=TRUE)
var.agg<-aggregate(sandy.vwc.df$VWC_20cm,by=list(round(sandy.vwc.df$VWC_5cm,2)),var,na.rm=TRUE)
sd.agg<-aggregate(sandy.vwc.df$VWC_20cm,by=list(round(sandy.vwc.df$VWC_5cm,2)),sd,na.rm=TRUE)
sd.plus<- mean.agg$x + sd.agg$x
sd.minus<-mean.agg$x - sd.agg$x
#lm fit line
#lines(lm$model$`sandy.vwc.df$VWC_5cm`,lm$fitted.values,col = "red3", lwd = 4, lty=3)


#correlation grouped VWC values
x<-data.frame(cbind(sandy.vwc.df$VWC_5cm,sandy.vwc.df$VWC_20cm))
x$x<-round(x$X1/.05)*.05 #round(x$X1,2)
y<-ddply(x, "x", summarize, corr=cor(X1, X2,method = "spearman",use="pairwise.complete.obs"))#use="pairwise.complete.obs"
frequ<-data.frame(count(round(sandy.vwc.df$VWC_5cm/0.05)*0.05))
corr.list<-merge(frequ,y, by="x")

arrows(mean.agg$Group.1, sd.minus, mean.agg$Group.1, sd.plus,lwd=1.2,col=alpha("black",0.8),
       length=0.05, angle=90, code=3)
points(mean.agg, col=alpha("black",0.8),  cex = 1,pch =15)


#legend
#legend(x=0.3,y=0.12,legend=month.abb[1:12], y.intersp = 0.75, x.intersp = 0.45,bty = "n",
  #     pt.cex = 1.25, cex=1, col = alpha("grey38",0.25), pt.bg =alpha(colB,0.6), pch=21, ncol=3) ## legend -  month.abb[1:12] OR  paste("sandy", 1:15, sep = " ")
legend(x=0.28,y=0.14,legend = c("loess fit","conditional\n mean +/- sd"),lty =c(1,1),cex=1,
       lwd=c(3,1), seg.len=1.5,x.intersp = 0.75,y.intersp = 0.8,bty = "n", col=c("black",alpha("black",0.7)))
#"linear fit", 
#"red3",

#add text
cor<-round(cor(sandy.vwc.df$VWC_5cm,sandy.vwc.df$VWC_40cm, method = "spearman",use="pairwise.complete.obs"),digits=3)
sigma.lm<-round(sqrt(sum(lm$residuals^2)/lm$df.residual),3)
sigma.lo<-round(sqrt(sum(lo$residuals^2)/lo$n),3)
#text(0.05,0.50,labels = bquote('R'[s]==.(cor)), cex = 1, col = "grey38")
#text(0.09,0.49,labels = bquote('RSE'[italic(lm)]==.(sigma.lm)), cex = 1.35, col = "grey38")
#text(0.09,0.46,labels = bquote('RSE'[italic(lo)]==.(sigma.lo)), cex = 1.35, col = "grey38")
#lm_coef <- round(coef(lm), 3) # extract coefficients lm
#text(0.07,0.375,labels=bquote(y(lm) == .(lm_coef[2])*x + .(lm_coef[1])), cex=1.25)


###Function for rounding to different sequences##
mround <- function(x,base){
  base*round(x/base)
} 
#dev.off()

###Rsiduals plot-----------------------------------------------------
#LORES
#resid lm and lo fit
lm.res<-resid(lm)
lo.res<-resid(lo)
count<-count(mround(sandy.vwc.df$VWC_5cm,0.01))
#cond mean +var+ bars
mean.lores<-aggregate(lo.res,by=list(mround(sandy.vwc.df$VWC_5cm,0.01)),mean,na.rm=TRUE)
#var
var.lores<-aggregate(lo.res,by=list(mround(sandy.vwc.df$VWC_5cm,0.01)),var,na.rm=TRUE)
var.lores[is.na(var.lores)]<-0
var.lores$cum<-cumsum(var.lores$x)
x<-as.matrix(var.lores$cum)
infl<-e.divisive(diff(x), sig.lvl=.01,k=NULL,min.size=2,alpha = 1, R=199)
#sd
sd.lores<-aggregate(lo.res,by=list(mround(sandy.vwc.df$VWC_5cm,0.01)),sd,na.rm=TRUE)
sd.lores.plus<- mean.agg$x + sd.agg$x
sd.lores.minus<-mean.agg$x - sd.agg$x


#######PLOT RESIDUALS LOES######
par(mar = c(3,3.2,2,1),mgp = c(1.7,0.6, 0), tck = -0.010)
plot(sandy.vwc.df$VWC_5cm,lo.res, main = bquote("Loess fit residuals"),pch=21, bg=alpha("grey38",0.35),
     col=alpha("grey38",0.25), ylab="Residuals", xlab="",cex.main= 1.5,cex.lab = 1.75,cex=0.25,
     cex.axis=1.4, lwd=0.25, ylim=c(-0.2, 0.2), xlim = c(0, max(sandy.vwc.df$VWC_5cm)))
abline(0,0)
par(new = T)
plot(var.lores$Group.1,var.lores$cum,type="l",lwd=4,cex.axis=1.4,col="grey30",yaxt="n",xaxt="n",xlab="",ylab="",xlim = c(0, max(sandy.vwc.df$VWC_5cm)), 
     ylim = c(0, max(var.lores$cum)))
abline(0,0, lwd = 0.5)
points(var.lores$Group.1,var.lores$cum,pch=18, cex=0.5,col="grey20")
lines(var.lores$Group.1,var.lores$x, type = "h", lwd=3, col = "grey50")
#add ecp estimates
inf<-infl$estimates
inf<-inf[-c(1, length(inf))]
abline(v =var.lores$Group.1[c(inf)], col = "black", lty =2, lwd =1)
axis(side=4, cex.axis = 1.4)

inf.list<-var.lores$Group.1[c(inf)]


mtext(expression("Soil Moisture at 5 cm"~(cm^{3}*cm^{-3})), side = 1, outer = TRUE, cex=1.7, line=-0.5)
mtext(expression("Soil Moisture at 20 cm"~(cm^{3}*cm^{-3})), side = 2, outer = TRUE, cex=1.5, line=0.5)
mtext(side = 4, line = 1.2, 'Cumulative Residual Variance',cex= 1.75,outer = TRUE)
#dev.off()


##-------------------------------------PLOT AS TIME SERIES - separate per year---------------------------------
#Create list of xts from list of data frame

#XTS SERIES - ORIGINAL DATA
require(xts)
library(zoo)
xts.list<-lapply(sandy.vwc.list,function(x) xts(x[,-1],x[,1]))

##PLOTTING XTS TIME SERIES########
#pdf(paste("/media/coleen/DDrive1/1_OWASIS/4_Papers/ts_dnlm/figures/station_ts_rev_v2.pdf"),
#   height=11 ,width=13, onefile = TRUE) 
par(mfrow= c(7,1),mar = c(0,0,0,0) + 0.01,mgp=c(1.5,0.55,0),oma = c(3.75,3.75,1,3.75), tck=-0.01)
for(i in 1:7){
  myColors <- c("skyblue1", "dodgerblue4", "grey20", "darkblue", "darkviolet")
  #Plot rainfall first so Soil moisture TS is more visible
  plot(as.zoo(xts.list[[i]][,7]),type="h",lwd=1.5,  col="grey30", yaxt="n",xlab="",ylab="", xaxt = "n",
       xlim= c(as.Date('2016-01-01', format="%Y-%m-%d"),as.Date('2018-01-01', format="%Y-%m-%d")),
      ylim = rev(c(0,80)))
  axis(side=4,mgp=c(0.7,0.5,0))
  par(new = T)
  plot(as.zoo(xts.list[[i]][,c(1,3)]),yaxt="n",xlab="",ylab="", xaxt = "n",
       xlim=c(as.Date('2016-01-01', format="%Y-%m-%d"),
              as.Date('2018-01-01', format="%Y-%m-%d")),cex.lab= 1.7,
       col = myColors, lwd=2,screens=1, ylim = c(0,0.60)) 
  text(as.Date('2017-12-28', format="%Y-%m-%d"),0.02, labels = names(xts.list[i]),cex=2,font =2)
  axis(side=2,at=seq(0,0.6,by=0.1), labels = c("",0.1,"",0.3,"",0.5,""),mgp=c(1.5,0.55,0))
  if (i %in% 7)
    axis.Date(1, at = seq(as.Date('2016-01-01', format="%Y-%m-%d"), 
                          as.Date('2018-01-01', format="%Y-%m-%d"), by = "years"), cex.axis= 1.8)
}
mtext(expression("Soil Moisture" ~ (cm^3~cm^{-3})), side=2, line=1.35,outer = TRUE,cex = 1.3)
mtext(side = 4, line=2.25, 'Rainfall (mm)', outer = TRUE,cex  = 1.35)
mtext(side = 1, line = 2, "Time", outer = TRUE,cex  = 1.35)
legend(x="bottomleft",legend = c("5cm","40cm", "rainfall"), y.intersp = 0.75, x.intersp = 0.25,bty = "n",
       seg.len=2, lwd = 3,lty = 1, col = myColors,cex=2)
#dev.off()





