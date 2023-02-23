##-------------------------------------------TRAFFICABILITY MAPS USING RESULTS OF S1 VALIDATION---------------------
library(raster)
library(rgdal)
#read files - S1 images RAAM area
setwd("/media/coleen/DDrive/SM_output_WGS_20141001_20180601__20181120__V2_CROP")
temp = list.files(pattern="*.tif")###Change CI_20 or CI_15
img<-lapply(temp,raster)


#------------------------------------1. Calculate field mean-----------NO EXTRACT TO POINTS!---------------------------------------
# ---2 CROP FIELDS---------------------
##import shapefile test_area1----------------
library(rgdal)
mais_RM07<-readOGR(dsn ="/media/coleen/DDrive/1_OWASIS/4_Papers/traffic/data/shp_wgs", layer = "BRP_sample_area_poly_Raam_RM07_mais")
mais2<-readOGR(dsn ="/media/coleen/DDrive/1_OWASIS/4_Papers/traffic/data/shp_wgs", layer = "BRP_sample_area_poly_Raam_mais2")


#Put in list
mais.list<-list(mais_RM07,mais2)


##A BIG FOR LOOP to calculate probabilities for each poylgon##
fld.mean.ccdf<-list()
for(j in seq_along(mais.list)){
#CROP and Mask raster using polygon
img.cr<-lapply(img, function(x) crop(x,mais.list[[j]]))
img.mask<-lapply(img.cr, function(x) mask(x,mais.list[[j]]))

##set values to NA and divide by 10000
img.mask<-lapply(img.mask, function(x) {x[values(x) == -9999]<-NA;x})
img.mask<-lapply(img.mask, function(x) x/10000)
plot(img.mask[[55]])

img.mask.mean<-sapply(img.mask, function(x) cellStats(x, stat='mean', na.rm=TRUE))#result is a vector of value

#Calculate probabilities from field mean
img.mask.mean<-round(img.mask.mean,2)

##Set NA to zero because it gives an error
img.mask.mean[is.na(img.mask.mean)]<-0
#Recalculate propability using the jcdf and mcdf
jcdf.test<-vector()
for(i in 1:length(img.mask.mean)){
  jcdf.test[[i]]<-round(jcdf.crop[as.character(img.mask.mean[[i]]),as.character(0.7)],2)
}

#Mcdf
mcdf.test<-vector()
for(i in 1:length(img.mask.mean)){
  mcdf.test[[i]]<-round(mcdf.crop[as.character(img.mask.mean[[i]]),2],2)
}

#ccdf
ccdf.test<-jcdf.test/mcdf.test
summary(ccdf.test)
##Convert NaN to NA
ccdf.test[is.nan(ccdf.test)]<-NA
#Round ccdf 
ccdf.test<-round(ccdf.test,2)

#Put in dataframe with the dates, and soil moisture values
z<-data.frame(cbind(img.mask.mean,ccdf.test))
#add dates from S1
z$Date<-as.Date(substr(temp, 9,nchar(temp) -4), format = "%Y%m%d")
fld.mean.ccdf[[j]]<-z
}



#-2 GRASS FIELDS---------------------
gras<-readOGR(dsn ="/media/coleen/DDrive/1_OWASIS/4_Papers/traffic/data/shp_wgs", layer = "BRP_sample_area_poly_Raam_grass1")
gras2<-readOGR(dsn ="/media/coleen/DDrive/1_OWASIS/4_Papers/traffic/data/shp_wgs", layer = "BRP_sample_area_poly_Raam_grass2")

#Put in list
gras.list<-list(gras,gras2)


##A BIG FOR LOOP to calculate probabilities for each poylgon##
gras.fld.mean.ccdf<-list()
for(j in seq_along(gras.list)){
#CROP and Mask raster using polygon
img.cr<-lapply(img, function(x) crop(x,gras.list[[j]]))
img.mask<-lapply(img.cr, function(x) mask(x,gras.list[[j]]))
  
##set values to NA and divide by 10000
img.mask<-lapply(img.mask, function(x) {x[values(x) == -9999]<-NA;x})
img.mask<-lapply(img.mask, function(x) x/10000)
plot(img.mask[[55]])
  
img.mask.mean<-sapply(img.mask, function(x) cellStats(x, stat='mean', na.rm=TRUE))#result is a vector of value
  
#Calculate probabilities from field mean
img.mask.mean<-round(img.mask.mean,2)
  
##Set NA to zero because it gives an error
img.mask.mean[is.na(img.mask.mean)]<-0
#Recalculate propability using the jcdf and mcdf
jcdf.test<-vector()
for(i in 1:length(img.mask.mean)){
  jcdf.test[[i]]<-round(jcdf.grass[as.character(img.mask.mean[[i]]),as.character(0.7)],2)
}
mcdf.img.mask.mean<-vector()
for(i in 1:length(img.mask.mean)){
  mcdf.test[[i]]<-round(mcdf.grass[as.character(img.mask.mean[[i]]),2],2)
}
#ccdf
ccdf.test<-jcdf.test/mcdf.test
summary(ccdf.test)
##Convert NaN to NA
ccdf.test[is.nan(ccdf.test)]<-NA
#Round ccdf 
ccdf.test<-round(ccdf.test,2)
#Put in dataframe with the dates, and soil moisture values
z<-data.frame(cbind(img.mask.mean,ccdf.test))
#add dates from S1
z$Date<-as.Date(substr(temp, 9,nchar(temp) -4), format = "%Y%m%d")
gras.fld.mean.ccdf[[j]]<-z
}

#----------------Plot  --- both Crop and grass fields results-----------------
#convert to xts to easy plotting
library(xts)

#------------------------Cultivated Probability-------------------
fld.mean.xts<-lapply(fld.mean.ccdf, function(x) xts(x[,-3],x[,3]))

##subset only 2016-2017
fld.mean.xts<-lapply(fld.mean.xts, function(x) window(x, start = as.Date("2016-01-01"), end = as.Date("2017-12-31")))

#pdf(paste("/media/coleen/DDrive1/1_OWASIS/4_Papers/traffic/figures/Traffic_TS_4panel.pdf", sep = ""),
 #   bg = "white",height=9,width=13)

par(mfrow = c(2,2),mar = c(3,2,1,1), oma = c(2,2.5,1,3), mgp = c(1.5,0.5,0), tck = -0.009)
plot(as.zoo(fld.mean.xts[[1]][,2]),cex.lab = 1.3, xlab = "", ylab ="",xaxt = "n",type = "b",pch = 19,cex=0.5,
    cex.axis = 1.4, col ="wheat3", lwd=2.5,screens=1, ylim = c(0,0.6), main = "Cultivated Fields")
par(new = T)
plot(as.zoo(fld.mean.xts[[2]][,2]),cex.lab = 1.3, xlab = "", ylab ="",xaxt = "n",lty=2,yaxt = "n",
     cex.axis = 1.4, col ="salmon4", lwd=1.5,screens=1, ylim = c(0,0.6))
points(as.zoo(fld.mean.xts[[2]][,2]), pch =20, cex =0.75)

#CUltivated Soil moisture from S1
plot(as.zoo(fld.mean.xts[[1]][,1]),cex.lab = 1.3, xlab = "", ylab ="",xaxt = "n",type = "b",pch = 19,cex=0.5,yaxt = "n",
      col ="slategray2", lwd=2.5,screens=1, ylim = c(0,0.5), main = "Soil moisture at Cultivated fields")
axis(side = 4, cex.axis = 1.4)
par(new = T)
plot(as.zoo(fld.mean.xts[[2]][,1]),cex.lab = 1.3, xlab = "", ylab ="",xaxt = "n",lty=2,yaxt="n",
     cex.axis = 1.4, col ="dodgerblue4", lwd=1.5,screens=1, ylim = c(0,0.5))
points(as.zoo(fld.mean.xts[[2]][,1]), pch =20, cex =0.75)


#---------------GRassfields probability--------------------
gras.fld.mean.xts<-lapply(gras.fld.mean.ccdf, function(x) xts(x[,-3],x[,3]))
plot(as.zoo(gras.fld.mean.xts[[2]][,2]),cex.lab = 1.3, xlab = "", ylab ="",xaxt = "n",lty=2,
     cex.axis = 1.4, col ="darkgreen", lwd=1.5,screens=1, ylim = c(0,0.15))
points(as.zoo(gras.fld.mean.xts[[2]][,2]), pch =20, cex =0.75)
##Add dates in the x-axis for bottom plots
dates = seq(as.Date("2016-01-01"), by = "quarter", length.out = 11)
lab <- format(dates,format="'%y-%b")
axis(1, at=dates, labels=FALSE)
text(x=dates, y=par()$usr[3]-0.025*(par()$usr[4]-par()$usr[3]),cex = 0.9,
     labels=lab, srt=45, adj=1, xpd=TRUE)

par(new = T)
plot(as.zoo(gras.fld.mean.xts[[1]][,2]),cex.lab = 1.3, xlab = "", ylab ="",xaxt = "n",type = "b",pch = 19,cex=0.5,yaxt = "n",
     cex.axis = 1.4, col =alpha("darkolivegreen3",0.5), lwd=1.5,screens=1, ylim = c(0,0.15), main = "Grass fields")

# GRassfield Soil moisture from S1
plot(as.zoo(gras.fld.mean.xts[[1]][,1]),cex.lab = 1.3, xlab = "", ylab ="",xaxt = "n",type = "b",pch = 19,cex=0.5,yaxt="n",
     cex.axis = 1.4, col ="slategray2", lwd=2.5,screens=1, ylim = c(0,0.5), main = "Soil moisture at Grass fields")
axis(side=4, cex.axis = 1.4)
axis(1, at=dates, labels=FALSE)
text(x=dates, y=par()$usr[3]-0.025*(par()$usr[4]-par()$usr[3]),cex = 0.9,
     labels=lab, srt=45, adj=1, xpd=TRUE)
par(new = T)
plot(as.zoo(gras.fld.mean.xts[[2]][,1]),cex.lab = 1.3, xlab = "", ylab ="",xaxt = "n",lty=2,yaxt="n",
     cex.axis = 1.4, col =alpha("dodgerblue4",0.8), lwd=1.5,screens=1, ylim = c(0,0.5))
points(as.zoo(gras.fld.mean.xts[[2]][,1]), pch =20, cex =0.75)


mtext(side =1, "Time", outer=TRUE, cex = 1.5, line = 0.5)
mtext(side = 2, "Probability",outer=TRUE, cex = 1.5, line =0.5)
mtext(side =4, expression("Soil moisture from Sentinel-1"~(cm^3~cm^{-3})), outer = TRUE, cex = 1.5, line = 1.75)

#dev.off()







#------------------------------------------2. Resample using results of S1 Validation -150m all pixels--------------------------
#----------------------------CULTIVATED FIELDS-------------------------------------------------
#read shp files of points in selected crop fields
pt.mais_RM07<-readOGR(dsn ="/media/coleen/DDrive/1_OWASIS/4_Papers/traffic/data/shp_wgs", layer = "pt_mais_RM07")
pt.mais2<-readOGR(dsn ="/media/coleen/DDrive/1_OWASIS/4_Papers/traffic/data/shp_wgs", layer = "pt_mais2")
mais.pt.list<-list(pt.mais_RM07,pt.mais2)

#read outline polygon for cropping
crop.area<-readOGR(dsn ="/media/coleen/DDrive/1_OWASIS/4_Papers/traffic/data/shp_wgs", layer = "sample_area_poly_Raam")
raam.cr<-lapply(img,crop,crop.area)
#Set values to NA
raam.cr<-lapply(raam.cr, function(x) {x[values(x) == -9999]<-NA;x})
#Resample images to 150m
ras150 <- lapply(raam.cr, function(x) aggregate(x, fact=15, fun=mean, na.rm=TRUE))

#Check resolution with res()
#convert degrees to meters by multiplying with 111,139
res(raam.cr[[1]])*111139
res(ras150[[1]])*111139

#extract values for each image to each point
ras150.st<-stack(ras150)
ras150.pts<-lapply(mais.pt.list, function(x) extract(ras150.st,x))
ras150.pts<-lapply(ras150.pts, function(x) round(x/10000,3))

#Recalculate propability using the jcdf and mcdf
jcdf.test<-lapply(ras150.pts, function(x) as.numeric(jcdf.crop[as.character(x),as.character(0.7)]))
#jcdf.test<-lapply(jcdf.test, function(x) round(x,2))
mcdf.test<-lapply(ras150.pts, function(x) as.numeric(mcdf.crop[as.character(x),2]))
#mcdf.test<-lapply(mcdf.test, function(x) round(x,2))
ccdf
ccdf.test<-mapply("/",jcdf.test,mcdf.test,SIMPLIFY = FALSE)
#ccdf.test<-lapply(ccdf.test, function(x) 1-x)

#Put into 1 data frame
ras150.pts.df<-data.frame(t(do.call("rbind",ras150.pts)))
colnames(ras150.pts.df)<-c("mais_RM07","mais2")
ras150.pts.df$Date<-as.Date(substr(temp, 9,nchar(temp) -4), format = "%Y%m%d")
ccdf.df<-data.frame(do.call("cbind",ccdf.test))
#ccdf.df<-1-ccdf.df
colnames(ccdf.df)<-c("mais_RM07","mais2")


#---------------------------GRASSFIELDS-------------------------------------------------
#read shp files of points in selected crop fields
pt.gras<-readOGR(dsn ="/media/coleen/DDrive/1_OWASIS/4_Papers/traffic/data/shp_wgs", layer = "pt_gras")
pt.gras2<-readOGR(dsn ="/media/coleen/DDrive/1_OWASIS/4_Papers/traffic/data/shp_wgs", layer = "pt_gras2")
gras.pt.list<-list(pt.gras,pt.gras2)

##Image cropping and stacking already done in cultivated fields -- see above!

#extract values for each image to each point
gr.ras150.pts<-lapply(gras.pt.list, function(x) extract(ras150.st,x))
gr.ras150.pts<-lapply(gr.ras150.pts, function(x) round(x/10000,3))

#Recalculate propability using the jcdf and mcdf
gr.jcdf.test<-lapply(gr.ras150.pts, function(x) as.numeric(jcdf.grass[as.character(x),as.character(0.7)]))
#gr.jcdf.test<-lapply(gr.jcdf.test, function(x) round(x,2))
gr.mcdf.test<-lapply(gr.ras150.pts, function(x) as.numeric(mcdf.grass[as.character(x),2]))
#gr.mcdf.test<-lapply(gr.mcdf.test, function(x) round(x,2))
#ccdf
gr.ccdf.test<-mapply("/",gr.jcdf.test,gr.mcdf.test,SIMPLIFY = FALSE)

#Put into 1 data frame
gr.ras150.pts.df<-data.frame(t(do.call("rbind",gr.ras150.pts)))
colnames(gr.ras150.pts.df)<-c("gras","gras2")
gr.ras150.pts.df$Date<-as.Date(substr(temp, 9,nchar(temp) -4), format = "%Y%m%d")
gr.ccdf.df<-data.frame(do.call("cbind",gr.ccdf.test))
colnames(gr.ccdf.df)<-c("gras","gras2")



##---------PLOTTING-----------
##TIME SERIES PLOTS
#convert to xts easy plotting
#---------CULTIVATED---------
library(xts)
sm.ras150.xts<-xts(ras150.pts.df[,-3],ras150.pts.df[,3])
ccdf.xts<-xts(ccdf.df,ras150.pts.df[,3])

##subset only 2016-2017
sm.ras150.xts<- window(sm.ras150.xts, start = as.Date("2016-01-01"), end = as.Date("2017-12-31"))
ccdf.xts<-window(ccdf.xts, start = as.Date("2016-01-01"), end = as.Date("2017-12-31"))


pdf(paste("/media/coleen/DDrive/1_OWASIS/4_Papers/traffic/figures/Traffic_TS_4panel_res150b_16_17.pdf", sep = ""),
  bg = "white",height=9,width=13)
par(mfrow = c(4,1),mar = c(3,2,1,1), oma = c(2,2.5,1,3), mgp = c(1.5,0.9,0), tck = -0.05)
plot(as.zoo(ccdf.xts[,1]),cex.lab = 1.3, xlab = "", ylab ="",xaxt = "n",type = "b",pch = 19,cex=0.5,
     cex.axis = 1.4, col ="wheat3", lwd=2.5,screens=1, ylim = c(0,0.8), main = "Cultivated Fields")
par(new = T)
plot(as.zoo(ccdf.xts[,2]),cex.lab = 1.3, xlab = "", ylab ="",xaxt = "n",lty=2,yaxt = "n",
     cex.axis = 1.4, col ="salmon4", lwd=1.5,screens=1, ylim = c(0,0.8))
points(as.zoo(ccdf.xts[,2]), pch =20, cex =0.75)
legend("bottomleft", legend =c("C1","C2"), lty = c(1,2), bty = "n", lwd = 2, seg.len = 3,y.intersp = 1, x.intersp = 0.45,
       cex = 1.25,col = c("wheat3","salmon4"), pch = 21, pt.bg = c("wheat3","black"), pt.lwd = 0, pt.cex =1.5 )


#CUltivated Soil moisture from S1
plot(as.zoo(sm.ras150.xts[,1]),cex.lab = 1.3, xlab = "", ylab ="",xaxt = "n",type = "b",pch = 19,cex=0.5,yaxt = "n",
     col ="slategray2", lwd=2.5,screens=1, ylim = c(0,0.5), main = "Soil moisture at Cultivated fields")
axis(side = 4, cex.axis = 1.4)
par(new = T)
plot(as.zoo(sm.ras150.xts[,2]),cex.lab = 1.3, xlab = "", ylab ="",xaxt = "n",lty=2,yaxt="n",
     cex.axis = 1.4, col ="dodgerblue4", lwd=1.5,screens=1, ylim = c(0,0.5))
points(as.zoo(sm.ras150.xts[,2]), pch =20, cex =0.75)
legend("bottomleft", legend =c("C1","C2"), lty = c(1,2), bty = "n", lwd = 2, seg.len = 3,y.intersp = 1, x.intersp = 0.45,
       cex = 1.25,col = c("slategray2","dodgerblue4"), pch = 21, pt.bg = c("slategray2","black"), pt.lwd = 0, pt.cex =1.5 )




#---------GRASS---------
gr.sm.ras150.xts<-xts(gr.ras150.pts.df[,-3],gr.ras150.pts.df[,3])
gr.ccdf.xts<-xts(gr.ccdf.df,gr.ras150.pts.df[,3])

##subset only 2016-2017
gr.sm.ras150.xts<- window(gr.sm.ras150.xts, start = as.Date("2016-01-01"), end = as.Date("2017-12-31"))
gr.ccdf.xts<-window(gr.ccdf.xts, start = as.Date("2016-01-01"), end = as.Date("2017-12-31"))


plot(as.zoo(gr.ccdf.xts[,1]),cex.lab = 1.3, xlab = "", ylab ="",xaxt = "n",type = "b",pch = 19,cex=0.5,
     cex.axis = 1.4, col =alpha("darkgreen",0.85), lwd=2.5,screens=1, ylim = c(0,0.15), main = "Grass Fields")
par(new = T)
plot(as.zoo(gr.ccdf.xts[,2]),cex.lab = 1.3, xlab = "", ylab ="",xaxt = "n",lty=2,yaxt = "n",
     cex.axis = 1.4, col="darkolivegreen3", lwd=2.5,screens=1, ylim = c(0,0.15))
points(as.zoo(gr.ccdf.xts[,2]), pch =20, cex =0.75)
legend("topleft", legend =c("G1","G2"), lty = c(1,2), bty = "n", lwd = 2, seg.len = 3,y.intersp = 1, x.intersp = 0.45,
       cex = 1.25,col = c("darkgreen","darkolivegreen3"), pch = 21, pt.bg = c("darkgreen","black"), pt.lwd = 0, pt.cex =1.5 )


#grass Soil moisture from S1
plot(as.zoo(gr.sm.ras150.xts[,1]),cex.lab = 1.3, xlab = "", ylab ="",xaxt = "n",type = "b",pch = 19,cex=0.5,yaxt = "n",
     col ="slategray2", lwd=2.5,screens=1, ylim = c(0,0.5), main = "Soil moisture at Grass fields")
axis(side = 4, cex.axis = 1.4)
##Add dates in the x-axis for bottom plots
dates = seq(as.Date("2016-01-01"), by = "quarter", length.out = 11)
lab <- format(dates,format="'%y-%b")
axis(1, at=dates, labels=FALSE)
text(x=dates, y=par()$usr[3]-0.12*(par()$usr[4]-par()$usr[3]),cex = 1.2,
     labels=lab, srt=30, adj=0.5, xpd=TRUE)
par(new = T)
plot(as.zoo(gr.sm.ras150.xts[,2]),cex.lab = 1.3, xlab = "", ylab ="",xaxt = "n",lty=2,yaxt="n",
     cex.axis = 1.4, col ="dodgerblue4", lwd=1.5,screens=1, ylim = c(0,0.5))
points(as.zoo(gr.sm.ras150.xts[,2]), pch =20, cex =0.75)
legend("topleft", legend =c("G1","G2"), lty = c(1,2), bty = "n", lwd = 2, seg.len = 3,y.intersp = 1, x.intersp = 0.45,
       cex = 1.25,col = c("slategray2","dodgerblue4"), pch = 21, pt.bg = c("slategray2","black"), pt.lwd = 0, pt.cex =1.5 )




mtext(side =1, "Time", outer=TRUE, cex = 1.5, line = 0.5)
mtext(side = 2, "Probability",outer=TRUE, cex = 1.5, line =0.5)
mtext(side =4, expression("Soil moisture from Sentinel-1"~(cm^3~cm^{-3})), outer = TRUE, cex = 1.5, line = 1.75)

dev.off()



##MAP inset - just plot separately, not easily combined with the time series plots! grrrrr!
library(OpenStreetMap)
library(ggmap)
library(rgdal)
library(devtools)


#SELECTED AREA PLOT 
#read parcel outline shapefiles
parcel_bdy<-readOGR(dsn ="/media/coleen/DDrive/1_OWASIS/4_Papers/traffic/data/shp_wgs", layer = "BRP_sample_area_poly_Raam")
lat <- c(parcel_bdy@bbox[2],parcel_bdy@bbox[4])
lon <- c(parcel_bdy@bbox[1],parcel_bdy@bbox[3])
bbox <- make_bbox(lon,lat,f=0.05)
#get_map
#b <- get_map(bbox,maptype="satellite",source="google")
#ggmap(b)

area_slct <- openmap(c(lat[1],lon[1]),c(lat[2],lon[2]),type = "bing")
area_longlat <- openproj(area_slct, projection = "+proj=longlat")
area_ras<-raster(area_longlat)

#pdf(paste("/media/coleen/DDrive/1_OWASIS/4_Papers/traffic/figures/subarea_ts_plot_traffic.pdf", sep = ""),
 #bg = "white",height=7,width=7)
par(mar = c(5,5,5,5), mgp = c(4,0.5,0),las = 0)
plotRGB(area_ras,axes= TRUE, ylab = "Latitude (Degrees)",xlab = "Longitude (Degrees)", alpha = 220)

#bord<-c("lightsalmon4","red2","darkolivegreen4")[parcel_bdy$CAT_GEWASC]
#plot(parcel_bdy,add=T, border = bord, lwd = 4)# color polygon by type of parcel, grassland or cultivated

plot.mais<-lapply(mais.list, function(x) plot(x,add= TRUE, border= alpha("sienna4",0.85), lwd = 5, col = alpha("sandybrown",0.6)))
rm(plot.mais)
plot.gras<-lapply(gras.list, function(x) plot(x,add= TRUE, border= alpha("dark green",0.5), lwd = 5, col = alpha("palegreen2",0.4)))
rm(plot.gras)
#plot.mais.pt<-lapply(mais.pt.list, function(x) plot(x,add= TRUE, pch=19, col= "black"))
#rm(plot.mais.pt)
#plot.gras.pt<-lapply(gras.pt.list, function(x) plot(x,add= TRUE, pch=19, col= "black"))
#rm(plot.gras.pt)

#dev.off()
