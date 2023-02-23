##-------------------------------------------TRAFFICABILITY MAPS USING RESULTS OF S1 VALIDATION---------------------
library(raster)
#read files - S1 images RAAM area
setwd("/media/coleen/DDrive/SM_output_WGS_20141001_20180601__20181120__V2_CROP")
temp = list.files(pattern="*.tif")###Change CI_20 or CI_15
img<-lapply(temp,raster)


#------------------------------------1. Calculate field mean-----------------------------------
# ---2 CROP FIELDS---------------------
##import shapefile test_area1----------------
library(rgdal)
mais_RM07<-readOGR(dsn ="/media/coleen/DDrive/1_OWASIS/4_Papers/traffic/data/shp_wgs", layer = "BRP_sample_area_poly_Raam_RM07_mais")
mais2<-readOGR(dsn ="/media/coleen/DDrive/1_OWASIS/4_Papers/traffic/data/shp_wgs", layer = "BRP_sample_area_poly_Raam_mais2")
mais3<-readOGR(dsn ="/media/coleen/DDrive/1_OWASIS/4_Papers/traffic/data/shp_wgs", layer = "BRP_sample_area_poly_Raam_mais3")
mais4<-readOGR(dsn ="/media/coleen/DDrive/1_OWASIS/4_Papers/traffic/data/shp_wgs", layer = "BRP_sample_area_poly_Raam_mais4")

#Put in list
mais.list<-list(mais_RM07,mais2,mais3,mais4)


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
  
  img.mask.mean<-sapply(img.mask, function(x) cellStats(x, stat='mean', na.rm=TRUE))#result is a vector of values
  #Calculate probabilities from field mean
  img.mask.mean<-round(img.mask.mean,2)
  
  ##Set NA to zero because it gives an error
  img.mask.mean[is.na(img.mask.mean)]<-0
  
  #Put in dataframe with the dates, and soil moisture values
  z<-data.frame(img.mask.mean)
  #add dates from S1
  z$Date<-as.Date(substr(temp, 9,nchar(temp) -4), format = "%Y%m%d")
  fld.mean.ccdf[[j]]<-z
}



#-2 GRASS FIELDS---------------------
gras<-readOGR(dsn ="/media/coleen/DDrive/1_OWASIS/4_Papers/traffic/data/shp_wgs", layer = "BRP_sample_area_poly_Raam_grass1")
gras2<-readOGR(dsn ="/media/coleen/DDrive/1_OWASIS/4_Papers/traffic/data/shp_wgs", layer = "BRP_sample_area_poly_Raam_grass2")
gras3<-readOGR(dsn ="/media/coleen/DDrive/1_OWASIS/4_Papers/traffic/data/shp_wgs", layer = "BRP_sample_area_poly_Raam_grass3")
gras4<-readOGR(dsn ="/media/coleen/DDrive/1_OWASIS/4_Papers/traffic/data/shp_wgs", layer = "BRP_sample_area_poly_Raam_grass4")
#Put in list
gras.list<-list(gras,gras2,gras3,gras4)


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
  
  #Put in dataframe with the dates, and soil moisture values
  z<-data.frame(img.mask.mean)
  #add dates from S1
  z$Date<-as.Date(substr(temp, 9,nchar(temp) -4), format = "%Y%m%d")
  gras.fld.mean.ccdf[[j]]<-z
}

#----------------Plot  --- both Crop and grass fields results-----------------
#convert to xts to easy plotting
library(xts)

#------------------------Cultivated Probability-------------------
fld.mean.xts<-lapply(fld.mean.ccdf, function(x) xts(x[,-2],x[,2]))

#pdf(paste("/media/coleen/DDrive1/1_OWASIS/4_Papers/traffic/figures/Traffic_TS_4points.pdf", sep = ""),
# bg = "white",height=9,width=13)
par(mfrow = c(2,1),mar = c(3,2,1,1), oma = c(2,2.5,1,3), mgp = c(1.5,0.5,0), tck = -0.009)
#CUltivated Soil moisture from S1
plot(as.zoo(fld.mean.xts[[1]][,1]),cex.lab = 1.3, xlab = "", ylab ="",xaxt = "n",type = "o",pch = 19,cex=0.5,yaxt = "n",
     col ="slategray2", lwd=1.5,screens=1, ylim = c(0.05,0.42), main = "Soil moisture at Cultivated fields")
axis(side = 4, cex.axis = 1.4)
par(new = T)
plot(as.zoo(fld.mean.xts[[2]][,1]),cex.lab = 1.3, xlab = "", ylab ="",xaxt = "n",lty=1,yaxt="n",
     cex.axis = 1.4, col ="cornflowerblue", lwd=1.5,screens=1, ylim = c(0.05,0.42))
points(as.zoo(fld.mean.xts[[2]][,1]), pch =20, cex =0.25)
par(new = T)
plot(as.zoo(fld.mean.xts[[3]][,1]),cex.lab = 1.3, xlab = "", ylab ="",xaxt = "n",lty=1,yaxt="n",
     cex.axis = 1.4, col ="azure4", lwd=1.5,screens=1, ylim = c(0.05,0.42))
points(as.zoo(fld.mean.xts[[3]][,1]), pch =20, cex =0.25)
par(new = T)
plot(as.zoo(fld.mean.xts[[4]][,1]),cex.lab = 1.3, xlab = "", ylab ="",xaxt = "n",lty=1,yaxt="n",
     cex.axis = 1.4, col ="aquamarine4", lwd=1.5,screens=1, ylim = c(0.05,0.42))
points(as.zoo(fld.mean.xts[[4]][,1]), pch =20, cex =0.25)


#---------------GRassfields probability--------------------
gras.fld.mean.xts<-lapply(gras.fld.mean.ccdf, function(x) xts(x[,-2],x[,2]))

# GRassfield Soil moisture from S1
plot(as.zoo(gras.fld.mean.xts[[1]][,1]),cex.lab = 1.3, xlab = "", ylab ="",xaxt = "n",type = "b",pch = 19,cex=0.5,yaxt="n",
     cex.axis = 1.4, col ="slategray2", lwd=1.5,screens=1, ylim = c(0.05,0.42), main = "Soil moisture at Grass fields")
axis(side=4, cex.axis = 1.4)
axis(1, at=dates, labels=FALSE)
text(x=dates, y=par()$usr[3]-0.025*(par()$usr[4]-par()$usr[3]),cex = 0.9,
     labels=lab, srt=45, adj=1, xpd=TRUE)
par(new = T)
plot(as.zoo(gras.fld.mean.xts[[2]][,1]),cex.lab = 1.3, xlab = "", ylab ="",xaxt = "n",lty=1,yaxt="n",
     cex.axis = 1.4, col =alpha("cornflowerblue",0.8), lwd=1.5,screens=1, ylim = c(0.05,0.42))
points(as.zoo(gras.fld.mean.xts[[2]][,1]), pch =20, cex =0.25)
par(new = T)
plot(as.zoo(gras.fld.mean.xts[[3]][,1]),cex.lab = 1.3, xlab = "", ylab ="",xaxt = "n",lty=1,yaxt="n",
     cex.axis = 1.4, col =alpha("azure4",0.8), lwd=1.5,screens=1, ylim = c(0.05,0.42))
points(as.zoo(gras.fld.mean.xts[[3]][,1]), pch =20, cex =0.25)
par(new = T)
plot(as.zoo(gras.fld.mean.xts[[4]][,1]),cex.lab = 1.3, xlab = "", ylab ="",xaxt = "n",lty=1,yaxt="n",
     cex.axis = 1.4, col =alpha("aquamarine4",0.8), lwd=1.5,screens=1, ylim = c(0.05,0.42))
points(as.zoo(gras.fld.mean.xts[[4]][,1]), pch =20, cex =0.25)

##Add dates in the x-axis for bottom plots
dates = seq(as.Date("2016-01-01"), by = "quarter", length.out = 11)
lab <- format(dates,format="'%y-%b")
axis(1, at=dates, labels=FALSE)
text(x=dates, y=par()$usr[3]-0.025*(par()$usr[4]-par()$usr[3]),cex = 0.9,
     labels=lab, srt=45, adj=1, xpd=TRUE)

#dev.off()

#SELECTED AREA PLOT 
##MAP inset - just plot separately, not easily combined with the time series plots! grrrrr!
library(OpenStreetMap)
library(ggmap)
library(rgdal)
library(devtools)
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

#pdf(paste("/media/coleen/DDrive1/1_OWASIS/4_Papers/traffic/figures/subarea_ts_plot_traffic_4pts.pdf", sep = ""),
 #   bg = "white",height=7,width=7)
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
