#####---------------------------VALIDATE S1 using in situ Raam stations----------------------------------------------------
library(sp)
library(raster)
library(scales)
library(rgdal)
library(scales)
#-------------1.BRP polygons-----------------
##Read BRP to keep fields only - read already dissovled polygons
Raam.brp<-shapefile("/media/coleen/DDrive/1_OWASIS/2_Statistical_analysis/valid_S1/RAAM_BRP_all_stations_v2_dis_WGS.shp")

##separate each polygon -- earier to handle for mask
raam.field.poly<-list()
for( i in 1:length(Raam.brp)){
  raam.field.poly[[i]]<-Raam.brp[Raam.brp$FLD==i,]  
  plot(raam.field.poly[[i]])
  names(raam.field.poly)<-paste0("RM",sprintf("%02d", i))
}
field.seq<-1:15
names(raam.field.poly)<-paste0("RM",sprintf("%02d", field.seq))

#Subset only stations used for traffic study!!
traffic.sub<-paste0("RM",sprintf("%02d",c(02,07,08,11,13,15)))
raam.field.poly<-raam.field.poly[traffic.sub]

#-------------2.S1 images RAAM area----------------
setwd("/media/coleen/DDrive/SM_Raam_Sw")
temp = list.files(pattern="*.tif")
ras <- lapply(temp,raster) 

##Im running out of memory!!

##Crop and mask raster stack for every polygon in Raam
#ras.crop<-lapply(ras, function(x) crop(x,Raam.brp))
ras.crop<-ras
for(i in 1:length(ras.crop)){
  values(ras.crop[[i]])[values(ras.crop[[i]]) < 0] = NA 
}

#--------------3.Points Raam Network---------------------
Raam.net<-shapefile("/media/coleen/DDrive/1_OWASIS/2_Statistical_analysis/valid_S1/SM_Stations_BOFEK_WGS")

#Test plot to see if correct points
plot(ras.crop[[1]])
plot(Raam.net,add = TRUE)
text(Raam.net@coords[,1], Raam.net@coords[,2], Raam.net@data$Name, pos = 4)

Raam.net@data<-Raam.net@data[5]
#Raam.net.wgs<-as(Raam.net.wgs,'SpatialPoints')
Raam.net@coords<-Raam.net@coords[,1:2]##dunne why there is a 3rd column. but this eliminates it. Extract wants only 2 columns for coordinates!


#Need to have a shapefile for every point!
Raam.each.pt.list<-list()

for(i in 1:length(Raam.net)){
  Raam.each.pt.list[[i]]<-Raam.net[i,]
}

names.vec<-substr(Raam.net@data$Name,7,8)
names(Raam.each.pt.list)<-paste0("RM",names.vec)

#Subset only stations used for traffic study!!
traffic.sub<-paste0("RM",sprintf("%02d",c(02,07,08,11,13,15)))
Raam.each.pt.list<-Raam.each.pt.list[traffic.sub]

##-----------------------4.in-situ soil moisture from Raam-------------------
setwd("/media/coleen/DDrive/1_OWASIS/2_Statistical_analysis/ts_raam/rm_ts_all_daily")
RM.situ.day<-list.files(pattern = "*_day.csv")

#subset stations used for traffic study!
RM.situ.day<-RM.situ.day[c(2,7,8,11,13,15)]

RM.situ.day.list<- lapply(RM.situ.day,read.csv) 
RM.situ.day.list<-lapply(RM.situ.day.list, function(x) x[,1:2])
RM.situ.day.list<-lapply(RM.situ.day.list, function(x) {x$date <- as.Date(substr(x$date,1,10), format = "%Y-%m-%d");x})
#create time sequence
Raam.time.seq<-as.data.frame(seq(as.Date('2016-04-05'), as.Date('2018-06-01'), by = "days"))
colnames(Raam.time.seq)<-"date"
#Put all into one df 
#not very pretty code!
RM.situ.all<-merge(Raam.time.seq, RM.situ.day.list[[1]], by.x = "date", by.y = colnames(RM.situ.day.list[[1]][1]), all.x = TRUE)
for(i in 2:length(RM.situ.day.list)){
  RM.situ.all<-merge(RM.situ.all, RM.situ.day.list[[i]], by.x = "date", by.y = colnames(RM.situ.day.list[[i]][1]), all.x = TRUE)
}
#rename colnames
RM.situ.names<-substr(RM.situ.day,1,4)
colnames(RM.situ.all)<-c("date",RM.situ.names)

#Subset only points used for traffic study!
#RM.situ.all<-RM.situ.all[,c("date",traffic.sub)]



###------------------------5. Buffer polygons for resampling centered at station location----
#Read point shapefiles of polygons( created from Arcmap)
setwd("/media/coleen/DDrive/1_OWASIS/2_Statistical_analysis/valid_S1/buf/")
temp = list.files(pattern="*_WGS.shp")
shp_buf = lapply(temp, function(x) shapefile(x))
ras.crs<-proj4string(ras[[1]])


#Subset buf field only fields used for traffic study!
sub.fields<-c(02,07,08,11,13,15)
shp_buf<-lapply(shp_buf, function(x) subset(x, Field %in% sub.fields))

#Put CRS
shp_buf<-lapply(shp_buf, function(x) {proj4string(x)<-ras.crs;x})
crs(shp_buf[[1]])

##separate each polygon -- easier to handle for mask
shp.buf.poly<-list()
shp.buf.poly.list<-list()

for(i in seq_along(shp_buf)){
  buf<-shp_buf[[i]]
  for(j in 1:length(sub.fields)){
    shp.buf.poly[[j]]<-buf[j,]
    #setNames(shp.buf.poly,nam)-paste0("RM",shp.buf.poly[[j]]$Field)
    
}
  nam<-lapply(shp.buf.poly, function(x) sprintf("%02d",x$Field))
  shp.buf.poly<-setNames(shp.buf.poly,paste0("RM",nam))
  shp.buf.poly.list[[i]]<-shp.buf.poly

}


cl <- colors(distinct = TRUE)
set.seed(14758962) # to set random generator seed
mycols2 <- sample(cl, 15)

plot(shp_buf[[1]])

for(i in seq_along(shp.buf.poly.list[[1]])){
  plot(shp.buf.poly.list[[1]][[i]], col = mycols2[i], add= TRUE)
  text(shp.buf.poly.list[[1]][[i]]@bbox[1,1],  shp.buf.poly.list[[1]][[i]]@bbox[2,1], shp.buf.poly.list[[1]][[i]]$Name, pos = 4)
}


######################################################################################################################
#--------------------1.Validate with all pixels - resample to 50m,100m, and 150m-------------------------
#Calculate back Sw


##----RESAMPLING!----
ras10<-ras.crop
ras50 <- lapply(ras.crop, function(x) aggregate(x, fact=5, fun=mean, na.rm = TRUE))
#ras70 <- lapply(ras, function(x) aggregate(x, fact=7, fun=mean))
ras100 <- lapply(ras.crop, function(x) aggregate(x, fact=10, fun=mean, na.rm = TRUE))
#ras120 <- lapply(ras, function(x) aggregate(x, fact=12, fun=mean))
ras150 <- lapply(ras.crop, function(x) aggregate(x, fact=15, fun=mean, na.rm=TRUE))

##Extract values to Raam points
ras.all.pix.list<-list(ras10,ras50,ras100,ras150)
ras.all.pix.stack<-lapply(ras.all.pix.list, function(x) stack(x))
names(ras.all.pix.stack)<-c("ras10","ras50","ras100","ras150")
dates.ras.list<-lapply(ras.all.pix.stack, function(x) substr(names(x),9,nchar(names(x))))

ext.list<-list()
ext<-list()
for(i in seq_along(ras.all.pix.stack)){
   for(j in seq_along(Raam.each.pt.list)){
    ext[[j]]<-as.vector(extract(ras.all.pix.stack[[i]],Raam.each.pt.list[[j]]))##This is where extract is
   }
  ext<-setNames(ext,names(Raam.each.pt.list))
  ext.list[[i]]<-ext
}
names(ext.list)<-names(ras.all.pix.stack)

#Convert to a single dfsetNames(ext,names(Raam.each.pt.list))
ext.df<-lapply(ext.list, function(x) data.frame(do.call("cbind",x)))
for (i in seq_along(ext.df)){
  ext.df[[i]]<- ext.df[[i]]/10000
}


#Create time series 
library(zoo)
library(xts)

S1_dates<-lapply(dates.ras.list, function(x) as.Date(x,format = "%Y%m%d"))
RM.S1.xts<-list()
for(i in seq_along(ext.df)){
  RM.S1.xts[[i]]<-xts(ext.df[[i]],S1_dates[[i]])##Function xts for time series
}
names(RM.S1.xts)<-names(ext.list)
plot(as.zoo(RM.S1.xts[[1]][,1]), ylim = c(0,0.5), main = colnames(RM.S1.xts[[1]][,1]))
RM.S1.xts.sub<-lapply(RM.S1.xts, function(x) window(x, start = as.Date("2016-04-05"), end =as.Date("2018-06-01")))


#subset RM.situ.all based on dates from S1 - used merge to combine common S1 dates and in situ data
S1_dates_sub<-as.data.frame(index(RM.S1.xts.sub[[1]]))
colnames(S1_dates_sub)<-"date"
#not very pretty code!
RM.situ.sub<-merge(S1_dates_sub, RM.situ.day.list[[1]], by.x = "date", by.y = colnames(RM.situ.day.list[[1]][1]), all.x = TRUE)
for(i in 2:length(RM.situ.day.list)){
  RM.situ.sub<-merge(RM.situ.sub, RM.situ.day.list[[i]], by.x = "date", by.y = colnames(RM.situ.day.list[[i]][1]), all.x = TRUE)
}
colnames(RM.situ.sub)<-c("date",RM.situ.names)
RM.situ.sub.xts<-xts(RM.situ.sub[,-1],RM.situ.sub[,1])


#IMPORTANT! ------Reorder the columns like in S1.xts.sub
col_order <-  colnames(RM.situ.sub.xts)
RM.S1.xts.sub <- lapply(RM.S1.xts.sub, function(x) x[, col_order])# 

##PLotting all the sites - S1 and in situ in one plot
#pdf(paste("/media/coleen/DDrive1/1_OWASIS/2_Statistical_analysis/penlog/figures/S1_all_pixel_150m_vs_situ_Raam_v1.pdf", sep = ""),
# bg = "white",height=15,width=21)
for(h in 1:length(RM.S1.xts.sub)){
  par(mfrow = c(4,4),mar = c(3,3,3,2))  
for(i in seq_along(1:ncol(RM.situ.sub.xts))){

  plot(as.zoo(RM.S1.xts.sub[[h]][,i]), ylim = c(0,0.6), col = "grey38", type = "o", pch = 19, cex = 0.7, lty=5, ylab = "") 
  par(new=TRUE)
  plot(as.zoo(RM.situ.sub.xts[,i]), ylim = c(0,0.6), lwd = 2, main = paste(names(RM.S1.xts.sub[[h]][,i]), names(RM.S1.xts.sub[h]), sep = " "), type = "o", 
       cex =1.1, pch = 15, col = "sienna4")
}
}



#--------------------2. Validate with masked field outline then resample pixels - resample to 50m,100m, and 150m-------------------------
##Crop and mask raster stack for every polygon in Raam
ras.stack<-stack(ras.crop)
img.crop.list<-lapply(raam.field.poly, function(x) crop(ras.stack,x))

#mask - dunno why lapply doesnt work! dont know how to do it!
img.mask.list<-list()
for(i in seq_along(img.crop.list)){
  img.mask.list[[i]]<-mask(img.crop.list[[i]],raam.field.poly[[i]])
}
names(img.mask.list)<-names(img.crop.list)

#Test plot
for(i in seq_along(img.mask.list)){
  plot(img.mask.list[[i]][[1]], main = names(img.mask.list[i]))
}

#Resample pixels per field from S1 
img.mask.list<-lapply(img.mask.list, function(x) x/10000)# divide by 10k for SM value in m3/m3
img.mask.list<-lapply(img.mask.list, function(x) {x[x== -0.9999]<-NA;x})#set values to NA
plot(img.mask.list[[1]][[1]])# plot to check if values in decimals now!

ras50.fld <- lapply(img.mask.list, function(x) aggregate(x, fact=5, fun=mean))
ras100.fld<-lapply(img.mask.list, function(x) aggregate(x, fact=10, fun=mean))
ras120.fld <- lapply(img.mask.list, function(x) aggregate(x, fact=12, fun=mean))
ras150.fld <- lapply(img.mask.list, function(x) aggregate(x, fact=15, fun=mean))



#Example plots
for(i in seq_along(ras120.fld)){
  plot(ras120.fld[[i]][[1]])
  plot(Raam.net,add= TRUE)
  text(Raam.net@coords[,1], Raam.net@coords[,2], Raam.net@data$Name, pos = 4)
}

#Reorder pts.list- IMPORTANT!
Raam.each.pt.list<-Raam.each.pt.list[names(ras50.fld)]

##Extract values to points
res.fld.list<-list(ras50.fld,ras100.fld,ras120.fld,ras150.fld)
names(res.fld.list)<-c("ras50","ras100","ras120","ras150")
res.fld.list<-lapply(res.fld.list, lapply,  function(x) stack(x))####needs to be stacked for extract to work

ext.fld.list<-list()
ext.fld<-list()

for(i in seq_along(res.fld.list)){
  res.fld<-res.fld.list[[i]]
  for(j in seq_along(Raam.each.pt.list)){
    ext.fld[[j]]<-as.vector(extract(res.fld[[j]],Raam.each.pt.list[[j]]))##This is where extract is
  }
  ext.fld.list[[i]]<-ext.fld
}

##Put all in one df
ext.fld.df<-lapply(ext.fld.list, function(x) data.frame(do.call("cbind",x)))
ext.fld.df<-lapply(ext.fld.df, function(x) {names(x)<-names(res.fld.list[[1]]);x})##Can you use only 1 since same order for all list

fld.dates<-substr(names(ras50[[1]]),9,nchar(names(ras50[[1]])))
RM.S1.ras50$Date<-as.Date(fld.dates, format = "%Y%m%d")

#Add date field
fld.dates<-lapply(res.fld.list, function(x) substr(names(x[[1]]),9,nchar(names(x[[1]]))))
fld.dates<-lapply(fld.dates, function(x) as.Date(x, format = "%Y%m%d"))

#Time series S1 resampled per field - use ext.fld.df
RM.S1.fld.xts<-list()
for(i in seq_along(ext.fld.df)){
  RM.S1.fld.xts[[i]]<-xts(ext.fld.df[[i]],fld.dates[[i]])##Function xts for time series
}

names(RM.S1.fld.xts)<-names(res.fld.list)
#test plot
plot(as.zoo(RM.S1.fld.xts[[1]][,1]), ylim = c(0,0.5), main = colnames(RM.S1.fld.xts[[1]][,1]))
#subset time
RM.S1.fld.xts.sub<-lapply(RM.S1.fld.xts, function(x) window(x, start = as.Date("2016-04-05"), end =as.Date("2018-06-01")))
plot(as.zoo(RM.S1.fld.xts.sub[[1]][,1]), ylim = c(0,0.5), main = colnames(RM.S1.fld.xts.sub[[1]][,1]))

#IMPORTANT! ------Reorder the columns like in S1.xts.sub
col_order <-  colnames(RM.situ.sub.xts)
RM.S1.fld.xts.sub <- lapply(RM.S1.fld.xts.sub, function(x) x[, col_order])# 

##PLotting all the sites - S1 and in situ in one plot
#pdf(paste("/media/coleen/DDrive1/1_OWASIS/2_Statistical_analysis/penlog/figures/S1_all_pixel_150m_vs_situ_Raam.pdf", sep = ""),
# bg = "white",height=15,width=21)
for(h in 1:length(RM.S1.fld.xts.sub)){
  par(mfrow = c(4,4),mar = c(3,3,3,2))  
  for(i in seq_along(1:ncol(RM.situ.sub.xts))){
    
    plot(as.zoo(RM.S1.fld.xts.sub[[h]][,i]), ylim = c(0,0.6), col = "darkslategrey", type = "o", pch = 19, cex = 0.7, lty=5, ylab = "") 
    par(new=TRUE)
    plot(as.zoo(RM.situ.sub.xts[,i]), ylim = c(0,0.6), lwd = 2, main = paste(names(RM.S1.fld.xts.sub[[h]][,i]), names(RM.S1.fld.xts.sub[h]),"masked field", sep = " "),
         type = "o", cex =1.1, pch = 15, col = "orangered4")
  }
}

#---------------------------------------3. S1 MASKED then calculate FIELD MEAN----------------------------------------------------

img.mask.mean<-lapply(img.mask.list, function(x) cellStats(x, stat='mean', na.rm=TRUE))

##Put all in one df.
RM.S1.mean.fld<-as.data.frame(do.call("cbind",img.mask.mean))
fld.mean.dates<-substr(rownames(RM.S1.mean.fld),9,nchar(rownames(RM.S1.mean.fld)))
RM.S1.mean.fld$Date<-as.Date(fld.mean.dates, format = "%Y%m%d")

#--Create time series 
RM.S1.fld.mean.xts<-xts(RM.S1.mean.fld[,-ncol(RM.S1.mean.fld)],RM.S1.mean.fld[,ncol(RM.S1.mean.fld)])
#test plot
plot(as.zoo(RM.S1.fld.mean.xts[,1]), ylim = c(0,0.5), main = colnames(RM.S1.fld.mean.xts[,1]))

RM.S1.fld.mean.xts.sub<-window(RM.S1.fld.mean.xts, start = as.Date("2016-04-05"), end =as.Date("2018-06-01"))
plot(as.zoo(RM.S1.fld.mean.xts.sub[,1]), ylim = c(0,0.5), main = colnames(RM.S1.fld.mean.xts.sub[,1]))

#Reorder the columns like in S1.sub.xts
RM.S1.fld.mean.xts.sub <- RM.S1.fld.mean.xts.sub[, col_order]




##PLotting all the sites - S1 and in situ in one plot
#pdf(paste("/media/coleen/DDrive1/1_OWASIS/2_Statistical_analysis/penlog/figures/S1__vs_situ_Raam_field_mean.pdf", sep = ""),
 #   bg = "white",height=15,width=21)
par(mfrow = c(4,4),mar = c(3,3,3,2))
for(i in seq_along(1:ncol(RM.S1.fld.mean.xts.sub))){
  
  plot(as.zoo(RM.situ.sub.xts[,i]), ylim = c(0,0.6), lwd = 2, main = paste(names(RM.S1.fld.mean.xts.sub[,i]), "field mean ras10", sep = " "), type = "o", cex =1.1, pch = 15, col = "sienna4")
  par(new=TRUE)
  plot(as.zoo(RM.S1.fld.mean.xts.sub[,i]), ylim = c(0,0.6), col = "grey38", type = "o", pch = 19, cex = 0.7, lty=5) 
}

#dev.off()

RM.S1.fld.mean.xts.sub<-list(RM.S1.fld.mean.xts.sub)##Don't run repeatedly! the dimensions change!

#--------------------------------------4. Masked from polygon centered on Station location-------------------------------
#Mask S1 using the buffer polygons
##Crop and mask raster stack for every polygon in Raam
img.crop.buf.list<-list()
for(i in seq_along(shp.buf.poly.list)){
  img.crop.buf.list[[i]]<-lapply(shp.buf.poly.list[[i]], function(x) crop(ras.stack,x))  
}



#calculate the mean per object
img.crop.buf.mean<-list()
for(i in seq_along(img.crop.buf.list)){
  img.crop.buf.mean[[i]]<-lapply(img.crop.buf.list[[i]], function(x) cellStats(x, stat='mean', na.rm=TRUE))  
}

#names of each list
names(img.crop.buf.mean)<-substr(temp,1, nchar(temp)-8)
#make a list of data frames
img.buf.mean.df<-lapply( img.crop.buf.mean, function(x)  do.call("cbind",x))
img.buf.mean.df<-lapply(img.buf.mean.df,function(x) x/10000)
img.buf.mean.df<-lapply(img.buf.mean.df, function(x) {x[x== -0.9999]<-NA;x})#set values to NA


#dfs<-names(img.buf.mean.df)
#for(i in seq_along(dfs)){
#x<-img.buf.mean.df[[i]] 
#assign(dfs[[i]],x)
#}

#Add date field
buf.dates<-lapply(img.buf.mean.df, function(x) substr(rownames(x),9,nchar(rownames(x))))
buf.dates<-lapply(buf.dates, function(x) as.Date(x,format = "%Y%m%d"))

img.buf.mean.df<-lapply(img.buf.mean.df, function(x) data.frame(x))


#Time series S1 
RM.S1.pt.center.xts<-list()
for(i in seq_along(img.buf.mean.df)){
  RM.S1.pt.center.xts[[i]]<-xts(img.buf.mean.df[[i]],buf.dates[[i]])##Function xts for time series
}

names(RM.S1.pt.center.xts)<-names(img.buf.mean.df)

#test plot
plot(as.zoo(RM.S1.pt.center.xts[[1]][,1]), ylim = c(0,0.5), main = colnames(RM.S1.pt.center.xts[[1]][,1]))

#subset time
RM.S1.pt.center.xts.sub<-lapply(RM.S1.pt.center.xts, function(x) window(x, start = as.Date("2016-04-05"), end =as.Date("2018-06-01")))
plot(as.zoo(RM.S1.pt.center.xts.sub[[1]][,1]), ylim = c(0,0.5), main = colnames(RM.S1.pt.center.xts.sub[[1]][,1]))

#IMPORTANT! ------Reorder the columns like in S1.xts.sub
RM.S1.pt.center.xts.sub <- lapply(RM.S1.pt.center.xts.sub, function(x) x[, col_order])# 



#Test plots
for(h in 1:length(RM.S1.pt.center.xts.sub)){
  par(mfrow = c(4,4),mar = c(3,3,3,2))  
  for(i in seq_along(1:ncol(RM.situ.sub.xts))){
    
    plot(as.zoo(RM.S1.pt.center.xts.sub[[h]][,i]), ylim = c(0,0.6), col = "darkslategrey", type = "o", pch = 19, cex = 0.7, lty=5, ylab = "") 
    par(new=TRUE)
    plot(as.zoo(RM.situ.sub.xts[,i]), ylim = c(0,0.6), lwd = 2, main = paste(names(RM.S1.pt.center.xts.sub[[h]][,i]), names(RM.S1.pt.center.xts.sub[h]),"masked field", sep = " "),
         type = "o", cex =1.1, pch = 15, col = "orangered4")
  }
}
