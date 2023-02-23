#-------------2.S1 images RAAM area----------------
#"/media/coleen/DDrive/SM_output_20141001_20170531__20181128_CROP/v2"= for twente
#""/media/coleen/DDrive/SM_output_WGS_20141001_20180601__20181120__V2_CROP" - for raam
setwd("/media/coleen/DDrive/SM_output_20141001_20170531__20181128_CROP/v2")
temp = list.files(pattern="*.tif")
ras <- lapply(temp,raster) 

##Im running out of memory!!

##Crop and mask raster stack for every polygon in Raam
#ras.crop<-lapply(ras, function(x) crop(x,Raam.brp))
library(raster)
ras.crop<-ras
#for(i in 1:length(ras.crop)){
#  values(ras.crop[[i]])[values(ras.crop[[i]]) < 0] = NA 
#}

#Calculate Sw
setwd("/media/coleen/DDrive/1_OWASIS/4_Papers/traffic/data/theta_sat_wp")
temp1 = list.files(pattern="*TwenteExact.tif")
ras_sat_wp <- lapply(temp1,raster) 
ras_sat<-ras_sat_wp[[1]]
ras_wp<-ras_sat_wp[[2]]


for(i in 51:425){
  Sw<-(ras.crop[[i]]-ras_wp)/(ras_sat-ras_wp)
  writeRaster(Sw, paste0("/media/coleen/DDrive/SM_Twente_Sw/",temp[[i]],"_Sw.tif"), format = "GTiff", overwrite = TRUE)##create dir if not present
}