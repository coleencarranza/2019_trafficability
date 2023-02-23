##CHOOSE WORKING DIRECTORY
setwd("/media/coleen/DDrive1/1_OWASIS/4_Papers/traffic/data/CI_TDR/2017")
tdr<-read.csv("TDR readings_2017.csv",  header =T, stringsAsFactors = FALSE)

######PLOT HISTORGRAM SOIL MOISTURE PER CROP#######
crop<-list("Grass","Corn","Potato","Winter wheat") ## ("Grass","Corn","Potato","Winter wheat") or ("gras","chicorei","aardappel","beets")
par(mfrow = c(2,2), mar = c(4,4,4,4) + 0.01,mgp=c(1.7,0.5,0), tck=-0.03)
#original values
tdr.o.crop<-list()
for(i in crop){
  tdr.o.crop[[i]]<-tdr[grep(i, tdr$Crop), ]
  hist(tdr.o.crop[[i]]$TDR,col = "grey", xlab="VWC 5cm", 
       cex.main =1.5, cex.lab = 1, cex.axis=1, xlim = c(0,50))
}

####CLEANING FILES - AVERAGING up to desired critical depth######
########USING LISTS##PENETROMETER DATA
setwd("/media/coleen/DDrive1/1_OWASIS/4_Papers/traffic/data/CI_TDR/2017/raw")## change file also for 2017
clean.files<-list.files(pattern = "clean.csv")
clean.names<-substr(clean.files,1,nchar(clean.files)-10)
clean.list<-lapply(clean.files, function(x) read.csv(x, header=T,stringsAsFactors = F)  )
clean.list<-setNames(clean.list,clean.names)
clean.list<-lapply(clean.list, function(x) {x[x==-1]<-NA;x})

agg.list<-lapply(clean.list, function(x) aggregate(x[,3:85], list(x$Plot),mean))
agg.list<-lapply(agg.list, function(x) {rownames(x)<-x$Group.1;x})
agg.listT<-lapply(agg.list, function(x) data.frame(t(x[,5:25])))#### 5:20 for up to 15cm or use 5:25 for 20cm
agg.listT<-lapply(agg.listT, function(x) {x$Depth <- c(0:20);x})

##test plot##
test.plot<-agg.listT[[1]]
plot(test.plot[,1],test.plot$Depth, type="o", col="blue",xlim = c(0,4),ylim = rev(c(0,20)))
x<-as.numeric(max(seq(test.plot)))
for(i in seq_along(test.plot[-x]))
{
  lines(test.plot[,i],test.plot$Depth,type="o", col="blue")
}

for(i in seq_along(agg.list)){
  agg.list[[i]]$DMY<-paste0(substr(names(agg.list[i]),7,8),
                            substr(names(agg.list[i]),5,6),substr(names(agg.list[i]),3,4))
  agg.list[[i]]$ID<-paste(agg.list[[i]]$DMY,gsub(".*-","",agg.list[[i]]$Group.1),sep="_")
}

##change rows to 5:20 for up to 15cm or 5:25 for 20cm
agg.list<-lapply(agg.list, function(x) {x$CI_20<-rowMeans(x[,5:25], na.rm = T);x})##Change CI_15 or CI_20
agg.list<-lapply(agg.list, function(x) {x$CI_5<-rowMeans(x[,5:10], na.rm = T);x})##Change CI_15 or CI_20
CI.list<-lapply(agg.list, function(x) x[,c(1:3,86,87,88),])
cols <- c("Field","Line","Pt", "TDR","Crop")
#CI.list<- lapply(CI.list, function(x) {x[cols]<-tdr[[cols]][match(x$ID, tdr$ID)];x})

for(i in seq_along(CI.list)){
  CI.list[[i]][cols]<-sapply(cols, function(x) tdr[[x]][match(CI.list[[i]]$ID, tdr$ID)])
  CI.list[[i]]$TDR<-as.numeric(CI.list[[i]]$TDR)
}

##Test plot #2######NOTE TDR values here are not yet calibrated! only for test plotting
RM.CI<-do.call("rbind",CI.list)
crop<-as.factor(RM.CI$Crop)## vector need to be as factor
library(RColorBrewer)
darkcols <- brewer.pal(5,"Spectral")
col <- darkcols[crop]
plot(RM.CI$TDR,RM.CI$CI_20,bg=col, pch = 21, ylim = c(0,4))

##Export to folder
CI.names<-as.vector(colnames(CI.list[[1]]))
for(i in seq_along(CI.list)){
  write.csv(CI.list[[i]],paste0("/media/coleen/DDrive1/1_OWASIS/4_Papers/traffic/data/CI_TDR/2017/CI_20cm/",
                                names(CI.list[i]),"_CI_20.csv"),row.names=F)
}

####CLEANING FILES - Saving CI's up to 80cm depth######
#Transpose Data 
agg.list<-lapply(agg.list, function(x) {rownames(x)<-x$Group.1;x})
agg.listT_80<-lapply(agg.list, function(x) data.frame(t(x[,5:84])))

for(i in seq_along(agg.listT_80)){
  colnames(agg.listT_80[[i]])<-agg.list[[i]]$ID
  
}

agg.listT_80<-lapply(agg.listT_80, function(x) {x$Depth<-c(0:79);x})
agg.listT_80<-lapply(agg.listT_80, function(x) {rownames(x)<-x$Depth;x})
#write .csv CI_80
for(i in seq_along(agg.listT_80)){
  write.csv(agg.listT_80[[i]],paste0("/media/coleen/DDrive1/1_OWASIS/4_Papers/traffic/data/CI_TDR/2017/CI_80cm/",
                                     names(agg.listT_80[i]),"_CI_80.csv"),row.names=F)
}


#########INDIVIDUAL FILES###################
##CHOOSE WORKING DIRECTORY
setwd("/media/coleen/DDrive1/1_OWASIS/2_Statistical_analysis/penlog/2017")## change file also for 2017
RM<-read.csv("20170630_Rm7_LORA_merged_clean.csv",   header =T, stringsAsFactors = FALSE)
#RM.lora<-read.csv("20170726_ITC10_clean.csv",   header =T, stringsAsFactors = FALSE)
RM[RM==-1] <- NA
#20160526_Rm_up_clean
#20160614_Rm_up_clean
#20160630_Rm_up_clean
#20160803_Rm_up_clean_merged
#20160810_Rm_low_clean
#20160623_TWE_clean
#20160715_TWE_merged_clean
#20160708_TWE_merged_clean

#20170420_Rm8_clean
#20170510_Rm8_clean
#20170510_Rm7_clean
#20170522_ITC02_clean
#20170522_ITC10_clean
#20170608_Rm7_clean
#20170608_Rm8_clean
#20170630_Rm7_LORA_merged_clean ---points not in the transects
#20170718_Rm08_clean
#20170726_ITC02_clean
#20170726_ITC10_clean
#20170821_Rm7LORA_2rows_clean
#20170821_Rm8_clean
#aggregate per plot
RM.agg<-aggregate(RM[, 3:85], list(RM$Plot), mean)

#PLot CI vs Depth
rownames(RM.agg)<-RM.agg$Group.1
RM.aggT<-data.frame(t(RM.agg[,5:20]))## transpose up of 0-15cm only
RM.aggT$Depth<-c(0:15)

plot(RM.aggT[,1],RM.aggT$Depth, type="o", col="blue",xlim = c(0,4),ylim = rev(c(0,15)))
x<-as.numeric(max(seq(RM.aggT)))
for(i in seq_along(RM.aggT[-x]))
{
  lines(RM.aggT[,i],RM.aggT$Depth,type="o", col="blue")
}
##add ID column format:DDMMYY_(gsub)###ALWAYS CHANGE THE DATES!
RM.agg$ID<-paste("300617_",gsub(".*-","",RM.agg$Group.1),sep="")## change with date

#subset CI_15cm or CI_20 --- 5:25
RM.agg$CI_15<-rowMeans(RM.agg[,5:20], na.rm = T) 

##subset RM.agg
RM.CI<-RM.agg[,c(1:3,85,86),]
xy <- c("Field","Line","Pt", "TDR","Crop")
RM.CI[xy] <- sapply(xy, function(x) tdr[[x]][match(RM.CI$ID, tdr$ID)])
RM.CI$TDR<-as.numeric(RM.CI$TDR)

crop<-as.factor(RM.CI$Crop)## vector need to be as factor
library(RColorBrewer)
darkcols <- brewer.pal(5,"Spectral")
col <- darkcols[crop]
plot(RM.CI$TDR,RM.CI$CI_15,bg=col, pch = 21, ylim = c(0,4))
##Export to folder
write.csv(RM.CI,"/media/coleen/DDrive1/1_OWASIS/2_Statistical_analysis/penlog/2017/R_outputs/20170630_Rm7LORA_CI_15.csv", row.names=F)

#CI_80
rownames(RM.agg)<-RM.agg$Group.1
RM.aggT_80<-data.frame(t(RM.agg[,5:84]))## transpose up of 0-80cm 
colnames(RM.aggT_80) <- RM.agg$ID
RM.aggT_80$Depth<-c(0:79)
rownames(RM.aggT_80)<-RM.aggT_80$Depth

plot(RM.aggT_80[,1],RM.aggT_80$Depth, type="b", col="blue",xlim = c(0,8),ylim = rev(c(0,80)))
x<-as.numeric(max(seq(RM.aggT_80)))
for(i in seq_along(RM.aggT_80[-x]))
{
  lines(RM.aggT_80[,i],RM.aggT_80$Depth,type="o", pch = 21, bg="blue", col = "black")
}
##Export to folder
write.csv(RM.aggT_80,"/media/coleen/DDrive1/1_OWASIS/2_Statistical_analysis/penlog/2017/R_outputs/20170630_Rm7LORA_80.csv", row.names=F)
