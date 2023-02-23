##REad output from Sentinel-1 validation 
setwd("/media/coleen/DDrive/1_OWASIS/4_Papers/traffic/data/S1_valid_tables")
raam = list.files(pattern="*.rm2.cdf.csv")
rm.list= lapply(raam, function(x) read.csv(x, header=T,stringsAsFactors = F))
raam.names<-substr(raam,1, nchar(raam)-12)
rm.list<-setNames(rm.list,raam.names)

twen = list.files(pattern="*.tw.cdf.csv")
tw.list= lapply(twen, function(x) read.csv(x, header=T,stringsAsFactors = F))
twen.names<-substr(twen,1, nchar(twen)-12)
tw.list<-setNames(tw.list,twen.names)



##Rename column names so uniform for Raam and Twente -- can rbind later-------
clnam<-c("res_10m","res_50m","res_100m","res_150m","field res_50","field res_100m","field res_120m", "field res_150m", "pt.center res_100m",
         "pt.center res_150m", "pt.center res_50m", "field mean", "station")

#Check if order of names for twente and raam are the same
names(rm.list)
names(tw.list)


#rename columns for each data frame
rm.list<-lapply(rm.list, function(x) {colnames(x)<-clnam;x})
tw.list<-lapply(tw.list, function(x) {colnames(x)<-clnam;x})


all.list<-list()
for(i in 1:4){
  all.list[[i]]<-rbind(rm.list[[i]],tw.list[[i]]) 

}##Check if each list contain 9 observations - for the 9 stations used in the study

all.list<-setNames(all.list,names(rm.list))

##Write dataframe as table --- optional!
#for(i in 1:length(all.list)){
#  write.csv(all.list.t[[i]], file = paste0("/media/coleen/DDrive1/1_OWASIS/4_Papers/traffic/data/S1_valid_tables/",names(all.list.t[i]),"_all.csv"),row.names=TRUE, quote=FALSE)
#}
  
  


##-------------PLOT------------------------------------
colors<-c("blue","green", "red")
a<-4
b<-3
d<-1
c<-3


#RMSE
#pdf(paste("/media/coleen/DDrive1/1_OWASIS/4_Papers/traffic/figures/S1_vs_situ_Raam_rmse_corr_v1.pdf", sep = ""),
#   bg = "white",height=11,width=15)
par(mfrow = c(4,1), mar = c(3,4,1,1), mgp = c(1.7,0.5,0), tck = -0.015)

plot(all.list[[3]][,1], pch =21, cex =2, bg = pt.colors[1], ylim = c(0.025,0.18), xlab = "", ylab = "RMSE m3/m3", cex.lab = 1.5,xaxt = "n")
for(i in 1:a){
  points(all.list[[3]][,i], pch= 20+i,cex=2,bg = alpha("royalblue3",1))
}
for(i in (a+1):(a+b)){
  points(all.list[[3]][,i], pch= 20+i-a,cex=2,bg = alpha("indianred2",1))
}
for(i in (a+b+1):(a+b+c)){
  points(all.list[[3]][,i], pch= 20+i-a-b,cex=2,bg = alpha("darkorchid2",1))
}

points(all.list[[3]][,12], pch= 21,cex=2, bg = alpha("olivedrab3",1))

#legend(x = 7, y = 0.15, legend = colnames(all.list[[3]])[1:9], pch =c(rep(21:24,2),21), pt.bg = pt.colors, bty = "n", ncol =3,text.width= 0.2,
     #  y.intersp =0.2, x.intersp = 0.2, pt.cex = 1.2)

#PLOT BIAS
plot(all.list[[1]][,1], pch =21, cex =2, ylim  = c(-0.08, 0.08),bg = colors[1],xlab = "", ylab = "Bias m3/m3", cex.lab = 1.5,
     xaxt = "n")
for(i in 1:a){
  points(all.list[[1]][,i], pch= 20+i,cex=2,bg = "royalblue3")
}
for(i in (a+1):(a+b)){
  points(all.list[[1]][,i], pch= 20+i-a,cex=2,bg = "indianred2")
}
for(i in (a+b+1):(a+b+c)){
  points(all.list[[1]][,i], pch= 20+i-a-b,cex=2,bg = alpha("darkorchid2",1))
}
points(all.list[[1]][,12], pch= 21,cex=2, bg = "olivedrab3")
#legend(x = 12, y = 0.07, legend = colnames(all.list[[1]])[1:9], pch =c(rep(21:24,2),21), pt.bg = c(rep("red",4),rep("blue",4),"green"), bty = "n", ncol =3,text.width= 0.42,
#   y.intersp = 0.25, x.intersp = 0.13, cex = 1)

#dev.off()


#PLOT UnBIASED RMSE
plot(all.list[[4]][,1], pch =21, cex =2, ylim = c(0.03,0.18),bg = colors[1],xlab = "", ylab = "Unbiased RMSE m3/m3", cex.lab = 1.5,
     xaxt = "n")
for(i in 1:a){
  points(all.list[[4]][,i], pch= 20+i,cex=2,bg = "royalblue3")
}
for(i in (a+1):(a+b)){
  points(all.list[[4]][,i], pch= 20+i-a,cex=2,bg = "indianred2")
}
for(i in (a+b+1):(a+b+c)){
  points(all.list[[4]][,i], pch= 20+i-a-b,cex=2,bg = alpha("darkorchid2",1))
}
points(all.list[[4]][,12], pch= 21,cex=2, bg = "olivedrab3")
#legend(x = 12, y = 0.07, legend = colnames(all.list[[4]])[1:9], pch =c(rep(21:24,2),21), pt.bg = c(rep("red",4),rep("blue",4),"green"), bty = "n", ncol =3,text.width= 0.42,
#   y.intersp = 0.25, x.intersp = 0.13, cex = 1)

#dev.off()

#PLOT COR
plot(all.list[[2]][,1], pch =21, cex =2, bg = colors[1], ylim = c(-0.2,0.8),xlab = "Station", ylab = "Spearman Cor (rho)", cex.lab = 1.5,
     xaxt = "n")
axis(side = 1, at=1:nrow(all.list[[2]]),labels = all.list[[2]]$station)
for(i in 1:a){
  points(all.list[[2]][,i], pch= 20+i,cex=2,bg = "royalblue3")
}
for(i in (a+1):(a+b)){
  points(all.list[[2]][,i], pch= 20+i-a,cex=2,bg = "indianred2")
}
for(i in (a+b+1):(a+b+c)){
  points(all.list[[2]][,i], pch= 20+i-a-b,cex=2,bg = alpha("darkorchid2",1))
}
points(all.list[[2]][,12], pch= 21,cex=2, bg = "olivedrab3")
#legend(x = 12, y = 0.07, legend = colnames(all.list[[2]])[1:9], pch =c(rep(21:24,2),21), pt.bg = c(rep("red",4),rep("blue",4),"green"), bty = "n", ncol =3,text.width= 0.42,
#   y.intersp = 0.25, x.intersp = 0.13, cex = 1)

#dev.off()





###########--------------------------PLOT TYPE 2------------------------
library(scales)
#Transpose list, but remove station row first so the entries will be of type numeric
all.list.t<-lapply(all.list, function(x) {x<-x[,-13];x})
all.list.t<-lapply(all.list.t,t)

st<-all.list[[1]][,13]
all.list.t<-lapply(all.list.t, function(x) {colnames(x)<-st;x})
all.list.t<-lapply(all.list.t, function(x) data.frame(x))

res<-c(10,50,100,150,50,100,120,150,100,150,50,10)
all.list.t<-lapply(all.list.t, function(x)  {x$res<-res;x})
#reorder all.list.t
list.ord<-c("rmse.all","bias.all","unb_rmse.all","cor.all")

all.list.t<-all.list.t[list.ord]

##LOOP over this!
library(RColorBrewer)
col.st<-palette(colorRampPalette(brewer.pal(9,"Paired"))(9))
grey.st<-c("burlywood4","cornsilk3","tan4", "peru","wheat3","chartreuse3","olivedrab4",
           "salmon4","peachpuff4")

plot.name<-c("All pixels", "Pixels within a field", "Surrounding pixels \nwith station at the center", "Mean of \nfield pixels")

#pdf(paste("/media/coleen/DDrive/1_OWASIS/4_Papers/traffic/figures/S1_valid_all_gr2_big.pdf", sep = ""),height=7,width=9)

par(mfrow = c(4,4),oma = c(2,3,2.5,0), mgp = c(1.5,0.65,0), tck = -0.03)
for(j in 1:length(all.list.t)){
test<-all.list.t[[j]]##RMSE
test_A <-test[1:4,]
test_B<-test[5:8,]

##Remove row with 120m resolution
test_B<-test_B[-3,]
test_C<-test[9:11,]
test_C<-test_C[order(test_C$res),]
test_D<-test[12,]

#A
par(mar= c(1.5,2,1,0),mgp = c(1.3,0.35,0), tck = -0.02)
plot(test_A$res,test_A$RM02, type = "b", pch=19, ylim = c(min(test[,-10]),max(test[,-10])), col = grey.st[1], cex = 2.5,
     xlab = "", xaxt = "n", yaxt = "n", xlim = c(0,160), ylab = "")
for(i in 2:9){
  points(test_A$res,test_A[,i], type = "b", pch = 19, col = alpha(grey.st[i],0.8), cex = 2.5)
}
axis(side = 2, cex.axis= 1.15)
if (j %in% 4)
axis(1, cex.axis = 1.3, at = c(10,50,100,150))
#if (j %in% 1)
#title( main=plot.name[[1]], cex.main = 1.3)


#B
par(mar= c(1.5,2,1,0))
plot(test_B$res,test_B$RM02, type = "b", pch=19, ylim = c(min(test[,-10]),max(test[,-10])), col = grey.st[1], cex = 2.5,
     xlab = "", xaxt = "n", yaxt = "n", xlim = c(0,160), ylab = "")
for(i in 2:9){
  points(test_B$res,test_B[,i], type = "b", pch = 19,  col = alpha(grey.st[i],0.8), cex = 2.5)
}
if (j %in% 4)
axis(1, cex.axis = 1.3, at = c(10,50,100,150))
axis(2,labels = FALSE)
#if (j %in% 1)
  #title( main=plot.name[[2]], cex.main = 1.3)

#C
par(mar= c(1.5,2,1,0))
plot(test_C$res,test_C$RM02, type = "b", pch=19, ylim = c(min(test[,-10]),max(test[,-10])), col = grey.st[1], cex = 2.5,
    xlab = "", xaxt = "n", yaxt = "n", xlim = c(0,160), ylab = "")
for(i in 2:9){
  points(test_C$res,test_C[,i], type = "b", pch = 19,  col = alpha(grey.st[i],0.8), cex = 2.5)
}
if (j %in% 4)
  axis(1, cex.axis = 1.3, at = c(10,50,100,150))
axis(2,labels = FALSE)
#if (j %in% 1)
 #title( main=plot.name[[3]], cex.main = 1.3, line = 0.15)

#D
par(mar= c(1.5,2,1,7))
plot(test_D$res,test_D$RM02, type = "b", pch=19,  ylim = c(min(test[,-10]),max(test[,-10])), col = grey.st[1], cex = 2.5,
     xlab = "", xaxt = "n", yaxt = "n", xlim = c(0,20), ylab = "")
for(i in 2:9){
  points(test_D$res,test_D[,i], type = "b", pch = 19,  col = alpha(grey.st[i],0.8), cex = 2.5)
}
if (j %in% 4)
  axis(1, cex.axis = 1.3, at = c(10,50,100,150))
axis(2,labels = FALSE)
}


legend(x=23,y=3,legend =colnames(test_D)[1:9], pch = 19,  col = alpha(grey.st,0.8), y.intersp =1.5,cex = 1.25,box.lwd = 0.1,box.col = alpha("grey38",0.4),
             x.intersp = 0.8,ncol = 1,pt.cex=2.25,text.width=8,xpd=NA)
#
##Annotation- text outside margin
mtext(side = 1, "Aggregation level (meters)", outer = TRUE, line = 0.5, cex = 1.2)
mtext(side =2, "Spearman's Rank\n Correlation Coefficient", adj = 0.02,outer =TRUE, cex=0.8)
mtext(side =2, "Unbiased RMSE", cex = 1,adj = 0.35, outer =TRUE)
#~(cm^{3}*cm^{-3})
mtext(side =2, "Bias", adj = 0.65, outer =TRUE,cex=1.2)
mtext(side =2, "RMSE", adj = 0.94, outer =TRUE,cex=1.2)
mtext(side =3, plot.name[[1]], adj = 0.12, outer =TRUE)
mtext(side =3, plot.name[[2]], adj = 0.38, outer =TRUE)
mtext(side =3, plot.name[[3]], adj = 0.68, outer =TRUE, padj = 0.36)
mtext(side =3, plot.name[[4]], adj = 0.87, outer =TRUE, padj = 0.36)


dev.off()
