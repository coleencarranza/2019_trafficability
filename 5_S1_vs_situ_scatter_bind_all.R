##Remove 120 resolution and reorder RM.S1.all
RM.S1.all<-c(rm.cdfm.list2,rm.cdfm.fld.list2,rm.cdfm.pt.center.list2,rm.cdfm.fld.mean.list2)
RM.S1.all<-RM.S1.all[-7]
names.list<-names(RM.S1.all)
names.list
RM.S1.all<-RM.S1.all[c(1:7,10,8,9,11)]
names.list<-names(RM.S1.all)
names.list

TW.S1.all<-c(tw.cdfm.list2,tw.cdfm.fld.list2,tw.cdfm.pt.center.list2,tw.cdfm.fld.mean.list2)
TW.S1.all<-TW.S1.all[-7]
names.list<-names(TW.S1.all)
names.list
TW.S1.all<-TW.S1.all[c(1:7,10,8,9,11)]
names.list<-names(TW.S1.all)
names.list


#PLOT 
grey.st<-c("burlywood4","cornsilk3","tan4", "peru","wheat3","chartreuse3","olivedrab4",
           "salmon4","peachpuff4")


pt.cols<-c("burlywood4","cornsilk3","tan4", "peru","wheat3","chartreuse3")
pt.cols2<-c("olivedrab4","salmon4","peachpuff4")
main.name<-c("10m","50m","100m","150m","50m","100m","150m","50m","100m","150m","field mean")
mat <- matrix(c(1,2,3,4,
                0,5,6,7,
                0,8,9,10,
                0,11,0,0), nrow = 4, byrow = TRUE)

pdf(paste("/media/coleen/DDrive/1_OWASIS/4_Papers/traffic/S1_vs_situ_cdfm.pdf", sep = ""),height=6,width=8)
layout(mat)
par(mar = c(1.5,1.5,1.5,1), mgp = c(1.5,0.3,0),oma = c(2.5,2,0,2), tck = -0.02)
for(h in 1:length(RM.S1.all)){
  #pdf(paste("/media/coleen/DDrive1/1_OWASIS/4_Papers/traffic/figures/S1_vs_situ_Raam_",names(RM.S1.all[h]),"_v1.pdf", sep = ""),
  #   bg = "white",height=11,width=15)
  
  RM.S1.sub<-RM.S1.all[[h]]
  plot(as.vector(RM.situ.sub.xts[,1]),as.vector(RM.S1.sub[,1]), xlim = c(0, 0.5), ylim = c(0,0.5), pch = 19, ylab= "",xlab= "",main = main.name[[h]],cex = 0.5,cex.main = 0.9,
       col = alpha(pt.cols[[1]],0.65), lwd = 0)
  for(i in 2:ncol(RM.situ.sub.xts)){
    points(as.vector(RM.situ.sub.xts[,i]),as.vector(RM.S1.sub[,i]), xlim = c(0, 0.5), ylim = c(0,0.5), pch = 19,lwd=0, col = alpha(pt.cols[[i]],0.65),cex = 0.5,)
  }
  
  TW.S1.sub<-TW.S1.all[[h]]
  for(k in 1:ncol(TW.situ.sub.xts)){
    points(as.vector(TW.situ.sub.xts[,k]),as.vector(TW.S1.sub[,k]), xlim = c(0, 0.5), ylim = c(0,0.5), pch = 19, lwd=0,col= alpha(pt.cols2[[k]],0.35),cex = 0.5,)
    
    abline(0,1, lty = 2)
  }
  #dev.off()
}
mtext("In situ measurements", side = 1, line= 1, outer =TRUE)
mtext("Sentinel1-derived values", side = 2, line= 0, outer =TRUE)
mtext("All pixels", side = 4,  outer = TRUE, adj = 0.9, cex= 0.7)
mtext("Pixels within a field", side = 4,  outer = TRUE, adj = 0.65, cex= 0.7)
mtext("Surrounding pixels \nwith station at the center", side = 4,  outer = TRUE, adj = 0.335, cex= 0.7, line = 0.8)
mtext("Mean of field pixels", side = 4,  outer = TRUE, adj = 0.01, cex= 0.7)


dev.off()