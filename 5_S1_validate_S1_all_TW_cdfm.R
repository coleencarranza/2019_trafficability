##
library(MASS)


##--------------------Aggregate ALL PIXELS 10m,50m,100m,150m-------------------------############
tw.cdfm.list<-list()
tw.cdfm.list2<-list()

for(i in seq_along(TW.S1.xts.sub)){
  par(mfrow = c(3,4))
  pix.size<-TW.S1.xts.sub[[i]]
  for(h in 1:ncol(pix.size)){
    st_situ_s1<-as.data.frame(cbind(TW.situ.sub.xts[,h],pix.size[,h]))
    #st_situ_s1<-st_situ_s1[complete.cases(st_situ_s1),]
    f.obs = fitdistr(na.exclude(st_situ_s1[,1]), "normal")
    f.mod = fitdistr(na.exclude(st_situ_s1[,2]), "normal")
    
    r=pnorm(st_situ_s1[,2],mean = f.mod$estimate[1],sd = f.mod$estimate[2])
    tw.cdfm = qnorm(r, mean = f.obs$estimate[1],sd = f.obs$estimate[2])
    tw.cdfm.list[[h]]<-tw.cdfm
    #test plot
    plot(st_situ_s1[,1], tw.cdfm, pch = 19, xlim = c(0,0.5), ylim = c(0,0.5))
    points(st_situ_s1[,1], st_situ_s1[,2],col = "blue",pch = 19)
    
    plot(ecdf(st_situ_s1[,1]), xlim = c(0,0.45))
    plot(ecdf(st_situ_s1[,2]),add=TRUE,col = "red")
    plot(ecdf(tw.cdfm), add=TRUE, cex=0.5, col="blue")
  }
  tw.cdfm.list<-data.frame(do.call("cbind",tw.cdfm.list))
  colnames(tw.cdfm.list)<-colnames(TW.situ.sub[2:4])
  tw.cdfm.list2[[i]]<-tw.cdfm.list
  
}
tw.cdfm.list2<-setNames(tw.cdfm.list2,names(TW.S1.xts.sub))



###------------Aggregate pixels within FIELD ONLY----------------------#####
tw.cdfm.fld.list<-list()
tw.cdfm.fld.list2<-list()

for(i in seq_along(TW.S1.fld.xts.sub)){
  par(mfrow = c(3,4))
  pix.size<-TW.S1.fld.xts.sub[[i]]
  for(h in 1:ncol(pix.size)){
    st_situ_s1<-as.data.frame(cbind(TW.situ.sub.xts[,h],pix.size[,h]))
    #st_situ_s1<-st_situ_s1[complete.cases(st_situ_s1),]
    f.obs = fitdistr(na.exclude(st_situ_s1[,1]), "normal")
    f.mod = fitdistr(na.exclude(st_situ_s1[,2]), "normal")
    
    r=pnorm(st_situ_s1[,2],mean = f.mod$estimate[1],sd = f.mod$estimate[2])
    tw.cdfm = qnorm(r, mean = f.obs$estimate[1],sd = f.obs$estimate[2])
    tw.cdfm.fld.list[[h]]<-tw.cdfm
    #test plot
    plot(st_situ_s1[,1], tw.cdfm, pch = 19, xlim = c(0,0.5), ylim = c(0,0.5))
    points(st_situ_s1[,1], st_situ_s1[,2],col = "blue",pch = 19)
    
    plot(ecdf(st_situ_s1[,1]), xlim = c(0,0.45))
    plot(ecdf(st_situ_s1[,2]),add=TRUE,col = "red")
    plot(ecdf(tw.cdfm), add=TRUE, cex=0.5, col="blue")
  }
  tw.cdfm.fld.list<-data.frame(do.call("cbind",tw.cdfm.fld.list))
  colnames(tw.cdfm.fld.list)<-colnames(TW.situ.sub[2:4])
  tw.cdfm.fld.list2[[i]]<-tw.cdfm.fld.list
  
}
tw.cdfm.fld.list2<-setNames(tw.cdfm.fld.list2,names(TW.S1.fld.xts.sub))


###------------FIELD MEAN----------------------#####
tw.cdfm.fld.mean.list<-list()
tw.cdfm.fld.mean.list2<-list()

for(i in seq_along(TW.S1.fld.mean.xts.sub)){
  par(mfrow = c(3,4))
  pix.size<-TW.S1.fld.mean.xts.sub[[i]]
  for(h in 1:ncol(pix.size)){
    st_situ_s1<-as.data.frame(cbind(TW.situ.sub.xts[,h],pix.size[,h]))
    #st_situ_s1<-st_situ_s1[complete.cases(st_situ_s1),]
    f.obs = fitdistr(na.exclude(st_situ_s1[,1]), "normal")
    f.mod = fitdistr(na.exclude(st_situ_s1[,2]), "normal")
    
    r=pnorm(st_situ_s1[,2],mean = f.mod$estimate[1],sd = f.mod$estimate[2])
    tw.cdfm = qnorm(r, mean = f.obs$estimate[1],sd = f.obs$estimate[2])
    tw.cdfm.fld.mean.list[[h]]<-tw.cdfm
    #test plot
    plot(st_situ_s1[,1], tw.cdfm, pch = 19, xlim = c(0,0.5), ylim = c(0,0.5))
    points(st_situ_s1[,1], st_situ_s1[,2],col = "blue",pch = 19)
    
    plot(ecdf(st_situ_s1[,1]), xlim = c(0,0.45))
    plot(ecdf(st_situ_s1[,2]),add=TRUE,col = "red")
    plot(ecdf(tw.cdfm), add=TRUE, cex=0.5, col="blue")
  }
  tw.cdfm.fld.mean.list<-data.frame(do.call("cbind",tw.cdfm.fld.mean.list))
  colnames(tw.cdfm.fld.mean.list)<-colnames(TW.situ.sub[2:4])
  tw.cdfm.fld.mean.list2[[i]]<-tw.cdfm.fld.mean.list
  
}
tw.cdfm.fld.mean.list2<-setNames(tw.cdfm.fld.mean.list2,names(TW.S1.fld.mean.xts.sub))


####-----------------------------Centered at Station location------------------#########################
tw.cdfm.pt.center.list<-list()
tw.cdfm.pt.center.list2<-list()

for(i in seq_along(TW.S1.pt.center.xts.sub)){
  par(mfrow = c(3,4))
  pix.size<-TW.S1.pt.center.xts.sub[[i]]
  for(h in 1:ncol(pix.size)){
    st_situ_s1<-as.data.frame(cbind(TW.situ.sub.xts[,h],pix.size[,h]))
    #st_situ_s1<-st_situ_s1[complete.cases(st_situ_s1),]
    f.obs = fitdistr(na.exclude(st_situ_s1[,1]), "normal")
    f.mod = fitdistr(na.exclude(st_situ_s1[,2]), "normal")
    
    r=pnorm(st_situ_s1[,2],mean = f.mod$estimate[1],sd = f.mod$estimate[2])
    tw.cdfm = qnorm(r, mean = f.obs$estimate[1],sd = f.obs$estimate[2])
    tw.cdfm.pt.center.list[[h]]<-tw.cdfm
    #test plot
    plot(st_situ_s1[,1], tw.cdfm, pch = 19, xlim = c(0,0.5), ylim = c(0,0.5))
    points(st_situ_s1[,1], st_situ_s1[,2],col = "blue",pch = 19)
    
    plot(ecdf(st_situ_s1[,1]), xlim = c(0,0.45))
    plot(ecdf(st_situ_s1[,2]),add=TRUE,col = "red")
    plot(ecdf(tw.cdfm), add=TRUE, cex=0.5, col="blue")
  }
  tw.cdfm.pt.center.list<-data.frame(do.call("cbind",tw.cdfm.pt.center.list))
  colnames(tw.cdfm.pt.center.list)<-colnames(TW.situ.sub[2:4])
  tw.cdfm.pt.center.list2[[i]]<-tw.cdfm.pt.center.list
  
}
tw.cdfm.pt.center.list2<-setNames(tw.cdfm.pt.center.list2,names(TW.S1.pt.center.xts.sub))



#---------------------------------------5.VALIDATION RMSE AND CORRELATION---------------------------------------------------------

#Combine all S1 stuff!

#Rename some lists soe not confusing!
names(tw.cdfm.fld.mean.list2)<-"ras10_fld_mean"
names(tw.cdfm.fld.list2)<-c("ras50_fld","ras100_fld","ras120_fld","ras150_fld")


tw.cdfm.all<-c(tw.cdfm.list2,tw.cdfm.fld.list2,tw.cdfm.pt.center.list2,tw.cdfm.fld.mean.list2)
names(TW.S1.all)


#pdf(paste("/media/coleen/DDrive1/1_OWASIS/2_Statistical_analysis/penlog/figures/S1_all_pixel_100m_vs_situ_Raam_rmse.pdf", sep = ""),
# bg = "white",height=15,width=15)

par(mfrow = c(4,4),mar = c(3,3,3,3), mgp = c(2,0.7,0))

#Put actual(in situ) and predicted(S1) values in a list
act.pred<-list()
act.pred.list<-list()
rmse.list<-list()
for(h in 1:length(tw.cdfm.all)){
  TW.S1.sub<-tw.cdfm.all[[h]]
  for(i in 1:ncol(TW.situ.sub.xts)){
    act.pred[[i]]<-data.frame(cbind(TW.situ.sub.xts[,i],TW.S1.sub[,i]))
    act.pred[[i]]<-act.pred[[i]][complete.cases(act.pred[[i]]),]
  }
  act.pred.list[[h]]<-act.pred
}




#Calculate RMSE and spearmans correlation, bias, unbiased RMSE per list
rmse.list<-lapply(act.pred.list, lapply,  function(x) round(sqrt(mean((x[,1] - x[,2])^2)),4))
rmse.list<-lapply(rmse.list,unlist)

cor.list<-lapply(act.pred.list, lapply, function(x) cor(x=x[,1], y=x[,2],use = "complete.obs", method = 'spearman'))
cor.list<-lapply(cor.list, lapply, function(x) round(as.numeric(x),3))
cor.list<-lapply(cor.list,unlist)

bias.list<-lapply(act.pred.list, lapply, function(x)  mean(x[,2]) - mean(x[,1]))## pred - act
bias.list<-lapply(bias.list, lapply, function(x) round(x,3))
bias.list<-lapply(bias.list,unlist)

#remove bias from predicted, then rmse
bias.list2<-lapply(act.pred.list, lapply, function(x)  mean(x[,2]) - mean(x[,1]))## pred - act
unb_rmse.list<-list()
unb_rmse<-list()
bias.usb<-list()
for(i in seq_along(act.pred.list)){
  act.pred.sub<-act.pred.list[[i]]
  bias.sub<-bias.list2[[i]]
  for(j in seq_along(act.pred.sub)){
    unb_rmse[[j]]<-act.pred.sub[[j]][,2]-bias.sub[[j]]
    act.pred.sub[[j]]$unb_pred<-unb_rmse[[j]]
  }
  unb_rmse.list[[i]]<-unb_rmse
  act.pred.list[[i]]<-act.pred.sub
}

unb_rmse.list<-lapply(act.pred.list,lapply, function(x) round(sqrt(mean((x[,1] - x[,3])^2)),4))
unb_rmse.list<-lapply(unb_rmse.list,unlist)


#PLOT 
pt.colors<-c(rep("royalblue3",4),rep("indianred2",4),rep("darkorchid2",3),"olivedrab4")
for(h in 1:length(tw.cdfm.all)){
  #pdf(paste("/media/coleen/DDrive1/1_OWASIS/4_Papers/traffic/figures/S1_vs_situ_Raam_",names(tw.cdfm.all[h]),"_v1.pdf", sep = ""),
  #   bg = "white",height=11,width=15)
  
  TW.S1.sub<-tw.cdfm.all[[h]]
  par(mfrow = c(3,3),mar = c(3,3,3,1), mgp = c(2,0.7,0))
  for(i in 1:ncol(TW.situ.sub.xts)){
    
    plot(as.vector(TW.situ.sub.xts[,i]),as.vector(TW.S1.sub[,i]), xlim = c(0, 0.5), ylim = c(0,0.5), pch = 19, ylab= "Soil Moisture S1",xlab= "Soil Moisture Raam Network",
         main = paste(names(tw.cdfm.all[[h]][,i]), names(tw.cdfm.all[h])),col = alpha(pt.colors[[h]],0.5))
    abline(0,1, lty = 2)
    text(x = 0.42,y = 0.08, labels = paste0("RMSE = ", rmse.list[[h]][[i]]))
    text(x = 0.42,y = 0.045, labels = paste0("Corr. (rho) = ", cor.list[[h]][[i]]))
    
  }
  #dev.off()
}




#---------------------------Plot RMSE trends for each station for varying pixel size-------------------
rmse.all<-data.frame(do.call("cbind",rmse.list))
colnames(rmse.all)<-names(tw.cdfm.all)
rmse.all$Station<-as.factor(names(tw.cdfm.all[[1]]))

cor.all<-data.frame(do.call("cbind",cor.list))
colnames(cor.all)<-names(tw.cdfm.all)
cor.all$Station<-as.factor(names(tw.cdfm.all[[1]]))

bias.all<-data.frame(do.call("cbind",bias.list))
colnames(bias.all)<-names(tw.cdfm.all)
bias.all$Station<-as.factor(names(tw.cdfm.all[[1]]))


unb_rmse.all<-data.frame(do.call("cbind",unb_rmse.list))
colnames(unb_rmse.all)<-names(tw.cdfm.all)
unb_rmse.all$Station<-as.factor(names(tw.cdfm.all[[1]]))


colors<-c("blue","green", "red")
a<-length(tw.cdfm.list2)
b<-length(tw.cdfm.fld.list2)
d<-length(tw.cdfm.fld.mean.list2)
c<-length(tw.cdfm.pt.center.list2)

##PLOT RMSE
#pdf(paste("/media/coleen/DDrive1/1_OWASIS/4_Papers/traffic/figures/S1_vs_situ_Raam_rmse_corr_v1.pdf", sep = ""),
#   bg = "white",height=11,width=15)
par(mfrow = c(4,1), mar = c(3,4,1,1), mgp = c(1.7,0.5,0), tck = -0.015)

plot(rmse.all[,1], pch =21, cex =2, bg = pt.colors[1], ylim = c(0.025,0.2), xlab = "", ylab = "RMSE m3/m3", cex.lab = 1.5,xaxt = "n")
for(i in 1:a){
  points(rmse.all[,i], pch= 20+i,cex=2,bg = alpha("royalblue3",1))
}
for(i in (a+1):(a+b)){
  points(rmse.all[,i], pch= 20+i-a,cex=2,bg = alpha("indianred2",1))
}
for(i in (a+b+1):(a+b+c)){
  points(rmse.all[,i], pch= 20+i-a-b,cex=2,bg = alpha("darkorchid2",1))
}

points(rmse.all[,12], pch= 21,cex=2, bg = alpha("olivedrab3",1))

legend(x = 11.5, y = 0.055, legend = colnames(rmse.all)[1:9], pch =c(rep(21:24,2),21), pt.bg = pt.colors, bty = "n", ncol =3,text.width= 0.92,
       y.intersp =1.1, x.intersp = 0.8, pt.cex = 1.2)

#PLOT BIAS
plot(bias.all[,1], pch =21, cex =2, ylim  = c(-0.05, 0.08),bg = colors[1],xlab = "", ylab = "Bias m3/m3", cex.lab = 1.5,
     xaxt = "n")
for(i in 1:a){
  points(bias.all[,i], pch= 20+i,cex=2,bg = "royalblue3")
}
for(i in (a+1):(a+b)){
  points(bias.all[,i], pch= 20+i-a,cex=2,bg = "indianred2")
}
for(i in (a+b+1):(a+b+c)){
  points(bias.all[,i], pch= 20+i-a-b,cex=2,bg = alpha("darkorchid2",1))
}
points(bias.all[,12], pch= 21,cex=2, bg = "olivedrab3")
#legend(x = 12, y = 0.07, legend = colnames(bias.all)[1:9], pch =c(rep(21:24,2),21), pt.bg = c(rep("red",4),rep("blue",4),"green"), bty = "n", ncol =3,text.width= 0.42,
#   y.intersp = 0.25, x.intersp = 0.13, cex = 1)

#dev.off()


#PLOT UnBIASED RMSE
plot(unb_rmse.all[,1], pch =21, cex =2, ylim = c(0.03,0.18),bg = colors[1],xlab = "", ylab = "Unbiased RMSE m3/m3", cex.lab = 1.5,
     xaxt = "n")
for(i in 1:a){
  points(unb_rmse.all[,i], pch= 20+i,cex=2,bg = "royalblue3")
}
for(i in (a+1):(a+b)){
  points(unb_rmse.all[,i], pch= 20+i-a,cex=2,bg = "indianred2")
}
for(i in (a+b+1):(a+b+c)){
  points(unb_rmse.all[,i], pch= 20+i-a-b,cex=2,bg = alpha("darkorchid2",1))
}
points(unb_rmse.all[,12], pch= 21,cex=2, bg = "olivedrab3")
#legend(x = 12, y = 0.07, legend = colnames(unb_rmse.all)[1:9], pch =c(rep(21:24,2),21), pt.bg = c(rep("red",4),rep("blue",4),"green"), bty = "n", ncol =3,text.width= 0.42,
#   y.intersp = 0.25, x.intersp = 0.13, cex = 1)

#dev.off()

#PLOT COR
plot(cor.all[,1], pch =21, cex =2, bg = colors[1], ylim = c(-0.2,0.8),xlab = "Raam Sation Number", ylab = "Spearman Cor (rho)", cex.lab = 1.5,
     xaxt = "n")
axis(side = 1, at=1:3,labels = colnames(TW.situ.sub.xts) )
for(i in 1:a){
  points(cor.all[,i], pch= 20+i,cex=2,bg = "royalblue3")
}
for(i in (a+1):(a+b)){
  points(cor.all[,i], pch= 20+i-a,cex=2,bg = "indianred2")
}
for(i in (a+b+1):(a+b+c)){
  points(cor.all[,i], pch= 20+i-a-b,cex=2,bg = alpha("darkorchid2",1))
}
points(cor.all[,12], pch= 21,cex=2, bg = "olivedrab3")
#legend(x = 12, y = 0.07, legend = colnames(cor.all)[1:9], pch =c(rep(21:24,2),21), pt.bg = c(rep("red",4),rep("blue",4),"green"), bty = "n", ncol =3,text.width= 0.42,
#   y.intersp = 0.25, x.intersp = 0.13, cex = 1)

#dev.off()




##Save df to file to save ram
write.csv(cor.all, file = "/media/coleen/DDrive/1_OWASIS/4_Papers/traffic/data/S1_valid_tables/cor.all.tw.sw.cdf.csv",row.names=FALSE)
write.csv(rmse.all, file = "/media/coleen/DDrive/1_OWASIS/4_Papers/traffic/data/S1_valid_tables/rmse.all.tw.sw.cdf.csv",row.names=FALSE)
write.csv(unb_rmse.all, file = "/media/coleen/DDrive/1_OWASIS/4_Papers/traffic/data/S1_valid_tables/unb_rmse.all.tw.sw.cdf.csv",row.names=FALSE)
write.csv(bias.all, file = "/media/coleen/DDrive/1_OWASIS/4_Papers/traffic/data/S1_valid_tables/bias.all.tw.sw.cdf.csv",row.names=FALSE)

