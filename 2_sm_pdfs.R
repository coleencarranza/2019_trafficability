##-------------------------------Soil moisture pdfs using Traffic datasets----------------------------------
require("graphics");library(RColorBrewer);library(scales);library(plyr);library(ecp)
library(lubridate);library(gdata)

setwd("/media/coleen/DDrive1/1_OWASIS/2_Statistical_analysis/penlog/ts_select_couple/")
rm_all_daily<-list.files(pattern = "_day.csv")

rm_all.list<-lapply(rm_all_daily,function(x) read.csv(x, header=T,stringsAsFactors = F)  )
rm.names<-substr(rm_all_daily,1,4)
rm_all.list<-setNames(rm_all.list,rm.names)
rm_all.list<-lapply(rm_all.list,function(x) {x[, "date"] <- as.POSIXct(x[, "date"],
                                                                       format = "%Y-%m-%d %H:%M:%S",tz = "Europe/Amsterdam");x})
rm_all.list<-lapply(rm_all.list,function(x) {x$mon <- month(x[,"date"]);x})

###TWENTE has difference format for separate the list
tw.vwc.list<-rm_all.list[c(7:9)]
rm_all.list<-rm_all.list[c(1:6)]
##subset each dataframe for only 5,10,20 cm SM#######
rm.vwc.list<-lapply(rm_all.list, function(x) x[,c(1:6,19)])
tw.vwc.list<-lapply(tw.vwc.list, function(x) x[,c(1:2,4,6,8,10,12)])

####Merge two lists to create groups based on sandy soil only!!!
##ITC10 recently changed position. Not confident on the continuity of trends there
##ITC07 is from different soil type..
##add only ITC02 to stations in the Raam
sandy.vwc.list<-do.call(c, list(rm.vwc.list,tw.vwc.list[-c(2:3)]))

##If not removing anything
#sandy.vwc.list<-do.call(c, list(rm.vwc.list,tw.vwc.list))

#Combine the list
sandy.df<-do.call("rbind",sandy.vwc.list)
sandy.df<-sandy.df[complete.cases(sandy.df),]
a<-100*sandy.df[2:6]
sandy.df<-cbind(sandy.df[1],100*sandy.df[2:6],sandy.df[7])
#Split by month
sandy.mon<-split(sandy.df,sandy.df$mon)


#plot histograms - 5cm
par(mfrow = c(3,4), mar = c(4,4,2,4) + 0.01,mgp=c(1.7,0.5,0), tck=-0.02)
for (i in seq_along(sandy.mon)){
   hist(sandy.mon[[i]]$VWC_5cm, col = "grey", border = "black",freq = F,xlab = "Soil moisture 5cm depth ",ylim = c(0,0.09),xlim = c(0,60),
   main=bquote("Month" == .(month.abb[unique(sandy.mon[[i]]$mon)])))
  lines(density(sandy.mon[[i]]$VWC_5cm, from = 0, to = 50,na.rm = T, bw = 5))  
}


#plot histograms - 20cm
par(mfrow = c(3,4), mar = c(4,4,2,4) + 0.01,mgp=c(1.7,0.5,0), tck=-0.02)
for (i in seq_along(sandy.mon)){
  hist(sandy.mon[[i]]$VWC_20cm, xlim = c(0,0.5),col = "grey", border = "grey",freq = F,xlab = "Soil moisture 20cm depth ", ylim= c(0, 12),
       main=bquote("Month" == .(month.abb[unique(sandy.mon[[i]]$mon)])))
  lines(density(sandy.mon[[i]]$VWC_20cm, from = 0, to = 0.5,na.rm = T, bw = 0.05))  
}
