
library(raster)
library(rgdal)
library(foreign)

viirs_dir = "/Users/stijnhantson/Documents/projects/VIIRS_ros/final_results8/"
frap=shapefile("/Users/stijnhantson/Documents/data/FRAP/fire18_1.shp")
frap2=shapefile("/Users/stijnhantson/Documents/data/FRAP/FIREP18_DRAFT_DO_NOT_DISTRIBUTE/FIREP18_DRAFT_DO_NOT_DISTRIBUTE.shp")

viirs_list =  list.files(viirs_dir, pattern = "daily_ros.shp$", recursive = TRUE, full.names=T)
viirs_dbf =  list.files(viirs_dir, pattern = "daily_ros.dbf$", recursive = TRUE, full.names=T)

viirs_all=shapefile(viirs_list[1])
result = viirs_all[viirs_all$YYYYMMDD==0,]
p=1
m=0
morning_mean=0
morning_max=0
morning_frp=0
after_mean=0
after_max=0
after_frp=0


for (p in 1:length(viirs_list)){
  print(c("fire number", p))
rows_p = read.dbf(viirs_dbf[p])
firename = substring(viirs_list[p],71,nchar(viirs_list[p])-14)
year1 = substring(viirs_list[p],66,69)

if (length(rows_p$YYYYMMDD)>0){

  viirs_all=shapefile(viirs_list[p])
  viirs_all$DOY2 = viirs_all$DOY - 0.6
  viirs_all$un = as.integer(viirs_all$DOY2)
  viirs_all$dec_doy = viirs_all$DOY2 - viirs_all$un
 un = unique(as.integer(viirs_all$DOY2))
  i=1
 for (i in 1:length(un)){
   viirs_da = viirs_all[viirs_all$un == un[i],]
   morning = viirs_da[viirs_da$dec_doy < 0.5,]
   afternoon = viirs_da[viirs_da$dec_doy > 0.5,]
   if (length(morning) > 0 & length(afternoon) > 0){
     m=m+1
     morning_mean[m] = mean(morning$ros)
     morning_max[m] = quantile(morning$ros, 0.95)
     morning_frp[m] = mean(morning$FRP)
     after_mean[m] = mean(afternoon$ros)
     after_max[m] = quantile(afternoon$ros, 0.95)
     after_frp[m] = mean(afternoon$FRP)
   }
 }
}
}

mean(morning_mean)
mean(morning_max)
mean(morning_frp)

mean(after_mean)
mean(after_max)
mean(after_frp)

hist(after_max-morning_max, xlim=c(-1000,1000))


boxplot(morning_max+1,after_max+1, log="y", ylab="Rate-of-spread (m/h)",names=c("morning","afternoon"), cex.lab=1.4,cex.axis = 1.3)


boxplot(morning_frp+1,after_frp+1, log="y", ylab="FRP (MW)",names=c("1:30PM","1:30AM"), cex.lab=1.4,cex.axis = 1.3)

