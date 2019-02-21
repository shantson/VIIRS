
library(raster)
library(rgdal)
library(foreign)

viirs_dir = "/Users/stijnhantson/Documents/projects/VIIRS_ros/output/"
prism_dir = "/Users/stijnhantson/Documents/data/PRISM/"

frap=shapefile("/Users/stijnhantson/Documents/data/FRAP/firep17_1.shp")

viirs_list =  list.files(viirs_dir, pattern = "ros_daily.shp$", recursive = TRUE, full.names=T)
viirs_dbf =  list.files(viirs_dir, pattern = "ros_daily.dbf$", recursive = TRUE, full.names=T)

viirs_all=shapefile(viirs_list[1])
result = viirs_all[viirs_all$YYYYMMDD==0,]
daily_res = c(0,0,0,0,0,0,0)

p=1  
    for (p in 1:length(viirs_list)){

rows_p = read.dbf(viirs_dbf[p])

if (length(rows_p$YYYYMMDD)>0){
      
viirs_all=shapefile(viirs_list[p])

dar = as.Date(viirs_all$dat) - 1      ####### time is in UTM, so we have the nighttime day as the day after which 
viirs_all$date =format(dar,"%Y%m%d")
#summary(viirs_all)


#plot(viirs_all$ros~viirs_all$FRP)
#plot(log(viirs_all$ros),log(viirs_all$FRP))

days1 = unique(viirs_all$date) 
i=1
for (i in 1:length(days1)){

viirs1 = viirs_all[viirs_all$date == days1[i],]  
  
l_precip= raster(list.files(paste(prism_dir,"precip/", sep=""), pattern = paste(as.character(days1[i]),"_bil.bil$",sep=""), recursive = TRUE, full.names=T))
l_tmax= raster(list.files(paste(prism_dir,"tmax/", sep=""), pattern = paste(as.character(days1[i]),"_bil.bil$",sep=""), recursive = TRUE, full.names=T))
l_tmean= raster(list.files(paste(prism_dir,"tmean/", sep=""), pattern = paste(as.character(days1[i]),"_bil.bil$",sep=""), recursive = TRUE, full.names=T))
l_vpdmax= raster(list.files(paste(prism_dir,"vpdmax/", sep=""), pattern = paste(as.character(days1[i]),"_bil.bil$",sep=""), recursive = TRUE, full.names=T))

precip = as.data.frame(extract(l_precip, viirs1))
tmax = as.data.frame(extract(l_tmax, viirs1))
tmean = as.data.frame(extract(l_tmean, viirs1))
vpdmax = as.data.frame(extract(l_vpdmax, viirs1))

te =as.matrix(cbind(precip,tmax,tmean,vpdmax))
colnames(te) = c("precip","tmax","tmean","vpdmax")

viirs2 = cbind(viirs1,te)

mean_precip = mean(extract(l_precip, viirs1))
mean_tmax = mean(extract(l_tmax, viirs1))
mean_tmean = mean(extract(l_tmax, viirs1))
mean_vpdmax = mean(extract(l_tmax, viirs1))
mean_ros = mean(viirs1$ros)
max_ros = max(viirs1$ros)
median95_ros = quantile(viirs1$ros, 0.95)
dail = cbind(mean_precip,mean_tmax,mean_tmean,mean_vpdmax, mean_ros,max_ros,median95_ros)
daily_res = rbind(daily_res, dail)
result = rbind(result, viirs2)

}
}
}    
  
  


########################  analysis of surface burnt  ###################


viirs_dir = "/Users/stijnhantson/Documents/projects/VIIRS_ros/output/"
frap=shapefile("/Users/stijnhantson/Documents/data/FRAP/firep17_1.shp")

viirs_list =  list.files(viirs_dir, pattern = "_daily.shp$", recursive = TRUE, full.names=T)
viirs_dbf =  list.files(viirs_dir, pattern = "ros_daily.dbf$", recursive = TRUE, full.names=T)

viirs_all=shapefile(viirs_list[1])
result = viirs_all[viirs_all$YYYYMMDD==0,]
daily_res = c(0,0,0,0,0,0,0)

p=1  
for (p in 1:length(viirs_list)){
  rows_p = read.dbf(viirs_dbf[p])
  
  if (length(rows_p$YYYYMMDD)>0){
   
     viirs_all=shapefile(viirs_list[p])
    


  }
}

  