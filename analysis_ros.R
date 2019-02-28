
library(raster)
library(rgdal)
library(foreign)

viirs_dir = "/Users/stijnhantson/Documents/projects/VIIRS_ros/final_results/"
prism_dir = "/Users/stijnhantson/Documents/data/PRISM/"
ncep_dir = "/Users/stijnhantson/Documents/data/daily_ncdp_wind/"

frap=shapefile("/Users/stijnhantson/Documents/data/FRAP/firep17_1.shp")

viirs_list =  list.files(viirs_dir, pattern = "daily_ros.shp$", recursive = TRUE, full.names=T)
viirs_dbf =  list.files(viirs_dir, pattern = "daily_ros.dbf$", recursive = TRUE, full.names=T)

viirs_all=shapefile(viirs_list[1])
result = viirs_all[viirs_all$YYYYMMDD==0,]
daily_res = c(0,0,0,0,0,0,0,0)

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
l_wind = raster(list.files(ncep_dir, pattern = days1[i], recursive = TRUE, full.names=T))

precip = as.data.frame(extract(l_precip, viirs1))
tmax = as.data.frame(extract(l_tmax, viirs1))
tmean = as.data.frame(extract(l_tmean, viirs1))
vpdmax = as.data.frame(extract(l_vpdmax, viirs1))
windspeed = as.data.frame(extract(l_wind, viirs1))

te =as.matrix(cbind(precip,tmax,tmean,vpdmax,windspeed))
colnames(te) = c("precip","tmax","tmean","vpdmax","wind")

viirs2 = cbind(viirs1,te)

mean_precip = mean(extract(l_precip, viirs1))
mean_tmax = mean(extract(l_tmax, viirs1))
mean_tmean = mean(extract(l_tmean, viirs1))
mean_vpdmax = mean(extract(l_vpdmax, viirs1))
mean_windspeed = mean(extract(l_wind, viirs1))
mean_ros = mean(viirs1$ros)
max_ros = max(viirs1$ros)
median95_ros = quantile(viirs1$ros, 0.95)
dail = cbind(mean_precip,mean_tmax,mean_tmean,mean_vpdmax,mean_windspeed, mean_ros,max_ros,median95_ros)
daily_res = rbind(daily_res, dail)
result = rbind(result, viirs2)

}
}
}    
  
rd="/Users/stijnhantson/Downloads/fveg15_1"


subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb)
print(fc_list)

# Read the feature class
fc <- readOGR(dsn=fgdb,layer="some_featureclass")

# Determine the FC extent, projection, and attribute information
summary(fc)


writeOGR(result, "/Users/stijnhantson/Documents/projects/VIIRS_ros/", layer= "all_ros_meteo", driver="ESRI Shapefile", overwrite_layer = T)
write.table(daily_res, "/Users/stijnhantson/Documents/projects/VIIRS_ros/daily_mean_ros_meteo.txt",sep="\t")

res=as.data.frame(daily_res)

plot(res$mean_tmean,log10(res$median95_ros+1))

min(res$median95_ros)


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




#######################  calculate wind speed   #########################
library(ncdf4)

mon = list.files("/Users/stijnhantson/Documents/data/ncep_hoursly_wind/", full.names = TRUE, recursive = TRUE)
outdir = "/Users/stijnhantson/Documents/data/daily_ncdp_wind/"


len3 = length(mon)
tm=1

s=as.list(0)
day = 1

for (tm in 1:len3){
  s=as.list(0)

nr = nchar(mon[tm])
year = substr(mon[tm], nr-13,nr-10)
month = substr(mon[tm], nr-9,nr-8)

nc_inf = nc_open(mon[tm])
hor = length(ncvar_get(nc_inf,"time"))
hr = 0
hors= 0
day = 0
for (hors in 1:hor){

pr1 = raster(mon[tm],varname = "V_GRD_L103",band = hors)
pr2 = raster(mon[tm],varname = "U_GRD_L103",band = hors)

wind = sqrt((pr1*pr1)+(pr2*pr2))
s = as.list(wind,s)

hr = hr+1  
if (hr == 24){
  hr = 0
  day = day +1
  max_wind = max(stack(s))
  mean_wind = mean(stack(s))
  s=as.list(0)
  
  if (day<10){
    days = paste("0",day,sep="")
  }else{days=day}
  
  maxname = paste(outdir,year, month, days, "_maxwind.tif",sep="")
  meanname = paste(outdir,year, month, days, "_meanwind.tif",sep="")
  writeRaster(max_wind,maxname,overwrite=TRUE)
  writeRaster(mean_wind,meanname,overwrite=TRUE) 
}
  

}
}










  