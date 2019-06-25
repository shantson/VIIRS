
library(raster)
library(rgdal)
library(foreign)

viirs_dir = "/Users/stijnhantson/Documents/projects/VIIRS_ros/final_results4/"
prism_dir = "/Users/stijnhantson/Documents/data/PRISM/"
ncep_dir = "/Users/stijnhantson/Documents/data/daily_ncdp_wind/"

frap=shapefile("/Users/stijnhantson/Documents/data/FRAP/firep17_1.shp")
frap2=shapefile("/Users/stijnhantson/Documents/data/FRAP/FIREP18_DRAFT_DO_NOT_DISTRIBUTE/FIREP18_DRAFT_DO_NOT_DISTRIBUTE.shp")

viirs_list =  list.files(viirs_dir, pattern = "daily_ros.shp$", recursive = TRUE, full.names=T)
viirs_dbf =  list.files(viirs_dir, pattern = "daily_ros.dbf$", recursive = TRUE, full.names=T)

viirs_all=shapefile(viirs_list[1])
result = viirs_all[viirs_all$YYYYMMDD==0,]
daily_res = c(0,0,0,0,0,0,0,0,0,0,0,0)

p=1  

for (p in 192:length(viirs_list)){

  rows_p = read.dbf(viirs_dbf[p])
  firename = substring(viirs_list[p],71,nchar(viirs_list[p])-14)
  year1 = substring(viirs_list[p],66,69)

    if (length(rows_p$YYYYMMDD)>0){
      
viirs_all=shapefile(viirs_list[p])

frr=subset(frap,YEAR_ == year1)
frr = subset(frr, FIRE_NAME == firename)
if ((length(frr$FIRE_NAME)) == 0){
  frr = subset(frap2, FIRE_NAME == firename)
}

cause = frr$CAUSE
startdate =  as.Date(frr$ALARM_DATE)
start_doy=as.numeric( strftime( startdate,format = "%j"))
viirs_all$DOY2 = as.integer(viirs_all$DOY - 0.5)
if (is.na(start_doy)){
  start_doy=min(viirs_all$DOY2)
}

dar = as.Date(viirs_all$dat) - 1      ####### time is in UTM, so we have the nighttime day as the day after which 
viirs_all$date =format(dar,"%Y%m%d")
viirs_all$dayfire = viirs_all$DOY2- start_doy
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
l_wind = raster((list.files(ncep_dir, pattern = days1[i], recursive = TRUE, full.names=T)[1]))
xmin(l_wind)= -125.0796
xmax(l_wind)= -95.0114

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

day_of_fire = viirs1$dayfire[1]

dail = cbind(firename,cause,startdate,day_of_fire,mean_precip,mean_tmax,mean_tmean,mean_vpdmax,mean_windspeed, mean_ros,max_ros,median95_ros)
daily_res = rbind(daily_res, dail)
result = rbind(result, viirs2)

}
}
}    
  

writeOGR(result, "/Users/stijnhantson/Documents/projects/VIIRS_ros/", layer= "all_ros_meteo", driver="ESRI Shapefile", overwrite_layer = T)
write.table(daily_res, "/Users/stijnhantson/Documents/projects/VIIRS_ros/daily_mean_ros_meteo.txt",sep="\t", overwrite=T)

daily_res=read.table("/Users/stijnhantson/Documents/projects/VIIRS_ros/daily_mean_ros_meteo.txt",header=T,row.names=NULL)

res=as.data.frame(daily_res)

res$tempbin[res$mean_tmax < 20] = 17.5
res$tempbin[res$mean_tmax >= 20 & res$mean_tmax < 25 ] = 22.5
res$tempbin[res$mean_tmax >= 25 & res$mean_tmax < 30 ] = 27.5
res$tempbin[res$mean_tmax >= 30 & res$mean_tmax < 35 ] = 32.5
res$tempbin[res$mean_tmax >= 35] = 37.5

res$vpdbin[res$mean_vpdmax < 20] = 15
res$vpdbin[res$mean_vpdmax >= 20 & res$mean_vpdmax < 30 ] = 25
res$vpdbin[res$mean_vpdmax >= 30 & res$mean_vpdmax < 40 ] = 35
res$vpdbin[res$mean_vpdmax >= 40] = 45

boxplot(log10(res$max_ros +1)~res$tempbin, xlab="maximum temperature (deg. C)", ylab="rate-of-spread(log10 m/hr.)")
boxplot(log10(res$max_ros+1)~res$vpdbin, xlab="maximum VPD (hPa)", ylab="rate-of-spread(log10 m/hr.)")

plot(res$mean_tmax,log10(res$median95_ros+1), xlab="Mean temperature (deg.C)", ylab="Rate-of-Spread (log10 m/hr.)")
plot(res$mean_windspeed,log10(res$median95_ros+1),xlab="Maximum Windspeed (m/s)", ylab="Rate-of-Spread (log10 m/hr.)")

plot(res$mean_vpdmax ,log10(res$median95_ros+1),xlab="Maximum VPD (hPa)", ylab="Rate-of-Spread (log10 m/hr.)")
plot(res$mean_tmean ,log10(res$median95_ros+1),xlab="Maximum Windspeed (m/s)", ylab="Rate-of-Spread (log10 m/hr.)")

summary(lm(log10(res$median95_ros+1)~res$mean_vpdmax + res$mean_tmax + res$mean_windspeed))

summary(lm(log10(res$median95_ros+1)~res$mean_windspeed+res$mean_tmean))
min(res$median95_ros)


########################  analysis of surface burnt  ###################


viirs_dir = "/Users/stijnhantson/Documents/projects/VIIRS_ros/final_results4/"
frap=shapefile("/Users/stijnhantson/Documents/data/FRAP/firep17_1.shp")
frap2=shapefile("/Users/stijnhantson/Documents/data/FRAP/FIREP18_DRAFT_DO_NOT_DISTRIBUTE/FIREP18_DRAFT_DO_NOT_DISTRIBUTE.shp")
frap2=subset(frap2, GIS_ACRES>1000)

frap=subset(frap,YEAR_ > 2011)
frap=subset(frap, GIS_ACRES>1000)
viirs_list =  list.files(viirs_dir, pattern = "_daily.shp$", recursive = TRUE, full.names=T)
viirs_dbf =  list.files(viirs_dir, pattern = "ros_daily.dbf$", recursive = TRUE, full.names=T)
size_dat = c(0,0,0,0,0)
size_dat_hum = c(0,0,0,0,0)
size_dat_lig = c(0,0,0,0,0)
trala = 1
for (trala in 1:length(viirs_list)){
  viirs=shapefile(viirs_list[trala])
  cause=viirs$CAUSE[1]
  firename = substring(viirs_list[trala],71,nchar(viirs_list[trala])-10)
  year1 = substring(viirs_list[trala],66,69)
  
  frr=subset(frap,YEAR_ == year1)
 frr = subset(frr, FIRE_NAME == firename)
 if ((length(frr$FIRE_NAME)) == 0){
   frr = subset(frap2, FIRE_NAME == firename)
 }
   
 startdate =  as.Date(frr$ALARM_DATE)
 start_doy=as.numeric( strftime( startdate,format = "%j"))
 viirs$DOY2 = as.integer(viirs$DOY - 0.5)
 if (is.na(start_doy)){
   start_doy=min(viirs$DOY2)
 }
 viirs$area <- area(viirs)/1000000
 qr=1
 size=0
 for (qr in 1:5){
   day_s = start_doy+(qr-1)
   vi = viirs[viirs$DOY2 == day_s,]
  size[qr] = vi$area[1]
 }
 dif = size[5]-size[1]
 
 print(paste(trala,firename,sep=" "))
 print(dif)
 
 size_dat=rbind(size_dat,size)
 if (cause == 1){
    size_dat_lig =rbind(size_dat_lig,size)
 }else if(cause != 14) {
    size_dat_hum =rbind(size_dat_hum,size)
 }
}
size_dat=as.data.frame(size_dat)
size_dat_lig=as.data.frame(size_dat_lig)
size_dat_hum=as.data.frame(size_dat_hum)
colnames(size_dat)=c("day1","day2","day3","day4","day5")
colnames(size_dat_lig)=c("day1","day2","day3","day4","day5")
colnames(size_dat_hum)=c("day1","day2","day3","day4","day5")


ylab.text = expression('fire size (km'^"2"*')') 
boxplot(size_dat,ylim=c(0,900), ylab= "")
mtext(ylab.text,side=2, line =2.5)
boxplot(size_dat_hum,ylim=c(0,700), ylab= "")
mtext(ylab.text,side=2, line =2.5)
boxplot(size_dat_lig,ylim=c(0,700), ylab= "")
mtext(ylab.text,side=2, line =2.5)

plot(t(size_dat[2,]),ylim=c(0,100))
points(t(size_dat[4,]))

plot(log10(size_dat$day1),log10(size_dat$day5), xlab = "fire size day1 (log10(km2))",ylab = "fire size day5 (log10(km2))",cex.lab =1.2, ylim=c(-1.5,4), xlim=c(-1.5,4))

summary(size_dat$day5-size_dat$day1)


plot(t(size_dat_hum[2,]),ylim=c(0,300), type = "l",ylab="",xaxt = "n",cex=1.3,xlab="")
mtext(ylab.text,side=2, line =2.5,cex=1.3)
for (k in 1:length(size_dat_hum[,1])){
  lines(t(size_dat_hum[k,]), col = colors()[k])
}
axis(1, labels = as.character(c("day1","day2","day3","day4","day5")), at = c(1:5),cex=1.3)

plot(t(size_dat_lig[2,]),ylim=c(0,300), type = "l",ylab="",xaxt = "n",cex=1.3,xlab="")
mtext(ylab.text,side=2, line =2.5,cex=1.3)
for (k in 1:length(size_dat_lig[,1])){
  lines(t(size_dat_lig[k,]), col = colors()[k])
}
axis(1, labels = as.character(c("day1","day2","day3","day4","day5")), at = c(1:5),cex=1.3)

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



############################################################################################################

################  extract data from gridmet  #####################################




viirs_dir = "/Users/stijnhantson/Documents/projects/VIIRS_ros/final_results4/"
gridmet_dir = "/Users/stijnhantson/Documents/data/gridmet/"


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
    
    dar = as.Date(viirs_all$dat) ####### time is in UTM, so we have the nighttime day as the day after which 
    viirs_all$date =format(dar,"%Y%m%d")
    viirs_all$DOY2 = as.integer(viirs_all$DOY - 0.5)
    #summary(viirs_all)
    year = substring(dar[1],1,4)
    
    #plot(viirs_all$ros~viirs_all$FRP)
    #plot(log(viirs_all$ros),log(viirs_all$FRP))
    
    days1 = unique(viirs_all$DOY2) 
    i=1
    for (i in 1:length(days1)){
     
      viirs1 = viirs_all[viirs_all$DOY2 == days1[i],]  
      
      gridmet_list= list.files(gridmet_dir, pattern = paste(year,".nc$",sep=""), recursive = TRUE, full.names=T)
      
      l_bi = raster(gridmet_list[1],band = days1[i] )#Burning index
      l_erc = raster(gridmet_list[2],band = days1[i] )#energy release component
      l_etr = raster(gridmet_list[3],band = days1[i] )#evapotranspiration
      l_fm100 = raster(gridmet_list[4],band = days1[i] )
      l_fm1000 = raster(gridmet_list[5],band = days1[i] )
      l_pet = raster(gridmet_list[7],band = days1[i] )#evapotranspiration
      l_pr = raster(gridmet_list[8],band = days1[i] )#precipitation
      l_rmax = raster(gridmet_list[9],band = days1[i] )#relative humididty max
      l_rmin = raster(gridmet_list[10],band = days1[i] )#relative humididty min
      l_sph = raster(gridmet_list[11],band = days1[i] )#mean specific humidity
      l_th = raster(gridmet_list[12],band = days1[i] )#wind direction
      l_tmmn = raster(gridmet_list[13],band = days1[i] ) #min temp
      l_tmmx = raster(gridmet_list[14],band = days1[i] ) #max temp
      l_vpd = raster(gridmet_list[15],band = days1[i] )#mean vapor pressure deficit
      l_vs = raster(gridmet_list[16],band = days1[i] )#windspeed
      
      crs(l_bi) = CRS("+init=epsg:4326")
      crs(l_erc) = CRS("+init=epsg:4326")
      crs(l_etr) = CRS("+init=epsg:4326")
      crs(l_fm100)= CRS("+init=epsg:4326")
      crs(l_fm1000)= CRS("+init=epsg:4326")
      crs(l_pet)= CRS("+init=epsg:4326")
      crs(l_pr)= CRS("+init=epsg:4326")
      crs(l_rmax)= CRS("+init=epsg:4326")
      crs(l_rmin)= CRS("+init=epsg:4326")
      crs(l_sph)= CRS("+init=epsg:4326")
      crs(l_th)= CRS("+init=epsg:4326")
      crs(l_tmmn)= CRS("+init=epsg:4326")
      crs(l_tmmx)= CRS("+init=epsg:4326")
      crs(l_vpd)= CRS("+init=epsg:4326")
      crs(l_vs)= CRS("+init=epsg:4326")
      
      bi = as.data.frame(extract(l_bi, viirs1))
      erc = as.data.frame(extract(l_erc, viirs1))
      etr = as.data.frame(extract(l_etr, viirs1))
      fm100 = as.data.frame(extract(l_fm100, viirs1))
      fm1000 = as.data.frame(extract(l_fm1000, viirs1))
      pet = as.data.frame(extract(l_pet, viirs1))
      pr = as.data.frame(extract(l_pr, viirs1))
      rmax = as.data.frame(extract(l_rmax, viirs1))
      rmin = as.data.frame(extract(l_rmin, viirs1))
      th = as.data.frame(extract(l_th, viirs1))
      tmmn = as.data.frame(extract(l_tmmn, viirs1))
      tmmx = as.data.frame(extract(l_tmmx, viirs1))
      vpd = as.data.frame(extract(l_vpd, viirs1))
      vs = as.data.frame(extract(l_vs, viirs1))
      
      te =as.matrix(cbind(bi,erc,etr,fm100,fm1000,pet,pr,rmax,rmin,th,tmmn,tmmx,vpd,vs))
      colnames(te) = c("bi","erc","etr","fm100","fm1000","pet","pr","rmax","rmin","th","tmmn","tmmx","vpd","vs")
      
      viirs2 = cbind(viirs1,te)
      
      
      bi = mean(bi[,1])
      erc = mean(erc[,1])
      etr = mean(etr[,1])
      fm100 = mean(fm100[,1])
      fm1000 = mean(fm1000[,1])
      pet = mean(pet[,1])
      pr = mean(pr[,1])
      rmax = mean(rmax[,1])
      rmin = mean(rmin[,1])
      th = mean(th[,1])
      tmmn = mean(tmmn[,1])
      tmmx = mean(tmmx[,1])
      vpd = mean(vpd[,1])
      vs = mean(vs[,1])

      
      mean_ros = mean(viirs1$ros)
      max_ros = max(viirs1$ros)
      median95_ros = quantile(viirs1$ros, 0.95)
      dail = cbind(mean_ros,max_ros,median95_ros,bi,erc,etr,fm100,fm1000,pet,pr,rmax,rmin,th,tmmn,tmmx,vpd,vs)
      daily_res = rbind(daily_res, dail)
      result = rbind(result, viirs2)
      
    }
  }
}    


writeOGR(result, "/Users/stijnhantson/Documents/projects/VIIRS_ros/", layer= "all_ros_meteo", driver="ESRI Shapefile", overwrite_layer = T)
write.table(daily_res, "/Users/stijnhantson/Documents/projects/VIIRS_ros/daily_mean_ros_meteo.txt",sep="\t")

daily_res=read.table("/Users/stijnhantson/Documents/projects/VIIRS_ros/daily_mean_ros_meteo.txt",header=T)
res=as.data.frame(daily_res)

summary(res)


summary(lm(log(res$median95_ros) ~ res$vpd + res$bi, na.omit=T ))
plot(res$vpd,res$median95_ros, ylim=c(0,600))



plot(res$vpd,log10(res$median95_ros+1))
plot(vpd,log10(median95_ros+1),data=na.omit(res))


res$logros = log10(res$median95_ros)
res$logros[is.infinite(res$logros)] = NA
cor(na.omit(res))



res$logros = log10(res$median95_ros)
res$logros[is.infinite(res$logros)] = NA

res1=na.omit(res)
res1=res1[-1,]
cor(na.omit(res1))

summary(lm(res1$logros~res1$vpd + res1$bi, na.rm=T))

plot(res1$vpd,log10(res1$median95_ros))
plot(res1$vpd,res1$median95_ros)


