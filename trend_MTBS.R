


library(raster)
library(rgeos)
library(lwgeom)
library(sf)
library(sp)
library(cleangeo)
library(fasterize)
mtbs_dir="/Users/stijnhantson/Documents/data/MTBS/DATA/"
med=shapefile("/Users/stijnhantson/Documents/data/veg_california/med_cal.shp")
north=shapefile("/Users/stijnhantson/Documents/data/veg_california/north_cal.shp")


#basal=shapefile("/Users/stijnhantson/Documents/data/basal_area_reduction_fire_california/basal_area_reduction.shp")


year=1984
for (year in 1984:2011){
  basal=shapefile(paste("/Users/stijnhantson/Documents/data/basal_area_reduction_fire_california/annual/basal_area_",year,".shp",sep=""))
  basal$BAR=NA
  basal$BAR[basal$BURNSEV == 1] = 0
  basal$BAR[basal$BURNSEV == 2] = 5
  basal$BAR[basal$BURNSEV == 3] = 12.5
  basal$BAR[basal$BURNSEV == 4] = 37.5
  basal$BAR[basal$BURNSEV == 5] = 62.5
  basal$BAR[basal$BURNSEV == 6] = 82.5
  basal$BAR[basal$BURNSEV == 7] = 95
  basal$BAR[basal$BURNSEV == 255] = NA
  basal=spTransform(basal,crs(dnbr))
  basal=st_as_sf(basal)
  
  dir_b=paste("/Users/stijnhantson/Documents/data/basal_area_reduction_fire_california/basal_area_",year,".tif",sep="")
  bas_ras= fasterize(basal,dnbr,field="BAR",fun = "first")
  writeRaster(bas_ras,dir_b,overwrite=TRUE)
}



year=1984
k=0
dnbr_all_med=0
rdnbr_all_med=0
bas_all_med=0
dnbr_all_nor=0
rdnbr_all_nor=0
bas_all_nor=0

median_dnbr_all_med=0
median_rdnbr_all_med=0
median_bas_all_med=0

median_dnbr_all_nor=0
median_rdnbr_all_nor=0
median_bas_all_nor=0

q95_dnbr_all_med=0
q95_rdnbr_all_med=0
q95_bas_all_med=0

q95_dnbr_all_nor=0
q95_rdnbr_all_nor=0
q95_bas_all_nor=0

len_dnbr_med=0
len_rdnbr_med=0
len_dnbr_nor=0
len_rdnbr_nor=0
len_bas_med=0
len_bas_nor=0

year=1987
for (year in 1984:2017){
  med=shapefile("/Users/stijnhantson/Documents/data/veg_california/med_cal.shp")
  north=shapefile("/Users/stijnhantson/Documents/data/veg_california/north_cal.shp")
  print(year)
  k=k+1
  mtbs_dir_year = paste(mtbs_dir,year,"/",sep="")
  bndy_list = list.files(mtbs_dir_year, pattern = "burn_bndy.shp$", recursive = T, full.names=T)
  shapefile_list <- lapply(bndy_list, shapefile)
  #  fires <- do.call(rbind, shapefile_list)
  fires <- do.call(bind, shapefile_list)
  # fires1= st_make_valid(st_as_sf(fires))
  #  fires1= st_make_valid(st_polygon(fires))
  #  fires2=as(fires1, 'sf')
  # fires = sf:::as_Spatial(fires2)
  fires=gBuffer(fires, byid=TRUE,width=0)
  fires=gUnaryUnion(fires)
  fires=clgeo_Clean(fires)
  fire_north = intersect(fires,north)
  fire_med = intersect(fires,med)
  
  dnbr = raster(paste(mtbs_dir,year,"_dnbr.tif",sep=""))
  dnbr[dnbr < -2000] <- NA
  rdnbr = raster(paste(mtbs_dir,year,"_rdnbr.tif",sep=""))
  rdnbr[rdnbr < -2000] <- NA
  bas= raster(paste("/Users/stijnhantson/Documents/data/basal_area_reduction_fire_california/basal_area_",year,".tif",sep=""))
  bas[bas < -2000] <- NA
  
  dnbr_ext_med = extract(dnbr,fire_med)
  dnbr_ext_nor = extract(dnbr,fire_north)
  
  rdnbr_ext_med = extract(rdnbr,fire_med)
  rdnbr_ext_nor = extract(rdnbr,fire_north)
  
  bas_ext_med = extract(bas,fire_med)
  bas_ext_nor = extract(bas,fire_north)
  
  dnbr_all_med[k]=mean(unlist(dnbr_ext_med),na.rm=T)
  rdnbr_all_med[k]=mean(unlist(rdnbr_ext_med),na.rm=T)
  bas_all_med[k]=mean(unlist(bas_ext_med),na.rm=T)
  
  dnbr_all_nor[k]=mean(unlist(dnbr_ext_nor),na.rm=T)
  rdnbr_all_nor[k]=mean(unlist(rdnbr_ext_nor),na.rm=T)
  bas_all_nor[k]=mean(unlist(bas_ext_nor),na.rm=T)
  
  median_dnbr_all_med[k]=quantile(unlist(dnbr_ext_med),na.rm=T,c(.5))
  median_rdnbr_all_med[k]=quantile(unlist(rdnbr_ext_med),na.rm=T,c(.5))
  median_bas_all_med[k]=quantile(unlist(bas_ext_med),na.rm=T,c(.5))
  
  median_dnbr_all_nor[k]=quantile(unlist(dnbr_ext_nor),na.rm=T,c(.5))
  median_rdnbr_all_nor[k]=quantile(unlist(rdnbr_ext_nor),na.rm=T,c(.5))
  median_bas_all_nor[k]=quantile(unlist(bas_ext_nor),na.rm=T,c(.5))
  
  q95_dnbr_all_med[k]=quantile(unlist(dnbr_ext_med),na.rm=T,c(.95))
  q95_rdnbr_all_med[k]=quantile(unlist(rdnbr_ext_med),na.rm=T,c(.95))
  q95_bas_all_med[k]=quantile(unlist(bas_ext_med),na.rm=T,c(.95))
  
  q95_dnbr_all_nor[k]=quantile(unlist(dnbr_ext_nor),na.rm=T,c(.95))
  q95_rdnbr_all_nor[k]=quantile(unlist(rdnbr_ext_nor),na.rm=T,c(.95))
  q95_bas_all_nor[k]=quantile(unlist(bas_ext_nor),na.rm=T,c(.95))
  
  len_dnbr_med[k] = length(unlist(dnbr_ext_med))
  len_rdnbr_med[k] = length(unlist(rdnbr_ext_med))
  len_bas_med[k] = length(unlist(bas_ext_med))
  
  len_dnbr_nor[k] = length(unlist(dnbr_ext_nor))
  len_rdnbr_nor[k] = length(unlist(rdnbr_ext_nor))
  len_bas_nor[k] = length(unlist(bas_ext_nor))
  
  removeTmpFiles(0)
  gc()
}
years=(1984:2017)

data_dnbr = as.data.frame(cbind(years,len_bas_nor,median_bas_all_nor,bas_all_med,bas_all_nor,median_bas_all_med,q95_bas_all_med,q95_bas_all_nor,len_bas_med,len_dnbr_med,len_rdnbr_med,len_dnbr_nor,len_rdnbr_nor,dnbr_all_med,rdnbr_all_med,dnbr_all_nor,rdnbr_all_nor,median_dnbr_all_med,median_rdnbr_all_med,median_dnbr_all_nor,median_rdnbr_all_nor,q95_dnbr_all_med,q95_rdnbr_all_med,q95_dnbr_all_nor,q95_rdnbr_all_nor))
write.table(data_dnbr,"/Users/stijnhantson/Documents/projects/VIIRS_ros/mean_dnbr_trend.txt",sep = ",")
plot(data_dnbr$years,data_dnbr$q95_dnbr_all_nor)
plot(data_dnbr$len_dnbr_nor,data_dnbr$median_dnbr_all_nor)
plot(data_dnbr$years,data_dnbr$len_rdnbr_nor)

summary(lm(data_dnbr$bas_all_med~data_dnbr$years))

summary(lm(data_dnbr$dnbr_all_med~data_dnbr$years, weight=data_dnbr$len_dnbr_med))

par(mfrow=c(3,3), mai=c(0.3,.55,0.1,0.1))
plot(data_dnbr$dnbr_all_nor~data_dnbr$years, ylim=c(0,500), xlab="",ylab="mean dNBR")
plot(data_dnbr$rdnbr_all_nor~data_dnbr$years, ylim=c(0,900), xlab="",ylab="mean rdNBR")
plot(data_dnbr$bas_all_nor~data_dnbr$years, ylim=c(0,100), xlab="",ylab="mean mortality (%)")

plot(data_dnbr$median_dnbr_all_nor~data_dnbr$years, ylim=c(0,550), xlab="", ylab="median dNBR")
plot(data_dnbr$median_rdnbr_all_nor~data_dnbr$years, ylim=c(0,700), xlab="", ylab="median rdNBR")
plot(data_dnbr$median_bas_all_nor~data_dnbr$years, ylim=c(0,100), xlab="", ylab="median mortality (%)")

plot(data_dnbr$q95_dnbr_all_nor~data_dnbr$years, ylim=c(0,1000), xlab="year", ylab="q95 dNBR")
plot(data_dnbr$q95_rdnbr_all_nor~data_dnbr$years, ylim=c(0,3000), xlab="year", ylab="q95 rdNBR")
plot(data_dnbr$q95_bas_all_nor~data_dnbr$years, ylim=c(0,100), xlab="year", ylab="q95 mortality (%)")


par(mfrow=c(3,3), mai=c(0.3,.55,0.1,0.1))
plot(data_dnbr$dnbr_all_med~data_dnbr$years, ylim=c(0,500), xlab="",ylab="mean dNBR")
plot(data_dnbr$rdnbr_all_med~data_dnbr$years, ylim=c(0,1200), xlab="",ylab="mean rdNBR")
plot(data_dnbr$bas_all_med~data_dnbr$years, ylim=c(0,100), xlab="",ylab="mean mortality (%)")

plot(data_dnbr$median_dnbr_all_med~data_dnbr$years, ylim=c(0,500), xlab="", ylab="median dNBR")
plot(data_dnbr$median_rdnbr_all_med~data_dnbr$years, ylim=c(0,1200), xlab="", ylab="median rdNBR")
plot(data_dnbr$median_bas_all_med~data_dnbr$years, ylim=c(0,100), xlab="", ylab="median mortality (%)")

plot(data_dnbr$q95_dnbr_all_med~data_dnbr$years, ylim=c(0,1000), xlab="year", ylab="q95 dNBR")
plot(data_dnbr$q95_rdnbr_all_med~data_dnbr$years, ylim=c(0,3000), xlab="year", ylab="q95 rdNBR")
plot(data_dnbr$q95_bas_all_med~data_dnbr$years, ylim=c(0,100), xlab="year", ylab="q95 mortality (%)")


