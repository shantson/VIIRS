

library(rgdal)
library(raster)
library(rgeos)
library(grDevices)
library(alphahull)
library(igraph)
library(geosphere)
library(foreach)
library(doParallel)
library(png)
library(maptools)

gridmet_dir = "/Users/stijnhantson/Documents/data/gridmet/"
mod = raster("/Users/stijnhantson/Documents/data/MCD64_v6/Win03/2000/MCD64monthly.A2000336.Win03.006.burndate.tif")
viirs_dir="/Users/stijnhantson/Documents/data/VIIRS/global_archive"
shape1 <- readOGR("/Users/stijnhantson/Documents/data/FRAP/fire18_1.shp") #readin FRAP fire perimeter data

man1 <- readOGR("/Users/stijnhantson/Documents/projects/VIIRS_ros/management_2018.shp")


shape1$YEAR=as.numeric(as.character(shape1$YEAR_)) 
man1$YEAR=as.numeric(as.character(man1$YEAR_)) 

lonlat = crs("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
TA <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +datum=NAD83 +units=m +ellps=GRS80")
dom = extent(-125,-114,32,42)
#mod=crop(mod,dom)
#mod1=projectRaster(mod,crs =TA,method="ngb",res=c(500,500))
#mod1[] = NA
sp=1

year=2018

shape2 <- subset(shape1, YEAR==year)  #extract fires which occured during the year of interest
crs_frap=crs(shape1)

dr = paste(year,"[0-9][0-9].C1.02.txt$" ,sep="")
viirs_list = list.files(viirs_dir, pattern = dr, recursive = TRUE, full.names=T)

dat=read.table(viirs_list[1],sep=",",header=T)
dat_v=dat[dat$lon < -114 ,]
for (i in 2:length(viirs_list)){
  dat=read.table(viirs_list[i],sep=",",header=T)
  dat=dat[dat$lon < -114 ,]
  dat_v=rbind(dat_v,dat)
}
ds=dat_v
dat_v=dat_v[dat_v$lon < -114 ,]
dat_v=dat_v[dat_v$lon > -125 ,]
dat_v=dat_v[dat_v$lat > 32 ,]
dat_v=dat_v[dat_v$lat < 42 ,]

dat_v$FIRE_NUM=1
dat_v=dat_v[as.character(dat_v$conf) != "     low",]
dat_v=dat_v[dat_v$pixarea < 0.25,]
x=dat_v$lon
y=dat_v$lat
po2=SpatialPointsDataFrame(cbind(x,y),dat_v,proj4string=lonlat)
po2=spTransform(po2,TA)

shape2=spTransform(shape2,TA)
shape2=subset(shape2, GIS_ACRES>1000)
crs(shape2)=crs(po2)
po2$dat = as.Date(as.character(po2$YYYYMMDD), format= "%Y%m%d")
nr_fire = 1


fire1 = gBuffer(shape2,width = 750) #extract all VIIRS points within a 750m buffer arround the perimeter
pts_in = po2[!is.na(over(po2,as(fire1,"SpatialPolygons"))),]
dar = as.Date(pts_in$dat)
pts_in$date =format(dar,"%Y%m%d")
pts_in$DOY = as.numeric( strftime(pts_in$dat,format = "%j"))
pts_in$type = "wild"
man1$type = "man"
man1=spTransform(man1,TA)


days1 = unique(pts_in$DOY)

gridmet_list= list.files(gridmet_dir, pattern = paste("2018",".nc$",sep=""), recursive = TRUE, full.names=T)
i=1
viirs2=0
for (i in 1:length(days1)){
  print(i)
  viirs1=0
  viirs1 = pts_in[pts_in$DOY == days1[i],]  
  
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
  
  bi = extract(l_bi, viirs1)
  erc = extract(l_erc, viirs1)
  etr = extract(l_etr, viirs1)
  fm100 = extract(l_fm100, viirs1)
  fm1000 = extract(l_fm1000, viirs1)
  pet = extract(l_pet, viirs1)
  pr = extract(l_pr, viirs1)
  rmax = extract(l_rmax, viirs1)
  rmin = extract(l_rmin, viirs1)
  th = extract(l_th, viirs1)
  tmmn = extract(l_tmmn, viirs1)
  tmmx = extract(l_tmmx, viirs1)
  vpd = extract(l_vpd, viirs1)
  vs = extract(l_vs, viirs1)
  
  viirs1=cbind(bi,erc,etr,fm100,fm1000,pet,pr,rmax,rmin,th,tmmn,tmmx,vpd,vs)
  
  viirs2=rbind(viirs2,viirs1)
  
}


days1 = unique(man1$DOY)

gridmet_list= list.files(gridmet_dir, pattern = paste("2018",".nc$",sep=""), recursive = TRUE, full.names=T)
i=1
viirs3=0
for (i in 1:length(days1)){
  print(i)
  viirs1=0
  viirs1 = man1[man1$DOY == days1[i],]  
  
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
  
  bi = extract(l_bi, viirs1)
  erc = extract(l_erc, viirs1)
  etr = extract(l_etr, viirs1)
  fm100 = extract(l_fm100, viirs1)
  fm1000 = extract(l_fm1000, viirs1)
  pet = extract(l_pet, viirs1)
  pr = extract(l_pr, viirs1)
  rmax = extract(l_rmax, viirs1)
  rmin = extract(l_rmin, viirs1)
  th = extract(l_th, viirs1)
  tmmn = extract(l_tmmn, viirs1)
  tmmx = extract(l_tmmx, viirs1)
  vpd = extract(l_vpd, viirs1)
  vs = extract(l_vs, viirs1)
  
  viirs1=cbind(bi,erc,etr,fm100,fm1000,pet,pr,rmax,rmin,th,tmmn,tmmx,vpd,vs)
  
  viirs3=rbind(viirs3,viirs1)
  
}
viirs2a = as.data.frame(viirs2)
viirs3a = as.data.frame(viirs3)

t.test(viirs2a$bi,viirs3a$bi)

hist.a =hist(viirs2a$vpd,breaks =c(0,1,2,3,4,5,6,7),plot=F)
hist.b =hist(viirs3a$vpd,breaks =c(0,1,2,3,4,5,6,7),plot=F)

hist.a =hist(viirs2a$fm100,breaks =c(0,3,6,9,12,15,18,50),plot=F)
hist.b =hist(viirs3a$fm100,breaks =c(0,3,6,9,12,15,18,50),plot=F)

fg = rbind(hist.a$counts,hist.b$counts)
dens = rbind((hist.a$counts/(sum(hist.a$counts)))*100,(hist.b$counts/(sum(hist.b$counts)))*100)

fr = barplot(dens, beside=TRUE,xlab="FM1000",ylab="% active fires",cex.lab=1.4,cex.axis = 1.3)
axis(1,at=c(0.5,3.5,6.5,9.5,12.5,15.5,18.5,21.5),labels=hist.a$breaks,cex.axis = 1.3)
legend("topleft",legend = c("management","wildfire"), fill=c("grey","black"),cex=1.4,bty = "n")
  
  
  
