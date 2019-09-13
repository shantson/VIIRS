
library(utils)
library(raster)
library(stringr)
li= list.files("/Users/stijnhantson/Documents/data/MTBS/DATA/",pattern = ".tar.gz$", recursive = TRUE, full.names=T)

#######################  untar files ############################
for (p in 1:length(li)){
  di =   substr(li[p], 1, (nchar(li[p])-49))
  untar(li[p],exdir=di)
}


################### analyse frap - DNBR  ####################

inpath = "/Users/stijnhantson/Documents/data/MTBS/DATA"
P4S.latlon <- CRS("+proj=longlat +datum=WGS84")
frap=shapefile("/Users/stijnhantson/Documents/data/FRAP/fire18_1.shp")

frap=subset(frap,YEAR_ > 1999)
frap=subset(frap, GIS_ACRES>1000)


#land = raster("/Users/stijnhantson/Documents/data/CAL_VEG/gaplf2011lc_v30_CA/gaplf2011lc_v30_ca.tif")
tab = read.table("/Users/stijnhantson/Documents/data/CAL_VEG/gaplf2011lc_v30_CA/GAP_LANDFIRE_National_Terrestrial_Ecosystems_2011_Attributes.txt",sep="\t",header=T)
tab=tab[,c("Value","CL")]
tab=as.matrix(tab)
#landcover=reclassify(land,tab)

out=frap[1,]
out$dnbr95 = NA
out$mean_dnbr = NA
out$rdnbr95 = NA
out$mean_rdnbr = NA
out$lon = NA
out$lat=NA
out=out[0,]

for (year in 2000:2016){
  frap_y =subset(frap,YEAR_ == year)
  dnbr =raster(paste(inpath,"/",year,"_dnbr.tif",sep=""))
  dnbr[dnbr < -2000] = NA
  rdnbr =raster(paste(inpath,"/",year,"_rdnbr.tif",sep=""))
  rdnbr[rdnbr < -2000] = NA
  
  for (kk in 1:length(frap_y)){
  fire = frap_y[kk,]
  
  te = spTransform(fire,P4S.latlon)
  centroids <- getSpPPolygonsLabptSlots(te)   #calculate centroids
  lon = centroids[1,1]
  lat = centroids[1,2]
  
  
  dnbr_samp = extract(dnbr,fire,na.rm=TRUE)
  mean_dnbr = mean(dnbr_samp[[1]],na.rm=T)
  dnbr95 = quantile(dnbr_samp[[1]], 0.95,na.rm=T)
  
  rdnbr_samp = extract(rdnbr,fire,na.rm=TRUE)
  mean_rdnbr = mean(rdnbr_samp[[1]],na.rm=T)
  rdnbr95 = quantile(rdnbr_samp[[1]], 0.95,na.rm=T)
  
  fire$dnbr95 = dnbr95
  fire$mean_dnbr = mean_dnbr
  fire$rdnbr95 = rdnbr95
  fire$mean_rdnbr = mean_rdnbr
  fire$lon = lon
  fire$lat = lat
  
  out=rbind(out,fire)
  }
}

out$dnbr95 = as.numeric(as.character(out$dnbr95))
out$mean_dnbr = as.numeric(as.character(out$mean_dnbr))
out$rdnbr95 = as.numeric(as.character(out$rdnbr95))
out$mean_rdnbr = as.numeric(as.character(out$mean_rdnbr))
out$lon = as.numeric(as.character(out$lon))
out$lat=as.numeric(as.character(out$lat))
out$CAUSE=as.numeric(as.character(out$CAUSE))

shape = shapefile("/Users/stijnhantson/Documents/data/veg_california/ca_eco_l3/ca_eco_l3.shp")
#pts <- SpatialPoints(out[,c("lon","lat")],P4S.latlon)
pts <- SpatialPoints(as.data.frame(out[,c("lon","lat")]),P4S.latlon)

shape = spTransform(shape,P4S.latlon)
pts = spTransform(pts,P4S.latlon)
eco = over(pts, shape)
out$L1CODE = eco$NA_L1CODE
out$L3name = eco$US_L3NAME

out$L1CODE = as.numeric(as.character(out$L1CODE))
out$L3name=as.character(out$L3name)

out_save = out

out=subset(out_save,L1CODE == 11)


out1 = subset(out,CAUSE == 1)
out2 = subset(out,CAUSE !=1 & CAUSE != 14)
out3 = subset(out,CAUSE == 7)


out1=subset(out1,GIS_ACRES>10000)
out2=subset(out2,GIS_ACRES>10000)
out3=subset(out3,GIS_ACRES>10000)


mean(out1$dnbr95,na.rm=T)
mean(out2$dnbr95,na.rm=T)
mean(out3$dnbr95,na.rm=T)

mean(out1$mean_dnbr,na.rm=T)
mean(out2$mean_dnbr,na.rm=T)
mean(out3$mean_dnbr,na.rm=T)


mean(out1$rdnbr95,na.rm=T)
mean(out2$rdnbr95,na.rm=T)
mean(out3$rdnbr95,na.rm=T)

mean(out1$mean_rdnbr,na.rm=T)
mean(out2$mean_rdnbr,na.rm=T)
mean(out3$mean_rdnbr,na.rm=T)

plot(log(out1$GIS_ACRES),out1$dnbr95)
plot(log(out2$GIS_ACRES),out2$dnbr95)

t.test(out1$rdnbr95,out2$rdnbr95)
t.test(out1$mean_rdnbr,out2$mean_rdnbr)

t.test(out1$dnbr95,out2$dnbr95)
t.test(out1$mean_dnbr,out2$mean_dnbr)

plot(out$)


#######################  make annual comoposite dNBR & rdNBR  ############################

path1 = "/Users/stijnhantson/Documents/data/MTBS/DATA/"
year=2011

for (year in 2014:2017){
  indir = paste(path1,year,"/",sep="")
  
li= list.files(indir,pattern = "_dnbr.tif$", recursive = TRUE, full.names=T)
li1= list.files(indir,pattern = "_rdnbr.tif$", recursive = TRUE, full.names=T)

ppp=0
ppp1=0
for (rr in 1:length(li)){
  
  fi_nam = substr(li[rr],1, (nchar(li[rr])-9))
  meta = paste(fi_nam,"_metadata.txt",sep="")
  f <- readLines(meta,warn = FALSE)   #### read nbr offset and aply it
#  cline <- grep("dNBR offset value used to calculate RdNBR",f,value=TRUE)
  cline <- grep("used to calculate RdNBR",f,value=TRUE)
  val <- as.numeric(str_extract(cline,"[-][0-9]+$"))
if (is.na(val)){
  val <- as.numeric(str_extract(cline,"[0-9]+$"))
}
  
moz = (raster(li[rr])) - val
ppp = c(ppp,moz)

moz1 = (raster(li[rr]))
ppp1 = c(ppp1,moz1)
}

ppp=ppp[2:length(ppp)]
ppp$fun <- mean
ppp$na.rm <- TRUE

ppp1=ppp1[2:length(ppp1)]
ppp1$fun <- mean
ppp1$na.rm <- TRUE

y <- do.call(mosaic, ppp)
y1 <- do.call(mosaic, ppp1)

outdir=paste(path1,year,"_dnbr.tif",sep="")
writeRaster(y,outdir, overwrite = T)

outdir=paste(path1,year,"_rdnbr.tif",sep="")
writeRaster(y1,outdir, overwrite = T)

}


#######################  create fire behaviour/dnbr relationship ############################


inpath = "/Users/stijnhantson/Documents/data/MTBS/DATA"
inpath_viirs = "/Users/stijnhantson/Documents/projects/VIIRS_ros/final_results4/"
nr_l = nchar(inpath_viirs)
fun95 = function(x){quantile(x, 0.95)}
data_s = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
P4S.latlon <- CRS("+proj=longlat +datum=WGS84")

land = raster("/Users/stijnhantson/Documents/data/CAL_VEG/gaplf2011lc_v30_CA/gaplf2011lc_v30_ca.tif")
tab = read.table("/Users/stijnhantson/Documents/data/CAL_VEG/gaplf2011lc_v30_CA/GAP_LANDFIRE_National_Terrestrial_Ecosystems_2011_Attributes.txt",sep="\t",header=T)
tab=tab[,c("Value","CL")]
tab=as.matrix(tab)
landcover=reclassify(land,tab)
landcover[landcover == 3 | landcover == 4  |  landcover == 6 |  landcover == 8 | landcover == 9 | landcover == 10] = 2

for (year in 2012:2017){
 dnbr =raster(paste(inpath,"/",year,"_dnbr.tif",sep=""))
  dnbr[dnbr < -2000] = NA

  rdnbr =raster(paste(inpath,"/",year,"_rdnbr.tif",sep=""))
  rdnbr[rdnbr < -2000] = NA
  
  lis= list.files(inpath_viirs ,pattern = "_daily.shp$", recursive = TRUE, full.names=T)
 viirs_list =  list.files(inpath_viirs, pattern = "daily_ros.shp$", recursive = TRUE, full.names=T)
 print(year)
 for (nr_fire in 1:length(lis)){

    test_year = substr(lis[nr_fire], nr_l+2, nr_l+5)
 fire = substr(lis[nr_fire], nr_l+7, (nchar(lis[nr_fire])-10))
 
 
  if (test_year == year){
   viirs= shapefile(lis[nr_fire])
   ro = shapefile(viirs_list[nr_fire])
   for (nr_days in 1:length(viirs)){
     if (nr_days ==1){
     viirs_d=viirs[nr_days,]
    
     }else{
       viirs_d=viirs[nr_days,]
       viirs_d1=viirs[nr_days-1,]
       viirs_d=erase(viirs_d,viirs_d1)
     }
   
       ro1=ro[ro$DOY==viirs_d$DOY,]
     mean_ros = mean(ro1$ros,na.rm=T)
     ros95 = quantile(ro1$ros,0.95,na.rm=T)
     
     
     
    if (length(viirs_d) == 0){
      mean_dnbr = NA
      dnbr95 = NA
      mean_rdnbr=NA
      rdnbr95=NA
      lon = NA
      lat=NA
      land_samp=NA
      land_samp=NA
    }else{
      land_samp = extract(landcover,viirs_d,na.rm=T)
      max_land = names(which.max(table(land_samp)))
     if (area(viirs_d) > 10000){
      te = spTransform(viirs_d,P4S.latlon)
    centroids <- getSpPPolygonsLabptSlots(te)   #calculate centroids
    lon = centroids[1,1]
    lat = centroids[1,2]
    
    
    dnbr_samp = extract(dnbr,viirs_d,na.rm=TRUE)
    rdnbr_samp = extract(rdnbr,viirs_d,na.rm=TRUE)
    
    mean_dnbr = mean(dnbr_samp[[1]],na.rm=T)
    dnbr95 = quantile(dnbr_samp[[1]], 0.95,na.rm=T)
    
    mean_rdnbr=mean(rdnbr_samp[[1]],na.rm=T)
    rdnbr95=quantile(rdnbr_samp[[1]], 0.95,na.rm=T)
     }else{
       mean_dnbr = NA
       dnbr95 = NA
       lon = NA
       lat=NA
       mean_rdnbr=NA
       rdnbr95=NA
     }
    }
  dat = c(lon,lat,fire,nr_days,max_land,mean_ros,ros95, mean_dnbr,dnbr95,mean_rdnbr,rdnbr95)
  data_s = rbind(data_s,dat)
  mean_ros=0
  ros95=0
  mean_dnbr=0
  dnbr95=0
  dnbr_samp=0
  ro1=0
  lon=0
  lat=0
  coordinates = 0
  max_land = 0
  mean_rdnbr=0
  rdnbr95=0
   }
  }
 }
}
rownames(data_s) <- c()
data_s1 = as.data.frame(data_s)

names(data_s1) = c("lon","lat","fire","nr_day","max_land","mean_ros","ros95","mean_dnbr","dnbr95","mean_rdnbr","rdnbr95")

data_s1=na.omit(data_s1)
data_s1$mean_ros =as.numeric(as.character(data_s1$mean_ros))
data_s1$ros95 =as.numeric(as.character(data_s1$ros95))
data_s1$mean_dnbr =as.numeric(as.character(data_s1$mean_dnbr))
data_s1$dnbr95 =as.numeric(as.character(data_s1$dnbr95))
data_s1$lon =as.numeric(as.character(data_s1$lon))
data_s1$lat =as.numeric(as.character(data_s1$lat))
data_s1=na.omit(data_s1)


shape = shapefile("/Users/stijnhantson/Documents/data/veg_california/ca_eco_l3/ca_eco_l3.shp")
pts <- SpatialPoints(data_s1[,c("lon","lat")],P4S.latlon)
shape = spTransform(shape,P4S.latlon)
eco = over(pts, shape)
data_s1$L1CODE = eco$NA_L1CODE
data_s1$L3name = eco$US_L3NAME

data_s1$log_ros = log10(data_s1$mean_ros+1)
data_s1$log_ros95 = log10(data_s1$ros95+1)
data_s1 = data_s1[data_s1$mean_ros >0,]

data_test = data_s1[data_s1$L1CODE == 6,]
plot(log(data_test$mean_ros+1), data_test$mean_dnbr)
plot(data_test$log_ros95, data_test$dnbr95,xlab="log10 RoS 95p (m/hr)",ylab="95p dNBR")
abline(lm(data_test$dnbr95~data_test$log_ros95))
summary(lm(data_test$dnbr95~data_test$log_ros95))

data_test = data_s1[data_s1$max_land == 1,]
plot(log(data_test$mean_ros+1), data_test$mean_dnbr)
plot(data_test$log_ros95, data_test$dnbr95,xlab="log10 RoS 95p (m/hr)",ylab="95p dNBR")
abline(lm(data_test$dnbr95~data_test$log_ros95))
summary(lm(data_test$dnbr95~data_test$log_ros95))
summary(lm(data_test$mean_dnbr~data_test$log_ros))


plot(log(data_s1$mean_ros+1), data_s1$mean_dnbr)
plot(log(data_s1$ros95 +1), data_s1$dnbr95)



resu = lm((data_s1$log_ros)~ data_s1$mean_dnbr)
resu = lm(data_s1$log_ros95~ data_s1$dnbr95)
summary(resu)

plot(log10(data_s1$ros95 +1), data_s1$dnbr95, xlab="log10 RoS 95p (m/hr)",ylab="95p dNBR")
plot(log10(data_s1$mean_ros+1), data_s1$mean_dnbr, xlab="log10 mean RoS (m/hr)",ylab="mean dNBR")








