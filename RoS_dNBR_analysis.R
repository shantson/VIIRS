
library(utils)
library(raster)
library(stringr)
li= list.files("/Users/stijnhantson/Documents/data/MTBS/DATA/",pattern = ".tar.gz$", recursive = TRUE, full.names=T)
li= list.files("/Users/stijnhantson/Documents/data/MTBS/DATA/",pattern = ".tif$", full.names=T)
trs1 = raster(li[57])
trs2 = raster(li[59])
trs3 = raster(li[61])
trs4 = raster(li[63])
trs5 = raster(li[65])
trs6 = raster(li[67])

dt = max(trs1,trs2,trs3,trs4,trs5,trs6)

#######################  untar files ############################
for (p in 1:length(li)){
  di =   substr(li[p], 1, (nchar(li[p])-49))
  untar(li[p],exdir=di)
}


################### analyse frap - DNBR  ####################
library(rgdal)
library(raster)
inpath = "/Users/stijnhantson/Documents/data/MTBS/DATA"
P4S.latlon <- CRS("+proj=longlat +datum=WGS84")
frap=shapefile("/Users/stijnhantson/Documents/data/FRAP/fire18_1.shp")

frap=subset(frap,YEAR_ > 1999)
frap=subset(frap, GIS_ACRES>1000)

#bast="/Users/stijnhantson/Documents/data/basal_area_reduction_fire_california/VegBurnSeverityBA18_1.gdb"
#basal=readOGR(dsn=bast,layer="VegBurnSeverityBA")

#basal=shapefile("/Users/stijnhantson/Documents/data/basal_area_reduction_fire_california/basal_area_reduction.shp")
#basal$BAR=-9999
#basal$BAR[basal$BURNSEV == 1] = 0
#basal$BAR[basal$BURNSEV == 2] = 5
#basal$BAR[basal$BURNSEV == 3] = 12.5
#basal$BAR[basal$BURNSEV == 4] = 37.5
#basal$BAR[basal$BURNSEV == 5] = 62.5
#basal$BAR[basal$BURNSEV == 6] = 82.5
#basal$BAR[basal$BURNSEV == 7] = 95
#basal$BAR[basal$BURNSEV == 255] = -9999


land = raster("/Users/stijnhantson/Documents/data/CAL_VEG/gaplf2011lc_v30_CA/gaplf2011lc_v30_ca.tif")
tab = read.table("/Users/stijnhantson/Documents/data/CAL_VEG/gaplf2011lc_v30_CA/GAP_LANDFIRE_National_Terrestrial_Ecosystems_2011_Attributes.txt",sep="\t",header=T)
tab=tab[,c("Value","CL")]
tab=as.matrix(tab)
landcover=reclassify(land,tab)
landcover[landcover == 3 | landcover == 4  |  landcover == 6 |  landcover == 8 | landcover == 9 | landcover == 10] = 2
bio = raster("/Users/stijnhantson/Documents/data/2010_Biomass.tif")
dem = raster("/Users/stijnhantson/Documents/data/output_srtm.tif")

out=frap[1,]
out$dnbr95 = NA
out$mean_dnbr = NA
out$rdnbr95 = NA
out$mean_rdnbr = NA
out$bas95 = NA
out$mean_bas = NA
out$lon = NA
out$lat=NA
out$max_land = NA
out$mean_land = NA
out$biomass = NA
out$elevation = NA
out=out[0,]
bas=0
for (year in 2012:2017){
  print(year)
#read annual Basal area reduction file
  nam=paste("/Users/stijnhantson/Documents/data/basal_area_reduction_fire_california/basal_area_",year,".tif",sep="")
  basal_area = raster(nam)
  frap_y =subset(frap,YEAR_ == year)
  dnbr =raster(paste(inpath,"/",year,"_dnbr.tif",sep=""))
  dnbr[dnbr < -2000] = NA
  rdnbr =raster(paste(inpath,"/",year,"_rdnbr.tif",sep=""))
  rdnbr[rdnbr < -2000] = NA
  
  for (kk in 1:length(frap_y)){
    print(kk)
  fire = frap_y[kk,]
  
  te = spTransform(fire,P4S.latlon)
  centroids <- getSpPPolygonsLabptSlots(te)   #calculate centroids
  lon = centroids[1,1]
  lat = centroids[1,2]
  
  te2=spTransform(fire,crs(basal_area))
  
  land_samp = extract(landcover,fire,na.rm=T)
  max_land = names(which.max(table(land_samp)))
  if (is.null(max_land)){
    max_land = NA
  }
  mean_land1 =as.numeric(as.character(unlist(land_samp)))
  mean_land = mean(mean_land1)
  
  biom = extract(bio,fire,na.rm=T)
  biomass = mean(na.omit(as.numeric(unlist((biom)))))
  
  dem1 = extract(dem,fire,na.rm=T)
  elevation = mean(na.omit(as.numeric(unlist((dem1)))))
  
  dnbr_samp = extract(dnbr,fire,na.rm=TRUE)
  mean_dnbr = mean(dnbr_samp[[1]],na.rm=T)
  dnbr95 = quantile(dnbr_samp[[1]], 0.95,na.rm=T)
  sd_dnbr = sd(dnbr_samp[[1]],na.rm=T)
  
  rdnbr_samp = extract(rdnbr,fire,na.rm=TRUE)
  mean_rdnbr = mean(rdnbr_samp[[1]],na.rm=T)
  rdnbr95 = quantile(rdnbr_samp[[1]], 0.95,na.rm=T)
  sd_rdnbr = sd(rdnbr_samp[[1]],na.rm=T)
  
  bas_samp = extract(basal_area,fire,na.rm=TRUE)
  mean_bas = mean(bas_samp[[1]],na.rm=T)
  bas95 = quantile(bas_samp[[1]], 0.95,na.rm=T)
  sd_bas = sd(bas_samp[[1]],na.rm=T)
  
  fire$dnbr95 = dnbr95
  fire$mean_dnbr = mean_dnbr
  fire$sd_dnbr = sd_dnbr
  
  fire$rdnbr95 = rdnbr95
  fire$mean_rdnbr = mean_rdnbr
  fire$sd_rdnbr = sd_rdnbr
  
  fire$mean_bas = mean_bas
  fire$bas95 = bas95
  fire$sd_bas =sd_bas
  fire$lon = lon
  fire$lat = lat
  fire$max_land = max_land
  fire$mean_land = mean_land
  fire$elevation = elevation
  fire$biomass = biomass
  out=rbind(out,fire)
  }
}

out$dnbr95 = as.numeric(as.character(out$dnbr95))
out$mean_dnbr = as.numeric(as.character(out$mean_dnbr))
out$sd_dnbr = as.numeric(as.character(out$sd_dnbr))
out$rdnbr95 = as.numeric(as.character(out$rdnbr95))
out$mean_rdnbr = as.numeric(as.character(out$mean_rdnbr))
out$sd_rdnbr = as.numeric(as.character(out$sd_rdnbr))
out$bas95 = as.numeric(as.character(out$bas95))
out$mean_bas = as.numeric(as.character(out$mean_bas))
out$sd_bas = as.numeric(as.character(out$sd_bas))
out$lon = as.numeric(as.character(out$lon))
out$lat=as.numeric(as.character(out$lat))
out$CAUSE=as.numeric(as.character(out$CAUSE))
out$max_land=as.numeric(as.character(out$max_land))
out$mean_land=as.numeric(as.character(out$mean_land))
out$biomass=as.numeric(as.character(out$biomass))
out$elevation=as.numeric(as.character(out$elevation))


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

write.table(out,"/Users/stijnhantson/Documents/projects/VIIRS_ros/per_fire_info.txt", row.names = F)
out_save = out

out=subset(out_save,L1CODE == 11) #6 = north,10 =desert,11 = socal


out1 = subset(out,CAUSE == 1)   #1=lightning; 14=unknown; 7=arson
out2 = subset(out,CAUSE !=1 & CAUSE != 14)
out3 = subset(out,CAUSE == 7)


#out1=subset(out1,GIS_ACRES>10000)
#out2=subset(out2,GIS_ACRES>10000)
#out3=subset(out3,GIS_ACRES>10000)

mean(out1$dnbr95,na.rm=T)
mean(out2$dnbr95,na.rm=T)

mean(out1$mean_dnbr,na.rm=T)
mean(out2$mean_dnbr,na.rm=T)

mean(out1$rdnbr95,na.rm=T)
mean(out2$rdnbr95,na.rm=T)

mean(out1$mean_rdnbr,na.rm=T)
mean(out2$mean_rdnbr,na.rm=T)

mean(out1$bas95,na.rm=T)
mean(out2$bas95,na.rm=T)

mean(out1$mean_bas,na.rm=T)
mean(out2$mean_bas,na.rm=T)


#plot(log(out1$GIS_ACRES),out1$dnbr95)
#plot(log(out2$GIS_ACRES),out2$dnbr95)

t.test(out1$rdnbr95,out2$rdnbr95)
t.test(out1$mean_rdnbr,out2$mean_rdnbr)

t.test(out1$dnbr95,out2$dnbr95)
t.test(out1$mean_dnbr,out2$mean_dnbr)

t.test(out1$bas95,out2$bas95)
t.test(out1$mean_bas,out2$mean_bas)

out1 = subset(out_save,CAUSE == 1)
plot(out_save$mean_land,out_save$mean_rdnbr)
points(out1$mean_land,out1$mean_rdnbr,col="red")


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
inpath_viirs = "/Users/stijnhantson/Documents/projects/VIIRS_ros/final_results6/"
nr_l = nchar(inpath_viirs)
fun95 = function(x){quantile(x, 0.95)}
data_s = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
P4S.latlon <- CRS("+proj=longlat +datum=WGS84")

land = raster("/Users/stijnhantson/Documents/data/CAL_VEG/gaplf2011lc_v30_CA/gaplf2011lc_v30_ca.tif")
tab = read.table("/Users/stijnhantson/Documents/data/CAL_VEG/gaplf2011lc_v30_CA/GAP_LANDFIRE_National_Terrestrial_Ecosystems_2011_Attributes.txt",sep="\t",header=T)
tab=tab[,c("Value","CL")]
tab=as.matrix(tab)
landcover=reclassify(land,tab)
landcover[landcover == 3 | landcover == 4  |  landcover == 6 |  landcover == 8 | landcover == 9 | landcover == 10] = 2

bio = raster("/Users/stijnhantson/Documents/data/2010_Biomass.tif")
dem = raster("/Users/stijnhantson/Documents/data/output_srtm.tif")

for (year in 2012:2017){
 dnbr =raster(paste(inpath,"/",year,"_dnbr.tif",sep=""))
  dnbr[dnbr < -2000] = NA

  rdnbr =raster(paste(inpath,"/",year,"_rdnbr.tif",sep=""))
  rdnbr[rdnbr < -2000] = NA
  
  nam=paste("/Users/stijnhantson/Documents/data/basal_area_reduction_fire_california/basal_area_",year,".tif",sep="")
  basal_area = raster(nam)

 # nam=paste("/Users/stijnhantson/Documents/data/basal_area_reduction_fire_california/annual/basal_area_",year,".shp",sep="")
 # bas = shapefile(nam)
#  bas = bas[bas$BAR > -1,]
#  bas=gBuffer(bas, width = 0, byid=TRUE ) 
  
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
      BA_red95 =NA
      mean_BA_red = NA
      elevation=NA
      biomass=NA
    }else{
      land_samp = extract(landcover,viirs_d,na.rm=T)
      max_land = names(which.max(table(land_samp)))
      if (is.null(max_land)){
        max_land = NA
      }
      mean_land1 =as.numeric(as.character(unlist(land_samp)))
      mean_land = mean(mean_land1)
      
      biom = extract(bio,viirs_d,na.rm=T)
      biomass = mean(na.omit(as.numeric(unlist((biom)))))
      
      dem1 = extract(dem,viirs_d,na.rm=T)
      elevation = mean(na.omit(as.numeric(unlist((dem1)))))
      
     if (area(viirs_d) > 10000){
      te = spTransform(viirs_d,P4S.latlon)
    centroids <- getSpPPolygonsLabptSlots(te)   #calculate centroids
    lon = centroids[1,1]
    lat = centroids[1,2]
  #  te2=spTransform(viirs_d,crs(bas))
  #  te2=gBuffer(te2, width = 0, byid=TRUE ) 
    
    dnbr_samp = extract(dnbr,viirs_d,na.rm=TRUE)
    rdnbr_samp = extract(rdnbr,viirs_d,na.rm=TRUE)
    basal_samp = extract(basal_area,viirs_d,na.rm=TRUE)
    
    mean_dnbr = mean(dnbr_samp[[1]],na.rm=T)
    dnbr95 = quantile(dnbr_samp[[1]], 0.95,na.rm=T)
    
    mean_rdnbr=mean(rdnbr_samp[[1]],na.rm=T)
    rdnbr95=quantile(rdnbr_samp[[1]], 0.95,na.rm=T)
    
    mean_BA_red=mean(basal_samp[[1]],na.rm=T)
    BA_red95=quantile(basal_samp[[1]], 0.95,na.rm=T)
    
  #  clip <- intersect(te2,bas) 
  #  clip$area = area(clip)
  #  BA_red = sum(clip$da)/sum(clip$area)
    
     }else{
       mean_dnbr = NA
       dnbr95 = NA
       lon = NA
       lat=NA
       mean_rdnbr=NA
       rdnbr95=NA
       mean_BA_red=NA
       BA_red95=NA
     }
    }
  dat = c(lon,lat,fire,nr_days,max_land,mean_land,elevation, biomass,mean_ros,ros95, mean_dnbr,dnbr95,mean_rdnbr,rdnbr95,mean_BA_red,BA_red95)
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
  mean_land = 0
  mean_rdnbr=0
  rdnbr95=0
  BA_red95=0
  mean_BA_red=0
  elevation=0
  biomass=0
   }
  }
 gc()
 
 }
}

write.table(data_s, "/Users/stijnhantson/Documents/projects/VIIRS_ros/daily_mean_ros_dNBR_V3.txt",sep="\t")
removeTmpFiles(1)

data_s=read.table("/Users/stijnhantson/Documents/projects/VIIRS_ros/daily_mean_ros_dNBR_V3.txt",row.names=NULL)
rownames(data_s) <- c()
data_s1 = as.data.frame(data_s[,2:17])


names(data_s1) = c("lon","lat","fire","nr_day","max_land","mean_land","elevation","biomass","mean_ros","ros95","mean_dnbr","dnbr95","mean_rdnbr","rdnbr95","mean_BA_red","BA_red95")


data_s1$mean_ros =as.numeric(as.character(data_s1$mean_ros))
data_s1$ros95 =as.numeric(as.character(data_s1$ros95))
data_s1$mean_dnbr =as.numeric(as.character(data_s1$mean_dnbr))
data_s1$dnbr95 =as.numeric(as.character(data_s1$dnbr95))
data_s1$mean_rdnbr =as.numeric(as.character(data_s1$mean_rdnbr))
data_s1$rdnbr95 =as.numeric(as.character(data_s1$rdnbr95))
data_s1$lon =as.numeric(as.character(data_s1$lon))
data_s1$lat =as.numeric(as.character(data_s1$lat))

data_s1$mean_land[is.na(data_s1$mean_land)]=data_s1$max_land[is.na(data_s1$mean_land)] #mean landcover gives NA when only one landcover is present
data_s1$mean_land =as.numeric(as.character(data_s1$mean_land))
data_s1$mean_BA_red =as.numeric(as.character(data_s1$mean_BA_red))
data_s1$BA_red95 =as.numeric(as.character(data_s1$BA_red95))
data_s1$biomass =as.numeric(as.character(data_s1$biomass))
data_s1$elevation =as.numeric(as.character(data_s1$elevation))

data_s1=na.omit(data_s1)
shape = shapefile("/Users/stijnhantson/Documents/data/veg_california/ca_eco_l3/ca_eco_l3.shp")
pts <- SpatialPoints(data_s1[,c("lon","lat")],P4S.latlon)
shape = spTransform(shape,P4S.latlon)
eco = over(pts, shape)
data_s1$L1CODE = eco$NA_L1CODE
data_s1$L3name = eco$US_L3NAME
data_s1$L1CODE =as.numeric(as.character(data_s1$L1CODE))

data_s1$log_ros = log10(data_s1$mean_ros)
data_s1$log_ros95 = log10(data_s1$ros95)
data_s1 = data_s1[data_s1$mean_ros >0,]


data_test = data_s1[data_s1$L1CODE == 11,]
plot(log(data_test$mean_ros+1), data_test$mean_dnbr)
plot(data_test$log_ros95, data_test$rdnbr95,xlab="log10 RoS 95p (m/hr)",ylab="95p dNBR")
abline(lm(data_test$dnbr95~data_test$log_ros95))
summary(lm(data_test$dnbr95~data_test$log_ros95))

data_test = data_s1[data_s1$max_land == 1,]
plot(log(data_test$mean_ros+1), data_test$mean_dnbr)
plot(data_test$mean_ros, data_test$dnbr95,xlab="log10 RoS 95p (m/hr)",ylab="95p dNBR")
abline(lm(data_test$dnbr95~data_test$log_ros95))
summary(lm(data_test$dnbr95~data_test$log_ros95))
summary(lm(data_test$mean_dnbr~data_test$log_ros))

#for tranparent colors
#library(scales)
#cols=("black")
#plot(data_test$ros95, data_test$mean_BA_red,log="x",xlab="Rate-of-Spread (m/hr)",ylab="Tree mortality (%)",xlim=c(0.2,1000),xaxt="n",col=alpha(cols,0.65))

data_test = data_s1[data_s1$max_land == 1,]
marks <- c(1,10,100,1000)

tiff("/Users/stijnhantson/Documents/Documents/articulos/en_proceso/VIIRS_ros/fig_basa_ros_v1.tif", width = 6, height = 5, units = 'in', res = 300)
plot(data_test$ros95, data_test$mean_BA_red,log="x",xlab="Rate-of-Spread (m/hr)",ylab="Tree mortality (%)",xlim=c(0.25,900),xaxt="n",cex.axis=1.4 ,cex.lab=1.4,cex=0.8)
axis(1,at=marks,labels=marks,cex.axis=1.4 )
#lines(lowess(data_test$ros95, data_test$mean_BA_red, f=0.41),col="black", lwd=3)
dev.off()


#abline(lm(data_test$mean_BA_red~data_test$log_ros95+(data_test$log_ros95^2)))
model = lm(data_test$mean_BA_red~ poly(data_test$log_ros95,2))
summary(model)
predicted.intervals <- predict(model,data.frame(x=data_test$log_ros95),interval='confidence',
                               level=0.99)

lines(data_test$log_ros95,predicted.intervals[,1],col='green',lwd=3)
lines(data_test$log_ros95,predicted.intervals[,2],col='black',lwd=1)
lines(data_test$log_ros95,predicted.intervals[,3],col='black',lwd=1)

plot(log(data_s1$mean_ros+1), data_s1$mean_dnbr)
plot(log(data_s1$ros95 +1), data_s1$dnbr95)



resu = lm((data_s1$log_ros)~ data_s1$mean_dnbr)
resu = lm(data_s1$log_ros95~ data_s1$dnbr95)
summary(resu)

plot(log10(data_s1$ros95 +1), data_s1$dnbr95, xlab="log10 RoS 95p (m/hr)",ylab="95p dNBR")
plot(log10(data_s1$mean_ros+1), data_s1$mean_dnbr, xlab="log10 mean RoS (m/hr)",ylab="mean dNBR")



########### final figures  ##############


###### dNBR ~ RoS

data_test = data_s1[data_s1$L1CODE == 6,]
plot(data_test$log_ros, data_test$mean_dnbr,xlab="log10 mean RoS (m/h)",ylab="mean dNBR", cex.lab=1.4,cex.axis = 1.3)
plot(data_test$log_ros95, data_test$dnbr95,xlab="log10 RoS 95p (m/h)",ylab="95p dNBR", cex.lab=1.4,cex.axis = 1.3)

data_test = data_s1[data_s1$L1CODE == 11,]
plot(data_test$log_ros, data_test$mean_dnbr,xlab="log10 mean RoS (m/h)",ylab="mean dNBR", cex.lab=1.4,cex.axis = 1.3)
plot(data_test$log_ros95, data_test$dnbr95,xlab="log10 RoS 95p (m/h)",ylab="95p dNBR", cex.lab=1.4,cex.axis = 1.3)


data_test = data_s1[data_s1$max_land == 1,]
plot(data_test$log_ros, data_test$mean_dnbr,xlab="log10 mean RoS (m/h)",ylab="mean dNBR", cex.lab=1.4,cex.axis = 1.3)
plot(data_test$log_ros95, data_test$dnbr95,xlab="log10 RoS 95p (m/h)",ylab="95p dNBR", cex.lab=1.4,cex.axis = 1.3)

data_test = data_s1[data_s1$max_land == 2,]
plot(data_test$log_ros, data_test$mean_dnbr,xlab="log10 mean RoS (m/h)",ylab="mean dNBR", cex.lab=1.4,cex.axis = 1.3)
plot(data_test$log_ros95, data_test$dnbr95,xlab="log10 RoS 95p (m/h)",ylab="95p dNBR", cex.lab=1.4,cex.axis = 1.3)


###### rdNBR ~ RoS

data_test = data_s1[data_s1$L1CODE == 6,]
pdf('/Users/stijnhantson/Documents/projects/VIIRS_ros/figures/forest_dNBR-meanRoS.pdf')
plot(data_test$log_ros, data_test$mean_dnbr,xlab="log10 mean RoS (m/h)",ylab="mean dNBR", cex.lab=1.4,cex.axis = 1.3)
dev.off()

pdf('/Users/stijnhantson/Documents/projects/VIIRS_ros/figures/forest_dNBR-95RoS.pdf')
plot(data_test$log_ros95, data_test$dnbr95,xlab="log10 RoS 95p (m/h)",ylab="95p dNBR", cex.lab=1.4,cex.axis = 1.3)
dev.off()

pdf('/Users/stijnhantson/Documents/projects/VIIRS_ros/figures/forest_rdNBR-meanRoS.pdf')
plot(data_test$log_ros, data_test$mean_rdnbr,xlab="log10 mean RoS (m/h)",ylab="mean rdNBR", cex.lab=1.4,cex.axis = 1.3)
dev.off()

pdf('/Users/stijnhantson/Documents/projects/VIIRS_ros/figures/forest_rdNBR-95RoS.pdf')
plot(data_test$log_ros95, data_test$rdnbr95,xlab="log10 RoS 95p (m/h)",ylab="95p rdNBR", cex.lab=1.4,cex.axis = 1.3)
dev.off()

data_test = data_s1[data_s1$L1CODE == 11,]
pdf('/Users/stijnhantson/Documents/projects/VIIRS_ros/figures/noforest_dNBR-meanRoS.pdf')
plot(data_test$log_ros, data_test$mean_dnbr,xlab="log10 mean RoS (m/h)",ylab="mean dNBR", cex.lab=1.4,cex.axis = 1.3)
dev.off()

pdf('/Users/stijnhantson/Documents/projects/VIIRS_ros/figures/noforest_dNBR-95RoS.pdf')
plot(data_test$log_ros95, data_test$dnbr95,xlab="log10 RoS 95p (m/h)",ylab="95p dNBR", cex.lab=1.4,cex.axis = 1.3)
dev.off()

pdf('/Users/stijnhantson/Documents/projects/VIIRS_ros/figures/noforest_rdNBR-meanRoS.pdf')
plot(data_test$log_ros, data_test$mean_rdnbr,xlab="log10 mean RoS (m/h)",ylab="mean rdNBR", cex.lab=1.4,cex.axis = 1.3)
dev.off()

pdf('/Users/stijnhantson/Documents/projects/VIIRS_ros/figures/noforest_rdNBR-95RoS.pdf')
plot(data_test$log_ros95, data_test$rdnbr95,xlab="log10 RoS 95p (m/h)",ylab="95p rdNBR", cex.lab=1.4,cex.axis = 1.3)
dev.off()


data_test = data_s1[data_s1$max_land == 1,]
pdf('/Users/stijnhantson/Documents/projects/VIIRS_ros/figures/sierra_rdNBR-meanRoS.pdf')
plot(data_test$log_ros, data_test$mean_rdnbr,xlab="log10 mean RoS (m/h)",ylab="mean rdNBR", cex.lab=1.4,cex.axis = 1.3)
dev.off()
pdf('/Users/stijnhantson/Documents/projects/VIIRS_ros/figures/sierra_rdNBR-95RoS.pdf')
plot(data_test$log_ros95, data_test$rdnbr95,xlab="log10 RoS 95p (m/h)",ylab="95p rdNBR", cex.lab=1.4,cex.axis = 1.3)
dev.off()

pdf('/Users/stijnhantson/Documents/projects/VIIRS_ros/figures/sierra_dNBR-meanRoS.pdf')
plot(data_test$log_ros, data_test$mean_dnbr,xlab="log10 mean RoS (m/h)",ylab="mean dNBR", cex.lab=1.4,cex.axis = 1.3)
dev.off()
pdf('/Users/stijnhantson/Documents/projects/VIIRS_ros/figures/sierra_dNBR-95RoS.pdf')
plot(data_test$log_ros95, data_test$dnbr95,xlab="log10 RoS 95p (m/h)",ylab="95p dNBR", cex.lab=1.4,cex.axis = 1.3)
dev.off()


data_test = data_s1[data_s1$max_land == 2,]
pdf('/Users/stijnhantson/Documents/projects/VIIRS_ros/figures/socal_rdNBR-meanRoS.pdf')
plot(data_test$log_ros, data_test$mean_rdnbr,xlab="log10 mean RoS (m/h)",ylab="mean rdNBR", cex.lab=1.4,cex.axis = 1.3)
dev.off()
pdf('/Users/stijnhantson/Documents/projects/VIIRS_ros/figures/socal_rdNBR-95RoS.pdf')
plot(data_test$log_ros95, data_test$rdnbr95,xlab="log10 RoS 95p (m/h)",ylab="95p rdNBR", cex.lab=1.4,cex.axis = 1.3)
dev.off()

data_test = data_s1[data_s1$max_land == 2,]
pdf('/Users/stijnhantson/Documents/projects/VIIRS_ros/figures/socal_dNBR-meanRoS.pdf')
plot(data_test$log_ros, data_test$mean_dnbr,xlab="log10 mean RoS (m/h)",ylab="mean dNBR", cex.lab=1.4,cex.axis = 1.3)
dev.off()
pdf('/Users/stijnhantson/Documents/projects/VIIRS_ros/figures/socal_dNBR-95RoS.pdf')
plot(data_test$log_ros95, data_test$dnbr95,xlab="log10 RoS 95p (m/h)",ylab="95p dNBR", cex.lab=1.4,cex.axis = 1.3)
dev.off()

