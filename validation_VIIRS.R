
library(raster)
library(rgeos)
library(lattice)

ref_dir = "/Users/stijnhantson/Documents/projects/VIIRS_ros/evaluation/reference_clean/"
viirs_dir = "/Users/stijnhantson/Documents/projects/VIIRS_ros/evaluation/VIIRS/"

#ref_dir = "/Users/stijnhantson/Documents/projects/VIIRS_ros/evaluation/test/ref/"
#viirs_dir = "/Users/stijnhantson/Documents/projects/VIIRS_ros/evaluation/test/viirs/"

ref_list =  list.files(ref_dir, pattern = ".shp$", recursive = TRUE, full.names=T)
viirs_list =  list.files(viirs_dir, pattern = ".shp$", recursive = TRUE, full.names=T)

i=1
names = c("bagley","erskin","fortcomplex", "gladiator","holloway", "lake", "littles","northpass","poco", "powerhouse","reading", "rough","sunflower","waldo", "WhiteBaldy")
year = c("2012","2016","2012","2012","2012","2015","2012","2012","2012","2013","2012","2015","2012","2012","2012")
#names = c("bagley","chips","fontenelle","fortcomplex","halstead","holloway","northpass","poco","reading","rush","sunflower","trinityridge")
crs_t=CRS("+proj=utm +zone=11 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")



for (i in 1:length(ref_list)){
  print("i")
  print(i)
  ref_sh =  shapefile(ref_list[i])
  
  ref_sh=spTransform(ref_sh,crs_t)
  
  xmax1=xmax(ref_sh)+10000
  xmin1=xmin(ref_sh)-10000
  ymax1=ymax(ref_sh)+10000
  ymin1=ymin(ref_sh)-10000
  t=raster()
  crs(t)=crs_t
  extent(t)=c(xmin1,xmax1,ymin1,ymax1)
  res(t) <- 100
  
  if(max(ref_sh$DOY)>0){
    
  }else if(max(ref_sh$DOI)>0){
    ref_sh$DOY = ref_sh$DOI
  }else{
    
    if (max(ref_sh$Date) > 0){
      ref_sh$CaptDate=ref_sh$Date
    }else  if (max(ref_sh$CptDt_1) > 0){
      ref_sh$CaptDate=ref_sh$CptDt_1
    }else if(max(ref_sh$DATE_)>0){
      ref_sh$CaptDate=ref_sh$DATE_
    }else if(max(ref_sh$date_)>0){
      ref_sh$CaptDate=ref_sh$date_
    }
    
    if (max(ref_sh$Hour) > 0){
      ref_sh$CaptTime=ref_sh$Hour
    }else if (max(ref_sh$CptTm_1) > 0){
      ref_sh$CaptTime=ref_sh$CptTm_1
    }else if (max(ref_sh$TIME_) > 0){
      ref_sh$CaptTime=ref_sh$TIME_
    }else if (max(ref_sh$time_) > 0){
      ref_sh$CaptTime=ref_sh$time_
    }
    hour=(as.numeric(ref_sh$CaptTime)/100) 
    
    hour[hour == 0] = 12
    
    ref_sh$dat = as.Date(as.character(ref_sh$CaptDate), format= "%Y/%m/%d")
    
    ref_sh$DOY = as.numeric( strftime(ref_sh$dat,format = "%j"))
    
    hour1=as.numeric(lapply(hour, as.integer)) 
    hour1[hour1 == 0] = 12
    dec=(((hour-hour1)/60)*(1/24))*100
    ref_sh$DOY2=((hour1/24)+dec)+ ref_sh$DOY
    print(ref_sh$DOY2)
    ref_sh$DOY = ref_sh$DOY2 - 0.44 # aprox. 10:30
    ref_sh$DOY = as.numeric(lapply(ref_sh$DOY, as.integer)) 
    detections = sort(unique(ref_sh$DOY))
    len1=length(detections)
    pp=1
  }
  r2 <- rasterize(ref_sh, t, field=ref_sh@data$DOY, fun = min, background = -9999)
  
  outname=paste("/Users/stijnhantson/Documents/projects/VIIRS_ros/evaluation/test/",names[i],".tif",sep="")
  writeRaster(r2,outname,overwrite=T)
  
}



i=1
for (i in 1:length(viirs_list)){
  viirs_sh =  shapefile(viirs_list[i])
  ref_sh =  shapefile(ref_list[i])
  
  ref_sh=spTransform(ref_sh,crs_t)
  viirs_sh=spTransform(viirs_sh,crs_t)
  
  xmax1=xmax(ref_sh)+10000
  xmin1=xmin(ref_sh)-10000
  ymax1=ymax(ref_sh)+10000
  ymin1=ymin(ref_sh)-10000
  t=raster()
  crs(t)=crs_t
  extent(t)=c(xmin1,xmax1,ymin1,ymax1)
  res(t) <- 100
  
  viirs_sh$DOY= viirs_sh$DOY-0.5
  #  viirs_sh$DOY = lapply(viirs_sh$DOY, as.integer)
  viirs_sh$DOY = as.integer(viirs_sh$DOY)
  
  r2 <- rasterize(viirs_sh, t, field=viirs_sh@data$DOY, fun = min, background = -9999)
  
  outname=paste("/Users/stijnhantson/Documents/projects/VIIRS_ros/evaluation/test/",names[i],"_viirs.tif",sep="")
  writeRaster(r2,outname,overwrite=T)
  
}

############# produce statistics of viirs performance

ras_dir = "/Users/stijnhantson/Documents/projects/VIIRS_ros/evaluation/test/"
ras_list =  list.files(ras_dir, pattern = ".tif$", recursive = TRUE, full.names=T)
modis_list = list.files("/Users/stijnhantson/Documents/data/MCD64_v6/Win03/", pattern = "burndate.tif$", recursive = TRUE, full.names=T)

t=0
ras_sh =1
tiff(file="/Users/stijnhantson/Documents/projects/VIIRS_ros/test1.tif",width=3000,height=3000, res=350)
par(mfrow=c(5,3),mar=c(3,3,1,0))
for (ras_sh in 1:((length(ras_list))/2)){
  print(names[ras_sh]) 
  t=t+1
  viirs = raster(ras_list[t])  
  t=t+1
  ref = raster(ras_list[t])  
  
  mod_list = list.files(paste("/Users/stijnhantson/Documents/data/MCD64_v6/Win03/",year[ras_sh],sep=""), pattern = "burndate.tif$", recursive = TRUE, full.names=T)
  mod_stack=stack(mod_list)
  mod_stack=brick(mod_stack)
  mod_doy=max(mod_stack)
  mod = projectRaster(mod_doy, ref,method="ngb")
  mod[mod<1]=NA
  plot(mod)
  
  mod[mod<1] = NA
  ref[ref<0] = NA
  viirs[viirs<0] = NA
  print(t)
  un = unique(ref)
  un2=diff(un)
  for (tt in 1:(length(un)-1)){
    tt2=tt+1
    if (un2[tt]>1){###remove observations where ref does not have daily continous observations
      ref[ref==un[tt2]] = NA
    }}
  
  
  dif=viirs-ref
  dif2=mod-ref
  
  if (names[ras_sh]=="rough" | names[ras_sh]=="littles"){## these seem to have data taken in the morning
    dif=dif+1
    dif2=dif2+1
  }
  
  if (names[ras_sh]=="WhiteBaldy"){## DOY not well calculated
    dif=dif-1
    dif2=dif2-1
  }
  
  dif[dif> 400] = NA
  dif[dif< -400] = NA
  
  dif2[dif2> 400] = NA
  dif2[dif2< -400] = NA
  
  rmse=((sum((na.omit(values(dif)))^2))/length(na.omit(values(dif))))^0.5
  print(rmse)
  rmse2=((sum((na.omit(values(dif2)))^2))/length(na.omit(values(dif2))))^0.5
  print(rmse2)
  
  dif[dif> 5] = 5
  dif[dif< -5] = -5
  
  dif2[dif2> 5] = 5
  dif2[dif2< -5] = -5
  
  dif_viirs = na.omit(values(dif))
  dif_mod = na.omit(values(dif2))
  
  #hist(dif,xlim=c(-10,10),breaks=21)
  
  #hist(dif2, breaks=seq(min(dif2)-0.5, max(dif2)+0.5, by=1), xlim=c(-5.5,5.5))
  #hist(dif2, breaks=seq(-9.5, 9.5, by=1))
  
  #plot(histogram(dif2, breaks=seq(min(dif2)-0.5, max(dif2)+0.5, by=1), xlim=c(-5.5,5.5),type="density",col = rgb(0.1,0.1,0.1,0)))
  
  plot(hist(dif_viirs, breaks=seq(min(dif_viirs)-0.5, max(dif_viirs)+0.5, by=1), xlim=c(-5.5,5.5),type="density",col = rgb(0.1,0.1,0.1,0),xlab=NULL,ylab=NULL,main=names[ras_sh]))
  plot(hist(dif_mod, breaks=seq(min(dif_mod)-0.5, max(dif_mod)+0.5, by=1), xlim=c(-5.5,5.5),type="density",col = rgb(0.1,0.1,0.1,0),xlab=NULL,ylab=NULL,main=names[ras_sh]))
  
  #plot(dif,zlim=c(-5,5))
}
dev.off()



############# validate fire growth  #####################
ras_dir = "/Users/stijnhantson/Documents/projects/VIIRS_ros/evaluation/test/"
ras_list =  list.files(ras_dir, pattern = ".tif$", recursive = TRUE, full.names=T)
modis_list = list.files("/Users/stijnhantson/Documents/data/MCD64_v6/Win03/", pattern = "burndate.tif$", recursive = TRUE, full.names=T)



tiff(file="/Users/stijnhantson/Documents/projects/VIIRS_ros/test1.tif",width=3000,height=3000, res=350)
par(mfrow=c(5,3),mar=c(3,3,1,0))
ff1=0
final_size_v=0
final_size_r=0
final_size_m=0
t=0
ras_sh =1
for (ras_sh in 1:((length(ras_list))/2)){
  print(names[ras_sh]) 
  t=t+1
  viirs = raster(ras_list[t])  
  t=t+1
  ref = raster(ras_list[t])  
  
  
  if (names[ras_sh]=="rough" | names[ras_sh]=="littles"){## these seem to have data taken in the morning
    ref=ref+1
  }
  if (names[ras_sh]=="WhiteBaldy"){## DOY not well calculated
    ref=ref+1
  }
  
  if (names[ras_sh]=="bagley"){## DOY not well calculated
    ref=ref-1
  }
  if (names[ras_sh]=="lake"){## DOY not well calculated
    ref=ref-1
  }
  
  mod_list = list.files(paste("/Volumes/MyBookDuo/MCD64_v6/Win03/",year[ras_sh],sep=""), pattern = "burndate.tif$", recursive = TRUE, full.names=T)
  mod_stack=stack(mod_list)
  mod_stack=brick(mod_stack)
  mod_doy=max(mod_stack)
  mod = projectRaster(mod_doy, ref,method="ngb")
  mod[mod<1]=NA
  # dev.off()
  plot(mod)
  mod1=as.list(mod1,mod)
  
  viirs_u=unique(viirs)
  ref_u=unique(ref)
  days = unique(c(viirs_u,ref_u))
  f_v=as.data.frame(freq(viirs))
  f_r=as.data.frame(freq(ref))
  f_m=as.data.frame(freq(mod))
  
  ff = merge(f_v,f_r,by=c("value","value"),all=T)
  ff= merge(ff,f_m,by=c("value","value"),all=T)
  ff=ff[!ff$value< (-999),]
  
  final_size_v[ras_sh] = sum(ff$count.x, na.rm=T)
  final_size_r[ras_sh] = sum(ff$count.y, na.rm=T)
  final_size_m[ras_sh] = sum(ff$count, na.rm=T)
  
  for (k in 2:(length(ff))){ ### remove values after a NA
    if (is.na(ff[k-1,3])){
      ff = ff[-k,]
    }
  }
  
  if (names[ras_sh] != "holloway"){
    ff1=rbind(ff1,ff)
  }}


ff1=ff1[-1,]
ff2=na.omit(ff1)
#ff2=na.omit(ff)

plot(ff2$count.x,ff2$count.y,xlim=c(0,20000),ylim=c(0,20000))
plot(ff2$count.y,ff2$count,xlim=c(0,20000),ylim=c(0,20000))
plot(log10(ff2$count.x),log10(ff2$count.y),xlim=c(0,5),ylim=c(0,5))
plot(log10(ff2$count),log10(ff2$count.y),xlim=c(0,5),ylim=c(0,5))

lm(log(ff2$count.x+1)~log10(ff2$count.y+1),na.rm=T)

plot(ff2$count.x,ff2$count.y)
summary(lm(ff2$count.x~ff2$count.y))
summary(lm(ff2$count~ff2$count.y))

summary(lm(log10(ff2$count.x)~log10(ff2$count.y)))
summary(lm(log10(ff2$count)~log10(ff2$count.y)))

plot(density(ff1$count.x,na.rm=T),col="blue")
lines(density(ff1$count.y,na.rm=T),col="red")
lines(density(ff1$count,na.rm=T),col="green")

histrv<-hist(ff1$count.x, breaks=c(0,200,500,1000,2000,5000,50000))
VIIRS=histrv$counts

histrv<-hist(ff1$count.y, breaks=c(0,200,500,1000,2000,5000,50000))
reference=histrv$counts

histrv<-hist(ff1$count, breaks=c(0,200,500,1000,2000,5000,50000))
MCD64=histrv$counts


histrv<-hist(ff1$count.x, breaks=c(0,200,600,1500,3000,50000))
VIIRS=histrv$counts
max(ff1$count.x,na.rm=T)
histrv<-hist(ff1$count.y, breaks=c(0,200,600,1500,3000,50000))
reference=histrv$counts
max(ff1$count.y,na.rm=T)
histrv<-hist(ff1$count, breaks=c(0,200,600,1500,3000,50000))
MCD64=histrv$counts
max(ff1$count,na.rm=T)

counts <- t(cbind(reference,VIIRS,MCD64))
mp=barplot(counts, main="",
           xlab="", col=c("lightgrey","black","darkgrey"),
           legend = rownames(counts), beside=TRUE,density=c(NA,8,20),angle = c(0,135,45),names.arg = c("0-200","200-600","600-1500","1500-3000",">3000"), axisnames = FALSE)

colnames(counts)=c("0-200","200-600","600-1500","1500-3000",">3000")
mtext(text = colnames(counts), side = 1, at = mp[2,], line = 0)
title(xlab=expression(paste("Fire expantion (ha day"^"2"*")")), mgp=c(1.6,1,0),cex.lab=1.2)

############ validate  fire size ################


plot(final_size_v,final_size_r)
plot(final_size_m,final_size_r)
summary(lm(final_size_v~final_size_r))
summary(lm(final_size_m~final_size_r))

