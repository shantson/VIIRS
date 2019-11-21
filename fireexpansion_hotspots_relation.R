

#make gif
# in terminal: 
#convert -delay 80 /Users/stijnhantson/Documents/projects/VIIRS_ros/output/*.png /Users/stijnhantson/Documents/projects/VIIRS_ros/test/test1.gif

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

lonlat = crs("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
TA <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")

sep_dis = 1900                    #distance in m to seprate ignitions, 6 pixels = 2250
plot_gif = F                      # whether you want to output the png for each timestep
only_night = T                    # daily or twice daily fire line


data_s = c("0",0,0,0,0,0,0,0,0)
viirs_dir="/Users/stijnhantson/Documents/data/VIIRS/global_archive"
shape1 <- readOGR("/Users/stijnhantson/Documents/data/FRAP/firep17_1.shp") #readin FRAP fire perimeter data

#shape1 <- readOGR("/Users/stijnhantson/Documents/data/FRAP/FIREP18_DRAFT_DO_NOT_DISTRIBUTE/FIREP18_DRAFT_DO_NOT_DISTRIBUTE.shp")

shape1$YEAR=as.numeric(as.character(shape1$YEAR_)) 
UseCores <- detectCores() -1
dir.create(file.path(out_dir), showWarnings = FALSE)

time_dif = 0.25
if (only_night == T){
  time_dif <- 0.75
}

year = 2012
for (year in 2012:2017){
#foreach(year=2012:2017,.packages=c("sp","rgeos","alphahull","geosphere","igraph","png","rgdal","raster")) %dopar% {
  
  #ra=mod1
  
  shape2 <- subset(shape1, YEAR_==year)  #extract fires which occured during the year of interest
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
  shape2=subset(shape2, GIS_ACRES>10000)
  crs(shape2)=crs(po2)
  po2$dat = as.Date(as.character(po2$YYYYMMDD), format= "%Y%m%d")
  nr_fire = 1
  

  
  for (nr_fire in 1:length(shape2)){  
    print("nrfire")
    print(nr_fire)
    fire = shape2[nr_fire,]
    
    firename = fire$FIRE_NAME[1]

    if (firename != "SPRING PEAK"){
    
    fire1 = gBuffer(fire,width = 750) #extract all VIIRS points within a 750m buffer arround the perimeter
    pts_in = po2[!is.na(over(po2,as(fire1,"SpatialPolygons"))),]
    #pts_in$dat = as.Date(as.character(pts_in$YYYYMMDD), format= "%Y%m%d")
    #extract only VIIRS data between incidence and containment data +-1
    ign2 = (as.Date(fire$ALARM_DATE))-1
    sup = (as.Date(fire$CONT_DATE))+1
    if (is.na(ign2)){
      ign2 = sup - 90
    }
    
    if (is.na(sup)){
      sup = ign2 + 90
    }
    pts_in = pts_in[pts_in$dat > ign2,]
    pts_in = pts_in[pts_in$dat < sup,]
    
    #convert day-hour-minute to decimal DOY
    pts_in$DOY = as.numeric( strftime(pts_in$dat,format = "%j"))
    hour=(pts_in$HHMM/100) 
    hour1=as.numeric(lapply(hour, as.integer)) 
    dec=(((hour-hour1)/60)*(1/24))*100
    pts_in$DOY=((hour1/24)+dec)+ pts_in$DOY
    detections = sort(unique(pts_in$DOY))
    len1=length(detections)
    
    rrrr=0
    if (len1>1){
      for (detr in 1:(len1-1)){
        if( detections[detr+1]-detections[detr] > 0.3){
          rrrr[detr] = detections[detr]
        }else{
          rrrr[detr] = detections[detr+1]
        }
      }
      rrrr[len1] = detections[len1]
      detections = sort(unique(rrrr))
      len1=length(detections)
      
      rrrr=0
      dddd =0
      if (only_night == T){            #only slect midnight detections
        rrrr= (as.integer(detections))
        dddd=detections-rrrr
        rrrr=rrrr[dddd<0.6]
        rrrr=rrrr+0.6
        detections = sort(unique(rrrr))
        len1=length(detections)
      }
      
    
          pat=paste("/Users/stijnhantson/Documents/projects/VIIRS_ros/final_results4/",year,"_",firename,"_daily.shp",sep="")
          firespread <- readOGR(pat) #readin FRAP fire perimeter data
          firespread=spTransform(firespread,TA)
          
      if (len1 >5){ 
        sp=1
        l<-c()  
        while (sp < len1+1){   
        print(sp)
        det_day =  pts_in[pts_in$DOY <= detections[sp] & pts_in$DOY > (detections[sp] - time_dif),]  #VIIRS points at timestep
      if (sp == 1){
       oud = 0
       allhotspot=length(det_day)
       fire_new = firespread[firespread$DOY<detections[(sp)] & firespread$DOY>(detections[(sp)]-1) ,]
       growth = gArea(fire_new)  
       frp=sum(det_day$FRP)
       frp_new=frp
       nr_obs=length(unique(det_day$HHMM))
       pixarea = mean(det_day$pixarea)
      }else{
        fire_prev = firespread[firespread$DOY<detections[(sp-1)] & firespread$DOY>(detections[(sp-1)] -1) ,]
        fire_new = firespread[firespread$DOY<detections[(sp)] & firespread$DOY>(detections[(sp)] -1),]
        growth = gArea(fire_new) - gArea(fire_prev)
        
        fire_prev = gBuffer(fire_prev,width = 10)
        eracht = det_day[!is.na(over(det_day,as(fire_prev,"SpatialPolygons"))),]
        
        oud = length(eracht)
        allhotspot =length(det_day)
        frp=sum(det_day$FRP)
        frp_new = frp - sum(eracht$FRP)
        nr_obs=length(unique(det_day$HHMM))
        pixarea = mean(det_day$pixarea)
      }
        datt = c(as.character(firename),sp,growth,oud,allhotspot,frp,nr_obs,frp_new,pixarea)
        data_s = rbind(data_s,datt)
        sp=sp+1
        }
      }}}}}

plot(data_s[,3],data_s[,5])

data_s1=as.data.frame(data_s)
data_s1$V2=as.numeric(as.character(data_s1$V2))
data_s1$V3=as.numeric(as.character(data_s1$V3))
data_s1$V4=as.numeric(as.character(data_s1$V4))
data_s1$V5=as.numeric(as.character(data_s1$V5))
data_s1$V6=as.numeric(as.character(data_s1$V6))
data_s1$V7=as.numeric(as.character(data_s1$V7))
data_s1$V8=as.numeric(as.character(data_s1$V8))
data_s1$V9=as.numeric(as.character(data_s1$V9))

data_s1$newhot = data_s1$V5-data_s1$V4
data_s1$oldfrp = data_s1$V6-data_s1$V8

plot(log10((data_s1$V5)+1),log10((data_s1$V3)+1),xlab= "log10(nr. all hotspots)",ylab="burnt area expancion (log10 m2)")
plot(log10((data_s1$newhot)+1),log10((data_s1$V3)+1),xlab= "log10(nr. infront fireline)",ylab="burnt area expancion (log10 m2)")
plot(log10((data_s1$V4)+1),log10((data_s1$V3)+1),xlab= "log10(nr. behind fireline)",ylab="burnt area expancion (log10 m2)")
plot(log10((data_s1$V6)+1),log10((data_s1$V3)+1),xlab= "log10(FRP sum)",ylab="burnt area expancion (log10 m2)")

plot((data_s1$V6),(data_s1$V5),xlab= "log10(FRP sum)",ylab="log10(nr. all hotspots)",xlim=c(0,10000),ylim=c(0,500))


ind_fire = data_s1[data_s1$V1 == "BAGLEY",]
ind_fire = data_s1[data_s1$V1 == "SOBERANES",]
ind_fire = data_s1[data_s1$V1 == "ROUGH",]

par(mar = c(5,5,2,5))
plot(ind_fire$V2,ind_fire$V3, lwd=2, type="l", ylab="fire size increase (m2/day)",xlab="day of fire")
par(new = T)
plot(ind_fire$V2,ind_fire$V5,col="grey", type="l",axes=F, xlab=NA, ylab=NA,lwd=1.5)
axis(side = 4)
mtext(side = 4, line = 3, 'nr. hotspots')
lines(ind_fire$V2,ind_fire$newhot,col="red")
lines(ind_fire$V2,ind_fire$V4,col="blue")

legend("topright",
       legend=c("burnt area increase","hotspots behind fireline","hotspots in front of fireline","all hotspots"),
       lty=c(1,1,1), col=c("black", "blue","red","grey"),cex=0.8)

####################
plot(ind_fire$V2,ind_fire$V3, lwd=2, type="l", ylab="fire size increase (m2/day)",xlab="day of fire")

par(new = T)
plot(ind_fire$V2,ind_fire$V8,col="green", type="l",axes=F, xlab=NA, ylab=NA,lwd=1.5, ylim=c(0,4000))
axis(side = 4)
mtext(side = 4, line = 3, 'sum FRP')

par(new = T)
plot(ind_fire$V2,ind_fire$oldfrp,col="orange", type="l",axes=F, xlab=NA, ylab=NA,lwd=1.5,ylim=c(0,4000))

#par(new = T)
#plot(ind_fire$V2,ind_fire$V6,col="orange", type="l",axes=F, xlab=NA, ylab=NA,lwd=1.5, , ylim=c(0,15000))


legend("topright",
       legend=c("burnt area increase","FRP new","FRP old"),
       lty=c(1,1,1), col=c("black", "green","orange"),cex=0.8)



tes = lm(data_s1$V3 ~ data_s1$V5 + (data_s1$V9)+ (data_s1$V7))
summary(tes)

#########################

plot(ind_fire$V2,ind_fire$V3, lwd=2, type="l", ylab="fire size increase (m2/day)",xlab="day of fire")

par(new = T)
plot(ind_fire$V2,ind_fire$V6,col="green", type="l",axes=F, xlab=NA, ylab=NA,lwd=1.5)
axis(side = 4)
mtext(side = 4, line = 3, 'FRP sum', col="green")

par(new = T)
plot(ind_fire$V2,ind_fire$V7,col="yellow", type="l",axes=F, xlab=NA, ylab=NA,lwd=1.5, ylim=c(-10,4))

par(new = T)
plot(ind_fire$V2,ind_fire$V9,col="orange", type="l",axes=F, xlab=NA, ylab=NA,lwd=1.5, ylim=c(-1,1))
axis(side = 4, line = 5.5, ylim=c(0,0.5))


legend("topright",
       legend=c("burnt area increase","FRP sum","mean pixel area","nr. overpasses (1-4)"),
       lty=c(1,1,1), col=c("black", "green","orange","yellow"),cex=0.8)



######## how much hotspots after initial burn

lonlat = crs("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
TA <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")

sep_dis = 1900                    #distance in m to seprate ignitions, 6 pixels = 2250
plot_gif = F                      # whether you want to output the png for each timestep
only_night = T                    # daily or twice daily fire line


data_p = c(0,0,0,0,0,0)
data_frp = c(0,0,0,0,0,0)
viirs_dir="/Users/stijnhantson/Documents/data/VIIRS/global_archive"
shape1 <- readOGR("/Users/stijnhantson/Documents/data/FRAP/firep17_1.shp") #readin FRAP fire perimeter data

#shape1 <- readOGR("/Users/stijnhantson/Documents/data/FRAP/FIREP18_DRAFT_DO_NOT_DISTRIBUTE/FIREP18_DRAFT_DO_NOT_DISTRIBUTE.shp")

shape1$YEAR=as.numeric(as.character(shape1$YEAR_)) 
UseCores <- detectCores() -1
dir.create(file.path(out_dir), showWarnings = FALSE)

time_dif = 0.25
if (only_night == T){
  time_dif <- 0.75
}

year = 2012
for (year in 2012:2017){
  #foreach(year=2012:2017,.packages=c("sp","rgeos","alphahull","geosphere","igraph","png","rgdal","raster")) %dopar% {
  
  #ra=mod1
  
  shape2 <- subset(shape1, YEAR_==year)  #extract fires which occured during the year of interest
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
  shape2=subset(shape2, GIS_ACRES>10000)
  crs(shape2)=crs(po2)
  po2$dat = as.Date(as.character(po2$YYYYMMDD), format= "%Y%m%d")
  nr_fire = 1
  
  
  
  for (nr_fire in 1:length(shape2)){  
    print("nrfire")
    print(nr_fire)
    fire = shape2[nr_fire,]
    
    firename = fire$FIRE_NAME[1]
    
    if (firename != "SPRING PEAK" & firename !="ELM"){
      
      fire1 = gBuffer(fire,width = 750) #extract all VIIRS points within a 750m buffer arround the perimeter
      pts_in = po2[!is.na(over(po2,as(fire1,"SpatialPolygons"))),]
      #pts_in$dat = as.Date(as.character(pts_in$YYYYMMDD), format= "%Y%m%d")
      #extract only VIIRS data between incidence and containment data +-1
      ign2 = (as.Date(fire$ALARM_DATE))-1
      sup = (as.Date(fire$CONT_DATE))+1
      if (is.na(ign2)){
        ign2 = sup - 90
      }
      
      if (is.na(sup)){
        sup = ign2 + 90
      }
      pts_in = pts_in[pts_in$dat > ign2,]
      pts_in = pts_in[pts_in$dat < sup,]
      
      #convert day-hour-minute to decimal DOY
      pts_in$DOY = as.numeric( strftime(pts_in$dat,format = "%j"))
      hour=(pts_in$HHMM/100) 
      hour1=as.numeric(lapply(hour, as.integer)) 
      dec=(((hour-hour1)/60)*(1/24))*100
      pts_in$DOY=((hour1/24)+dec)+ pts_in$DOY
      detections = sort(unique(pts_in$DOY))
      len1=length(detections)
      
      rrrr=0
      if (len1>1){
        for (detr in 1:(len1-1)){
          if( detections[detr+1]-detections[detr] > 0.3){
            rrrr[detr] = detections[detr]
          }else{
            rrrr[detr] = detections[detr+1]
          }
        }
        rrrr[len1] = detections[len1]
        detections = sort(unique(rrrr))
        len1=length(detections)
        
        rrrr=0
        dddd =0
        if (only_night == T){            #only slect midnight detections
          rrrr= (as.integer(detections))
          dddd=detections-rrrr
          rrrr=rrrr[dddd<0.6]
          rrrr=rrrr+0.6
          detections = sort(unique(rrrr))
          len1=length(detections)
        }
        
        
        pat=paste("/Users/stijnhantson/Documents/projects/VIIRS_ros/final_results4/",year,"_",firename,"_daily.shp",sep="")
        firespread <- readOGR(pat) #readin FRAP fire perimeter data
        firespread=spTransform(firespread,TA)
        
        if (len1 >2){ 
          sp=1
          l<-c()  
          while (sp < len1+1){   
    print("day") 
            print(sp)
      fire_day = firespread[sp,]
      min_doy = as.integer(fire_day$DOY -0.6)
      if (sp>1){
        fire_day = erase(fire_day,firespread[(sp-1),])
      }
      if (length(fire_day )>0){
      fire_day = gBuffer(fire_day,width = 1) #extract all VIIRS points within a 750m buffer arround the perimeter
      
      pts_in_new =  pts_in[!is.na(over( pts_in,as(fire_day,"SpatialPolygons"))),]
      pts_in_new$DOY2 = as.integer(pts_in_new$DOY - 0.6)

     
      valu = pts_in_new$DOY2 -   min_doy
      pts_in_new$valu = pts_in_new$DOY2 -   min_doy
      
      frp0 = pts_in_new[pts_in_new$valu == 0,]
      frp1 = pts_in_new[pts_in_new$valu == 1,]
      frp2 = pts_in_new[pts_in_new$valu == 2,]
      frp3 = pts_in_new[pts_in_new$valu == 3,]
      frp4 = pts_in_new[pts_in_new$valu == 4,]
      frp5 = pts_in_new[pts_in_new$valu == 5,]
      
      frp0 = sum(frp0$FRP)
      frp1 = sum(frp1$FRP)
      frp2 = sum(frp2$FRP)
      frp3 = sum(frp3$FRP)
      frp4 = sum(frp4$FRP)
      frp5 = sum(frp5$FRP)
      
     a = as.data.frame(table(valu))
    a$valu= as.numeric(as.character(a$valu))
     
    r0 = subset(a, valu == 0)[,2]
    r1 = subset(a, valu == 1)[,2]
    r2 = subset(a, valu == 2)[,2]
    r3 = subset(a, valu == 3)[,2]
    r4 = subset(a, valu == 4)[,2]
    r5 = sum(subset(a, valu >4)[,2])
  
    
    if (identical(r0, integer(0))){r0=0}
    if (identical(r1, integer(0))){r1=0}
    if (identical(r2, integer(0))){r2=0}
    if (identical(r3, integer(0))){r3=0}
    if (identical(r4, integer(0))){r4=0}
    if (identical(r5, integer(0))){r5=0}

    
            datt = c(r0,r1,r2,r3,r4,r5)
            frp_fin = c(frp0,frp1,frp2,frp3,frp4,frp5)
            data_p = rbind(data_p,datt)
            data_frp = rbind(data_frp,frp_fin)
      }
            sp=sp+1
          }
        }}}}}

afterfire = as.data.frame(data_p)
frp_after = as.data.frame(data_frp)
 pl =colSums(afterfire)
 barplot(pl,names.arg=c("Day0", "Day1", "Day2", "Day3", "Day4",">Day4"),ylab="sum hotspots",cex.lab=1.5,cex.axis=1.5,cex.names=1.5)

 pl_frp =colSums(frp_after)
 barplot(pl_frp,names.arg=c("Day0", "Day1", "Day2", "Day3", "Day4",">Day4"),ylab="sum FRP",cex.lab=1.5,cex.axis=1.5,cex.names=1.5)
 
