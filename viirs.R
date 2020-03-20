

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

sep_dis = 1900                    #distance in m to seprate ignitions, 6 pixels = 2250
plot_gif = F                      # whether you want to output the png for each timestep
only_night = T                    # daily or twice daily fire line

out_dir = "/Users/stijnhantson/Documents/projects/VIIRS_ros/final_results7/"
out_dir2 = "/Users/stijnhantson/Documents/projects/VIIRS_ros/test/"

mod = raster("/Users/stijnhantson/Documents/data/MCD64_v6/Win03/2000/MCD64monthly.A2000336.Win03.006.burndate.tif")
viirs_dir="/Users/stijnhantson/Documents/data/VIIRS/global_archive"
shape1 <- readOGR("/Users/stijnhantson/Documents/data/FRAP/fire18_1.shp") #readin FRAP fire perimeter data

#shape1 <- readOGR("/Users/stijnhantson/Documents/data/FRAP/FIREP18_DRAFT_DO_NOT_DISTRIBUTE/FIREP18_DRAFT_DO_NOT_DISTRIBUTE.shp")

shape1$YEAR=as.numeric(as.character(shape1$YEAR_)) 
UseCores <- detectCores() -1
dir.create(file.path(out_dir), showWarnings = FALSE)

time_dif = 0.25
if (only_night == T){
  time_dif <- 0.75
  }

point2pol = function(x,y,da,pro){     # function which converts VIIRS points to fire perimeter
 al=0.01                          # alpha value, which increment with 0.005 if perimeter cannot be closed
 p=1
 while (p==1) {
 # print(al)
      po1=ashape(x,y,alpha=al)
  order.chull <- graph.edgelist(cbind(as.character(po1$edges[, "ind1"]), as.character(po1$edges[,"ind2"])), directed = FALSE)
  
    cutg <- order.chull - E(order.chull)[1]
  ends <- names(which(degree(cutg) == 1))
  if (is.na(ends[1])){
#    print("trr")
    al=al+0.005
  }else if (!is.connected(order.chull)) {
 #     print("trr")
      al=al+0.005
    }else if (any(degree(order.chull) != 2)) {
 #     print("trr")
      al=al+0.005
    }else if (clusters(order.chull)$no > 1) {
#      print("trr")
      al=al+0.005
    }else{
#    print("2")
    p <- 0
    }
 }
  path <- get.shortest.paths(cutg, ends[1], ends[2])[[1]]   #extract polygone from alpha shape
  pathX <- as.numeric(V(order.chull)[unlist(path[[1]])]$name)
  pathX = c(pathX, pathX[1])
  data.chull <- da[pathX, ]
#  data.chull <- po1$x[pathX, ]
  data.chull.poly <- SpatialPolygons(list(Polygons(list(Polygon(data.chull)),1))) #transform to spatial polygon data
  crs(data.chull.poly) = pro
  return(data.chull.poly)
}

rbPal <- colorRampPalette(c('red','yellow',"green","blue","black"))
lonlat = crs("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
TA <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +datum=NAD83 +units=m +ellps=GRS80")
dom = extent(-125,-114,32,42)
#mod=crop(mod,dom)
#mod1=projectRaster(mod,crs =TA,method="ngb",res=c(500,500))
#mod1[] = NA
sp=1

year=2012
cl       <- makeCluster(UseCores)
registerDoParallel(cl)

for (year in 2012:2018){
#foreach(year=2012:2018,.packages=c("sp","rgeos","alphahull","geosphere","igraph","png","rgdal","raster")) %dopar% {
   
#ra=mod1

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




#foreach(nr_fire=1:length(shape2),.packages=c("sp","rgeos","alphahull","geosphere","igraph","png","rgdal","raster")) %dopar% {
  
for (nr_fire in 1:length(shape2)){  # perform analysis for each fire
#subset VIIRS data spatialy and temporaly

 fire = shape2[nr_fire,]

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

#fir = mod1
pts_in$dist = NA
pts_in$ros = NA
pts_in$pre_date = NA
pts_in$prelon = NA
pts_in$prelat = NA
ros = pts_in[0,]
sp=1

if (plot_gif ==T){
plot(fire)
outna = paste(out_dir2,fire$FIRE_NAME[1],".png",sep="")
png(file = outna)
dev.set(which = 2)
dev.copy(which = 4)
dev.off()
}

if (len1 <2){   #we only consider fires wiht more than 1 timestep
  
}else{
sp=1
l<-c()  
  while (sp < len1+1){        #move trough each timestep with the VIIRS data
    #list to save spatial polygon data in
    print("day")
 print(sp)
    det_day =  pts_in[pts_in$DOY <= detections[sp] & pts_in$DOY > (detections[sp] - time_dif),]  #VIIRS points at timestep
  det = pts_in[pts_in$DOY <= detections[sp],]         #VIIRS points before and at timestep
   pre_det = pts_in[pts_in$DOY < (detections[sp] - time_dif),]     #VIIRS points before timestep
   sp=sp+1
  
   lon = det$lon
   lat = det$lat

  det$dist <- distm(cbind(lon,lat),cbind(lon[1],lat[1])) #calculate distance of each VIIRS point to the first VIIRS point in the dataset (to detect if there are multiple clusters)
  det1 = det[det@data$dist[,1] <= sep_dis,]
  det2 = det[det@data$dist[,1] > sep_dis,]
  
if  (length(det1) == length(det)){  #if all VIIRS points are within 3km from the first point
    nr_ign = 1
}else{                              #else loop trough until all VIIRS points are whithin 3km from each other or whether there are 2 groups sperated >3km
  spp=1
  
  #loop trough untill all hotspots are added to dist1 or untill all in dist2 are further than 3km away
  while(spp == 1 & length(det1) != length(det)){ 
    det2$dist = apply(distm(cbind(det2$lon,det2$lat), cbind(det1$lon,det1$lat)),1,FUN=min)#minimum disntance for each point in dist2 from any point in dist1

 if(min(det2$dist) <= sep_dis){                # when there are points in det2 which fall within 3km from any point in det1 extract them from det2 and add to det1
   rr = det2[det2@data$dist <= sep_dis,]
   det1=rbind(det1,rr)
   rr =0
   det2=det2[det2@data$dist > sep_dis,]
   spp=1
   nr_ign = 1
 }else{ # there are 2 or more clusters, check whetehr there is a 3e one. 
  len=length(det2)
  nr_ign=2
   det2$dist <- distm(cbind(det2$lon,det2$lat),cbind(det2$lon[1],det2$lat[1]))
   det3 = det2[det2@data$dist[,1] > sep_dis,]
   
     det2 = det2[det2@data$dist[,1] <= sep_dis,]
   stt=1
   while(stt == 1 & len != length(det2)){
     
     det3$dist = apply(distm(cbind(det3$lon,det3$lat),cbind(det2$lon,det2$lat)),1,FUN=min)
     
     if(min(det3$dist) <= sep_dis){
     rr=det3[det3@data$dist <= sep_dis,]
     det2=rbind(det2,rr)
     rr=0
     det3=det3[det3@data$dist > sep_dis,]
     }else{ #there are 3 or more clusters
      len_a = length(det3)
       nr_ign=3
       det3$dist <- distm(cbind(det3$lon,det3$lat),cbind(det3$lon[1],det3$lat[1]))
       
       det4 = det3[det3@data$dist[,1] > sep_dis,]
       
       det3 = det3[det3@data$dist[,1] <= sep_dis,]
       stt=1
       srr=1
       while(srr == 1 & len_a != length(det3)){
         
         det4$dist = apply(distm(cbind(det4$lon,det4$lat),cbind(det3$lon,det3$lat)),1,FUN=min)
         
         if(min(det4$dist) <= sep_dis){
           rr=det4[det4@data$dist <= sep_dis,]
           det3=rbind(det3,rr)
           rr=0
           det4=det4[det4@data$dist > sep_dis,]
         }else{ 
           nr_ign=4
           srr=2
         }
         
       }
       stt=2
      spp=2
     }
   }
   spp=2
   
 }
 
  }
  }

  maxdoy1 = max(det$DOY)
  maxyear1 = max(det$YYYYMMDD)

  ign=1
  for (ign in 1:nr_ign){   #loop trough each seperate perimeter
#    print(ign)
    if (ign == 1){
      det = det1
    }else if (ign == 2){
      det=det2
    }else if (ign == 3){
      det=det3
    }else if (ign == 4){
      det=det4    
      }
#print(det)
    len2 = length(unique(det$DOY))
 uni2 = sort(unique(det$DOY))
  rrrr2=0
  if (len2 == 1){#in case there is only one timestep detection
    uni=len2
  }else{
  for (detr in 1:(len2-1)){
    if( uni2[detr+1]-uni2[detr] > 0.3){
      rrrr2[detr] = uni2[detr]
    }else{
      rrrr2[detr] = uni2[detr+1]
    }
  }
  rrrr2[len2] = uni2[len2]
  uni2 = sort(unique(rrrr2))
  uni=length(uni2)
  }
  rrrr2=0
  dddd =0
  if (only_night == T){            #only slect midnight detections
    rrrr2= (as.integer(uni2))
    dddd=uni2-rrrr2
    rrrr2=rrrr2[dddd<0.6]
    rrrr2=rrrr2+0.6
    uni2 = sort(unique(rrrr2))
    uni=length(uni2)
  }
 
  if (uni != 0){ # jump to the end in case the first detection is not a nighttime one. 
  
  if (uni == 1 || length(det)<4){ # sp ==2 indicates first timestep or when ther eare less than 3 datapoints
    if (uni == 1 & length(det)>4 ){
      y= det$lat
      x=det$lon
      pol2 = point2pol(x,y,det,TA)
      pol2$DOY = maxdoy1
      pol2$YYYYMMDD = maxyear1
#      pol2$HHMM = max(det$HHMM)
      pol2$YEAR = year
      pol2$CAUSE = fire$CAUSE[1]
       l<-c(l,pol2)
    }else{
      y= det$lat
      x=det$lon
      
      mean_x = mean(x)
      mean_y = mean(y)
      df = data.frame(a = 1)
      center = SpatialPointsDataFrame(cbind(mean_x,mean_y),df,proj4string=lonlat)
      center= spTransform(center,TA)
      pol2 <- gBuffer( center, width=187.5, byid=TRUE )
      
      pol2$DOY = maxdoy1
      pol2$YYYYMMDD = maxyear1
#      pol2$HHMM = max(det$HHMM)
      pol2$YEAR = year
      pol2$CAUSE = fire$CAUSE[1]
      pol2=pol2[,-1]
      l<-c(l,pol2)
    }
    
 }else{
  
y= det$lat
x=det$lon
pol2 = point2pol(x,y,det,TA)
buf_pol2 =  gBuffer(pol2,width = -100)

if (length(buf_pol2) == 0){
  buf_pol2 =  gBuffer(pol2,width = -20)
}

if (length(buf_pol2) == 0){
  buf_pol2 =  gBuffer(pol2,width = 0)
}

det_day =  det[det$DOY <= detections[sp-1] & det$DOY > (detections[sp-1] - time_dif),] 
pre_det = det[det$DOY < (detections[sp-1]- time_dif),] 

det_day$new = !is.na(over(det_day,buf_pol2))
det_new =  det_day[det_day$new == "FALSE",]

if (length(det_new) > 0){ #if there are no new thermal anomalies on the fire edge
maxdate = max(det_day$YYYYMMDD)
trs = det_day[det_day$YYYYMMDD==maxdate,]
maxhour = max(trs$HHMM)
pre_maxdoy = max(pre_det$DOY)
maxdoy = max(det_day$DOY)



#if (length(pre_det)<4){
#  y= pre_det$lat
#  x=pre_det$lon
#  mean_x = mean(x)
#  mean_y = mean(y)
#  df = data.frame(a = 1)
#  center = SpatialPointsDataFrame(cbind(mean_x,mean_y),df,proj4string=lonlat)
#  center= spTransform(center,TA)
 
#   det_new$dist = apply(gDistance(det_new, center,byid=TRUE),2,min)
#  det_new$ros = det_new$dist/(( detections[sp-1] - detections[sp-2])*24)
#  pts_in$pre_date = pre_det$DOY[1]
 
#   pol1 = spTransform(center,lonlat)
#  det_new$prelon = xmin(pol1)
#  det_new$prelat = ymin(pol1)
#  
#  ros = rbind(ros, det_new)

# }else{

#y= pre_det$lat
#x=pre_det$lon
#pol3 = point2pol(x,y,pre_det,TA)

#det_new$dist = apply(gDistance(det_new, pol3,byid=TRUE),2,min)
det_new$dist = apply(gDistance(det_new, l2,byid=TRUE),2,min)
det_new$ros = det_new$dist/((maxdoy - pre_maxdoy)*24)
det_new$pre_date = pre_maxdoy

#print(det_new$dist)

pol1 = spTransform(l2,lonlat)
det_new2=spTransform(det_new,lonlat)
pr=as.data.frame(dist2Line(det_new2,pol1))     #gives you intersection point

det_new$prelon = pr$lon
det_new$prelat = pr$lat

ros = rbind(ros, det_new)

if (plot_gif == T){
lines(pol2)   #plot lines for each timestep

outna = paste(out_dir2,fire$FIRE_NAME[1],det_new$DOY[1],".png",sep="")
png(file = outna)
dev.set(which = 2)
dev.copy(which = 4)
dev.off()

img =readPNG(outna) #open png new to write text date on it
#get size
h<-dim(img)[1]
w<-dim(img)[2]
#open new file for output
png(outna, width=w, height=h)
par(mar=c(0,0,0,0), xpd=NA, mgp=c(0,0,0), oma=c(0,0,0,0), ann=F)
plot.new()
plot.window(0:1, 0:1)
#fill plot with image
usr<-par("usr")    
rasterImage(img, usr[1], usr[3], usr[2], usr[4])
#add text
text(.8,.05, det_new$YYYYMMDD[1], cex=3, col="black")
#close image
dev.off()
}
}

pol2$DOY = maxdoy1
pol2$YYYYMMDD = maxyear1
#pol2$HHMM = maxhour
pol2$YEAR = year
pol2$CAUSE = fire$CAUSE[1]
l<-c(l,pol2)     # include new polygon in list of polygons

#print(l)
  }}}#}
  le = length(l)          #make polygon file from all polygons after timeset is over to be used as pre-timestep reference
  l2=l[[1]]
  if (le > 1){
    for (tr in 2:le){
      l2=rbind(l2,l[[tr]])
    }
  }
 print("l2")
   }

# pts_in$Col <- rbPal(100)[as.numeric(cut(pts_in$YYYYMMDD,breaks = 20))]
# pts_in$Col <- rbPal(100)[as.numeric(cut(pts_in$sample ,breaks = 100))]
# pts_in$Col <- rbPal(100)[as.numeric(cut(pts_in$pixarea ,breaks = 100))]



le = length(l)######## put them together
l2=l[[1]]
if (le > 1){
for (tr in 2:le){
l2=rbind(l2,l[[tr]])
}
}


li = unique(l2$DOY) ###### put polygons of the same day together
li_l = length(li)
l3=c()
for (lit in 1:li_l){
 pss = l2[l2$DOY==li[lit],]
 if (length(pss$DOY)>1){
 pss =  aggregate(pss, c("DOY","YYYYMMDD","YEAR","CAUSE")) 
 }
 l3=c(l3,pss)
}

l4=c()######## intersect with posterior polgygons
pri = length(l3)
kr=1
for (kr in 1:(pri-1)){
 # print(kr)
   plu=kr+1
  plu1=kr+2
  toge=l3[[plu]]
  if (pri-plu > 0){
    for (tr in plu1:pri){
      toge=rbind(toge,l3[[tr]])
    }
  }
  para=intersect(l3[[kr]],toge)
  if (is.null(para)){
    l4=c(l4,l3[[kr]])
  }else{
  para$area <- area(para)/1000000
 min_ar = min(para$area)
 para=para[para$area==min_ar,]
  
  para <- para[,-(5:9)]
  names(para@data)[1] <- "DOY"
  names(para@data)[2] <- "YYYYMMDD"
  names(para@data)[3] <- "YEAR"
  names(para@data)[4] <- "CAUSE"
  l4=c(l4,para)
  }
}
l4=c(l4,l3[[pri]])


l5=c()
le = length(l4)
l5=l4[[1]]
if (le > 1){
  for (tr in 2:le){
    l5=rbind(l5,l4[[tr]])
  }
}

l2<-aggregate(l5, c("DOY","YYYYMMDD","YEAR","CAUSE")) 

#fire = gBuffer(fire, byid=TRUE, width=0) # correct small topological errors
#fire2 = aggregate(fire)   #agregate the reference fire perimeter
#l2 = intersect(l2, fire)    # clip the 


writeOGR(l2, out_dir, layer= paste(year,"_",fire$FIRE_NAME[1],"_daily",sep=""), driver="ESRI Shapefile", overwrite_layer = T)
writeOGR(ros, out_dir, layer= paste(year,"_",fire$FIRE_NAME[1],"_daily_ros",sep=""), driver="ESRI Shapefile", overwrite_layer = T)
}}
#if (!is.na(ros$ros[1]) & length(ros) >1){
# ros$Col <- rbPal(100)[as.numeric(cut(ros$ros,breaks = 100))]
##plot(fire)
#points(ros, col=ros$Col)
#}
}
#writeRaster(ra,"/Users/stijnhantson/Documents/data/test250.tif")
}

stopCluster(cl)









