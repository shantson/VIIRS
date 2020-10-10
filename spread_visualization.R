


library(rgeos)
library(raster)
library(rgdal)
library(foreign)
library(ncdf4)

out_dir = "/Users/stijnhantson/Documents/projects/VIIRS_ros/visualization/"
dir.create(out_dir)
viirs_dir = "/Users/stijnhantson/Documents/projects/VIIRS_ros/final_results8_daily/"
gridmet_dir = "/Volumes/mypassport/datasets/gridmet/"

viirs_list = list.files(viirs_dir, pattern = "daily_ros.shp$", recursive = TRUE, full.names=T)
viirs_dbf = list.files(viirs_dir, pattern = "daily_ros.dbf$", recursive = TRUE, full.names=T)
spread_list =list.files(viirs_dir, pattern = "_daily.shp$", recursive = TRUE, full.names=T)

p=1  
for (p in 1:length(viirs_list)){
  print(p)
  rows_p = read.dbf(viirs_dbf[p])
  
  if (length(rows_p$YYYYMMDD)>0){
    viirs_all=shapefile(viirs_list[p])
    firespread <- readOGR(spread_list[p]) 
    firespread$DOY2 = as.integer(firespread$DOY - 0.5)
    firename = substring(viirs_list[p],71,(nchar(viirs_list[p])-14))
    year1 = substring(viirs_list[p],66,69)
    dar = (as.Date(viirs_all$dat)) ####### time is in UTM, so we have the nighttime day as the day after which 
    viirs_all$date =format(dar,"%Y%m%d")
    viirs_all$DOY2 = as.integer(viirs_all$DOY - 0.5)
    #summary(viirs_all)
    year = substring(dar[1],1,4)
    month = substring(dar[1],6,7)
    doy_out =     viirs_all$DOY2[1]
    firespread$YYYYMMDD2 = as.character((as.Date(as.character(firespread$YYYYMMDD),"%Y%m%d"))-1)
    
    inputyear=paste(year,"-01-01",sep="")
    days2 =  as.Date((unique(firespread$DOY2)-1) , origin = inputyear)
    days1 = unique(firespread$DOY2) 
    YYYYMMDD = unique(viirs_all$YYYYMMDD)
    total = firespread[firespread$YYYYMMDD == max(firespread$YYYYMMDD) ,]
    l<-c()  
    for (i in 1:length(days1)){
      
     
      fire_new = firespread[firespread$DOY2 == days1[i] ,]
      if (length(fire_new) > 0){
        growth = gArea(fire_new)  
      }else{growth = NA}
      yearmonday = days2[i]
      
      
      mont = substring(yearmonday,6,7)
      dag = substring(yearmonday,9,10)
      #   
      gridmet_list= list.files(gridmet_dir, pattern = paste(year,".nc$",sep=""), recursive = TRUE, full.names=T)
      l_tmmx = raster(gridmet_list[14],band = days1[i] ) #max temp
      l_th = raster(gridmet_list[12],band = days1[i] )#wind direction
      l_vs = raster(gridmet_list[16],band = days1[i] )#windspeed
      
      crs(l_vs)= CRS("+init=epsg:4326")
      crs(l_th)= CRS("+init=epsg:4326")
      crs(l_tmmx)= CRS("+init=epsg:4326")

      th = as.data.frame(extract(l_th, fire_new))
      tmmx = as.data.frame(extract(l_tmmx, fire_new))
      vs = as.data.frame(extract(l_vs, fire_new))
      
      fire_new$direction = as.numeric(quantile(th[,1], 0.5,na.rm=T))
      fire_new$tmmx = mean(tmmx[,1])
      fire_new$windspeed = mean(vs[,1])
      
      l<-c(l,fire_new)
    }
    le = length(l)          #make polygon file from all polygons after timeset is over to be used as pre-timestep reference
    l2=l[[1]]
    if (le > 1){
      for (tr in 2:le){
        l2=rbind(l2,l[[tr]])
      }
    }
  
    writeOGR(l2, out_dir, layer= paste(year,"_",firename,"_daily",sep=""), driver="ESRI Shapefile", overwrite_layer = T)
    
  }}


