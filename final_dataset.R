


################  extract data from downscaled data UCLA  #####################################



library(rgeos)
library(raster)
library(rgdal)
library(foreign)
library(ncdf4)
viirs_dir = "/Users/stijnhantson/Documents/projects/VIIRS_ros/final_results7/"
gridmet_dir = "/Users/stijnhantson/Documents/data/gridmet/"
wrf_data = "/Volumes/MyBookDuo/wrf_UCLA/"

frap=shapefile("/Users/stijnhantson/Documents/data/FRAP/fire18_1.shp")
frap=subset(frap,YEAR_ > 2011)
frap=subset(frap, GIS_ACRES>1000)
frap2=shapefile("/Users/stijnhantson/Documents/data/FRAP/FIREP18_DRAFT_DO_NOT_DISTRIBUTE/FIREP18_DRAFT_DO_NOT_DISTRIBUTE.shp")
frap2=subset(frap2, GIS_ACRES>1000)

viirs_list = list.files(viirs_dir, pattern = "daily_ros.shp$", recursive = TRUE, full.names=T)
viirs_dbf = list.files(viirs_dir, pattern = "daily_ros.dbf$", recursive = TRUE, full.names=T)
spread_list =list.files(viirs_dir, pattern = "_daily.shp$", recursive = TRUE, full.names=T)


#land = raster("/Users/stijnhantson/Documents/data/CAL_VEG/gaplf2011lc_v30_CA/gaplf2011lc_v30_ca.tif")
#tab = read.table("/Users/stijnhantson/Documents/data/CAL_VEG/gaplf2011lc_v30_CA/GAP_LANDFIRE_National_Terrestrial_Ecosystems_2011_Attributes.txt",sep="\t",header=T)
#tab=tab[,c("Value","CL")]
#tab=as.matrix(tab)
#landcover=reclassify(land,tab)
#landcover[landcover == 3 | landcover == 4  |  landcover == 6 |  landcover == 8 | landcover == 9 | landcover == 10] = 2
#landcover[landcover == 11] = NA
#writeRaster(landcover,"/Users/stijnhantson/Documents/data/cal_tree_grass.tif",overwrite=T)
landcover=raster("/Users/stijnhantson/Documents/data/cal_tree_grass.tif")
shape = shapefile("/Users/stijnhantson/Documents/data/veg_california/ca_eco_l3/ca_eco_l3.shp")
shape = spTransform(shape,crs(frap))


bio = raster("/Users/stijnhantson/Documents/data/2010_Biomass.tif")

viirs_all=shapefile(viirs_list[1])
result = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
daily_res = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

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
    
    frr=subset(frap,YEAR_ == year1)
    frr = subset(frr, FIRE_NAME == firename)
    if ((length(frr$FIRE_NAME)) == 0){
      frr = subset(frap2, FIRE_NAME == firename)
    }
    if ((length(frr$FIRE_NAME)) > 1){
      firespread$area = area(firespread)
      maxarea = max(firespread$area)
      viirs_sf = firespread[firespread$area == maxarea,]
      viirs_sf = as(viirs_sf[1,], 'SpatialPolygons')
      frr_sf = as(frr, 'SpatialPolygons')
      
      pi_sf = gIntersects(viirs_sf,frr_sf,byid=TRUE)
      frr = frr[as.vector(pi_sf),]
    }
    
    
    if (  is.null(firespread$ALARM_DATE) ){
      startdate = as.Date((as.character(min(firespread$YYYYMMDD))[1]),"%Y%m%d")-1
    } else {
      startdate =  as.Date(firespread$ALARM_DATE[1])
    }

    if (is.na(startdate)) {
      startdate = as.Date((as.character(min(firespread$YYYYMMDD))[1]),"%Y%m%d")-1
    }
     day_dif = abs(as.numeric(startdate - (as.Date((as.character(min(firespread$YYYYMMDD))[1]),"%Y%m%d")-1)))
     if (firename == "SWEDES"){  #NORTHPASS, SWEDES  fires have wrong alarm date, check HIDDEN & LAKES (2016)
       startdate = as.Date((as.character(min(firespread$YYYYMMDD))[1]),"%Y%m%d")-1
     }
    start_doy=as.numeric( strftime( startdate,format = "%j"))
    
    cause = as.numeric(as.character(firespread$CAUSE_1[1]))
    if (length(cause) == 0){
      cause = as.numeric(as.character(firespread$CAUSE[1]))
    }
    dar = as.Date(viirs_all$dat) ####### time is in UTM, so we have the nighttime day as the day after which 
    viirs_all$date =format(dar,"%Y%m%d")
    viirs_all$DOY2 = as.integer(viirs_all$DOY - 0.5)
    #summary(viirs_all)
    year = substring(dar[1],1,4)
    month = substring(dar[1],6,7)
    doy_out =     viirs_all$DOY2[1]
    #plot(viirs_all$ros~viirs_all$FRP)
    #plot(log(viirs_all$ros),log(viirs_all$FRP))
    
    if (is.na(start_doy)){
      start_doy=min(firespread$DOY2)
    }
    
    inputyear=paste(year,"-01-01",sep="")
    days2 =  as.Date((unique(firespread$DOY2)-1) , origin = inputyear)
    days1 = unique(firespread$DOY2) 
    YYYYMMDD = unique(viirs_all$YYYYMMDD)
    total = firespread[firespread$YYYYMMDD == max(firespread$YYYYMMDD) ,]
    total_area = gArea(total) 

 #   eco = as.data.frame(over(frr,shape))  
    eco_inter=intersect(frr,shape)
    eco_inter$area = area(eco_inter)
    eco= eco_inter[which.max(eco_inter$area),]
    eco=eco[1,]
    eco1= eco$NA_L1CODE
    eco2= eco$NA_L2CODE
    eco3= eco$NA_L3CODE
    
    
    i=1
    for (i in 1:length(days1)){
    
      fire_day = (days1[i]+1) - start_doy
      print(paste("day", i))
      fire_new = firespread[firespread$DOY2 == days1[i] ,]
      if (length(fire_new) > 0){
        growth = gArea(fire_new)  
      }else{growth = NA}
      yearmonday = days2[i]
      
      
      mont = substring(yearmonday,6,7)
      dag = substring(yearmonday,9,10)
#      wrf_inp = paste(wrf_data,"wrfpost_d02","_",year,"/wrfpost_d02","_",year,mont,".nc", sep="")
      
      
#      ncfname=wrf_inp
#      ncin <- nc_open(ncfname)
##      lon <- ncvar_get(ncin,"longitude")
 #     lat <- ncvar_get(ncin,"latitude")
      
 #     coord=cbind(c(lon),c(lat))
      
 #     e=extent(-130.5668, -112.9903, 29.10237, 44.44697)
#      r <- raster(e)
 #     crs(r) = CRS("+init=epsg:4326")
 #     r = projectRaster(r,crs="+proj=lcc +lat_1=33 +lat_2=45 +lat_0=37.7662 +lon_0=-119.6612 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs",res=10000 )
      #   res(r)=c(4000,4000)
      
 #     tmp_array1 <- ncvar_get(ncin,"U10")
 #     tmp_array2 <- ncvar_get(ncin,"V10")
      
 #     m = as.numeric(dag)*4
      
#      tmp_slice1 <- c((tmp_array1[,,m]) )
#      tmp_slice2 <- c((tmp_array1[,,(m-1)]) )
#      tmp_slice3 <- c((tmp_array1[,,(m-2)]) )
#      tmp_slice4 <- c((tmp_array1[,,(m-3)]) )
      
#      tmp_slice1v <- c((tmp_array2[,,m]) )
#      tmp_slice2v <- c((tmp_array2[,,(m-1)]) )
#      tmp_slice3v <- c((tmp_array2[,,(m-2)]) )
#      tmp_slice4v <- c((tmp_array2[,,(m-3)]) )
      
##      pts1= as.data.frame(cbind(c(lon),c(lat),tmp_slice1)) 
#      pts2= as.data.frame(cbind(c(lon),c(lat),tmp_slice2)) 
#      pts3= as.data.frame(cbind(c(lon),c(lat),tmp_slice3)) 
#      pts4= as.data.frame(cbind(c(lon),c(lat),tmp_slice4)) 
      
##      pts1v= as.data.frame(cbind(c(lon),c(lat),tmp_slice1v)) 
#      pts2v= as.data.frame(cbind(c(lon),c(lat),tmp_slice2v)) 
#      pts3v= as.data.frame(cbind(c(lon),c(lat),tmp_slice3v)) 
#      pts4v= as.data.frame(cbind(c(lon),c(lat),tmp_slice4v)) 
      
#      das1  =  SpatialPointsDataFrame(coord,pts1) 
#      das2  =  SpatialPointsDataFrame(coord,pts2) 
#      das3  =  SpatialPointsDataFrame(coord,pts3) 
#      das4  =  SpatialPointsDataFrame(coord,pts4) 
      
#      das1v  =  SpatialPointsDataFrame(coord,pts1v) 
#      das2v  =  SpatialPointsDataFrame(coord,pts2v) 
#      das3v  =  SpatialPointsDataFrame(coord,pts3v) 
#      das4v  =  SpatialPointsDataFrame(coord,pts4v) 
      
#      proj4string(das1) = CRS("+init=epsg:4326")
#      proj4string(das2) = CRS("+init=epsg:4326")
#      proj4string(das3) = CRS("+init=epsg:4326")
#      proj4string(das4) = CRS("+init=epsg:4326")
      
#      proj4string(das1v) = CRS("+init=epsg:4326")
#      proj4string(das2v) = CRS("+init=epsg:4326")
#      proj4string(das3v) = CRS("+init=epsg:4326")
#      proj4string(das4v) = CRS("+init=epsg:4326")
      
#      das1 = spTransform(das1,  CRS("+proj=lcc +lat_1=33 +lat_2=45 +lat_0=37.7662 +lon_0=-119.6612 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") ) 
#      das2 = spTransform(das2,  CRS("+proj=lcc +lat_1=33 +lat_2=45 +lat_0=37.7662 +lon_0=-119.6612 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") ) 
#      das3 = spTransform(das3,  CRS("+proj=lcc +lat_1=33 +lat_2=45 +lat_0=37.7662 +lon_0=-119.6612 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") ) 
#      das4 = spTransform(das4,  CRS("+proj=lcc +lat_1=33 +lat_2=45 +lat_0=37.7662 +lon_0=-119.6612 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") ) 
      
#      das1v = spTransform(das1v,  CRS("+proj=lcc +lat_1=33 +lat_2=45 +lat_0=37.7662 +lon_0=-119.6612 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") ) 
#      das2v = spTransform(das2v,  CRS("+proj=lcc +lat_1=33 +lat_2=45 +lat_0=37.7662 +lon_0=-119.6612 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") ) 
#      das3v = spTransform(das3v,  CRS("+proj=lcc +lat_1=33 +lat_2=45 +lat_0=37.7662 +lon_0=-119.6612 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") ) 
#      das4v = spTransform(das4v,  CRS("+proj=lcc +lat_1=33 +lat_2=45 +lat_0=37.7662 +lon_0=-119.6612 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") ) 
      
 #     wrf_u1 = rasterize(das1,r,fun=mean)
#      wrf_u2 = rasterize(das2,r,fun=mean)
#      wrf_u3 = rasterize(das3,r,fun=mean)
#      wrf_u4 = rasterize(das4,r,fun=mean)
      
#      wrf_v1 = rasterize(das1v,r,fun=mean)
#      wrf_v2 = rasterize(das2v,r,fun=mean)
#      wrf_v3 = rasterize(das3v,r,fun=mean)
#      wrf_v4 = rasterize(das4v,r,fun=mean)
      
      viirs1 = viirs_all[viirs_all$DOY2 == days1[i],]  
      if (length(viirs1) == 0){
        viirs1 = fire_new
      }
      
#      wrf_v=mean(stack(wrf_v1$tmp_slice1v ,wrf_v2$tmp_slice2v ,wrf_v3$tmp_slice3v,wrf_v4$tmp_slice4v))
#      wrf_u= mean(stack(wrf_u1$tmp_slice1 ,wrf_u2$tmp_slice2,wrf_u3$tmp_slice3,wrf_u4$tmp_slice4))
      
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
      
      #      crs(wrf_v) = CRS("+init=epsg:4326")
      #      crs(wrf_u) = CRS("+init=epsg:4326")
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
      
#      l_ws=((wrf_u*wrf_u)+(wrf_v*wrf_v))^0.5
      
#      ws =  as.data.frame(extract(l_ws, viirs1))
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
      biomass= as.data.frame(extract(bio, viirs1))
      #     te =as.matrix(cbind(bi,erc,etr,fm100,fm1000,pet,pr,rmax,rmin,th,tmmn,tmmx,vpd,vs,ws,growth,total_area))
      #    colnames(te) = c("bi","erc","etr","fm100","fm1000","pet","pr","rmax","rmin","th","tmmn","tmmx","vpd","vs","ws","growth","tot_area")
      #     
      #     viirs2 = cbind(viirs1,te)
      
#      ws = mean(ws[,1])
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
      biomass= mean(biomass[,1])
      land_samp = extract(landcover,fire_new,na.rm=T)
      max_land = names(which.max(table(land_samp)))
      mean_land1 =as.numeric(as.character(unlist(land_samp)))
      mean_land = mean(mean_land1, na.rm=TRUE)
      
      if (length(max_land) < 1){
        max_land = NA
        biomass = NA
        mean_land  = NA
        
      }
      
      mean_ros = mean(viirs1$ros)
      max_ros = max(viirs1$ros)
      median95_ros = quantile(viirs1$ros, 0.95)
      
      mean_frp = mean(viirs1$FRP)
      frp_95 = as.numeric(quantile(viirs1$FRP, 0.95))
      
      dail = cbind(mean_ros,max_ros,median95_ros,bi,erc,etr,fm100,fm1000,pet,pr,rmax,rmin,th,tmmn,tmmx,vpd,vs,biomass,max_land,mean_land,growth,total_area, mean_frp, frp_95,firename,fire_day,year,month,doy_out,cause,eco1,eco2,eco3)
      daily_res = rbind(daily_res, dail)
      #    result = rbind(result, viirs2)
      
    }
  }
}    


writeOGR(result, "/Users/stijnhantson/Documents/projects/VIIRS_ros/", layer= "all_ros_meteo", driver="ESRI Shapefile", overwrite_layer = T)
write.table(daily_res, "/Users/stijnhantson/Documents/projects/VIIRS_ros/final_dataset_V3.txt",row.names = F, sep="\t")

##### extract number and size statistics from frap   ################
writeOGR(frap, "/Users/stijnhantson/Documents/projects/VIIRS_ros/", layer= "frap_subset", driver="ESRI Shapefile", overwrite_layer = T)
dr =shapefile("/Users/stijnhantson/Documents/projects/VIIRS_ros/frap_subset.shp")
dr1 =shapefile("/Users/stijnhantson/Documents/data/FRAP/fire18_1.shp")
dr1$YEAR_=as.numeric(as.character(dr1$YEAR_))
dr1$Shape_Area=as.numeric(as.character(dr1$Shape_Area))
dr1=dr1[!is.na(dr1$YEAR_), ]
dr1=dr1[dr1$YEAR_>2011,]
sum(dr1$Shape_Area)
sum(dr$Shape_Area)




daily_res=read.table("/Users/stijnhantson/Documents/projects/VIIRS_ros/final_dataset_V2.txt",header=T)

res=as.data.frame(daily_res)

res$mean_ros =as.numeric(as.character(res$mean_ros))
res$max_ros =as.numeric(as.character(res$max_ros))
res$median95_ros =as.numeric(as.character(res$median95_ros))
res$bi =as.numeric(as.character(res$bi))
res$erc =as.numeric(as.character(res$erc))
res$etr =as.numeric(as.character(res$etr))
res$fm100 =as.numeric(as.character(res$fm100))
res$fm1000 =as.numeric(as.character(res$fm1000))
res$pet =as.numeric(as.character(res$pet))
res$pr =as.numeric(as.character(res$pr))
res$rmax =as.numeric(as.character(res$rmax))
res$rmin =as.numeric(as.character(res$rmin))
res$th =as.numeric(as.character(res$th))
res$tmmn =as.numeric(as.character(res$tmmn))
res$tmmx =as.numeric(as.character(res$tmmx))
res$vpd =as.numeric(as.character(res$vpd))
res$ws =as.numeric(as.character(res$ws))
res$vs =as.numeric(as.character(res$vs))
res$growth =as.numeric(as.character(res$growth))
res$total_area =as.numeric(as.character(res$total_area))
res$mean_frp =as.numeric(as.character(res$mean_frp))
res$frp_95 =as.numeric(as.character(res$frp_95))
res$max_land =as.numeric(as.character(res$max_land))
res$mean_land =as.numeric(as.character(res$mean_land))

res$biomass =as.numeric(as.character(res$biomass))

res = res[-1,]

summary(res)

res$per_ba = res$growth/res$total_area

############ how many days does it take to reach 75% burnt area #################
res75 = res[res$per_ba > 0.75,]
peak_day = as.data.frame(aggregate(res75$fire_day, by = list(res75$firename,res$cause), min))
peak_day=subset(peak_day,x < 55)
hi = hist(peak_day$x,prob =F, breaks= c(0:53), xlim=c(0,55), ylab="number of fires", xlab="days", cex.lab=1.4,cex.axis=1.3)

out1 = subset(res75,cause == 1 )   #1=lightning; 14=unknown; 7=arson
out2 = subset(res75,cause !=1 & cause != 14 )
peak_day1 = as.data.frame(aggregate(out1$fire_day, by = list(out1$firename), min))
peak_day2 = as.data.frame(aggregate(out2$fire_day, by = list(out2$firename), min))

quantile(peak_day1$x,0.50,type=3) 
quantile(peak_day2$x,0.50,type=3) 

peak_day1=subset(peak_day1,x < 56)
peak_day2=subset(peak_day2,x < 56)
hist.a =hist(peak_day1$x,breaks =c(0:55),plot=F)
hist.b =hist(peak_day2$x,breaks =c(0:55),plot=F)
fg = rbind(hist.a$counts,hist.b$counts)

fr = barplot(fg,xlab="Days after ignition",ylab="Number of fires",cex.lab=1.4,cex.axis = 1.3, xlim=c(1,65), ylim=c(0,30))
axis(1,c(0.7,5.5,11.5,17.5,23.5,29.5,35.5,41.5,47.5,53.5,59.5,65.5),labels=c(1,5,10,15,20,25,30,35,40,45,50,55),cex.axis = 1.3)
legend("topright",legend = c("human","lightning"), fill=c("grey","black"),cex=1.4,bty = "n")



################### final figures   ###############################

res$ros1 = res$max_ros+1
res$ros_km = (res$median95_ros*24)/1000

out1 = subset(res,cause == 1 )   #1=lightning; 14=unknown; 7=arson
out2 = subset(res,cause !=1 & cause != 14 )

hum1 = out2[out2$fire_day ==1,]
hum2 = out2[out2$fire_day ==2,]
hum3 = out2[out2$fire_day ==3,]
hum4 = out2[out2$fire_day ==4,]
hum5 = out2[out2$fire_day ==5,]
lig1 = out1[out1$fire_day ==1,]
lig2 = out1[out1$fire_day ==2,]
lig3 = out1[out1$fire_day ==3,]
lig4 = out1[out1$fire_day ==4,]
lig5 = out1[out1$fire_day ==5,]
mean(hum1$growth)
mean(hum2$growth)
mean(hum3$growth)
mean(hum4$growth)
mean(hum5$growth)
mean(lig1$growth)
mean(lig2$growth)
mean(lig3$growth)
mean(lig4$growth)
mean(lig5$growth)
t.test(log10(hum1$growth),log10(lig1$growth))
t.test(log10(hum2$growth),log10(lig2$growth))
t.test(log10(hum3$growth),log10(lig3$growth))
t.test(log10(hum4$growth),log10(lig4$growth))
t.test(log10(hum5$growth),log10(lig5$growth))

hum1 = out2[out2$fire_day ==1 & out2$eco1==6,]
hum2 = out2[out2$fire_day ==2 & out2$eco1==6,]
hum3 = out2[out2$fire_day ==3 & out2$eco1==6,]
hum4 = out2[out2$fire_day ==4 & out2$eco1==6,]
hum5 = out2[out2$fire_day ==5 & out2$eco1==6,]
lig1 = out1[out1$fire_day ==1 & out1$eco1==6,]
lig2 = out1[out1$fire_day ==2 & out1$eco1==6,]
lig3 = out1[out1$fire_day ==3 & out1$eco1==6,]
lig4 = out1[out1$fire_day ==4 & out1$eco1==6,]
lig5 = out1[out1$fire_day ==5 & out1$eco1==6,]
mean(hum1$growth)
mean(hum2$growth)
mean(hum3$growth)
mean(hum4$growth)
mean(hum5$growth)
mean(lig1$growth, na.rm=T)
mean(lig2$growth, na.rm=T)
mean(lig3$growth, na.rm=T)
mean(lig4$growth, na.rm=T)
mean(lig5$growth, na.rm=T)
t.test(log10(hum1$growth),log10(lig1$growth))
t.test(log10(hum2$growth),log10(lig2$growth))
t.test(log10(hum3$growth),log10(lig3$growth))
t.test(log10(hum4$growth),log10(lig4$growth))
t.test(log10(hum5$growth),log10(lig5$growth))

hum1 = out2[out2$fire_day ==1 & out2$eco1==11,]
hum2 = out2[out2$fire_day ==2 & out2$eco1==11,]
hum3 = out2[out2$fire_day ==3 & out2$eco1==11,]
hum4 = out2[out2$fire_day ==4 & out2$eco1==11,]
hum5 = out2[out2$fire_day ==5 & out2$eco1==11,]
lig1 = out1[out1$fire_day ==1 & out1$eco1==11,]
lig2 = out1[out1$fire_day ==2 & out1$eco1==11,]
lig3 = out1[out1$fire_day ==3 & out1$eco1==11,]
lig4 = out1[out1$fire_day ==4 & out1$eco1==11,]
lig5 = out1[out1$fire_day ==5 & out1$eco1==11,]
mean(hum1$growth)
mean(hum2$growth)
mean(hum3$growth)
mean(hum4$growth)
mean(hum5$growth)
mean(lig1$growth, na.rm=T)
mean(lig2$growth, na.rm=T)
mean(lig3$growth, na.rm=T)
mean(lig4$growth, na.rm=T)
mean(lig5$growth, na.rm=T)
t.test(log10(hum1$growth),log10(lig1$growth))
t.test(log10(hum2$growth),log10(lig2$growth))
t.test(log10(hum3$growth),log10(lig3$growth))
t.test(log10(hum4$growth),log10(lig4$growth))
t.test(log10(hum5$growth),log10(lig5$growth))




mean(out1$ros_km,na.rm=T)
mean(out2$ros_km,na.rm=T)

hist.a =hist(out1$ros_km,breaks =c(0,0.01,0.05,0.1,0.25,0.5,1,2,5,10,20,30),plot=F)
hist.b =hist(out2$ros_km,breaks =c(0,0.01,0.05,0.1,0.25,0.5,1,2,5,10,20,30),plot=F)
fg = rbind(hist.a$counts,hist.b$counts)

fr = barplot(fg, beside=TRUE,xlab="Rate-of-Spread (km/day)",ylab="Number of fire days",cex.lab=1.4,cex.axis = 1.3)
axis(1,at=c(0.5,3.5,6.5,9.5,12.5,15.5,18.5,21.5,24.5,27.5,30.5,33.5),labels=hist.a$breaks,cex.axis = 1.3)
legend("topright",legend = c("human","lightning"), fill=c("grey","black"),cex=1.4,bty = "n")

out1 = subset(res,cause == 1 & )   #1=lightning; 14=unknown; 7=arson
out2 = subset(res,cause !=1 & cause != 14 )

hist.a =hist(out1$ros_km,breaks =c(0,0.01,0.05,0.1,0.25,0.5,1,2,5,10,20,30),plot=F)
hist.b =hist(out2$ros_km,breaks =c(0,0.01,0.05,0.1,0.25,0.5,1,2,5,10,20,30),plot=F)
fg = rbind(hist.a$counts,hist.b$counts)

fr = barplot(fg, beside=TRUE,xlab="Rate-of-Spread (km/day)",ylab="Number of fire days",cex.lab=1.4,cex.axis = 1.3)
axis(1,at=c(0.5,3.5,6.5,9.5,12.5,15.5,18.5,21.5,24.5,27.5,30.5,33.5),labels=hist.a$breaks,cex.axis = 1.3)
legend("topright",legend = c("human","lightning"), fill=c("grey","black"),cex=1.4,bty = "n")




hist.a =hist(res$ros_km,breaks =c(0,0.01,0.05,0.1,0.25,0.5,1,2,5,10,20,30),plot=F)
plot(hist.a$count,type="h",xaxt="n",lwd=30,lend=2,xlab="Rate-of-Spread (km/day)",ylab="Number of fire days",cex.lab=1.4,cex.axis = 1.3,xlim=c(0.5,11.5),ylim = c(0,550),yaxs="i")
axis(1,at=(0:length(hist.a$mids)+0.5),labels=hist.a$breaks,cex.axis = 1.3)


hist.a =hist(res$median95_ros,breaks =c(0,1,2,4,8,16,32,64,128,256,512,1024,2048),plot=F)
plot(hist.a$count,type="h",xaxt="n",lwd=20,lend=2,xlab="Rate-of-Spread (m/hr.)",ylab="Number of fires",cex.lab=1.4,cex.axis = 1.3,xlim=c(0,13))
axis(1,at=(0:length(hist.a$mids)+0.5),labels=hist.a$breaks,xlab="Rate-of-Spread (m/hr.)",cex.axis = 1.3)

data_s1=na.omit(data_s1)

hist.a =hist(data_s1$ros95,breaks =c(0,1,2,4,8,16,32,64,128,256,512,1024,2048),plot=F)
plot(hist.a$count,type="h",xaxt="n",lwd=20,lend=2,xlab="Rate-of-Spread (m/hr.)",ylab="Count",cex.lab=1.4,cex.axis = 1.3,xlim=c(0.9,12.3))
axis(1,at=(0:length(hist.a$mids)+0.5),labels=hist.a$breaks,xlab="Rate-of-Spread (m/hr.)",cex.axis = 1.3)



