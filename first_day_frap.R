
library(rgeos)
library(raster)
library(rgdal)
library(foreign)
library(ncdf4)
library(cleangeo)
viirs_dir = "/Users/stijnhantson/Documents/projects/VIIRS_ros/final_results6/"
gridmet_dir = "/Users/stijnhantson/Documents/data/gridmet/"
wrf_data = "/Volumes/MyBookDuo/wrf_UCLA/"
shape = shapefile("/Users/stijnhantson/Documents/data/veg_california/ca_eco_l3/ca_eco_l3.shp")
shape = spTransform(shape,crs(viirs1))
frap=shapefile("/Users/stijnhantson/Documents/data/FRAP/fire18_1.shp")
frap=subset(frap,YEAR_ > 2011)
#frap=subset(frap, GIS_ACRES>1000)
#frap2=shapefile("/Users/stijnhantson/Documents/data/FRAP/FIREP18_DRAFT_DO_NOT_DISTRIBUTE/FIREP18_DRAFT_DO_NOT_DISTRIBUTE.shp")
#frap2=subset(frap2, GIS_ACRES>1000)

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

bio = raster("/Users/stijnhantson/Documents/data/2010_Biomass.tif")

viirs_all=shapefile(viirs_list[1])
result = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
daily_res = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

p=1  
for (p in 1:length(frap$YEAR_)){
  print(p)
  #  rows_p = read.dbf(viirs_dbf[p])
  
  firespread = frap[p,]
  firespread=clgeo_Clean(firespread)
  
  firename = firespread$FIRE_NAME
  if (  is.null(firename) ){
    firename = NaN
  }  
  if (  is.null(firespread$ALARM_DATE) ){
    startdate = NaN
  } else {
    startdate =  as.Date(firespread$ALARM_DATE[1])
  }
  
  if (is.na(startdate)){
    
  }else{
    
    start_doy=as.numeric( strftime( startdate,format = "%j"))
    
    cause = as.numeric(as.character(firespread$CAUSE[1]))
    total_area = gArea(firespread) 
    year = firespread$YEAR_
    centroids <- getSpPPolygonsLabptSlots(firespread) 
    long = centroids[1,1]
    lati = centroids[1,2]
    
    inputyear=paste(year,"-01-01",sep="")
    
    viirs1 = firespread
    
    mont = substring(startdate ,6,7)
    dag = substring(startdate ,9,10)
    wrf_inp = paste(wrf_data,"wrfpost_d02","_",year,"/wrfpost_d02","_",year,mont,".nc", sep="")
    
    if (wrf_inp == "/Volumes/MyBookDuo/wrf_UCLA/wrfpost_d02_2015/wrfpost_d02_201501.nc"){
      ws=NA
    }else{
      ncfname=wrf_inp
      ncin <- nc_open(ncfname)
      lon <- ncvar_get(ncin,"longitude")
      lat <- ncvar_get(ncin,"latitude")
      
      coord=cbind(c(lon),c(lat))
      
      e=extent(-130.5668, -112.9903, 29.10237, 44.44697)
      r <- raster(e)
      crs(r) = CRS("+init=epsg:4326")
      r = projectRaster(r,crs="+proj=lcc +lat_1=33 +lat_2=45 +lat_0=37.7662 +lon_0=-119.6612 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs",res=10000 )
      #   res(r)=c(4000,4000)
      
      tmp_array1 <- ncvar_get(ncin,"U10")
      tmp_array2 <- ncvar_get(ncin,"V10")
      
      m = as.numeric(dag)*4
      
      tmp_slice1 <- c((tmp_array1[,,m]) )
      tmp_slice2 <- c((tmp_array1[,,(m-1)]) )
      tmp_slice3 <- c((tmp_array1[,,(m-2)]) )
      tmp_slice4 <- c((tmp_array1[,,(m-3)]) )
      
      tmp_slice1v <- c((tmp_array2[,,m]) )
      tmp_slice2v <- c((tmp_array2[,,(m-1)]) )
      tmp_slice3v <- c((tmp_array2[,,(m-2)]) )
      tmp_slice4v <- c((tmp_array2[,,(m-3)]) )
      
      pts1= as.data.frame(cbind(c(lon),c(lat),tmp_slice1)) 
      pts2= as.data.frame(cbind(c(lon),c(lat),tmp_slice2)) 
      pts3= as.data.frame(cbind(c(lon),c(lat),tmp_slice3)) 
      pts4= as.data.frame(cbind(c(lon),c(lat),tmp_slice4)) 
      
      pts1v= as.data.frame(cbind(c(lon),c(lat),tmp_slice1v)) 
      pts2v= as.data.frame(cbind(c(lon),c(lat),tmp_slice2v)) 
      pts3v= as.data.frame(cbind(c(lon),c(lat),tmp_slice3v)) 
      pts4v= as.data.frame(cbind(c(lon),c(lat),tmp_slice4v)) 
      
      das1  =  SpatialPointsDataFrame(coord,pts1) 
      das2  =  SpatialPointsDataFrame(coord,pts2) 
      das3  =  SpatialPointsDataFrame(coord,pts3) 
      das4  =  SpatialPointsDataFrame(coord,pts4) 
      
      das1v  =  SpatialPointsDataFrame(coord,pts1v) 
      das2v  =  SpatialPointsDataFrame(coord,pts2v) 
      das3v  =  SpatialPointsDataFrame(coord,pts3v) 
      das4v  =  SpatialPointsDataFrame(coord,pts4v) 
      
      proj4string(das1) = CRS("+init=epsg:4326")
      proj4string(das2) = CRS("+init=epsg:4326")
      proj4string(das3) = CRS("+init=epsg:4326")
      proj4string(das4) = CRS("+init=epsg:4326")
      
      proj4string(das1v) = CRS("+init=epsg:4326")
      proj4string(das2v) = CRS("+init=epsg:4326")
      proj4string(das3v) = CRS("+init=epsg:4326")
      proj4string(das4v) = CRS("+init=epsg:4326")
      
      das1 = spTransform(das1,  CRS("+proj=lcc +lat_1=33 +lat_2=45 +lat_0=37.7662 +lon_0=-119.6612 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") ) 
      das2 = spTransform(das2,  CRS("+proj=lcc +lat_1=33 +lat_2=45 +lat_0=37.7662 +lon_0=-119.6612 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") ) 
      das3 = spTransform(das3,  CRS("+proj=lcc +lat_1=33 +lat_2=45 +lat_0=37.7662 +lon_0=-119.6612 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") ) 
      das4 = spTransform(das4,  CRS("+proj=lcc +lat_1=33 +lat_2=45 +lat_0=37.7662 +lon_0=-119.6612 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") ) 
      
      das1v = spTransform(das1v,  CRS("+proj=lcc +lat_1=33 +lat_2=45 +lat_0=37.7662 +lon_0=-119.6612 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") ) 
      das2v = spTransform(das2v,  CRS("+proj=lcc +lat_1=33 +lat_2=45 +lat_0=37.7662 +lon_0=-119.6612 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") ) 
      das3v = spTransform(das3v,  CRS("+proj=lcc +lat_1=33 +lat_2=45 +lat_0=37.7662 +lon_0=-119.6612 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") ) 
      das4v = spTransform(das4v,  CRS("+proj=lcc +lat_1=33 +lat_2=45 +lat_0=37.7662 +lon_0=-119.6612 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") ) 
      
      wrf_u1 = rasterize(das1,r,fun=mean)
      wrf_u2 = rasterize(das2,r,fun=mean)
      wrf_u3 = rasterize(das3,r,fun=mean)
      wrf_u4 = rasterize(das4,r,fun=mean)
      
      wrf_v1 = rasterize(das1v,r,fun=mean)
      wrf_v2 = rasterize(das2v,r,fun=mean)
      wrf_v3 = rasterize(das3v,r,fun=mean)
      wrf_v4 = rasterize(das4v,r,fun=mean)
      
      
      wrf_v=mean(stack(wrf_v1$tmp_slice1v ,wrf_v2$tmp_slice2v ,wrf_v3$tmp_slice3v,wrf_v4$tmp_slice4v))
      wrf_u= mean(stack(wrf_u1$tmp_slice1 ,wrf_u2$tmp_slice2,wrf_u3$tmp_slice3,wrf_u4$tmp_slice4))
      
      l_ws=((wrf_u*wrf_u)+(wrf_v*wrf_v))^0.5
      ws =  as.data.frame(extract(l_ws, viirs1))
      ws = mean(ws[,1])
    }
    
    gridmet_list= list.files(gridmet_dir, pattern = paste(year,".nc$",sep=""), recursive = TRUE, full.names=T)
    
    l_bi = raster(gridmet_list[1],band = start_doy )#Burning index
    l_erc = raster(gridmet_list[2],band = start_doy )#energy release component
    l_etr = raster(gridmet_list[3],band = start_doy )#evapotranspiration
    l_fm100 = raster(gridmet_list[4],band = start_doy )
    l_fm1000 = raster(gridmet_list[5],band = start_doy )
    l_pet = raster(gridmet_list[7],band = start_doy )#evapotranspiration
    l_pr = raster(gridmet_list[8],band = start_doy )#precipitation
    l_rmax = raster(gridmet_list[9],band = start_doy )#relative humididty max
    l_rmin = raster(gridmet_list[10],band = start_doy)#relative humididty min
    l_sph = raster(gridmet_list[11],band = start_doy)#mean specific humidity
    l_th = raster(gridmet_list[12],band = start_doy )#wind direction
    l_tmmn = raster(gridmet_list[13],band = start_doy ) #min temp
    l_tmmx = raster(gridmet_list[14],band = start_doy) #max temp
    l_vpd = raster(gridmet_list[15],band = start_doy )#mean vapor pressure deficit
    l_vs = raster(gridmet_list[16],band = start_doy )#windspeed
    
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
    land_samp = extract(landcover,firespread ,na.rm=T)
    max_land = names(which.max(table(land_samp)))
    mean_land1 =as.numeric(as.character(unlist(land_samp)))
    mean_land = mean(mean_land1, na.rm=TRUE)
    
    if (length(max_land) < 1){
      max_land = NA
      biomass = NA
      mean_land  = NA
      
    }
    
    eco = as.data.frame(over(viirs1,shape))  
    eco=eco[1,]
    eco1= eco$NA_L1CODE
    eco2= eco$NA_L2CODE
    eco3= eco$NA_L3CODE
    
    dail = cbind(bi,erc,etr,fm100,fm1000,pet,pr,rmax,rmin,th,tmmn,tmmx,vpd,vs,ws,biomass,max_land,mean_land,total_area,eco1,eco2,eco3,firename,cause,year, mont,long,lati)
    daily_res = rbind(daily_res, dail)
    #    result = rbind(result, viirs2)
  }
}


write.table(daily_res, "/Users/stijnhantson/Documents/projects/VIIRS_ros/all_ignitions_V3.txt",row.names = F, sep="\t")

daily_res=read.table("/Users/stijnhantson/Documents/projects/VIIRS_ros/all_ignitions_V3.txt",header=T)

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
res$total_area =as.numeric(as.character(res$total_area))
res$max_land =as.numeric(as.character(res$max_land))
res$mean_land =as.numeric(as.character(res$mean_land))

res$biomass =as.numeric(as.character(res$biomass))

res = res[-1,]

summary(res)

out1 = res[res$cause !=1 & res$cause != 14,] 
out2 = res[res$cause ==1,] 

out1 = subset(res,(eco1 == 6 |eco1 == 7) & res$cause !=1 & res$cause != 14)   #1=lightning; 14=unknown; 7=arson
out2 = subset(res,(eco1 == 6 |eco1 == 7) & res$cause ==1)   #1=lightning; 14=unknown; 7=arson

out1 = subset(res,eco1 == 11& res$cause !=1 & res$cause != 14)
out2 = subset(res,eco1 == 11 & res$cause ==1)

out1 = subset(res,(eco1 == 6 |eco1 == 7) & res$cause !=1 & res$cause != 14 & res$mont > 5 & res$mont < 10 )   #1=lightning; 14=unknown; 7=arson
out2 = subset(res,(eco1 == 6 |eco1 == 7) & res$cause ==1& res$mont > 5 & res$mont < 10)   #1=lightning; 14=unknown; 7=arson

t.test(out1$bi,out2$bi)
t.test(out1$erc,out2$erc)
t.test(out1$etr,out2$etr)
t.test(out1$fm100,out2$fm100)
t.test(out1$fm1000,out2$fm1000)
t.test(out1$pet,out2$pet)
t.test(out1$pr,out2$pr)
t.test(out1$rmax,out2$rmax)
t.test(out1$rmin,out2$rmin)
t.test(out1$th,out2$th)
t.test(out1$tmmn,out2$tmmn)
t.test(out1$tmmx,out2$tmmx)
t.test(out1$vpd,out2$vpd)
t.test(out1$vs,out2$vs)
t.test(out1$ws,out2$ws)
t.test(out1$biomass,out2$biomass)
t.test(out1$mean_land,out2$mean_land)
t.test(log10(out1$total_area),log10(out2$total_area))

ta = table(res$human,res$mont)
ps = barplot(ta, beside=TRUE, ylab="number of fires",xpd=T,xlab= "month", xaxt='n',ylim=c(0,300), axis.lty=1,cex.lab = 1.4,cex.axis = 1.2 )
axis(1,at=c(2,5,8,11,14,17,20,23,26,29,32,35), labels =c(1:12),xlim=c(0,36),xpd=F ,cex.lab = 1.4,cex.axis = 1.3)
legend("topright",c("human","lightning"),fill = c("grey", "black"), bty="n",cex=1.4)
text(1,285,"a)",cex=1.8)
out1 = subset(res,eco1 == 6 |eco1 == 7)   #1=lightning; 14=unknown; 7=arson
out2 = subset(res,eco1 == 11)

ta = table(out1$human,out1$mont)
ps = barplot(ta, beside=TRUE, ylab="number of fires",xpd=T, xaxt='n',xlab= "month", ylim=c(0,200), axis.lty=1,cex.lab = 1.4,cex.axis = 1.2 )
axis(1,at=c(2,5,8,11,14,17,20,23,26,29,32,35), labels =c(1:12),xlim=c(0,36),xpd=F,cex.lab = 1.4,cex.axis = 1.3 )
legend("topright",c("human","lightning"),fill = c("grey", "black"), bty="n",cex=1.4)
text(1,190,"b)",cex=1.8)

ta = table(out2$human,out2$mont)
ps = barplot(ta, beside=TRUE, ylab="number of fires",xpd=T,xaxt='n',xlab= "month", ylim=c(0,200), axis.lty=1,cex.lab = 1.4,cex.axis = 1.2 )
axis(1,at=c(2,5,8,11,14,17,20,23,26,29,32,35), labels =c(1:12),xlim=c(0,36),xpd=F,cex.lab = 1.4,cex.axis = 1.3 )
legend("topright",c("human","lightning"),fill = c("grey", "black"), bty="n",cex=1.4)
text(1,190,"c)",cex=1.8)




out1 = subset(out,CAUSE == 1)   #1=lightning; 14=unknown; 7=arson
out2 = subset(out,CAUSE !=1 & CAUSE != 14)
out3 = subset(out,CAUSE == 7)





