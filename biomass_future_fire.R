

library(raster)
library(plyr)
library(zoo)
library(prioritizr)
library(gdalUtilities)

k=0  ## make sure k=0 is before the year loop
i=1
p = 281
dataset=0

road_red=raster("/Users/stijnhantson/Documents/data/GRIP_roads/road_reduced_05.tif")
road = raster("/Users/stijnhantson/Documents/data/GRIP_roads/road_05.tif")

livestock = raster("/Users/stijnhantson/Documents/data/livestock/FAO_spatial/TLU_km_2010_buffalo_cattle_goat_horse_sheep_05d.tif")
biomass = raster("/Users/stijnhantson/Documents/data/GLOBBIOMASS/final_05.tif")
biomass1 = raster("/Users/stijnhantson/Documents/data/avitabile_Biomass_05_v2.tif")
biomass2 = raster("/Users/stijnhantson/Documents/data/carvalhais_biomass/Carvalhais.cVeg_50.360.720.1.nc")
ex = extent(biomass)
biomass1=extend(biomass1,ex)
extent(biomass1)=ex

max_temp=raster("/Users/stijnhantson/Documents/data/WorldClim2/bioclimat/wc2.1_10m_Max_Temp.tif")
min_temp=raster("/Users/stijnhantson/Documents/data/WorldClim2/bioclimat/wc2.1_10m_min_temp.tif")
mean_temp=raster("/Users/stijnhantson/Documents/data/WorldClim2/bioclimat/wc2.1_10m_Annual_Mean_Temperature.tif")
precip=raster("/Users/stijnhantson/Documents/data/WorldClim2/bioclimat/wc2.1_10m_Annual_Precipitation.tif")
prec75=raster("/Users/stijnhantson/Documents/data/WorldClim2/nr_months_75precip.tif")
srad = stack(list.files("/Users/stijnhantson/Documents/data/WorldClim2/srad/",pattern = ".tif",full.names=T))
srad=sum(srad)

elevation=raster("/Users/stijnhantson/Documents/data/WorldClim2/wc2.1_10m_elev.tif")

max_temp= aggregate(max_temp, fact = 0.5/res(elevation), fun = "mean")
min_temp= aggregate(min_temp, fact = 0.5/res(elevation), fun = "mean")
mean_temp= aggregate(mean_temp, fact = 0.5/res(elevation), fun = "mean")
precip= aggregate(precip, fact = 0.5/res(elevation), fun = "mean")
prec75= aggregate(prec75, fact = 0.5/res(elevation), fun = "mean")
srad=aggregate(srad, fact = 0.5/res(elevation), fun = "mean")
elevation= aggregate(elevation, fact = 0.5/res(elevation), fun = "mean")

year=2010
ann3 = raster("/Users/stijnhantson/Documents/data/LUH2/states.nc", varname="c3ann", band= year-849)
ann4 = raster("/Users/stijnhantson/Documents/data/LUH2/states.nc", varname="c4ann", band= year-849)
per3 = raster("/Users/stijnhantson/Documents/data/LUH2/states.nc", varname="c3per", band= year-849)
per4 = raster("/Users/stijnhantson/Documents/data/LUH2/states.nc", varname="c4per", band= year-849)
cropland=ann3+ann4+per3+per4
cropland= aggregate(cropland, fact = 2, fun = "mean")

pop =  raster("/Users/stijnhantson/Documents/data/pop_dens/pop_dens_2000-2020.tif",band=(year-1999))
pop=crop(pop, elevation)

pop_ru = raster(paste("/Users/stijnhantson/Documents/data/HYDE3.2/",year,"AD_pop/rurc_",year,"AD.asc",sep=""))
pop_ur = raster(paste("/Users/stijnhantson/Documents/data/HYDE3.2/",year,"AD_pop/urbc_",year,"AD.asc",sep=""))

pop_ru = aggregate(pop_ru, fact = 0.5/res(pop_ru), fun = "mean")
pop_ur = aggregate(pop_ur, fact = 0.5/res(pop_ur), fun = "mean")

pop_ru = resample(pop_ru, elevation, method="ngb")
pop_ur = resample(pop_ur, elevation, method="ngb")

clay1=raster("/Users/stijnhantson/Documents/data/HWSD_1247/data/T_CLAY.nc4")
clay2=raster("/Users/stijnhantson/Documents/data/HWSD_1247/data/S_CLAY.nc4")
clay=(clay1+clay2)/2
clay= aggregate(clay, fact = 10, fun = "mean")

sand1=raster("/Users/stijnhantson/Documents/data/HWSD_1247/data/T_SAND.nc4")
sand2=raster("/Users/stijnhantson/Documents/data/HWSD_1247/data/S_SAND.nc4")
sand=(sand1+sand2)/2
sand= aggregate(sand, fact = 10, fun = "mean")

gfas=raster("/Users/stijnhantson/Documents/projects/FireMIP/benchmarking/benchmarks_data/GFAS/GFAS.nc")
gfas=mean(gfas)
  
ba=stack(list.files("/Users/stijnhantson/Documents/projects/FireMIP/benchmarking/benchmarks_data/burnt_area/",pattern = "GFED4s_BF",full.names=T))
#ba=stack("/Users/stijnhantson/Documents/projects/FireMIP/benchmarking/benchmarks_data/burnt_area/GFED4s_v2.nc")
ba_mean = mean(ba[[1:156]])*12
ba_mean = mean(ba)*12

r= stack(biomass,pop,pop_ru,road_red,road,livestock,max_temp,min_temp,mean_temp,precip,prec75,srad,elevation,cropland,ba_mean)
names(r) = c("biomass","pop","pop_ru","road_red","road","livestock","max_temp","min_temp","mean_temp",'precip',"prec75","srad","elevation","cropland","ba_mean")

r= stack(biomass,pop,livestock,max_temp,min_temp,mean_temp,precip,prec75,srad,cropland,ba_mean)
names(r) = c("biomass","pop","livestock","max_temp","min_temp","mean_temp",'precip',"prec75","srad","cropland","ba_mean")

r= stack(biomass,pop,pop_ru,road_red,road,livestock,max_temp,min_temp,mean_temp,precip,prec75,srad,elevation,cropland,ba_mean)
names(r) = c("biomass","pop","pop_ru","road","road_red","livestock","max_temp","min_temp","mean_temp",'precip',"prec75","srad","elevation","cropland","ba_mean")

r= stack(biomass,pop,pop_ru,road,livestock,max_temp,min_temp,mean_temp,precip,prec75,srad,elevation,cropland,ba_mean)
names(r) = c("biomass","pop","pop_ru","road","livestock","max_temp","min_temp","mean_temp",'precip',"prec75","srad","elevation","cropland","ba_mean")

r= stack(biomass2,pop,pop_ru,road,livestock,max_temp,min_temp,mean_temp,precip,prec75,srad,elevation,cropland,ba_mean,clay,sand)
names(r) = c("biomass","pop","pop_ru","road","livestock","max_temp","min_temp","mean_temp",'precip',"prec75","srad","elevation","cropland","ba_mean","clay","sand")

r= stack(biomass2,pop,livestock,max_temp,min_temp,mean_temp,precip,prec75,srad,elevation,cropland,ba_mean)
names(r) = c("biomass","pop","livestock","max_temp","min_temp","mean_temp",'precip',"prec75","srad","elevation","cropland","ba_mean")

r2=as.data.frame(r)
r2=na.omit(r2)

library(randomForest)
library("foreach")
library("doSNOW")
NumberOfCluster <- 3
cl <- makeCluster(NumberOfCluster) # Make clusters 
registerDoSNOW(cl)
rf <- foreach(ntree = rep(300, 3), .combine = combine,.multicombine=TRUE, .packages = "randomForest") %dopar% randomForest(formula=biomass ~., data=r2, ntree= ntree)
stopCluster(cl)
#rf1 <- randomForest(formula=biomass ~., data=r2, ntree=500)

save(rf,file = "/Users/stijnhantson/Documents/projects/CMIP6_fire/rf.RData")
rf1 = get(load("/Users/stijnhantson/Documents/projects/CMIP6_fire/rf.RData"))

varImpPlot(rf)
ApPl_prob1 <- predict(r,rf)

plot((biomass - ApPl_prob1), zlim=c(-30,30))

ar = area(ApPl_prob)

cellStats(ar*biomass,sum)
cellStats(ar*ApPl_prob,sum)
cellStats(ApPl_prob1,sum)

partialPlot(rf,r2,ba_mean)
partialPlot(rf,r2,cropland)

library(visreg)
library(mgcv)
m1 <- gam(biomass ~ s(pop) + s(livestock) + s(max_temp)+ s(min_temp)+ s(mean_temp)+ s(precip)+ s(prec75)+ s(srad)+ s(cropland)+ s(ba_mean),
          data = r2)
plot(m1
     )
####### future projection  ##############
gfed_region = raster("/Users/stijnhantson/Documents/data/GFED4s/GFED_720_360_Regions.nc")
data_path = "/Users/stijnhantson/Documents/data/CMIP6_fire/DATA_ALL/DATA/"
data_path1 = "/Users/stijnhantson/Documents/data/CMIP6_fire/DATA_ALL/DATA_C/"
data_path2 = "/Users/stijnhantson/Documents/data/CMIP6_fire/DATA_ALL/DATA_CL/"

models =c("CESM2","CESM2-WACCM","CNRM-ESM2-1","EC-Earth3-Veg","MPI-ESM1-2-LR","NorESM2-MM")
future_senario=c("historical","ssp126","ssp245","ssp370","ssp585")
mod=1

veg_his=0
veg_ssp126=0
veg_ssp245=0
veg_ssp370=0
veg_ssp585=0
ba_his = 0
ba_ssp126= 0
ba_ssp245=0
ba_ssp370=0
ba_ssp585=0

for (mod in 1:length(models)){
   print(mod)
   max_temp=raster("/Users/stijnhantson/Documents/data/WorldClim2/bioclimat/wc2.1_10m_Max_Temp.tif")
   min_temp=raster("/Users/stijnhantson/Documents/data/WorldClim2/bioclimat/wc2.1_10m_min_temp.tif")
   mean_temp=raster("/Users/stijnhantson/Documents/data/WorldClim2/bioclimat/wc2.1_10m_Annual_Mean_Temperature.tif")
   precip=raster("/Users/stijnhantson/Documents/data/WorldClim2/bioclimat/wc2.1_10m_Annual_Precipitation.tif")
   
   max_temp= aggregate(max_temp, fact = 0.5/res(max_temp), fun = "mean")
   min_temp= aggregate(min_temp, fact = 0.5/res(min_temp), fun = "mean")
   mean_temp= aggregate(mean_temp, fact = 0.5/res(mean_temp), fun = "mean")
   precip= aggregate(precip, fact = 0.5/res(precip), fun = "mean")
   
   hist_precip =  list.files(paste(data_path2,future_senario[1],"/",models[mod],"/",sep=""),pattern="pr_Amon_",full.names=T)
   hist_temp =  list.files(paste(data_path2,future_senario[1],"/",models[mod],"/",sep=""),pattern="tas_Amon_",full.names=T)
   hist_precip=raster(hist_precip[1])
   hist_temp=raster(hist_temp[1])
   extent(hist_precip)=extent(0,360,-90,90)
   extent(hist_temp)=extent(0,360,-90,90)

   veg_c = 0
   ba=0
   for (future in 1:5){
      print("future")
      print(future)
      
   ba_mod_l= list.files(paste(data_path,future_senario[future],"/",models[mod],"/",sep=""),pattern="burntFractionAll",full.names=T)
#      ba_mod_l= list.files(paste(data_path,future_senario[1],"/",models[mod],"/",sep=""),pattern="burntFractionAll",full.names=T)
      
   if (length(ba_mod_l) == 0){
   ba_mod_l= list.files(paste(data_path,future_senario[future],"/",models[mod],"/",sep=""),pattern="fFire_",full.names=T)
   ba_mod_l1= list.files(paste(data_path,future_senario[1],"/",models[mod],"/",sep=""),pattern="fFire_",full.names=T)
   em = raster(ba_mod_l[1])
   em_hist = raster(ba_mod_l1[1])
   
   em_hist=rotate(em_hist)
   em_hist=resample(em_hist, biomass2, method="ngb")
   
   em=rotate(em)
   em=resample(em, biomass2, method="ngb")

   gfed_region1=rotate(gfed_region)
   cor_fact=gfed_region1
   for (k in 1:24){
      em1=em_hist
      ba1 = ba_mean
   em1[gfed_region1!=k] =NA
   ba1[gfed_region1!=k] =NA
   
   emis= cellStats(em1,sum)
   burn= cellStats(ba1,sum)
   core = burn/emis
   cor_fact[gfed_region1 == k] = core
   }
   ba_mod = em*cor_fact

}else{
   ba_mod=raster(ba_mod_l[1])
   ba_mod=rotate(ba_mod)
   ba_mod=resample(ba_mod, biomass2, method="ngb")

}
 
   if (cellStats(ba_mod,max) >1){
      ba_mod=ba_mod/100
   }
   
   if (future > 1){  # add precip & temp anomaly
      prec_l= list.files(paste(data_path2,future_senario[future],"/",models[mod],"/",sep=""),pattern="pr_Amon_",full.names=T)
      temp_l= list.files(paste(data_path2,future_senario[future],"/",models[mod],"/",sep=""),pattern="tas_Amon_",full.names=T)
      
      future_precip = raster(prec_l[1])
      future_temp=raster(temp_l[1])
      extent(future_precip)=extent(0,360,-90,90)
      extent(future_temp)=extent(0,360,-90,90)
      
      future_precip_ratio=future_precip/hist_precip
      future_temp_ratio=future_temp/hist_temp
      
      future_precip_ratio=rotate(future_precip_ratio)
      future_precip_ratio=resample(future_precip_ratio, biomass2, method="ngb")
  
      future_temp_ratio=rotate(future_temp_ratio)
      future_temp_ratio=resample(future_temp_ratio, biomass2, method="ngb")
      
      max_temp=raster("/Users/stijnhantson/Documents/data/WorldClim2/bioclimat/wc2.1_10m_Max_Temp.tif")
      min_temp=raster("/Users/stijnhantson/Documents/data/WorldClim2/bioclimat/wc2.1_10m_min_temp.tif")
      mean_temp=raster("/Users/stijnhantson/Documents/data/WorldClim2/bioclimat/wc2.1_10m_Annual_Mean_Temperature.tif")
      precip=raster("/Users/stijnhantson/Documents/data/WorldClim2/bioclimat/wc2.1_10m_Annual_Precipitation.tif")
      
      max_temp= aggregate(max_temp, fact = 0.5/res(max_temp), fun = "mean")
      min_temp= aggregate(min_temp, fact = 0.5/res(min_temp), fun = "mean")
      mean_temp= aggregate(mean_temp, fact = 0.5/res(mean_temp), fun = "mean")
      precip= aggregate(precip, fact = 0.5/res(precip), fun = "mean")
      
      max_temp = future_temp_ratio*max_temp
      min_temp = future_temp_ratio*min_temp
      mean_temp = future_temp_ratio *mean_temp
      precip = precip*future_precip_ratio
   }
   print("test")
    r= stack(pop,pop_ru,road,livestock,max_temp,min_temp,mean_temp,precip,prec75,srad,elevation,cropland,ba_mod,clay,sand)
   names(r) = c("pop","pop_ru","road","livestock","max_temp","min_temp","mean_temp",'precip',"prec75","srad","elevation","cropland","ba_mean","clay","sand")
#   r= stack(pop,livestock,max_temp,min_temp,mean_temp,precip,prec75,srad,elevation,cropland,ba_mod)
 #  names(r) = c("pop","livestock","max_temp","min_temp","mean_temp",'precip',"prec75","srad","elevation","cropland","ba_mean")
   
   veg_f = predict(r,rf)

   veg_c = c(veg_c,veg_f)
   ba=c(ba,ba_mod)
}
   veg_his =c(veg_his,veg_c[[2]])
   veg_ssp126= c(veg_ssp126,veg_c[[3]])
   veg_ssp245=c(veg_ssp245,veg_c[[4]])
   veg_ssp370=c(veg_ssp370,veg_c[[5]])
   veg_ssp585=c(veg_ssp585,veg_c[[6]])
   
   ba_his = c(ba_his,ba[[2]])
   ba_ssp126= c(ba_ssp126,ba[[3]])
   ba_ssp245=c(ba_ssp245,ba[[4]])
   ba_ssp370=c(ba_ssp370,ba[[5]])
   ba_ssp585=c(ba_ssp585,ba[[6]])
}

plot(veg_ssp585[[2]]-veg_his[[2]],zlim=c(-1000,1000))
plot(veg_ssp585[[4]]-veg_his[[4]],zlim=c(-1000,1000))
plot(veg_ssp585[[5]]-veg_his[[5]],zlim=c(-1000,1000))
plot(veg_ssp585[[6]]-veg_his[[6]],zlim=c(-1000,1000))
plot(veg_ssp585[[7]]-veg_his[[7]],zlim=c(-1000,1000))

veg_ssp=mean(veg_ssp585[[2]],veg_ssp585[[3]],veg_ssp585[[4]],veg_ssp585[[5]],veg_ssp585[[6]],veg_ssp585[[7]])
veg_ssp370_all =mean(veg_ssp370[[2]],veg_ssp370[[3]],veg_ssp370[[4]],veg_ssp370[[5]],veg_ssp370[[6]],veg_ssp370[[7]])
veg_hist=mean(veg_his[[2]],veg_his[[3]],veg_his[[4]],veg_his[[5]],veg_his[[6]],veg_his[[7]])

ba_ssp = mean(ba_ssp585[[2]],ba_ssp585[[3]],ba_ssp585[[4]],ba_ssp585[[5]],ba_ssp585[[6]],ba_ssp585[[7]])
ba_ssp370 = mean(ba_ssp370[[2]],ba_ssp370[[3]],ba_ssp370[[4]],ba_ssp370[[5]],ba_ssp370[[6]],ba_ssp370[[7]])
ba_hist = mean(ba_his[[2]],ba_his[[3]],ba_his[[4]],ba_his[[5]],ba_his[[6]],ba_his[[7]])

for (kr in 2:7){
plot(((ba_ssp585[[kr]]-ba_his[[kr]])/ba_his[[kr]]),zlim=c(-1,2))
}

plot(ba_ssp-ba_hist, zlim=c(-0.02,0.02))


plot(veg_hist)
plot(veg_ssp)
dif=veg_ssp-veg_hist
dif370=veg_ssp370_all-veg_hist

ba_dif = ba_ssp-ba_hist

plot(ba_dif)
plot(dif)
plot(ba_dif,dif, zlim=c(-0.02,0.02))


#plot(dif,zlim=c(-20,20))
dif[dif > 2000]=2000
dif[dif < -2000]= -2000

plot(dif)

dif_all_change = dif
plot(dif_all_change-dif)
pal1 <- colorRampPalette(c("red", "white","blue"))
plot(dif,col= pal1(100) )


###### try with model ouput  #############

library(randomForest)
data_path = "/Users/stijnhantson/Documents/data/CMIP6_fire/DATA_ALL/DATA/"
data_path1 = "/Users/stijnhantson/Documents/data/CMIP6_fire/DATA_ALL/DATA_C/"
data_path2 = "/Users/stijnhantson/Documents/data/CMIP6_fire/DATA_ALL/DATA_CL/"

models =c("CESM2","CESM2-WACCM","CNRM-ESM2-1","EC-Earth3-Veg","MPI-ESM1-2-LR","NorESM2-MM")
future_senario=c("historical","ssp126","ssp245","ssp370","ssp585")
mod=1
change_cveg_em =0
change_cveg_ba =0
change_em_all=0
change_ba_all=0
change_cveg_em_gam=0
change_cveg_ba_gam=0
change_cveg_gam=0


future = 4
for (mod in 1:length(models)){

ba_mod_l= list.files(paste(data_path,future_senario[future],"/",models[mod],"/",sep=""),pattern="fFire_",full.names=T)
ba_mod_l1= list.files(paste(data_path,future_senario[1],"/",models[mod],"/",sep=""),pattern="fFire_",full.names=T)
em_future = raster(ba_mod_l[1])
em_hist = raster(ba_mod_l1[1])

extent(em_future)=extent(0,360,-90,90)
extent(em_hist)=extent(0,360,-90,90)

ba_mean1=raster::shift(rotate(raster::shift(ba_mean, 180)), 180)


gfed_region1=resample(gfed_region,em_future, method="ngb")
cor_fact=gfed_region1
cor_fact[] = 0
for (k in 1:24){
   em1=em_hist
   ba1 = ba_mean1
   em1[gfed_region1!=k] =NA
   ba1[gfed_region!=k] =NA
   
   emis= cellStats(em1,sum)
   burn= cellStats(ba1,sum)
   core = burn/emis
   cor_fact[gfed_region1 == k] = core
}
ba_historic = em_hist*cor_fact
ba_future = em_future*cor_fact

ba_historic[ba_historic>0.75] = 0.75
ba_future[ba_future>0.75] = 0.75

print(test1)

cveg_l= list.files(paste(data_path1,future_senario[future],"/",models[mod],"/",sep=""),pattern="cVeg_Lmon",full.names=T)
cveg_l1= list.files(paste(data_path1,future_senario[1],"/",models[mod],"/",sep=""),pattern="cVeg_Lmon",full.names=T)
cveg_future = raster(cveg_l[1])
cveg_hist = raster(cveg_l1[1])

extent(cveg_future)=extent(0,360,-90,90)
extent(cveg_hist)=extent(0,360,-90,90)

crop_l= list.files(paste(data_path,future_senario[future],"/",models[mod],"/",sep=""),pattern="cropFrac",full.names=T)
crop_l1= list.files(paste(data_path,future_senario[1],"/",models[mod],"/",sep=""),pattern="cropFrac",full.names=T)

if (length(crop_l)>0){
crop_future = raster(crop_l[1])
crop_hist = raster(crop_l1[1])
}else{
   crop_future = resample(crop_future,cveg_future)
   crop_hist = resample(crop_hist,cveg_future)
}

extent(crop_future)=extent(0,360,-90,90)
extent(crop_hist)=extent(0,360,-90,90)

gpp_l= list.files(paste(data_path,future_senario[future],"/",models[mod],"/",sep=""),pattern="gpp",full.names=T)
gpp_l1= list.files(paste(data_path,future_senario[1],"/",models[mod],"/",sep=""),pattern="gpp",full.names=T)

if (length(gpp_l) <1){
   gpp_future = resample(gpp_future,cveg_future)
   gpp_hist = resample(gpp_hist,cveg_future)
}else{
gpp_future = raster(gpp_l[1])
gpp_hist = raster(gpp_l1[1])

}
extent(gpp_future)=extent(0,360,-90,90)
extent(gpp_hist)=extent(0,360,-90,90)

prec_l= list.files(paste(data_path2,future_senario[future],"/",models[mod],"/",sep=""),pattern="pr_Amon_",full.names=T)
temp_l= list.files(paste(data_path2,future_senario[future],"/",models[mod],"/",sep=""),pattern="tas_Amon_",full.names=T)

prec_his= list.files(paste(data_path2,future_senario[1],"/",models[mod],"/",sep=""),pattern="pr_Amon_",full.names=T)
temp_his= list.files(paste(data_path2,future_senario[1],"/",models[mod],"/",sep=""),pattern="tas_Amon_",full.names=T)

future_precip = raster(prec_l[1])
future_temp=raster(temp_l[1])
extent(future_precip)=extent(0,360,-90,90)
extent(future_temp)=extent(0,360,-90,90)

his_precip = raster(prec_his[1])
his_temp=raster(temp_his[1])
extent(his_precip)=extent(0,360,-90,90)
extent(his_temp)=extent(0,360,-90,90)


r_mod1= stack(cveg_hist,his_precip,his_temp,ba_historic,crop_hist,gpp_hist)
names(r_mod1) = c("cveg","precip","temp","fire_em","cropland","gpp")
r_mod2= stack(cveg_future,future_precip,future_temp,ba_future,crop_future,gpp_future)
names(r_mod2) = c("cveg","precip","temp","fire_em","cropland","gpp")

r2=as.data.frame(r_mod1)
r2=na.omit(r2)

r3=as.data.frame(r_mod2)
r3=na.omit(r3)

input_d=rbind(r2,r3)

rf1 <- randomForest(formula=cveg ~., data=r2, ntree=500)

m1 <- gam(cveg ~ s(precip) + s(temp) + s(fire_em)+ s(cropland)+ s(gpp),
          data = r2,family = "gaussian"(link = "log"))

test1= stack(cveg_hist,his_precip,his_temp,ba_historic,crop_hist,gpp_hist)
names(test1) = c("cveg","precip","temp","fire_em","cropland","gpp")

test2= stack(cveg_hist,his_precip,his_temp,ba_future,crop_hist,gpp_hist)
names(test2) = c("cveg","precip","temp","fire_em","cropland","gpp")

hist_predict <- predict(test1,rf1)
future_predict <- predict(test2,rf1)

hist_gam <- predict(test1,m1,type = "response")
future_gam <- predict(test2,m1,type = "response")

plot((hist_predict - future_predict),zlim=c(-1,1))
plot(ba_historic-ba_future, zlim=c(-0.5,0.5))

plot((hist_gam - future_gam))


change_cveg = future_predict-hist_predict
change_ba =ba_historic-ba_future
change_cveg_gam = hist_gam - future_gam

change_cveg_ba = c(change_cveg_ba,change_cveg)
change_ba_all = c(change_ba_all,change_ba)
change_cveg_ba_gam = c(change_cveg_ba_gam,change_cveg_gam)

r_mod1= stack(cveg_hist,his_precip,his_temp,em_hist,crop_hist,gpp_hist)
names(r_mod1) = c("cveg","precip","temp","fire_em","cropland","gpp")
r_mod2= stack(cveg_future,future_precip,future_temp,em_future,crop_future,gpp_future)
names(r_mod2) = c("cveg","precip","temp","fire_em","cropland","gpp")

r2=as.data.frame(r_mod1)
r2=na.omit(r2)

r3=as.data.frame(r_mod2)
r3=na.omit(r3)

input_d=rbind(r2,r3)

rf1 <- randomForest(formula=cveg ~., data=r2, ntree=500)

m1 <- gam(cveg ~ s(precip) + s(temp) + s(fire_em)+ s(cropland)+ s(gpp),
          data = r2,family = "gaussian"(link = "log"))

test1= stack(cveg_hist,his_precip,his_temp,em_hist,crop_hist,gpp_hist)
names(test1) = c("cveg","precip","temp","fire_em","cropland","gpp")

test2= stack(cveg_hist,his_precip,his_temp,em_future,crop_hist,gpp_hist)
names(test2) = c("cveg","precip","temp","fire_em","cropland","gpp")

hist_predict <- predict(test1,rf1)
future_predict <- predict(test2,rf1)

hist_gam <- predict(test1,m1,type = "response")
future_gam <- predict(test2,m1,type = "response")

plot((hist_predict - future_predict),zlim=c(-1,1))
plot(em_hist-em_future)

plot((hist_gam - future_gam),zlim=c(-5,5))

change_cveg = future_predict-hist_predict
change_em =em_hist-em_future
change_cveg_gam = hist_gam - future_gam

change_cveg_em = c(change_cveg_em,change_cveg)
change_em_all = c(change_em_all,change_em)
change_cveg_em_gam = c(change_cveg_em_gam,change_cveg_gam)

}

plot(change_cveg_em[[5]],change_em_all[[5]])
plot(change_cveg_ba[[2]],change_ba_all[[2]])
plot(change_cveg_em_gam[[2]],zlim=c(-5,5))


cveg_em=biomass2
cveg_ba=biomass2
cveg_em[]=NA
cveg_ba[]=NA
cveg_em_gam = cveg_em
cveg_ba_gam = cveg_em
em_all=biomass2
ba_all=biomass2
em_all[]=NA
ba_all[]=NA

for (i in 1:(length(change_cveg_em)-1)){
   cv_em =rotate(change_cveg_em[[i+1]])
   cv_ba =rotate(change_cveg_ba[[i+1]])
   cv_em_gam=rotate(change_cveg_em_gam[[i+1]])
   cv_ba_gam=rotate(change_cveg_ba_gam[[i+1]])
   
   cv_em  = resample(cv_em, biomass2, method="ngb")
   cv_ba = resample(cv_ba, biomass2, method="ngb")
   cv_em_gam  = resample(cv_em_gam, biomass2, method="ngb")
   cv_ba_gam = resample(cv_ba_gam, biomass2, method="ngb")

cveg_em=stack(cveg_em,cv_em)
cveg_ba=stack(cveg_ba,cv_ba)
cveg_em_gam=stack(cveg_em_gam,cv_em_gam)
cveg_ba_gam=stack(cveg_ba_gam,cv_ba_gam)

em =rotate(change_em_all[[i+1]])
ba =rotate(change_ba_all[[i+1]])

em  = resample(em, biomass2, method="ngb")
ba = resample(ba, biomass2, method="ngb")

em_all=stack(em_all,em)
ba_all=stack(ba_all,ba)
}

mean_cveg_em= mean(cveg_em,na.rm=T )
mean_cveg_em= mean(cveg_em[[2]],cveg_em[[3]],cveg_em[[4]],cveg_em[[6]],cveg_em[[7]])

mean_cveg_ba= mean(cveg_ba,na.rm=T )
mean_cveg_ba= mean(cveg_ba[[2]],cveg_ba[[3]],cveg_ba[[4]],cveg_ba[[6]],cveg_em[[7]])

mean_cveg_em_gam= mean(cveg_em_gam,na.rm=T )
mean_cveg_em_gam= mean(cveg_em_gam[[2]],cveg_em_gam[[3]],cveg_em_gam[[4]],cveg_em_gam[[6]],cveg_em_gam[[7]])

mean_cveg_ba_gam= mean(cveg_ba_gam,na.rm=T )
mean_cveg_ba_gam= mean(cveg_ba_gam[[2]],cveg_ba_gam[[3]],cveg_ba_gam[[4]],cveg_ba_gam[[6]],cveg_em_gam[[7]])

mean_ba= mean(ba_all,na.rm=T )
mean_ba= mean(ba_all[[2]],ba_all[[3]],ba_all[[4]],ba_all[[6]],cveg_em[[7]])

mean_em= mean(em_all,na.rm=T )
mean_em= mean(em_all[[2]],em_all[[3]],em_all[[4]],em_all[[6]],cveg_em[[7]])

writeRaster(mean_cveg_em_gam,"/Users/stijnhantson/Documents/projects/CMIP6_fire/mean_cveg_em_gam.tif")

mean_cveg_em_gam[mean_cveg_em_gam > 5] = 5
plot(mean_cveg_ba, zlim=c(-5,5))   
plot(mean_cveg_em, zlim=c(-5,5)) 
plot((mean_cveg_ba_gam*-1), zlim=c(-5,5))   
plot((mean_cveg_em_gam*-1), zlim=c(-5,5)) 
plot(mean_em, zlim=c(-2,2))

cellStats(mean_cveg_ba,mean)
cellStats(mean_cveg_em_gam,mean)

plot(mean_ba,zlim=c(-0.1,0.1))

varImpPlot(rf1)

test1= stack(cveg_hist,his_precip,his_temp,em_hist,crop_hist,gpp_future)
names(test1) = c("cveg","precip","temp","fire_em","cropland","gpp")

test2= stack(cveg_hist,his_precip,his_temp,em_future,crop_future,gpp_future)
names(test2) = c("cveg","precip","temp","fire_em","cropland","gpp")

hist_predict <- predict(test1,rf1)
future_predict <- predict(test2,rf1)

plot((hist_predict - future_predict),zlim=c(-1,1))
plot(ba_historic-ba_future, zlim=c(-0.5,0.5))

plot((hist_predict - future_predict),(ba_historic-ba_future))

ar = area(ApPl_prob)

cellStats(ar*biomass,sum)
cellStats(ar*ApPl_prob,sum)
cellStats(ApPl_prob1,sum)

partialPlot(rf1,input_d,fire_em)

########## calculate nr months to reach 75% precip  ###################
prec = stack(list.files("/Users/stijnhantson/Documents/data/WorldClim2/precip/",pattern = ".tif",full.names=T))
drought <- function(x) {
 p = x[order(x,decreasing=T)]
 s = sum(x)
 for (i in 1:12){
   k[i] = (sum(p[1:i]))/s
 }   
 k[k <= 0.75] = 0.5
   k[k > 0.75] = 0
   k[k > 0.2] = 1
 pre=sum(k)
  return(pre)
}

ds=calc(prec,drought)
plot(ds,zlim=c(0,13))
writeRaster(ds,"/Users/stijnhantson/Documents/data/WorldClim2/nr_months_75precip.tif")


av = raster("/Users/stijnhantson/Documents/projects/FireMIP/benchmarks_firemip/datasets/extra/avitabile/GEOCARBON_Global_Forest_Biomass/GEOCARBON_Global_Forest_AGB_10072015.tif")
av[is.na(av)] = 0
av05= aggregate(av, fact = 50, fun = "mean")





