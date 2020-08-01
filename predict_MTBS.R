

library(raster)
library("iml")
library("randomForest")
library("rfUtilities")
library("asbio")
library("party")
daily_res=read.table("/Users/stijnhantson/Documents/projects/VIIRS_ros/final_dataset_V4.txt",header=T)

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
#res$ws =as.numeric(as.character(res$ws))
res$vs =as.numeric(as.character(res$vs))
res$growth =as.numeric(as.character(res$growth))
res$total_area =as.numeric(as.character(res$total_area))
res$mean_frp =as.numeric(as.character(res$mean_frp))
res$frp_95 =as.numeric(as.character(res$frp_95))
res$max_land =as.numeric(as.character(res$max_land))
res$mean_land =as.numeric(as.character(res$mean_land))
res$biomass =as.numeric(as.character(res$biomass))
res$year =as.numeric(as.character(res$year))
res$month =as.numeric(as.character(res$month))
res$doy_out =as.numeric(as.character(res$doy_out))

res$mean_dnbr =as.numeric(as.character(res$mean_dnbr))
res$mean_rdnbr =as.numeric(as.character(res$mean_rdnbr))
res$mean_bas =as.numeric(as.character(res$mean_bas))
res$median_dnbr =as.numeric(as.character(res$median_dnbr))
res$median_rdnbr =as.numeric(as.character(res$median_rdnbr))
res$median_bas =as.numeric(as.character(res$median_bas))
res$q95_dnbr =as.numeric(as.character(res$q95_dnbr ))
res$q95_rdnbr =as.numeric(as.character(res$q95_rdnbr))
res$q95_bas =as.numeric(as.character(res$q95_bas))

res = res[-1,]
res$per_ba = res$growth/res$total_area
res$growth_km =res$growth/1000000

res$human = 0
res$human[res$cause !=1 & res$cause !=14 & res$cause !=17]=1
res$human[res$cause ==1 ]=2

res$ros_km = (res$median95_ros*24)/1000
res$ros_mean_km = (res$mean_ros*24)/1000
summary(res)

res1=res
res1 = res[res$max_land == 1,]

res2 = res1[,c(4:18,35)]
res2=na.omit(res2)
summary(res2)
plot(res2$tmmx,res2$median_dnbr)
summary(lm(res1$tmmx~res1$mean_bas))

cf1 <- randomForest(mean_rdnbr ~ . , data = res2,control=cforest_unbiased(mtry=6,ntree=500))
#varimp(cf1)
summary(cf1)
varImpPlot(cf1,conditional=TRUE)
cf1 





