
library(rgdal)
library(sp)
library(raster)

indir="/Users/stijnhantson/Documents/data/reference_firegrowth/2013/Powerhouse/"
outdir1="/Users/stijnhantson/Documents/data/reference_firegrowth/2013/"

shape1 <- readOGR("/Users/stijnhantson/Documents/data/reference_firegrowth/2013/Powerhouse.shp") #readin FRAP fire perimeter data
pro = crs(shape1)

viirs_list = list.files(indir, pattern = "HeatPerimeter.shp$", recursive = TRUE, full.names=T)
viirs_list1 = list.files(indir, pattern = "HeatPerimeter.shp$", recursive = TRUE, full.names=F)

i=1
len<-nchar(viirs_list1[i])
#date1<-substr(viirs_list1[i], 1, len-6)
  
darr = readOGR(viirs_list[1])

for (i in 2:length(viirs_list)){
dass = readOGR(viirs_list[i])
darr=bind(darr,dass)
  
}
darr2=intersect(darr,dass)
plot(darr2)
crs(darr2)=pro
writeOGR(darr2, outdir1, layer= "Powerhouse", driver="ESRI Shapefile", overwrite_layer = T)

plot(dass)