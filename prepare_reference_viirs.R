
library(rgdal)
library(sp)
library(raster)

indir="/Users/stijnhantson/Documents/data/reference_firegrowth/2016/erskin/"
outdir1="/Users/stijnhantson/Documents/data/reference_firegrowth/2016/"

shape1 <- readOGR("/Users/stijnhantson/Documents/data/reference_firegrowth/2013/Powerhouse.shp") #readin FRAP fire perimeter data
pro = crs(shape1)

viirs_list = list.files(indir, pattern = ".shp$", recursive = TRUE, full.names=T)
viirs_list1 = list.files(indir, pattern = "FirePolygon_NAD_1983_UTM_Zone_10N.shp$", recursive = TRUE, full.names=F)

i=1
len<-nchar(viirs_list[i])
#date1<-substr(viirs_list1[i], 1, len-6)
  
darr = readOGR(viirs_list[1])
for (i in 2:length(viirs_list)){
  print(i)
dass = readOGR(viirs_list[i])
darr=bind(darr,dass)
  
}
darr2=intersect(darr,dass)
plot(darr2)
crs(darr2)=pro
writeOGR(darr, outdir1, layer= "erskin", driver="ESRI Shapefile", overwrite_layer = T)

plot(dass)