
library(raster)

x <- shapefile('/Users/stijnhantson/Documents/projects/VIIRS_ros/output/2015_ROUGH.shp')

crs(x)
x$area_ha <- area(x)/10000
x

write.table(x, '/Users/stijnhantson/Documents/projects/VIIRS_ros/test.txt')


ros<- shapefile('/Users/stijnhantson/Documents/projects/VIIRS_ros/output/2015_ROUGH_ros.shp')
rr= as.data.frame(unique(ros$YYYYMMDD))
rr$mean = tapply(ros$ros,ros$YYYYMMDD, "mean")
rr$max = tapply(ros$ros,ros$YYYYMMDD, "max")

write.table(rr,'/Users/stijnhantson/Documents/projects/VIIRS_ros/test1.txt' )
summary(rr
        )