
install.packages("poweRlaw")
library(poweRlaw)

da = shapefile("/Users/stijnhantson/Documents/data/FRAP/fire18_1.shp")
da= da[which(da$GIS_ACRES>1),]
data("moby")

summary(da)

da80 = da[which(da$YEAR_ > 1980 & da$YEAR_ <= 1990), ]
da90 = da[which(da$YEAR_ > 1990 & da$YEAR_ <= 2000), ]
da00 = da[which(da$YEAR_ > 2000 & da$YEAR_ <= 2010), ]
da10 = da[which(da$YEAR_ > 2010 & da$YEAR_ <= 2020), ]

m_m = displ$new(as.integer(da80$GIS_ACRES))
m_m1 = displ$new(as.integer(da90$GIS_ACRES))
m_m2 = displ$new(as.integer(da00$GIS_ACRES))
m_m3 = displ$new(as.integer(da10$GIS_ACRES))


m_m$getXmin()
m_m$getPars()
est = estimate_pars(m_m)
est = estimate_xmin(m_m)
est=1000
m_m$getXmin()
m_m$getPars()
m_m1$getXmin()
m_m1$getPars()
m_m2$getXmin()
m_m2$getPars()
m_m3$getXmin()
m_m3$getPars()

m_m$setXmin(est)
m_m1$setXmin(est)
m_m2$setXmin(est)
m_m3$setXmin(est)


plot(m_m)
points(m_m1,col="red")
plot(m_m1)




library(igraph)
fit_power_law(as.integer(da90$GIS_ACRES),xmin=1000,start=-1.5,implementation="plfit",force.continuous
=T)

minsize = 2.7
minsize1=10^minsize

da = shapefile("/Users/stijnhantson/Documents/data/FRAP/fire18_1.shp")
da= da[which(da$GIS_ACRES>minsize1),]


centroids <- getSpPPolygonsLabptSlots(da)   #calculate centroids
da$lon = centroids[,1]
da$lat = centroids[,2]

shape = shapefile("/Users/stijnhantson/Documents/data/veg_california/ca_eco_l3/ca_eco_l3.shp")
#pts <- SpatialPoints(out[,c("lon","lat")],P4S.latlon)
pts <- SpatialPoints(as.data.frame(out[,c("lon","lat")]),P4S.latlon)

shape = spTransform(shape,P4S.latlon)
da = spTransform(da,P4S.latlon)
eco = over(da, shape)
da$L1CODE = eco$NA_L1CODE
da$L3name = eco$US_L3NAME

da1=da[which(da$L1CODE == "6"),]
da1=da[which(da$L1CODE == "11"),]

summary(da)

da80 = da1[which(da1$YEAR_ > 1980 & da1$YEAR_ <= 1990), ]
da90 = da1[which(da1$YEAR_ > 1990 & da1$YEAR_ <= 2000), ]
da00 = da1[which(da1$YEAR_ > 2000 & da1$YEAR_ <= 2010), ]
da10 = da1[which(da1$YEAR_ > 2010 & da1$YEAR_ <= 2020), ]



size1 = log10(da80$GIS_ACRES)
maxsize1 = max(size1)
size2 = log10(da90$GIS_ACRES)
maxsize2 = max(size2)
size3 = log10(da00$GIS_ACRES)
maxsize3 = max(size3)
size4 = log10(da10$GIS_ACRES)
maxsize4 = max(size4)

len1 = length(size1)
len2 = length(size2)
len3 = length(size3)
len4 = length(size4)

maxsize = max(maxsize1,maxsize2,maxsize3,maxsize4)
step_size_classes <- 0.2

data_interval=0
p=0
k=0
while (p == 0){
  k=k+1
  data_interval[k]<- minsize+(step_size_classes*(k-1))
  
  if (data_interval[k] > maxsize){
    p=1
  }
}



bin_center<-1		#bin_center almacena el tamaÒo central de cada bin
l=0
test1=0
area1=0
test2=0
area2=0
test3=0
area3=0
test4=0
area4=0
for (q in 2:k){				#hay 10 bins de datos
  l=l+1
  bin_center[l]<-log10(mean(10^data_interval[l], 10^data_interval[q])) 	#el lower bound queda igual cuando no hubo valores en el anterior
  binsize = 10^data_interval[q] - 10^data_interval[l]
  test1 <- size1[size1 < data_interval[q]]
  test2 <- size2[size2 < data_interval[q]]
  test3 <- size3[size3 < data_interval[q]]
  test4 <- size4[size4 < data_interval[q]]
  area1[l] <- log10(((length(test1[test1 >= data_interval[l]]))/binsize))
  area2[l] <- log10(((length(test2[test2 >= data_interval[l]]))/binsize))
  area3[l] <- log10(((length(test3[test3 >= data_interval[l]]))/binsize))
  area4[l] <- log10(((length(test4[test4 >= data_interval[l]]))/binsize))
  
}

bin_center = 0
area1=0
r=0
q=1
k=1
p=1
size = size1
len=length(size)
while (r == 0){
 k=k+1
test1 <- size[size < data_interval[k]]
binsize = 10^data_interval[k] - 10^data_interval[k-p]
area1[q] <- log10(((length(test1[test1 >= data_interval[k-p]]))/binsize)/len)
bin_center[q]<-log10(mean(c(10^data_interval[k], 10^data_interval[k-p]))) 	#el lower bound queda igual cuando no hubo valores en el anterior

if (area1[q] != -Inf){
q=q+1
p=1
}else{
  p=p+1
}
if (data_interval[k] > max(size)){
  r=1
}
}

    data_entrada1 <- data.frame(bin_center,area1)	#make data.frame for power law analysis
    data_entrada2 <- data.frame(bin_center,area1)	#make data.frame for power law analysis
    data_entrada3 <- data.frame(bin_center,area1)	#make data.frame for power law analysis
    data_entrada4 <- data.frame(bin_center,area1)	#make data.frame for power law analysis
    
    data_entrada1[ data_entrada1 == "-Inf"] <- NA 
    data_entrada2[ data_entrada2 == "-Inf"] <- NA 
    data_entrada3[ data_entrada3 == "-Inf"] <- NA 
    data_entrada4[ data_entrada4 == "-Inf"] <- NA 
    
lm(area1 ~ bin_center, data=data_entrada1)
lm(area1 ~ bin_center, data=data_entrada2)
lm(area1 ~ bin_center, data=data_entrada3)
lm(area1 ~ bin_center, data=data_entrada4)

plot(data_entrada4, ylim=c(-9,-3), xlab="fire size (log10(acres))", ylab="log10(frequency)",col="white")
points(data_entrada1,col="orange")
points(data_entrada2,col="red")
points(data_entrada3,col="blue")
points(data_entrada4,col="green")

legend( x="topright",legend=c("1980s :b = 1.889","1990s :b = 1.721","2000s :b = 1.729","2010s :b = 1.575 "),col=c("orange","red","blue","green"),cex=1.2,pch=1,bty = "n")
legend( x="topright",legend=c("1980s","1990s","2000s","2010s"),col=c("orange","red","blue","green"),cex=1.2,pch=1,bty = "n")

    coef<-summary(reg)$coefficients
    slope<-coef[2,1]
    intercep<-coef[1,1]
    r_square<-summary(reg)$r.squared
    
    lmp <- function (modelobject) {			##to extract p-value
      if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
      f <- summary(modelobject)$fstatistic
      p <- pf(f[1],f[2],f[3],lower.tail=F)
      attributes(p) <- NULL
      return(p)
    }
    
    p_value<-lmp(reg)
    

library(EnvStats)

1/(log(geoMean(10^size1))-log(501))

bet=0
res=0
beta=1
for (i in 1:100){
beta = beta+0.01
res[i] = (log((beta-1)/(((501^(beta*(-1))+1)-(130000^(beta*(-1))+1)))))-(beta*(log(geoMean(10^size1))))
bet[i] = beta
}

cbind(res,bet)
