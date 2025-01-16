library(sf)
library(sp)
library(mapview)
library(ggplot2)
library(gstat)
library(viridis)
library(raster)
library(spatstat)
library(rgdal)

setwd("C:/Users/riley/OneDrive/Documents/Programming/India/fc_files")
df <- read.csv("Populated_Places.txt", sep="\t", header=T)

ggplot(data=df, aes(x=long_dd, y=lat_dd)) +
  geom_point(color="red", size = 0.1, alpha = 0.05) +
  coord_equal(ratio=1)

total.sp <- df
coordinates(total.sp) <- ~long_dd+lat_dd
longitudeExtent <- c(-60, 110)
latitudeExtent <- c(0, 40)
tmp.ppp <- as.ppp(dplyr::select(df, long_dd,lat_dd),owin(longitudeExtent,latitudeExtent))
tmp.hull <- convexhull(tmp.ppp)
tmp.owin <- expand.owin(tmp.hull, distance = .8)
total.ppp <- as.ppp(dplyr::select(df, long_dd,lat_dd),W = tmp.owin)
Q <- quadratcount(total.ppp, nx= 1000, ny=1000)
Q.d <- intensity(Q)


ore <- df[grepl("ore ", paste(df$full_name, " ")),]
uru <- df[grepl("uru ", paste(df$full_name, " ")),]

# PUR

pur <- df[grepl("pur ", paste(df$full_name, " ")),]

mapview(pur, xcol = "long_dd", ycol = "lat_dd", grid = FALSE)

ggplot(data=pur, aes(x=long_dd, y=lat_dd)) +
  geom_point(color="red", size = 0.1, alpha = 0.05) +
  coord_equal(ratio=1)
  #stat_density2d(aes(fill = ..density..), geom = "raster", contour = FALSE, h = c(0.25, 0.25))
  #stat_density2d(aes(x=long_dd, y=lat_dd, fill = after_stat(level)), alpha = .5, geom = "polygon", data = pur)

ggplot()+
  geom_point(data=pur, aes(x=long_dd, y=lat_dd), color="red", size = .1, alpha = 0.05) +
  geom_point(data=puram, aes(x=long_dd, y=lat_dd), color="green", size = .1, alpha = 0.05) +
  geom_point(data=pura, aes(x=long_dd, y=lat_dd), color="blue", size = .1, alpha = 0.05) +
  geom_point(data=puri, aes(x=long_dd, y=lat_dd), color="black", size = .1, alpha = 0.05) +
  coord_equal(ratio=1)

ggplot()+
  fortify(India_State_Boundary.shp)
  geom_point(data=gaon, aes(x=long_dd, y=lat_dd), color="red", size = .1, alpha = 0.05) +
  geom_point(data=ganj, aes(x=long_dd, y=lat_dd), color="green", size = .1, alpha = 0.05) +
  coord_equal(ratio=1)

ggplot()+
  geom_point(data=guda, aes(x=long_dd, y=lat_dd), color="red", size = .1, alpha = 0.05) +
  geom_point(data=gudi, aes(x=long_dd, y=lat_dd), color="green", size = .1, alpha = 0.05) +
  geom_point(data=god, aes(x=long_dd, y=lat_dd), color="blue", size = .1, alpha = 0.05) +
  coord_equal(ratio=1)

pur.sp <- pur
coordinates(pur.sp) <- ~long_dd+lat_dd
longitudeExtent <- c(-65, 105)
latitudeExtent <- c(0, 35)
tmp.ppp <- as.ppp(dplyr::select(pur, long_dd,lat_dd),owin(longitudeExtent,latitudeExtent))
tmp.hull <- convexhull(tmp.ppp)
tmp.owin <- expand.owin(tmp.hull, distance = .8)
pur.ppp <- as.ppp(dplyr::select(pur, long_dd,lat_dd),W = tmp.owin)
density <- density(pur.ppp)
raster(density) -> r
plot(r)
