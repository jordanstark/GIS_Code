# Script to stratify points sampled from across park
# and choose sampling points in Cosby based on stratification 
# Mar 2020
# Jordan Stark

#configuration test

#### setup ####
# packages
library(sf)
library(raster)

# paths
lib  <- "C:/Users/Jordan/Desktop/Smokies trips/March_2020_Smokies/"

# import data
ETRrast <- raster(paste(lib,"SensorLocationPlan/ETRzip/ETRzip.gri",sep=""))

stdcrs <- crs(ETRrast)

installedpts <- read.csv(paste(lib,"Installed_SitesMar20.csv",sep=""))

# prep spatial points
pts <- installedpts[,c("X","Y")]
pts <- SpatialPoints(pts)
crs(pts) <- stdcrs #check!

pts <- SpatialPointsDataFrame(pts,installedpts)

pts$ETR <- extract(ETRrast,pts)


# Import and transform trails and roads (for mapping only)
#trails <- st_read(paste(GISlib,"GRSM_TRAILS/GRSM_TRAILS.shp",sep=""))
#roads  <- st_read(paste(GISlib,"GRSM_ROAD_CENTERLINES/GRSM_ROAD_CENTERLINES.shp",sep=""))
#trails <- st_transform(trails,stdcrs)
#roads  <- st_transform(roads,stdcrs)



#### plotting ####
library(tmap)
library(tmaptools)

tmap_mode("view")

EVI_Area    <- raster(paste(GISlib,"EVI_Summary/EVI_Summary.gri",sep=""),
                      band=1)
parkbound <- st_read(paste(GISlib,"GRSM_BOUNDARY_POLYGON/GRSM_BOUNDARY_POLYGON.shp",sep=""))

parkbound <- st_transform(parkbound,stdcrs)
parkbound[,2:21] <- NULL # don't need attrs just the points
parkbound <- parkbound[parkbound$OBJECTID<18,]

tm_shape(cosby,is.master=T) +
  tm_polygons(alpha=0.1,border.col="blue") +
tm_shape(finalsites) +
  tm_dots(col="ETR",size=0.2) +
tm_shape(trails) +
  tm_lines(col="black",lty=2,id="TRAILNAME") +
tm_shape(roads) +
  tm_lines(col="red",id="RDLABEL") 


tm_shape(cosby_ETR)+
  tm_raster(style="cat",palette="Set3",stretch.palette=F)




tm_shape(trails) +
  tm_lines(col="black",lty=2,id="TRAILNAME") +
tm_shape(roads) +
  tm_lines(col="red",id="RDLABEL") +
tm_shape(EVI_Area) +
  tm_raster(n=5) +
tm_shape(cosby) +
  tm_polygons(alpha=0.4,border.col="blue") +
tm_shape(parkbound) +
  tm_polygons(alpha=0.05,border.col="blue")




