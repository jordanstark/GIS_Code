# Generate interactive map of trails for planning

#### setup ####
# packages
library(sf)
library(raster)

# paths
#GISlib  <- "C:/Users/Jordan/Desktop/Smokies trips/March_2020_Smokies/SensorLocationPlan/"
GISlib <- "E:/GIS_SensorStratification/GIS/"



# import data
ETRrast <- raster(paste(GISlib,"ETRzip/ETRzip.gri",sep=""))

stdcrs <- crs(ETRrast)

# Import and transform trails and roads (for mapping only)
trails <- st_read(paste(GISlib,"GRSM_TRAILS/GRSM_TRAILS.shp",sep=""))
roads  <- st_read(paste(GISlib,"GRSM_ROAD_CENTERLINES/GRSM_ROAD_CENTERLINES.shp",sep=""))
trails <- st_transform(trails,stdcrs)
roads  <- st_transform(roads,stdcrs)

# Import and transform watershed data amd buffer
allbuffer  <- st_read(paste(GISlib,"SampleBufferFeb2020_3/SampleBufferFeb2020_3.shp",sep=""))
watersheds <- st_read(paste(GISlib,"GRSM_WATERSHEDS/GRSM_WATERSHEDS.shp",sep=""))

allbuffer <- st_transform(allbuffer,stdcrs)

# Import sampled points
CosbySites  <- st_read(paste(GISlib,"Cosby_SampleSites/Cosby_SampleSites.shp",sep=""))
BigCrSites  <- st_read(paste(GISlib,"BigCr_SampleSites/BigCr_SampleSites.shp",sep=""))



#### plotting ####
library(tmap)
library(tmaptools)

tmap_mode("view")

#EVI_Area    <- raster(paste(GISlib,"EVI_Summary/EVI_Summary.gri",sep=""),
#                      band=1)
parkbound <- st_read(paste(GISlib,"GRSM_BOUNDARY_POLYGON/GRSM_BOUNDARY_POLYGON.shp",sep=""))

parkbound <- st_transform(parkbound,stdcrs)
parkbound[,2:21] <- NULL # don't need attrs just the points
parkbound <- parkbound[parkbound$OBJECTID<18,]

tm_shape(parkbound,is.master=T) +
  tm_polygons(alpha=0.1,border.col="blue") +
tm_shape(trails) +
  tm_lines(col="black",lty=2,id="TRAILNAME") +
tm_shape(roads) +
  tm_lines(col="red",id="RDLABEL") +
tm_shape(CosbySites) +
  tm_dots(col="ETR") +
tm_shape(BigCrSites)+
  tm_dots(col="ETR")

