# Script to stratify points sampled from across park
# and choose sampling points in Cosby based on stratification 
# Mar 2020
# Jordan Stark


#### setup ####
# packages
library(sf)
library(raster)
library(lubridate)

# paths
lib  <- "C:/Users/Jordan/Desktop/Smokies Data/"

# import data
ETRrast <- raster(paste(lib,"ETRzip/ETRzip.gri",sep=""))

stdcrs <- crs(ETRrast)

installedpts <- read.csv(paste(lib,"SensorLocationHistory.csv",sep=""))

# prep spatial points
pts <- installedpts[,c("X","Y")]
pts <- SpatialPoints(pts)
crs(pts) <- stdcrs #check!

pts <- SpatialPointsDataFrame(pts,installedpts)
pts$Deploy_Date <- mdy(pts$Deploy_Date)

pts$ETR <- extract(ETRrast,pts)

sitelist <- pts[pts$Last_Status=="good" & pts$Deploy_Date > ymd("2019-10-01"),]

algo_sitelist <- sitelist[sitelist$SRS=="yes",]
  
watershedlist <- aggregate(SiteID ~ ETR + Watershed, sitelist@data, FUN=length)

table(algo_sitelist$ETR)
View(sitelist@data)


# Import and transform trails and roads (for mapping only)
GISlib <- "C:/Users/Jordan/Desktop/Smokies trips/March_2020_Smokies/SensorLocationPlan/"
trails <- st_read(paste(GISlib,"GRSM_TRAILS/GRSM_TRAILS.shp",sep=""))
roads  <- st_read(paste(GISlib,"GRSM_ROAD_CENTERLINES/GRSM_ROAD_CENTERLINES.shp",sep=""))
trails <- st_transform(trails,stdcrs)
roads  <- st_transform(roads,stdcrs)

finalsites <- st_read(paste(GISlib,"BigCr_SampleSites/BigCr_Samplesites.shp",sep=""))



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




