## Prepping data to stratify soil moisture/temp sensor deployments
# Phenometric dataset from MCD12Q2 in AppEEARS
# Jordan Stark, Feb 2020

#Data locations
CCPath <- "E:/GIS_SensorStratification/GIS/Canopy_Cover/"
ElevPath  <- "E:/GIS_SensorStratification/GIS/Elevation/Elev.gri"

library(raster)
library(sp)
library(rgdal)
library(rgeos)
library(lubridate)


#### Data import ####
# EVI amplitude
  CC_list <- list.files(paste(CCPath, "Percent_Cover/",sep=""),full.names=T)

  CC_stack <- stack()
  
  for(i in 1:length(CC_list)){
    CC_stack <- stack(CC_stack, raster(CC_list[i]))
  }
  
  CC_stack[CC_stack==200] <- NA #water value
  CC_stack[CC_stack==253] <- NA #fill value
  

# elev as template for data
  elev <- raster(ElevPath)

  
#### Raster calculations ####

# calculate max of each pixel
  CC_max <- calc(CC_stack,function(x) max(x,na.rm=T))

# reproject and crop
  CC_max_r <- projectRaster(CC_max,crs=crs(elev),res=res(CC_max))
  CC_max_r <- crop(CC_max_r, extent(elev))

  writeRaster(CC_max_r,"E:/GIS_SensorStratification/GIS/Max_Canopy_Cover")
 