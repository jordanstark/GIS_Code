# prep sensor list for Tiff

# libraries
library(sp)

# import data
sensordat <- read.csv("C:/Users/Jordan/Desktop/Smokies Data/SensorLocationHistory.csv")

# clean site info

sites <- sensordat[,c("SiteID","Watershed","X","Y")]
sites <- unique(sites)
names(sites) <- c("SiteID","Watershed","UTM_E","UTM_N")

sites_sp <- SpatialPointsDataFrame(sites[,c("UTM_E","UTM_N")],data=sites,
                                   proj4string=CRS("+proj=utm +zone=17 +datum=WGS84 +units=m"))

sites_latlon <- spTransform(sites_sp,CRS("+proj=longlat"))

final_dat <- sites_latlon@data
final_dat$lon <- sites_latlon@coords[,1]
final_dat$lat <- sites_latlon@coords[,2]

write.csv(final_dat,
          "C:/Users/Jordan/Desktop/Smokies Data/SiteLocations.csv",
          row.names=F)
