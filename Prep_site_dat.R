# prep sensor list for Tiff

# libraries
library(sp)
library(lubridate)

# import data
sensordat <- read.csv("C:/Users/Jordan/Desktop/Smokies Data/SensorLocationHistory.csv")

# clean site info

sites <- sensordat[,c("SiteID","Watershed","X","Y","Deploy_Date","Remove_Date")]

sites$Deploy_Date <- mdy(sites$Deploy_Date)
sites$Remove_Date <- mdy(sites$Remove_Date)

first_deploy <- aggregate(Deploy_Date ~ SiteID, sites, min, na.rm=T)
last_remove  <- aggregate(Remove_Date ~ SiteID, sites, max, na.action="na.pass")

sites$Deploy_Date <- NULL
sites$Remove_Date <- NULL

sites <- unique(sites)

names(sites) <- c("SiteID","Watershed","UTM_E","UTM_N")

sites <- merge(sites,first_deploy,all.x=T)
sites <- merge(sites,last_remove,all.x=T)


sites_sp <- SpatialPointsDataFrame(sites[,c("UTM_E","UTM_N")],data=sites,
                                   proj4string=CRS("+proj=utm +zone=17 +datum=WGS84 +units=m"))

sites_latlon <- spTransform(sites_sp,CRS("+proj=longlat"))

final_dat <- sites_latlon@data
final_dat$lon <- sites_latlon@coords[,1]
final_dat$lat <- sites_latlon@coords[,2]

write.csv(final_dat,
          "C:/Users/Jordan/Desktop/Smokies Data/SiteLocations.csv",
          row.names=F)
