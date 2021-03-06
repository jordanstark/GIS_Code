# clean calculation of solar irradiance using hillShade
# Jordan Stark
# Dec 2019


gispath = gispath
intermediatepath = intermediatepath

# packages
library(raster)
library(rgdal)
library(rgeos)
library(solartime)

# import and crop DEM
elev <- raster(paste(gispath,"dem10",sep=""))
stdcrs <- crs(proj4string(elev))

DEM <- aggregate(elev,fact=3) #30m scale for 1st batch of models
names(DEM) <- "DEM"
DEM <- crop(DEM,extent(225982,317052,3921251,3964481)) #strdist extent + 2km


# calculate slope and aspect in degrees and radians
slope <- terrain(DEM,opt="slope",unit="degrees")
aspect <- terrain(DEM,opt="aspect",unit="degrees")
slope_r <- terrain(DEM,opt="slope")
aspect_r <- terrain(DEM,opt="aspect")

# calculate hourly sun positions for every day of the year
  # using 'solartime'
  # consider calculating directly, using unique lat/lon by raster point
jday <- 1:365
solar_angles <- vector(mode="list",length=365)


for(i in jday){
  allangles <- data.frame(computeSunPositionDoyHour(doy=i, hour=seq(0,23,by=0.5), 
                                                    latDeg=35.574204, longDeg=-83.482040,
                                                    timeZone=-5))
  dayangles <- allangles[which(allangles$elevation>0),]
  dayangles$decDeg <- dayangles$declination * 180 / pi
  dayangles$elevDeg <- dayangles$elevation * 180 / pi
  dayangles$azDeg <- dayangles$azimuth * 180 / pi
  solar_angles[[i]] <- data.frame(hour = dayangles$hour, 
                                  declination = dayangles$decDeg, 
                                  solar.elev = dayangles$elevDeg, 
                                  azimuth = dayangles$azDeg,
                                  doy = i)
}



# calculate relative irradiance following Pierce w/hillshade
hillshade_stack <- stack()
slope_r  <- terrain(DEM,opt="slope")
aspect_r <- terrain(DEM,opt="aspect") # to get these in radians

for(i in 1:365){
  tempstack <- stack()
  day <- solar_angles[[i]]
  for(j in 1:length(day$hour)){
    azimuth <- day$azimuth[j]
    angle <- day$solar.elev[j]
    hour_rast <- hillShade(slope_r,aspect_r,angle=angle,direction=azimuth, normalize=T)
    tempstack <- stack(tempstack,hour_rast)
  }
  dailysum <- sum(tempstack,na.rm=T)/2 # at half-hour timestep
  writeRaster(dailysum,paste(intermediatepath,"Rad_Rasters_d10/rad_",i,sep=""))
}
