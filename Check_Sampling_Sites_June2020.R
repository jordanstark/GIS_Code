# Check distribution of other variables and correlation across installed sensor locations
# June 2020 

#### setup ####
# Libraries
library(raster)
library(sf)
library(ggplot2)
library(tmap)
library(tmaptools)
library(patchwork)

# paths
GISlib <- "E:/GIS_SensorStratification/GIS/"


# import points sampled across park at 250m
sitelist <- read.csv("E:/GIS_SensorStratification/SensorLocationHistory.csv")
installed_sites <- sitelist[sitelist$Last_Status=="good",]

sp <- SpatialPoints(sitelist[,c("X","Y")])
sites <- SpatialPointsDataFrame(sp,sitelist)


parksites <- st_read(paste(GISlib,"rastersample_250m/rastersample_250m.shp",sep="")) # all points in the park



# import GIS data to add
max_CC <- raster(paste(GISlib,"Max_Canopy_Cover/Max_Canopy_Cover.gri",sep=""))
Elev   <- raster(paste(GISlib,"Elev_park/Elev_park.gri",sep=""))
TCI    <- raster(paste(GISlib,"TCI/tci.gri",sep=""))
lTCI   <- log(TCI)
Totrad <- raster(paste(GISlib,"Totrad/Totrad.gri",sep=""))
strdist <- raster(paste(GISlib,"streamdist",sep=""))
lstrdist <- log(strdist)

stdcrs <- crs(Elev)

crs(sites) <- stdcrs

# import trails and roads for planning
trails <- st_read(paste(GISlib,"GRSM_TRAILS/GRSM_TRAILS.shp",sep=""))
roads  <- st_read(paste(GISlib,"GRSM_ROAD_CENTERLINES/GRSM_ROAD_CENTERLINES.shp",sep=""))
trails <- st_transform(trails,stdcrs)
roads  <- st_transform(roads,stdcrs)


# extract other GIS data to parksites and cosbysites

sites$max_CC  <- extract(max_CC,sites)
sites$Elev    <- extract(Elev,sites)
sites$TCI     <- extract(TCI,sites)
sites$log_TCI    <- extract(lTCI,sites)
sites$Totrad  <- extract(Totrad,sites)
sites$strdist <- extract(strdist,sites)

parksites$max_CC <- extract(max_CC,parksites)

sitedf <- sites@data
sitedf_out <- sitedf[,c("SiteID","Watershed","X","Y","SRS","max_CC","Elev","log_TCI","Totrad","strdist","TCI")]
sitedf_out <- sitedf_out[!duplicated(sitedf_out$SiteID),]

write.csv(sitedf_out,"E:/GIS_SensorStratification/SiteGISdat.csv")


sitedf <- sitedf[sitedf$Last_Status=="good",]


#### evaluate sites for distrib of strdist, max canopy cover, elev, TCI, totrad

g1 <- ggplot(parksites, aes(x=Elev,y=max_CC)) +
  geom_point(alpha=0.05) +
  geom_point(data=sitedf[sitedf$SRS=="yes",],color="red",size=4) +
  geom_point(data=sitedf[sitedf$SRS=="no",],color="salmon",alpha=0.8,size=4) +
  geom_point(data=sitedf[sitedf$SRS=="Validation",],color="cyan3",alpha=0.8,size=4) +
  theme_classic()

g2 <- ggplot(parksites, aes(x=Elev,y=strdist)) +
  geom_point(alpha=0.05) +
  geom_point(data=sitedf[sitedf$SRS=="yes",],color="red",size=4) +
  geom_point(data=sitedf[sitedf$SRS=="no",],color="salmon",alpha=0.8,size=4) +
  geom_point(data=sitedf[sitedf$SRS=="Validation",],color="cyan3",alpha=0.8,size=4) +
  theme_classic()

g3 <- ggplot(parksites, aes(x=Elev,y=log_TCI)) +
  geom_point(alpha=0.05) +
  geom_point(data=sitedf[sitedf$SRS=="yes",],color="red",size=4) +
  geom_point(data=sitedf[sitedf$SRS=="no",],color="salmon",alpha=0.8,size=4) +
  geom_point(data=sitedf[sitedf$SRS=="Validation",],color="cyan3",alpha=0.8,size=4) +
  theme_classic()

g4 <- ggplot(parksites, aes(x=Elev,y=Totrad)) +
  geom_point(alpha=0.05) +
  geom_point(data=sitedf[sitedf$SRS=="yes",],color="red",size=4) +
  geom_point(data=sitedf[sitedf$SRS=="no",],color="salmon",alpha=0.8,size=4) +
  geom_point(data=sitedf[sitedf$SRS=="Validation",],color="cyan3",alpha=0.8,size=4) +
  theme_classic()
    
g3 | g1 | g2 | g4

#### ID trails etc

tmap_mode("view")

tm_shape(trails) +
  tm_lines(col="black",lty=2,id="TRAILNAME") +
  tm_shape(roads) +
  tm_lines(col="red",id="RDLABEL") 




tm_shape(sites) +
  tm_dots(col="SRS",size=0.2,id="ptID") +
tm_shape(trails) +
  tm_lines(col="black",lty=2,id="TRAILNAME") +
tm_shape(roads) +
  tm_lines(col="red",id="RDLABEL") 

tm_shape(BigCr,is.master=T) +
  tm_polygons(alpha=0.1,border.col="blue")

tm_shape(BigCrsites) +
  tm_dots(col="ETR",size=0.2,id="ptID") +
  tm_shape(trails) +
  tm_lines(col="black",lty=2,id="TRAILNAME") +
  tm_shape(roads) +
  tm_lines(col="red",id="RDLABEL") 


tm_shape(SOM) +
  tm_polygons()








