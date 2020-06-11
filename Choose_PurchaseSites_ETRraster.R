# Script to stratify points sampled from across park
# and choose sampling points in Cosby based on stratification 
# Mar 2020
# Jordan Stark

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
#cataloochee     <- st_union(watersheds[watersheds$Name=="Cataloochee Creek",])
palmer          <- st_union(watersheds[watersheds$Name=="Palmer Creek",])

allbuffer <- st_transform(allbuffer,stdcrs)
palmer     <- st_transform(palmer,stdcrs)

palmerbuf <- st_intersection(allbuffer,palmer)

# Make 100m grid within buffer area
palmer_allpts <- st_intersection(palmerbuf,
                                st_make_grid(palmerbuf,
                                             cellsize=100,what="centers")) 

# extract ETR of potential points
palmer_ETR <- crop(ETRrast,palmerbuf)
palmer_allpts$ETR <- extract(palmer_ETR,palmer_allpts)

palmer_allpts$ptID <- 1:length(palmer_allpts$ETR)

pt_dist <- as.matrix(dist(st_coordinates(palmer_allpts)))


NearestPts <- function(ID,distmat) {
  pt_dists <- distmat[ID,]
  pt_dists <- sort(pt_dists)
  as.numeric(names(pt_dists[1:25]))
}


table(palmer_allpts$ETR)

#### sample sites ####
set.seed(25646984)
palmer_allpts$include <- F


palmerSplit <- split(palmer_allpts,palmer_allpts$ETR)

for(i in 1:length(palmerSplit)) {
  dat <- palmerSplit[[i]]
  
  samps <- sample.int(length(dat$ETR),
                      size = 1)
  dat$include[samps] <- T
  
  palmerSplit[[i]] <- dat
}

palmer_sites1 <- do.call(rbind,palmerSplit)
palmer_include1 <- palmer_sites1[palmer_sites1$include==T,]

PossibleETRs1 <- unique(palmer_sites1$ETR)

for(i in 1:length(PossibleETRs1)) {
  ID <- palmer_include1$ptID[i]
  ETR <- palmer_include1$ETR[i]
  nearPts <- NearestPts(ID,pt_dist)
  palmer_sites1[which(palmer_sites1$ptID %in% nearPts & palmer_sites1$ETR==ETR),c("FID","ETR","ptID","include")] <- NA
} # including geometry column creates an error-not sure why

table(palmer_sites1$ETR) 


palmerSplit <- split(palmer_sites1,palmer_sites1$ETR)
PossibleETRs2 <- unique(palmer_sites1$ETR)

for(i in 1:length(palmerSplit)) {
  dat <- palmerSplit[[i]]

  samps <- sample.int(length(dat$ETR),
                        size = 1)
  dat$include[samps] <- T
  
  palmerSplit[[i]] <- dat
}

palmer_sites2 <- do.call(rbind,palmerSplit)
palmer_include2 <- palmer_sites2[palmer_sites2$include==T,]

for(i in 1:length(PossibleETRs2)) {
  ID <- palmer_include2$ptID[i]
  ETR <- palmer_include2$ETR[i]
  nearPts <- NearestPts(ID,pt_dist)
  palmer_sites2[which(palmer_sites2$ptID %in% nearPts & palmer_sites2$ETR==ETR),c("FID","ETR","ptID","include")] <- NA
}

table(palmer_sites2$ETR)

palmerSplit <- split(palmer_sites2,palmer_sites2$ETR)

for(i in 1:length(palmerSplit)) {
  dat <- palmerSplit[[i]]
  
  samps <- sample.int(length(dat$ETR),
                      size = 1)
  dat$include[samps] <- T
  
  palmerSplit[[i]] <- dat
}

palmer_sites3 <- do.call(rbind,palmerSplit)
palmer_include3 <- palmer_sites3[palmer_sites3$include==T,]

finalsites <- rbind(palmer_include1,palmer_include2,palmer_include3)
finalsites$ETR <- factor(finalsites$ETR)

st_write(finalsites,paste(GISlib,"palmer_SampleSites",sep=""),driver="ESRI Shapefile")
write.csv(finalsites,"E:/GIS_SensorStratification/palmer_SampleSites.csv")



#### plotting ####
library(tmap)
library(tmaptools)

tmap_mode("view")



tm_shape(palmer,is.master=T) +
  tm_polygons(alpha=0.1,border.col="blue") +
tm_shape(finalsites) +
  tm_dots(col="ETR",size=0.2) +
tm_shape(trails) +
  tm_lines(col="black",lty=2,id="TRAILNAME") +
tm_shape(roads) +
  tm_lines(col="red",id="RDLABEL") 


tm_shape(palmer_ETR)+
  tm_raster(style="cat",palette="Set3",stretch.palette=F)




tm_shape(trails) +
  tm_lines(col="black",lty=2,id="TRAILNAME") +
tm_shape(roads) +
  tm_lines(col="red",id="RDLABEL") +
tm_shape(EVI_Area) +
  tm_raster(n=5) +
tm_shape(palmer) +
  tm_polygons(alpha=0.4,border.col="blue") +
tm_shape(parkbound) +
  tm_polygons(alpha=0.05,border.col="blue")




