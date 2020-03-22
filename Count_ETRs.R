# Summarize extant site characteristics
# Jordan Stark
# March 2020


# load data
points <- read.csv("C:/Users/Jordan/Desktop/Smokies trips/March_2020_Smokies/ExtantPoints.csv")

points <- points[points$Status=="good",]

# load ETR description
strata <- read.csv("C:/Users/Jordan/Desktop/Smokies trips/March_2020_Smokies/SensorLocationPlan/strata_boundaries.csv")

# summarize by ETR
summary <- aggregate(Site~ETR,points,length)
names(summary) <- c("ETR","Count")
summary <- merge(summary,strata,by.x="ETR",by.y="X")



# Summarize proposed & installed sites by trail
newpoints <- read.csv("C:/Users/Jordan/Desktop/Smokies trips/March_2020_Smokies/SitesMar20.csv",skip = 1)
ETR_bytrail <- aggregate(X ~ Trail + ETR,newpoints,length)

ETRcount <- aggregate(ETR ~ Trail+Watershed,newpoints,function(x) length(unique(x)))


accessiblePoints <- newpoints[newpoints$Inaccessible!="y",]
installedPoints <- newpoints[newpoints$Installed=="y",]

installedETRs <- unique(installedPoints$ETR)

pointsNewETR <- accessiblePoints[-which(accessiblePoints$ETR %in% installedETRs),]

newETRs <- aggregate(ETR ~ Trail+Watershed,pointsNewETR,function(x) length(unique(x)))
