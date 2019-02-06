######################################################################################################
# WILD 562 - R code for LAB 2 that we need again in Lab 5
######################################################################################################

#set working directory for SPATIAL data
setwd("/Users/mark.hebblewhite/Box Sync/Teaching/UofMcourses/WILD562/Spring2019/Labs/lab4/new/")

wolfyht<-shapefile("wolfyht.shp")

# OBJECTIVE 2 - Home Range Analysis - note I've done these here for the individual level I added the pack-level
#first convert the spatialpointsdataframe to spatial points object for both wolf packs
rd.data<-wolfyht[wolfyht@data$Pack=="Red Deer",]
bv.data<-wolfyht[wolfyht@data$Pack=="Bow valley",]

# calculate 99% minimum convex polygon for both wolf packs

#first convert the spatialpointsdataframe to spatial points object
x<-wolfyht@data$EASTING
y<-wolfyht@data$NORTHING
xy<-cbind(x,y)

all <- data.frame(as.character(wolfyht@data$Pack))
coordinates(all) <- xy
proj4string(all) <-  CRS("+proj=utm +zone=11 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

#calculate 99% KDE for both wolf packs
allUD <- kernelUD(all, grid=30, extent=0.5, same4all=TRUE) # reference grid

#get polygons for home ranges
homerangeALL <- getverticeshr(allUD)

#Estimate UD in raster mode
allud <- getvolumeUD(allUD) 


## store the volume under the UD (as computed by getvolumeUD) 
## of the first animal in fud 
fud <- allud[[1]] #for first wolf pack only
## store the value of the volume under UD in a vector hr95 
hr95 <- as.data.frame(fud)[,1] 
## if hr95 is <= 95 then the pixel belongs to the home range
## (takes the value 1, 0 otherwise)
hr95 <- as.numeric(hr95 <= 95) 
## Converts into a data frame 
hr95 <- data.frame(hr95) 
## Converts to a SpatialPixelsDataFrame 
coordinates(hr95) <- coordinates(allud[[1]])
gridded(hr95) <- TRUE 

########################################################################################################################################

# OBJECTIVE 3 - Learn how to sample availabiliity within home ranges - also see GeospatialModelingEnvironment

#subset polygons by wolf pack
red.deerPOLY<-homerangeALL[homerangeALL@data$id=="Red Deer",]
bow.valleyPOLY<-homerangeALL[homerangeALL@data$id=="Bow valley",]

#generate 1000 points from Red Deer wolf pack KDE polygon
set.seed(11)
rd.avail<-spsample(red.deerPOLY, 1000, "random")
head(rd.avail@coords)

#generate 1000 points from Bow valley wolf pack KDE polygon
set.seed(11)
bv.avail<-spsample(bow.valleyPOLY, 1000, "random")
head(bv.avail@coords)

