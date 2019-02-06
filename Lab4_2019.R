##'WILD 562 Lab4 : Categorical Covariates'

## Preliminaries - Loading Packages
#function to install and load required packages
ipak <- function(pkg){
new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
if (length(new.pkg)) 
install.packages(new.pkg, dependencies = TRUE)
sapply(pkg, require, character.only = TRUE)
}

#load or install these packages:
packages <- c("plyr", "ks", "adehabitatHS", "adehabitatHR", "maptools", "rgdal", "sp", "raster","ggplot2","colorRamps","rgeos")

#run function to install packages
ipak(packages)
setwd("/Users/mark.hebblewhite/Box Sync/Teaching/UofMcourses/WILD562/Spring2019/Labs/lab4/new")

## Loading and Manipulating Landcover Data
setwd("/Users/mark.hebblewhite/Box Sync/Teaching/UofMcourses/WILD562/Spring2019/Labs/lab4/landcover/")
landcover<-raster("landcover")
image(landcover, col=rainbow(16))
#str(landcover)
landcover@data@attributes

landcover@crs@projargs
extent(landcover)
landcover
#### now lets write this as a TIFF format (easier to use) to be the same as all other raster stack objects
#### note you can sometime get errors creating a raster stack out of different kinds of rasters
setwd("/Users/mark.hebblewhite/Box Sync/Teaching/UofMcourses/WILD562/Spring2019/Labs/lab4/new/")  
writeRaster(landcover, "landcover16.tif", "GTiff", overwrite = TRUE)
landcover16 <- raster("landcover16.tif") # bringing it back in
str(landcover16@data@attributes)

data1 <- data.frame(landcover@data@attributes[[1]][2:3])
names(landcover16@data@attributes[[1]])[2]<-"COUNT"
landcover16@data@attributes <- merge(landcover16@data@attributes, data1, by="COUNT")
landcover16@data@attributes<-landcover16@data@attributes[c(2,1,3)]
landcover16@data@attributes

wolfyht<-shapefile("wolfyht.shp")
plot(landcover16, col=rainbow(16))
plot(wolfyht, add=TRUE, type="p", color = "gray25", pch=19, cex = 0.75)
extent(landcover16) 

## lets make a second plot zooming into a specific area of the Red Deer pack
yht.raster <- raster()
extent(yht.raster) <- c(xmin=570000, xmax=600000, ymin=5720000, ymax=5740000) 	
plot(landcover16, col=rainbow(16), ext=yht.raster)
legend("topleft", legend = c("Open Conifer", "Mod. Conifer", "Closed Conifer", "Deciduous", "Mixed", "Regen", "Herb", "Shrub", "Water", "Rock-Ice", "Cloud", "Burn-Forest", "Burn-Grassland", "Burn-Shrub", "Alpine Herb", "Alpine Shrub"), fill = rainbow(16), cex=0.75)
plot(wolfyht, add=TRUE, type="p", color = "gray25", pch=19)

bv.raster <- raster()
extent(bv.raster) <- c(xmin=560000, xmax=600000, ymin=5660000, ymax=5690000) 
elk_w<-raster("/Users/mark.hebblewhite/Box Sync/Teaching/UofMcourses/WILD562/Spring2019/Labs/Lab1_rintro/Lab1_data/elk_w2.tif")
plot(elk_w, col=heat.colors(7), ext=bv.raster)
legend("topleft", legend = c("1", "2", "3", "4", "5", "6", "7"), fill = rainbow(16), cex=0.75)
plot(wolfyht, add=TRUE, type="p", color = "gray25", pch=19)

extent(elk_w)
#res(elk_w) ## this was also used to check the resolutions were the same, which they were
extent(landcover16)
#res(landcover16)

## Aligning Different Extents in Raster Data
mask.raster <- raster()
extent(mask.raster) <- c(xmin=443680.6, xmax=650430.4, ymin=5618405, ymax=5789236) 	
res(mask.raster) = 30
#match projection to elc_habitat shapefile
projection(mask.raster)<- "+proj=utm +zone=11 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
#set all values of mask.raster to zero
mask.raster[]<-0
## Learn more about the format of resampling in resample using:
?resample

landcover2<-resample(landcover16, mask.raster, method="ngb") 
extent(landcover2)
## Bringing in the Distance to High Human Access layer - see Lab 2 section 1b. 
setwd("/Users/mark.hebblewhite/Box Sync/Teaching/UofMcourses/WILD562/Spring2019/Labs/Lab1_rintro/Lab1_data/")
disthhu<-raster("/Users/mark.hebblewhite/Box Sync/Teaching/UofMcourses/WILD562/Spring2019/Labs/Lab1_rintro/Lab1_data/DistFromHighHumanAccess2.tif")
plot(disthhu)
extent(disthhu)
disthhu2<-resample(disthhu, mask.raster, method="bilinear")
extent(disthhu2)

## Re-extracting both spatial data frame
Lets bring in all the data from our original lab 1 data - where ever you still have these data, and continue to merge these. 
setwd("/Users/mark.hebblewhite/Box Sync/Teaching/UofMcourses/WILD562/Spring2019/Labs/Lab1_rintro/Lab1_data/")
deer_w<-raster("/Users/mark.hebblewhite/Box Sync/Teaching/UofMcourses/WILD562/Spring2019/Labs/Lab1_rintro/Lab1_data/deer_w2.tif")
moose_w<-raster("/Users/mark.hebblewhite/Box Sync/Teaching/UofMcourses/WILD562/Spring2019/Labs/Lab1_rintro/Lab1_data/moose_w2.tif")
#elk_w<-raster("elk_w2.tif") # already brought in above
sheep_w<-raster("/Users/mark.hebblewhite/Box Sync/Teaching/UofMcourses/WILD562/Spring2019/Labs/Lab1_rintro/Lab1_data/sheep_w2.tif")
goat_w<-raster("/Users/mark.hebblewhite/Box Sync/Teaching/UofMcourses/WILD562/Spring2019/Labs/Lab1_rintro/Lab1_data/goat_w2.tif")
wolf_w<-raster("/Users/mark.hebblewhite/Box Sync/Teaching/UofMcourses/WILD562/Spring2019/Labs/Lab1_rintro/Lab1_data/wolf_w2.tif")
## Note the next two layers should already be resampled
elevation2<-raster("/Users/mark.hebblewhite/Box Sync/Teaching/UofMcourses/WILD562/Spring2019/Labs/Lab1_rintro/Lab1_data/Elevation2.tif") #resampled
disthumanaccess2<-raster("/Users/mark.hebblewhite/Box Sync/Teaching/UofMcourses/WILD562/Spring2019/Labs/Lab1_rintro/Lab1_data/DistFromHumanAccess2.tif") #resampled

## Creating a Raster Stack  
#all_rasters<-stack(deer_w, moose_w, elk_w, sheep_w, goat_w, wolf_w, elevation2, disthumanaccess2, landcover16)
#Error in compareRaster(x) : different extent

#Note the problem of different extents in using our landcover16 model BEFORE we reprojected it. In the hashed out text above I show the error you get if you used different extents in the raster list. 
all_rasters<-stack(deer_w, moose_w, elk_w, sheep_w, goat_w, wolf_w,elevation2, disthumanaccess2, disthhu2, landcover2)
names(all_rasters)

## Re-Running Lab 2 from Source; To learn more about sourcing from Script, 
?source
#We are going to have to make sure our working directory in the Lab2NeededforLab5.R file is correctly pointed to where we have the wolfyht.shp file (in Lab 4/new)

source("/Users/mark.hebblewhite/Box Sync/Teaching/UofMcourses/WILD562/Spring2019/Labs/lab4/new/Lab2NeededforLab4.R", verbose = FALSE)

rd.data<-wolfyht[wolfyht@data$Pack=="Red Deer",]
cov.outRD<-extract(all_rasters, rd.data) 

#Extract covariate values for available points
cov.availRD<-extract(all_rasters, rd.avail)

#Extract covariate values for Bow valley wolf data
bv.data<-wolfyht[wolfyht@data$Pack=="Bow valley",]
cov.outBV<-extract(all_rasters, bv.data)

#Extract covariate values for available points
cov.availBV<-extract(all_rasters, bv.avail)


## Attaching spatial X, Y data to Used and Available data (visualization, etc)
head(rd.data@coords)

## Red Deer Pack
cov.outRD2<-cbind(cov.outRD, coordinates(rd.data)[,1],coordinates(rd.data)[,2])
#set column names for coordinates
colnames(cov.outRD2)[11:12] <- c("EASTING","NORTHING")
#attach coordinates for red deer pack available points
cov.availRD2<-cbind(cov.availRD, coordinates(rd.avail)[,1],coordinates(rd.avail)[,2])
#set column names for coordinates
colnames(cov.availRD2)[11:12] <- c("EASTING","NORTHING")
#head(cov.outRD2)

#attach coorvinates for bow valley pack used points
cov.outBV2<-cbind(cov.outBV, coordinates(bv.data)[,1],coordinates(bv.data)[,2])
#set column names for coordinates
colnames(cov.outBV2)[11:12] <- c("EASTING","NORTHING")
#attach coobvinates for bow valley pack available points
cov.availBV2<-cbind(cov.availBV, coordinates(bv.avail)[,1],coordinates(bv.avail)[,2])
#set column names for coordinates
colnames(cov.availBV2)[11:12] <- c("EASTING","NORTHING")
#head(cov.outBV2)

## now lets combine the USED datasets again
## now lets combine them again
rdused <- as.data.frame(cov.outRD2)
rdused$pack <- c("Red Deer")
#str(rdused)

## repeat for Bow Valley pack
bvused <- as.data.frame(cov.outBV2)
bvused$pack <- c("Bow Valley")
#str(bvused)

## merge the two USED samples together
wolfused <- rbind(rdused, bvused)
## and for next week, lets add a new column for a 1=used 0 = avail
wolfused$used <- 1
#head(wolfused)

## now lets combine the AVAIL datasets again 
rdavail <- as.data.frame(cov.availRD2)
rdavail$pack <- c("Red Deer")
#str(rdavail)

## repeat for Bow Valley pack
bvavail <- as.data.frame(cov.availBV2)
bvavail$pack <- c("Bow Valley")
#str(bvavail)

## merge the two availability samples together
wolfavail <- rbind(rdavail, bvavail)
## and for next week, lets add a new column for a 1=used 0 = avail
wolfavail$used <- 0
#head(wolfavail)

# Finally, we will Merge the wolf used and avail samples together for the KDE availability estimator
wolfkde <- rbind(wolfused, wolfavail)
#str(wolfkde)
table(wolfkde$used, wolfkde$pack)
wolfkde$usedFactor <- factor(wolfkde$used, labels=c('0','1'))
write.table(wolfkde, file = "/Users/mark.hebblewhite/Box Sync/Teaching/UofMcourses/WILD562/Spring2019/Labs/lab4/new/wolfkde.csv", row.names=FALSE, na="", col.names=TRUE, sep=",")
wolfkde <- read.csv("/Users/mark.hebblewhite/Box Sync/Teaching/UofMcourses/WILD562/Spring2019/Labs/lab4/new/wolfkde.csv", sep=",")

#Now we can make a single nice plot of the X and Y locations by Used by Pack, where used locaitons are a bright blue, and available locations are dark blue. 

ggplot(wolfkde, aes(x=EASTING, y = NORTHING, color=usedFactor)) + geom_point() + stat_density2d() + facet_grid(pack ~ ., scales="free")

# or, Facetting by Used
ggplot(wolfkde, aes(x=EASTING, y = NORTHING)) + geom_point() + stat_density2d() + facet_grid(pack ~ usedFactor, scales="free")

####################################################################################
# Objective 2.0  Univariate Model-fitting 

#These are a repeat of the commands from lab 3, but here I will show you how to extract covariate tables from multiple models. Note in the script I also did this for Bv and rd wolf packs. We will just keep these handy for later, as today's focus is on categorical analysis of landcover. But, it might be handy to relate certain landcover types with certain prey (e.g., alpine and sheep/goats) for discussion. 

### First for all packs
elev <- glm(used ~ Elevation2, family=binomial(logit), data=wolfkde)
distacc <- glm(used ~ DistFromHumanAccess2, family=binomial(logit), data=wolfkde)
disthha <- glm(used ~ DistFromHighHumanAccess2, family=binomial(logit), data=wolfkde)
sheep <- glm(used ~ sheep_w2, family=binomial(logit), data=wolfkde)
goat <- glm(used ~ goat_w2, family=binomial(logit), data=wolfkde)
elk <- glm(used ~ elk_w2, family=binomial(logit), data=wolfkde)
moose <- glm(used ~ moose_w2, family=binomial(logit), data=wolfkde)
deer <- glm(used ~ deer_w2, family=binomial(logit), data=wolfkde)

# creating tables of B, SE
# First grab all of the estimates and standard errors
models = rbind(summary(elev)$coefficients[,1:2], summary(disthha)$coefficients[,1:2], summary(distacc)$coefficients[,1:2], summary(sheep)$coefficients[,1:2], summary(goat)$coefficients[,1:2], summary(elk)$coefficients[,1:2], summary(moose)$coefficients[,1:2], summary(deer)$coefficients[,1:2])
# Name your models
modelnames = c("elev","disthha", "distacc", "sheep", "goat", "elk", "moose", "deer")
# Now put all of your estimates in a pretty table with names that you'll remember!
  estimates.all = matrix(models, nrow=2*length(modelnames), ncol=2, dimnames = list(paste(rep(modelnames, each=2),c("intercept", "coefficient")), c("B", "SE")))
estimates.all

##################################################################################################
# Objective 3.0  Categorical Resource Selection Functions

#The first thing we need to do is add a column with the name habitat type (landcover) in it to help us keep track of what the different landcover codes mean.

levels(wolfkde$landcover16) ## see, all we have is landcover code

wolfkde$habitatType = ifelse(wolfkde$landcover16 == 0, "NA", 
                             ifelse(wolfkde$landcover16 == 1, "Open Conifer", 
                                    ifelse(wolfkde$landcover16 == 2, "Moderate Conifer", 
                                           ifelse(wolfkde$landcover16 == 3, "Closed Conifer", 
                                                  ifelse(wolfkde$landcover16 == 4, "Deciduous", 
                                                         ifelse(wolfkde$landcover16 == 5, "Mixed", 
                                                                ifelse(wolfkde$landcover16 == 6, "Regen", 
                                                                       ifelse(wolfkde$landcover16 == 7, "Herbaceous",                 
                                                                              ifelse(wolfkde$landcover16 == 8, "Shrub",                       
                                                                                     ifelse(wolfkde$landcover16 == 9, "Water", 
                                                                                            ifelse(wolfkde$landcover16 == 10, "Rock-Ice", 
                                                                                                   ifelse(wolfkde$landcover16 == 11, "Cloud", 
                                                                                                          ifelse(wolfkde$landcover16 == 12, "Burn-Forest",               
                                                                                                                 ifelse(wolfkde$landcover16 == 13, "Burn-Grassland", 
                                                                                                                        ifelse(wolfkde$landcover16 == 14, "Burn-Shrub", 
                                                                                                                               ifelse(wolfkde$landcover16 == 15, "Alpine Herb", "Alpine Shrub"))))))))))))))))

table(wolfkde$landcover16, wolfkde$used)

table(wolfkde$habitatType, wolfkde$usedFactor)
ggplot(wolfkde, aes(x=landcover16, y=..density.., fill = used)) +geom_histogram(binwidth = 1) + facet_grid(used~.)

#What should we do about the NA or Clouds? We will have to discuss what to do with NA's and Cloud? For now, we will decide to remove clouds as missing data

wolfkde2 <- wolfkde[wolfkde$landcover16 != 11, ]
wolfkde3 <-wolfkde2[wolfkde2$landcover16 != 0, ]
table(wolfkde3$habitatType, wolfkde3$usedFactor)
## see we have removed clouds and NA's


## Legend File
Next we will create a 'legend' file (names.m) to help us keep track of contrasts

names.m = data.frame(unique(wolfkde3$landcover16),unique(wolfkde3$habitatType))
# Now I put it order
names.m = names.m[order(names.m)[1:15],]
names.m
# Define a factor variable, landcov.f, # the sorted table makes defining the names of your factor level easy!

wolfkde3$landcov.f = factor(wolfkde3$landcover16,labels = names.m$unique.wolfkde3.habitatType)

#Note that there are many alternative ways of defining your landcover/habitattype 
# as a factor. This method seemed most explicit in terms of defining the design matrix
table(wolfkde3$landcov.f, wolfkde3$usedFactor)


## Univariate Selection Ratio's


table(wolfkde3$landcov.f, wolfkde3$usedFactor)
landcovSelection <- table(wolfkde3$landcov.f, wolfkde3$usedFactor)
landcovSelection2 <- as.data.frame.matrix(landcovSelection)
colnames(landcovSelection2)[1:2] <- c("avail","used")
landcovSelection2$selection <- landcovSelection2$used / landcovSelection2$avail
## LEts take a look, this is the selection ratio, the ratio of the proportion used to the prortion available from our first equation above. 
landcovSelection2$selection
## Next we take the natural logarithm, ln() which in R is represented by log()
landcovSelection2$lnSelection <- log(landcovSelection2$selection)
landcovSelection2
## lets make a new field called landcover Type
landcovSelection2$landcoverType <- unique(wolfkde3$landcov.f)
## lets make a plot of the Manly Selectivity Coefficients
plot(unique(wolfkde3$landcov.f), landcovSelection2$lnSelection, las=2)

## it might be handy to save this

write.table(landcovSelection2, "wolfselection.csv", sep=",", row.names = TRUE, col.names=TRUE)
str(landcovSelection2)

### How do the selection ratio and Selectivity coefficient relate to each other??? Lets make ggplots of the Selectivity and Ln Selectivity Coefficients
                                                                 
## Selection ratio
ggplot(landcovSelection2, aes(x=landcoverType, y = selection)) + geom_bar(stat="Identity") + theme(axis.text.x = element_text(angle = 90))

## Ln-Selection Ratio
ggplot(landcovSelection2, aes(x=landcoverType, y = lnSelection)) + geom_bar(stat="Identity") + theme(axis.text.x = element_text(angle = 90))#

# What is the relationship between the selection ratio and the Ln-Selection Ratio?
## Fancier ggplot
ggplot(landcovSelection2, aes(x=selection, y = lnSelection)) + stat_smooth()
                                                                    
# Discussion: why is the relationship between the seleciton ratio and the Ln selection ration curvilinear like this?
                                                                    
# Selection Ratio's in adehabitatHS


## Estimated available proportions on design I data
elk.avail <- c(15, 61, 84, 40)
elk.used <- c(3, 90, 181, 51)
names(elk.used) <- c("0%", "1-25%", "26-75%", ">75%")
names(elk.avail) <- names(elk.used)
## Computation of wi
(wiRatio <- widesI(elk.used, elk.avail, avknown=FALSE))

## plot the values of the selection ratios
plot(wiRatio)

################################################################################################
# Objective 4.  Categorical Logistic Regression

#Lastly, we will learn about analyzing categorical variables using a new approach compared to categories. To learn more about how R uses contrasts to set the design matrix in any linear model search for help on contrast matrices

?contrast

#Even though this may seem 'new' if you have ever done ANOVA (a linear model) in R, you have used contrast matrices to do so.  

contrasts(wolfkde3$landcov.f) = contr.treatment(15) ## note here also that in my case I had 15 landcover types
# To see the design matrix assigned
attributes(wolfkde3$landcov.f)
levels(wolfkde3$landcov.f)


#Note that while we have cleaned up the clouds and NA's, what should we do about Burned-Grassland, Burned-Herbaceous and Burn-Forests? Recall that these 3 landcover types had some categories with 0 observed wolf uses in them, so we coudl reclassify them all as 'burned'. We will return to this in a minute. Here, checking above, we see that 11, 12, and 13 are all burns. 

levels(wolfkde3$landcov.f)[11:13] = "Burn"
                                                                    
## note this then reduces us from 15 to 13 categories
contrasts(wolfkde3$landcov.f) = contr.treatment(13)
attributes(wolfkde3$landcov.f)

#Note how the design matrix has collapsed burn into one category? What other categories should we consider? Perhaps Alpine? 
                                                                    
                                                                    
## Incorrectly Treating Landcover as Continuous
                                                                    
#First, we will use Logistic regression incorectly analyzing treating landcover16 as a continuous covariate
naive.nf = glm(used~landcover16,data=wolfkde3, family=binomial(logit))
summary(naive.nf)
 
oc = glm(used~I(landcov.f=="Open Conifer"),data=wolfkde3, family = binomial(logit))
summary(oc)
str(summary(oc))

#Now lets manually evaluate the predicted probability of a wolf used location occuring in Open Conifer
exp(-1.622+0.711*1)/(1+exp(-1.622+0.711*1))
## now compare to the probability of wolf use in non-conifer landcovers ?
                                                                    
exp(-1.622+0.711*0)/(1+exp(-1.622+0.711*0))
                                                                    

#################################################################################################
############Multiple Logistic Regression example with multiple categories
                                                                  
## with just open conifer and burns
                                                                    
ocb = glm(used~I(landcov.f=="Open Conifer")+I(landcov.f=="Burn"), data = wolfkde3, family = binomial(logit))
summary(ocb)
                                                                    
### and with a few more variables
conif = glm(used~I(landcov.f=="Open Conifer")+I(landcov.f=="Moderate Conifer")+I(landcov.f=="Closed Conifer"), data = wolfkde3, family = binomial(logit))
summary(conif)
 
##  Full model with all categories considered
# Full model
                                                                 
full = glm(used~I(landcov.f), data=wolfkde3, family = binomial(logit))
summary(full)

#Discussion: What is the intercept? Where did alpine (landcover 15) go? Why did landcover types 4 (decid), 6 (regen) and alpine- herb (12) 'blow' up? Go back and look at this table to undestand
                                                         
                                                                 
table(wolfkde3$landcov.f, wolfkde3$usedFactor)
#They blew up because there was 0 used observed.  See what its trying to estimate?
 
exp(-0.974 - 15.592*1)/(1+exp(-0.974 - 15.592*1)) 
## these are the intercept and coefficient for deciduous

## Models Without an Intercept

full.NoInt = glm(used~I(landcov.f) -1, data=wolfkde3, family = binomial(logit))
summary(full.NoInt)

full.model = glm(used~I(landcov.f=="Moderate Conifer")+I(landcov.f=="Closed Conifer") +I(landcov.f=="Deciduous")+I(landcov.f=="Mixed")+I(landcov.f=="Herbaceous")+I(landcov.f=="Regen")+I(landcov.f=="Shrub")+I(landcov.f=="Water")+I(landcov.f=="Rock-Ice") +I(landcov.f=="Burn")+I(landcov.f=="Alpine Herb")+I(landcov.f=="Alpine Shrub"), data = wolfkde3, family = binomial(logit))
summary(full.model)

########################################################################
# Objective 5. Changing the Reference Category

## first recheck which # Rock-Ice is
levels(wolfkde3$landcov.f) ## Ok it is # 10

contrasts(wolfkde3$landcov.f) = contr.treatment(13, base = 10)
attributes(wolfkde3$landcov.f)
# and note that rock-ice now is 0. 

rockintercept.model = glm(used~I(landcov.f=="Moderate Conifer") +I(landcov.f=="Closed Conifer") +I(landcov.f=="Deciduous")+I(landcov.f=="Mixed")+I(landcov.f=="Herbaceous") +I(landcov.f=="Regen")+I(landcov.f=="Shrub")+I(landcov.f=="Water")+I(landcov.f=="Open Conifer")+I(landcov.f=="Burn")+I(landcov.f=="Alpine Herb")+I(landcov.f=="Alpine Shrub"), data = wolfkde3, family = binomial(logit))
summary(rockintercept.model)
#Discussion:_Now compare coefficients from each model with open conifer vs. Rock and Ice as the intercept models? What has changed? For an excercise, chose other reference categories on your own?
  
  ## Manual Dummy (Indicator) Coding
wolfkde3$closedConif = ifelse(wolfkde3$habitatType == "Closed Conifer", 1, 0)
wolfkde3$modConif = ifelse(wolfkde3$habitatType == "Moderate Conifer", 1, 0)
wolfkde3$openConif = ifelse(wolfkde3$habitatType == "Open Conifer", 1, 0)
wolfkde3$decid = ifelse(wolfkde3$habitatType == "Deciduous", 1, 0)
wolfkde3$regen = ifelse(wolfkde3$habitatType == "Regen", 1, 0)
wolfkde3$mixed = ifelse(wolfkde3$habitatType == "Mixed", 1, 0)
wolfkde3$herb = ifelse(wolfkde3$habitatType == "Herbaceous", 1, 0)
wolfkde3$shrub = ifelse(wolfkde3$habitatType == "Shrub", 1, 0)
wolfkde3$water = ifelse(wolfkde3$habitatType == "Water", 1, 0)
wolfkde3$rockIce = ifelse(wolfkde3$habitatType == "Rock-Ice", 1, 0)
## note here I reclassified all burn = 1 
wolfkde3$burn = ifelse(wolfkde3$habitatType == "Burn-Grassland", 1, ifelse(wolfkde3$habitatType == "Burn-Shrub", 1, ifelse(wolfkde3$habitatType == "Burn-Forest", 1,0 )))
wolfkde3$alpineHerb = ifelse(wolfkde3$habitatType == "Alpine Herb", 1, 0)
wolfkde3$alpineShrub = ifelse(wolfkde3$habitatType == "Alpine Shrub", 1, 0)

head(wolfkde3)
# manually adding alpine together. 
wolfkde3$alpine = wolfkde3$alpineHerb + wolfkde3$alpineShrub

#Refitting model with Open Conifer as the intercept and alpine/burn pooled
oc.intercept.model = glm(used~closedConif + modConif + decid+ regen+mixed+herb+water+rockIce+burn+alpine, data = wolfkde3, family = binomial(logit))
summary(oc.intercept.model)

### refitting model with just Alpine and Rock and Ice as the intercept
rockintercept.alpine.model = glm(used~closedConif + openConif + modConif + decid+ regen+mixed+herb+water+burn+alpine, data = wolfkde3, family = binomial(logit))
summary(rockintercept.alpine.model)

### refitting model manually dropping Decid and Regen - where do they no go?
rock.alpine.regen.decid.intercept.model = glm(used~closedConif + openConif + modConif + mixed+herb+water+burn+alpine, data = wolfkde3, family = binomial(logit))
summary(rock.alpine.regen.decid.intercept.model)


## Comparing coefficients from two different models with different intercepts
# I adopt the code from section 2.0 above to pull out all the coefficients and SE's and put them in one long table

                                                                    
rockintercept.alpine.model.df <- data.frame(summary(rockintercept.alpine.model)$coefficients[,1:2])

oc.intercept.model.df <- data.frame(summary(oc.intercept.model)$coefficients[,1:2])
                                                                   
coef.table <- rbind(rockintercept.alpine.model.df,oc.intercept.model.df)
                                                                    
coef.table$habitatType <- c(row.names((summary(rockintercept.alpine.model)$coefficients[,1:2])),row.names(summary(oc.intercept.model)$coefficients[,1:2]))
                                                                    
coef.table$habitatType[1] <- "rockIce"
                                                                    
coef.table$habitatType[12] <- "openConif"
                                                                   
 coef.table$model <-c(rep("Open Conif Intercept",11),rep( "RockIce Intercept",11))
                                                                    
 coef.table
                                                                
#Now use this table to compare the ABSOLUTE differences say between burn and alpine in both models. In the oc.model the B coefficient for Burn = 0.29 and Alpine = -3.002 an absolute differences of 3.29. In the rock.model the B coefficient for Burn = 2.2 and Alpine is -1.086, an absolute difference of 3.29 - the same! Why is this?

#Now lets make a figure of the Beta coefficients (you can figure out how to add SE's yourself :)
ggplot(coef.table, aes(x=habitatType, y=Estimate, colour=model)) + geom_point(size = 5) + theme(axis.text.x = element_text(angle = 90))

#This figure tells us that RELATIVELY nothing has changed, only where the coefficients are relative to the yAxis

# # Homework 4
# 
# LAB 4 – ASSIGNMENT 
# DUE IN CLASS Tuesday February 12th, 2019
# 
# Answer the following questions in your lab report, feeling free to use TABLES and FIGURES properly labeled and in JWM format.  Feel free to re-use (revised If I made suggestions) introductions, study areas, basic methods from previous labs, and focus just on the goal of summarizing what the best statistical model for categorical landcover selection is. 
# 
# 1.	What would be you’re a-priori hypothesis for this lab? In other words, based on previous literature, and previous analyses you’ve done, what landcover types would you expect wolves to select? In the discussion, answer whether the results of your study compare broadly well to previous studies (Oakleaf et al. 2006, Hebblewhite et al. 2005 – OIKOS), etc.??  How do you relate selection for landcover to previous results of selection for habitat suitability indices for ungulates? Report differences in landcover between packs as well .
# 
# 2.	What is your ‘best’ model for categorical habitat types? What are the effects of landcover type on wolves? Be sure to write the linear part of the logistic regression formula for the top model in the results, and for this lab, present the selection data in both tabular and categorical form.  
# 
# 3.	Discuss what the effects of changing the reference category in your RSF say, from closed conifer to rock and ice, does to your results? As an appendix, present a figure and table of the results from one other analysis with a difference reference category.
# 
# 4. What is the relationship between the selection ratio, the ln(selection ratio), and the beta coefficients estimated from a logistic regression equation? 
#   