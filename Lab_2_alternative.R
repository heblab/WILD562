#' ---
#' title: "WILD 562 Lab 2: Alternative methods"
#' author: "Team Hebblewhite"
#' date: "22 January 2019"
#' ---
#'   
## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, cache=TRUE)
knitr::opts_knit$set(root.dir = 'D:/Lab1_data')

#' 
## ---- echo=F-------------------------------------------------------------
setwd("D:/Lab1_data/")

#' 
#' # Installing and loading packages
#' 
#' Here's a custom function that will install packages we need for today's lab that you don't already have and load 
#' them all into your global enviroment.
## ---- warning = FALSE, message=F, results=F------------------------------
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("ks", "lattice", "plotrix", "adehabitatHR", "maptools", "foreign", "rgdal", "sp", "raster", "ggplot2", 
              "colorRamps", "rgeos", "sf", "tidyverse", "fasterize", "cowplot")

ipak(packages)

#' 
#' 
#' # Quick intro to `sf` package 
#' 
#' The `sf` (simple features) package is a new package that is still being developed, but it has many advantages over 
#' `sp` and is meant to eventually replace `sp`. Two big advantages are that `sf` objects have far simpler structures 
#' than `sp` objects and are compatible with the `tidyverse`, so you can use `dplyr` functions with them.
#' 
#' If you import shapefiles with the `read_sf` function and view them you will see a `tibble`. 
## ------------------------------------------------------------------------
elc_habitat_sf <- read_sf("elc_habitat.shp")
wolfyht_sf <- read_sf("wolfyht.shp")
humanaccess_sf <- read_sf("humanacess.shp")

wolfyht_sf

#' 
#' If we only print the last few columns, you can see that the spatial data is just stored in the `geometry` column, 
#' which is the case for all `sf` objects, regardless of whether they are lines, points or polygons.
## ------------------------------------------------------------------------
print(wolfyht_sf[18:22], n=8)

#' 
#' Plotting an `sf` object is a bit different than in `sp`. The default is to plot features colored by *every* column. 
#' If you only want to see the spatial geometry itself, use `st_geometry` within the call. We can also quickly make a 
#' plot that is automatically colored by a field we're interested in, such as *pack*. The `st_` prefix stands for "spatial 
#' temporal".
## ------------------------------------------------------------------------
plot(wolfyht_sf)
plot(st_geometry(wolfyht_sf), axes=T)
plot(wolfyht_sf["Pack"], key.pos=1, axes=T)

#' 
#' You probably noticed that two packs have very different spatial distributions. This is because they inhabit very 
#' different landscapes:
## ----echo=FALSE, fig.cap="*The Bow Valley of Banff National Park - Bow Valley Wolf Pack home range", out.width = '75%'----
knitr::include_graphics("D:/Lab1_data/BowValleyImage.JPG")

#' 
## ----echo=FALSE, fig.cap="The Ya Ha Tinda - Red Deer pack home range", out.width = '75%'----
knitr::include_graphics("D:/Lab1_data/YaHaTinda.JPG")

#' 
#' Because `sf` plays nicely with the `tidyverse`, you can easily use it with `ggplot2`. This plot does take a while because 
#' the `elc_habitat_sf` layer is very large. Here, we color the landcover layer by the elk HSI. The blue represents water.
## ------------------------------------------------------------------------
ggplot() + geom_sf(data=elc_habitat_sf, aes(fill = as.factor(ELK_W)), color=NA) +
  scale_fill_manual(name="ELK HSI", values=c("black", "gray40", "gray80", "yellow", "orange", "red", "darkblue")) +
  theme_bw()

#' 
#' # OBJECTIVE 1: Creating raster layers
#' We can create a mask raster to use as a template for converting shapefile data to rasters.
#' 
#' First you'll want to check the spatial extents of the elc_habitat and humanaccess layers using `st_bbox`. Then you can 
#' specify an extent that covers both of them. You can also set the resolution (in meters) and match the coordinate 
#' reference system to that of `elc_habitat_sf`. 
## ------------------------------------------------------------------------
st_bbox(elc_habitat_sf)
st_bbox(humanaccess_sf)

mask.raster <- raster(xmn=443680.6, xmx=650430.4, ymn=5618405, ymx=5789236, res=30, 
                      crs= st_crs(elc_habitat_sf)$proj4string)

#' 
#' 
#' Because the `rasterize` function in the `raster` package is ridiculously slow, we can use a more efficient function 
#' with a very apt name: `fasterize`. It's the only function in the `fasterize` package. Wow, it's so *FAST*!
## ------------------------------------------------------------------------
deer_w <- fasterize(elc_habitat_sf, mask.raster, field = "DEER_W")
moose_w <- fasterize(elc_habitat_sf, mask.raster, field = "MOOSE_W")
elk_w <- fasterize(elc_habitat_sf, mask.raster, field = "ELK_W")
sheep_w <- fasterize(elc_habitat_sf, mask.raster, field = "SHEEP_W")
goat_w <- fasterize(elc_habitat_sf, mask.raster, field = "GOAT_W")
wolf_w <- fasterize(elc_habitat_sf, mask.raster, field = "WOLF_W")

plot(wolf_w)

#' 
#' Resampling in R is also *very* slow, so using ArcGIS or QGIS might be a good call. But here are examples of the code 
#' to resample and write the rasters to memory for future use in case you want a fully replicable workflow:
#' 
#' `elevation2 <-resample(elevation, mask.raster, method="bilinear")`
#' `writeRaster(deer_w, "deer_w2.tiff", "GTiff")`
#' 
#' ## Create distance to human access layer 
#' Mark already created this raster layer. Again, this took days in R on a desktop. Might want to use "Euclidean distance" 
#' tool in ArcGIS instead, because it's way faster.
#' 
## ------------------------------------------------------------------------
dist.raster <- raster(ext=extent(humanaccess_sf), res=30, 
                      crs= st_crs(humanaccess_sf)$proj4string)

#' Unfortunately, our new favorite favorite function, `fasterize`, only works for polygons and not for lines. So we have 
#' to use slow `rasterize` function instead, setting human features (e.g., roads, trails) to 1. This takes a long time so 
#' we won't do it in lab, but the raster is already created in our data folder. We've included the code below but don't 
#' run it.
#' 
#' `human.raster <- rasterize(humanaccess_sf, dist.raster, 1)`
#' 
#' Calculate distance to human access
#' `accessdist <-system.time(distance(human.raster))`
#' 
#' Write raster to file
#' `writeRaster(accessdist, "DistFromHumanAccess.tiff", "GTiff")`
#' 
#' ## Create distance to HIGH human access
#' The `SUM_CLASS` field in `humanaccess_sf` has information on the different classes of human access (e.g., low, 
#' moderate, high) but there's some inconsistencies in the naming
#' 
#' The nice thing about `sf` is that the `tibble` shows us the class of each field (or column). We can see that the 
#' `SUM_CLASS` field is a character field, and has 10 unique values. 
## ------------------------------------------------------------------------
unique(humanaccess_sf$SUM_CLASS)

#' 
#' There are a lot of ways to rename these fields, but we use a series of `ifelse` statements:
## ------------------------------------------------------------------------
humanaccess_sf$SUM_CLASS <-
  ifelse(humanaccess_sf$SUM_CLASS %in% c("NIL", "Nil", "0", "NA"), "NIL",
         ifelse(humanaccess_sf$SUM_CLASS %in% c("Low", "LOW"), "LOW",
                ifelse(humanaccess_sf$SUM_CLASS %in% c("MEDIUM", "Moderate"), "MODERATE",
                       ifelse(humanaccess_sf$SUM_CLASS %in% c("High", "HIGH"), "HIGH", "VERY HIGH"))))

#' 
#' If we wanted to make a plot with these levels of human access we should also change this field to a factor and 
#' specify the order of the levels:
## ------------------------------------------------------------------------
humanaccess_sf$SUM_CLASS <- factor(humanaccess_sf$SUM_CLASS,
                                   levels = c("NIL", "LOW", "MODERATE", "HIGH", "VERY HIGH"))
levels(humanaccess_sf$SUM_CLASS)


#' 
#' Create a new layer that only has the high human access lines. Both of these code lines produce the same result. 
#' The first subsets the data using base `R` while the second uses `dplyr::filter`. Pick your poison:
## ------------------------------------------------------------------------
highaccess_sf <- humanaccess_sf[humanaccess_sf$SUM_CLASS=="HIGH" | humanaccess_sf$SUM_CLASS=="VERY HIGH", ]
highaccess_sf <- filter(humanaccess_sf, SUM_CLASS %in% c("HIGH", "VERY HIGH"))

#' 
#' Let's see what the high access layer looks like compared to all the human access features:
## ------------------------------------------------------------------------
plot(st_geometry(humanaccess_sf), axes=T)
plot(st_geometry(highaccess_sf), col="red", add=TRUE, axes=T)

#' 
#' Now you could use the `rasterize` function or rasterize it in ArcMap or QGIS. To save time, we'll just load the 
#' one Mark made and resampled to match the extent and resolution of the other covariate layers: 
## ------------------------------------------------------------------------
disthhu2 <- raster("DistFromHighHumanAccess2.tif") 
plot(disthhu2)

#' 
#' # OBJECTIVE 2: Creating home ranges
#' ## Minimum convex polygons by individual
#' We use the `adehabitatHR` package for calculating minimum convex polygons and kernel density estimate utilization 
#' distributions. This package was written well before `sf` was available so its functions rely on `sp` objects as 
#' inputs. Specifically, `adehabitatHR` requires either a `SpatialPoints` object or a `SpatialPolygonsDataFrame` 
#' (with only one column, for animal id). We will quickly convert our `sf` objects to `sp` objects.
#' 
#' First, we need at least 5 locations per animal to make an MCP. Let's convert the field with our animal names to a 
#' `factor` and then see how many locations we have per animal:
## ------------------------------------------------------------------------
table(wolfyht_sf$NAME)

#' 
#' Only keep animals with at least five locations. Here, we're using the pipe symbol -- originally from the `magrittr` 
#' package but now used in `dplyr` and the entire `tidyverse`. It lets us call a series of functions, passing forward 
#' the output from one step to the next.
## ------------------------------------------------------------------------
wolfyht_sf_5 <-
  wolfyht_sf %>% 
  group_by(NAME) %>% 
  filter(n() >= 5) %>% 
  ungroup()

#' 
#' Start with Red Deer Pack:
## ------------------------------------------------------------------------
rd_sf <- filter(wolfyht_sf_5, Pack=="Red Deer")

#' 
#' Convert `sf` object to a `SpatialPolygonsDataFrame`:
## ------------------------------------------------------------------------
rd_spdf <- as(rd_sf, "Spatial")

#' 
#' Only select the "NAME" column for animal id (requirement of `mcp` function):
## ------------------------------------------------------------------------
rd_spdf <- rd_spdf[,"NAME"]
mcp_rd <- mcp(rd_spdf, percent = 99)  

#' 
#' We can plot each MCP (with transparency because they overlap), and then overlay the points with matching colors. 
#' You'll notice the red polygon appears orange because of the overlap with the green polygon. We have to change the 
#' "NAME" field to a factor to display the colors by NAME.
## ------------------------------------------------------------------------
plot(mcp_rd[mcp_rd$id=="70",], col=alpha("green", 0.4), axes=T)
plot(mcp_rd[mcp_rd$id=="60",], col=alpha("red", 0.4), add=T)
plot(mcp_rd[mcp_rd$id=="42",], col=alpha("blue", 0.4), add=T)
plot(rd_spdf, col=c("blue", "red", "green")[as.factor(rd_spdf$NAME)], add=T)

#' 
#' We can check the area for all three Red Deer wolves 
## ------------------------------------------------------------------------
as.data.frame(mcp_rd)

#' 
#' We can also calculate and plot the area for different percents of the MCP by using the `mcp.area` function. For 
#' example, the the 50% MCP would include the core 50% of locations.
## ------------------------------------------------------------------------
mcp.area(rd_spdf, percent=seq(50, 100, by=5))

#' 
#' ## Utilization distributions (UDs) by individual
#' Below we create and plot kernel density estimates (KDE) for all three Red Deer wolves. By default, this function 
#' uses the "href" smoothing parameter and the reference grid. For more information on smoothing parameters and grid 
#' sizes, check out [this paper](https://animalbiotelemetry.biomedcentral.com/articles/10.1186/s40317-015-0051-x).
## ------------------------------------------------------------------------
red.deerUD <- kernelUD(rd_spdf, grid=30, extent=0.5, same4all=TRUE) 
image(red.deerUD)

#' 
#' And then we can create and polygons from these UDs, which are stored as a `SpatialPolygonsDataFrame` from `sp`
## ------------------------------------------------------------------------
homerangeRD <- getverticeshr(red.deerUD)
as.data.frame(homerangeRD)
class(homerangeRD)
plot(homerangeRD, col=alpha(c("blue", "red", "green"), 0.4), axes=T)

#' 
#' Now we can repeat the whole process for the Bow Valley pack. Instead, we will move on from creating individual 
#' animal MCPs and UDs to creating pack-level MCPs and UDs.
## ------------------------------------------------------------------------
all_spdf <- as(wolfyht_sf_5, "Spatial")
all_spdf <- all_spdf[,"Pack"]

#' 
#' ## Creating MCPs and KDEs by pack
## ------------------------------------------------------------------------
cp_all <- mcp(all_spdf, percent=99)

#' 
#' Plotting the points first is a quick way to make sure we capture the entire spatial extent in the plot:
## ------------------------------------------------------------------------
plot(all_spdf, col="black", axes=T)
plot(cp_all[cp_all$id=="Bow valley",], col=alpha("steelblue", .4), add=TRUE)
plot(cp_all[cp_all$id=="Red Deer",], col=alpha("lightcoral", .4), add=TRUE)
plot(all_spdf, col=c("steelblue", "lightcoral")[as.factor(all_spdf$Pack)], add=TRUE)

#' 
#' Check area for each wolf pack:
## ------------------------------------------------------------------------
as.data.frame(cp_all)

#' 
#' Calculate area for different percents of MCP:
## ------------------------------------------------------------------------
mcp.area(all_spdf, percent=seq(50, 100, by=5))

#' 
#' 
#' Calculate 99% KDE for both wolf packs:
## ------------------------------------------------------------------------
allUD <- kernelUD(all_spdf, grid=30, extent=0.5, same4all=TRUE)
image(allUD)

#' 
#' Create polygons for home ranges and plot them.
## ------------------------------------------------------------------------
homerangeALL <- getverticeshr(allUD)
as.data.frame(homerangeALL)
class(homerangeALL)
plot(homerangeALL, col=c("steelblue", "lightcoral"), axes=T)

#' 
#' 
#' Estimate UD in raster mode.
## ------------------------------------------------------------------------
allUD_raster <- getvolumeUD(allUD) 
allUD_raster

#' 
#' Set up graphical parameters for plotting the UD of the Bow Valley pack (the first in the list).
## ------------------------------------------------------------------------
par(mar=c(0,0,2,0)) #set margin
image(allUD_raster[[1]]) 
title("Bow Valley UD with density contours") 
xyzv <- as.image.SpatialGridDataFrame(allUD_raster[[1]]) 
contour(xyzv, add=TRUE)

#' 
#' 
#' # OBJECTIVE 3: Sample availabiliity within home ranges 
#' 
#' Subset polygons by wolf pack.
## ------------------------------------------------------------------------
rd_poly <- subset(homerangeALL, id=="Red Deer")
bv_poly <- subset(homerangeALL, id=="Bow valley")

#' 
#' Generate 1000 points from Red Deer wolf pack KDE polygon.
## ------------------------------------------------------------------------
set.seed(55)
rd_avail <- spsample(rd_poly, 1000, "random")
plot(rd_avail, axes=T)

#' 
#' Calculate distances between randomly sampled points (output is a matrix of distances) and make sure none are 
#' less than 30 m apart to avoid sampling the same raster pixels more than once.  
## ------------------------------------------------------------------------
dists_rd <- spDists(rd_avail)
dists_rd <- ifelse(dists_rd==0, NA, dists_rd)
which(dists_rd <= 30, arr.ind = T)

#' 
#' Generate 1000 points from Red Deer wolf pack KDE polygon. In the Bow Valley pack, there are 8 sets of 
#' random points that are within 30 m of eachother. the `to_delete` matrix gives us the indexes of each the points
#' we want to delete so we don't sample the same pixel twice. 
## ------------------------------------------------------------------------
set.seed(55)
bv_avail <- spsample(bv_poly, 1000, "random")
plot(bv_avail, axes=T)
dists_bv <- spDists(bv_avail)
dists_bv <- ifelse(dists_bv==0, NA, dists_bv)
to_delete <- which(dists_bv <= 30, arr.ind = T)
to_delete 

#' 
#' Now we delete those points from the `SpatialPoints` object. `bv_avail` now has 992 (8 fewer) points. 
## ------------------------------------------------------------------------
bv_avail <- bv_avail[-to_delete[,1],]
bv_avail

#' 
#' We can check to make sure that it worked:
## ------------------------------------------------------------------------
dists_bv <- spDists(bv_avail)
dists_bv <- ifelse(dists_bv==0, NA, dists_bv)
which(dists_bv <= 30, arr.ind = T)

#' 
#' 
#' Let's plot them all together, used points and home-range level availability:
## ------------------------------------------------------------------------
plot(all_spdf, col=c("steelblue","lightcoral")[as.factor(all_spdf$Pack)], axes=T, pch=19)

plot(bv_avail, col=alpha("steelblue", 0.1), pch=1, add=T)
plot(rd_avail, col=alpha("lightcoral", 0.1), pch=1, add=T)

legend(595000,5700000, unique(all_spdf$Pack), col=c("steelblue", "lightcoral"), pch=19)

#' 
#' # OBJECTIVE 4: Extracting GIS covariates for point location data
#' 
#' Note the raster layers we created earlier: deer_w, moose_w, elk_w, sheep_w, goat_w, wolf_w, elevation2, 
#' disthumanaccess2. Below we show how to calculate distances between different features. These calculations take a 
#' long time, so we won't run this code. We are using the `gDistance` function from the `rgeos` package.
#' 
#' Distance between human access and used points for Bow Valley.
#' `dist.outBV <- gDistance(bv.data, humanaccess, byid=TRUE)`
#' 
#' Note that the above function calculates the distance between each point and all road segments, but we just want 
#' the minimum value.
#' `dist.outBV2 <- data.frame(apply(dist.outBV, 2, min))`
#' `names(dist.outBV2) <- "Dist.to.access"`
#' 
#' Distance between human access and used points for Red Deer.
#' `dist.outRD <- gDistance(rd.data, humanaccess, byid=TRUE)`
#' `dist.outRD2 <- data.frame(apply(dist.outRD,2,min))`
#' `names(dist.outRD2) <- "Dist.to.access"`
#' 
#' Distance between human access and available points for Bow Valley.
#' `dist.availBV <- gDistance(bv.avail, humanaccess, byid=TRUE)`
#' `dist.availBV2 <- data.frame(apply(dist.availBV,2,min))`
#' `names(dist.availBV2) <- "Dist.to.access"`
#' 
#' Distances betwee human access and available points for Red Deer
#' `dist.availRD <- gDistance(rd.avail, humanaccess, byid=TRUE)`
#' `dist.availRD2 <- data.frame(apply(dist.availRD,2,min))`
#' `names(dist.availRD2) <- "Dist.to.access"`
#' 
#' 
#' ## Extracting spatial covariates
#' We can extract the spatial covariates at used (and available) points using the `extract` function in the 
#' `raster package`. The fastest way to do this is to create a `RasterStack` of all the raster layers. To do this, 
#' all rasters must have the same coordinate reference system, spatial resolution, and spatial extent. 
#' 
#' First let's load in a few resampled rasters.
## ------------------------------------------------------------------------
elevation2 <- raster("Elevation2.tif") 
disthumanaccess2 <- raster("DistFromHumanAccess2.tif") 

#' 
#' Then we can stack them. Since we never wrote the HSI rasters (deer, elk, etc.) to a file, they don't have 
#' meaningful layer names.
## ------------------------------------------------------------------------
all_rasters <- stack(deer_w, moose_w, elk_w, sheep_w, goat_w, wolf_w, elevation2, disthumanaccess2, disthhu2)
names(all_rasters)
names(all_rasters) <- c("deer_w2", "moose_w2", "elk_w2", "sheep_w2", "goat_w2", "wolf_w2", "Elevation2", 
                        "DistFromHumanAccess2", "DistFromHighHumanAccess2")

#' 
#' Extract covariate values for Red Deer wolf data  
## ------------------------------------------------------------------------
cov_RD <- raster::extract(all_rasters, rd_spdf)
head(cov_RD)

#' 
#' 
## ------------------------------------------------------------------------
bv_sf <- filter(wolfyht_sf_5, Pack=="Bow valley")

#' 
#' Convert `sf` object to a `SpatialPolygonsDataFrame`
## ------------------------------------------------------------------------
bv_spdf <- as(bv_sf, "Spatial")
bv_spdf <- bv_spdf[,"NAME"]

#' 
#' Extract covariate values for Bow valley wolf data  
## ------------------------------------------------------------------------
cov_BV <- raster::extract(all_rasters, bv_spdf)

#' 
#' We could also do this for available points, but today we're just focused on habitat *use*.
#' 
#' # OBJECTIVE 5: Exploratory analyses of wolf habitat use
#' 
#' Today we will focus on some exploratory analyses of JUST use of habitat covariates by the two wolf packs. The 
#' first step is to merge the cov_RD and cov_BV dataframes with a new field for pack. But before that we need to 
#' convert the matrix to a data frame, and add a new column for pack name.
## ------------------------------------------------------------------------
rd_used <- as_tibble(cov_RD)
rd_used$pack <- c("Red Deer")

#' 
## ------------------------------------------------------------------------
bv_used <- as_tibble(cov_BV)
bv_used$pack <- c("Bow Valley")

#' 
## ------------------------------------------------------------------------
wolf_used <- bind_rows(rd_used, bv_used)
wolf_used

#' 
#' Here we're using `read_csv` rather than `read.csv` because it doesn't convert `character` data to a `factor` like 
#' read.csv does, so you don't have to write `stringsAsFactors = F`. The `read_csv` and `write_csv` functions are in 
#' the `readr` package, which is part of the `tidyverse`.
## ---- message=F----------------------------------------------------------
write_csv(wolf_used, "wolf_used.csv")
wolf_used <- read_csv("wolf_used.csv")

#' 
#' Calculate some quick summaries
## ------------------------------------------------------------------------
summary(wolf_used)

#' 
#' Missing data are a problem. Let's check how many rows have NA values (in any field) before we do anything. Then we 
#' can count the total number of rows in the dataset.
## ------------------------------------------------------------------------
sum(!complete.cases(wolf_used))
nrow(wolf_used)

#' Looks like there are 16 rows with NAs out of 403.
#' 
#' For this example, we'll remove all rows with missing data. Another option would be to replace all NA values with 
#' another quantity such as 0; however, if we replace a distance metric with 0 this could lead to erroneous results. 
#' Here's some code to replace NAs with zeros: `wolf_used <- replace(wolf_used, is.na(wolf_used), 0)`. The `drop_na` 
#' function is in `tidyr`.
## ------------------------------------------------------------------------
wolf_used <- drop_na(wolf_used)
nrow(wolf_used)

#' Looks like we dropped 16 rows, which is what we wanted.
#' 
#' Using `dplyr`, we can group our data by pack and then calculate whatever summary stats we want. The last line just 
#' makes sure it displays all columns in the output. If you wanted standard deviations or medians instead of means, 
#' you could use `sd` or `median` here.
## ------------------------------------------------------------------------
wolf_used %>% 
  group_by(pack) %>% 
  summarize(
    n=n(),
    mean_elevation = mean(Elevation2),
    mean_dist_human = mean(DistFromHumanAccess2),
    mean_deer = mean(deer_w2),
    mean_elk = mean(elk_w2),
    mean_moose = mean(moose_w2),
    mean_sheep = mean(sheep_w2),
    mean_goat = mean(goat_w2)) %>% 
  print(., width=Inf)

#' 
#' 
#' ## Visualizing wolf use
#' 
#' ### Histograms
#' Here are some histograms of wolf use (these figure pool both packs together) by prey HSI:
## ------------------------------------------------------------------------
par(mfrow = c(2,3))
hist(wolf_used$deer_w2)
hist(wolf_used$elk_w2)
hist(wolf_used$moose_w2)
hist(wolf_used$sheep_w2)
hist(wolf_used$goat_w2)

#' 
#' And by elevation and distance to human access:
## ------------------------------------------------------------------------
par(mfrow = c(1,3))
hist(wolf_used$Elevation2, main="Elevation")
hist(wolf_used$DistFromHumanAccess2, main = "Distance from human access")
hist(wolf_used$DistFromHighHumanAccess2, main="Distance from high human access")

#' 
#' Histogram of elevation use by pack. We've used `y=..density..` to make these *counts per unit* rather than counts 
#' because of the sample size discrepancy between packs (Red Deer pack has far fewer individuals and locations). If you 
#' wanted counts, you chould just delete the `y=..density..` part.
## ---- message=F----------------------------------------------------------
  ggplot(wolf_used, aes(x=Elevation2, fill=pack, group=pack)) + 
        geom_histogram(aes(y=..density..), 
                       position="dodge") + 
  scale_fill_manual(values = c("steelblue", "lightcoral")) +
  ggtitle("Wolf use of elevation") +
  xlab("Elevation") +  
  theme_bw()

#' 
#' Histogram of cover distance by pack:
## ---- message=F----------------------------------------------------------
  ggplot(wolf_used, aes(x=DistFromHumanAccess2, fill=pack, group=pack)) + 
        geom_histogram(aes(y=..density..), 
                       position="dodge") + 
  scale_fill_manual(values = c("steelblue", "lightcoral")) +
  ggtitle("Cover distance") +
  xlab("Cover distance") +  
  theme_bw()

#' 
#' Histogram of sheep HSI by pack: 
## ------------------------------------------------------------------------
  ggplot(wolf_used, aes(x=sheep_w2, fill=pack, group=pack)) + 
        geom_histogram(aes(y=..density..), 
                       breaks= seq(0, 7, by = 1),
                       position="dodge") + 
  scale_fill_manual(values = c("steelblue", "lightcoral")) +
  ggtitle("Sheep HSI") +
  xlab("Sheep HSI") +  
  theme_bw()

#' 
#' ### Violin plots in ggplot2
#' We will make plots of elevation and cover distance by pack, and then use place them side by side on one page. We 
#' include boxplots with the violin plot so it's easy to see the median and interquartile range.
## ------------------------------------------------------------------------
elev_plot <- 
  ggplot(wolf_used, aes(x=pack, y=Elevation2)) +
  geom_violin(aes(fill=pack)) +
  geom_boxplot(width=0.02) +
  ggtitle("Wolf use of elevation") +
  scale_fill_manual(values = c("steelblue", "lightcoral")) +
  xlab("Pack") + ylab("Elevation (m)") +
  theme_bw() +
  theme(legend.position = "none")

dist_human_plot <- 
  ggplot(wolf_used, aes(x=pack, y=DistFromHumanAccess2)) +
  geom_violin(aes(fill=pack)) +
  geom_boxplot(width=0.02)+
  ggtitle("Wolf use of cover distance") +
  scale_fill_manual(values = c("steelblue", "lightcoral")) +
  xlab("Pack") + ylab("Cover distance (m)") +
  theme_bw() +
  theme(legend.position = "none")

#' 
#' Arrange them on one page using the `plot_grid` function in the `cowplot` package:
## ------------------------------------------------------------------------
plot_grid(elev_plot, dist_human_plot, nrow = 2)

#' 
#' 
