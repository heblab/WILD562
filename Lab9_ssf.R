## Lab 9 SSF models

library(sp)
library(raster)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(mapview)
library(maptools)
library(leaflet)
library(magrittr)
#install.packages("amt")
## note have to do this in the Lab
#options(buildtools.check = function(action) TRUE)
library(devtools)
devtools::install_github("jmsigner/amt")
library(amt)

#library(amt)


#### Correlated Random Walks

BCRW <- function(a = 2, b = 1, rho = 0.95, Z.center = 0, attraction = 0.5, n = 50, Z0 = 0){
  require(CircStats)
  
  Z <- c(Z0, rep(NA,n-1))
  phi <- runif(1, -pi, pi)
  for(i in 2:n)
  {
    # relative orientation to center of attraction
    chi <- Arg(Z.center - Z[i-1])
    dphi <- chi-phi
    if(abs(chi - phi) > pi) dphi <- (chi - phi) - pi
    
    # adjust the location 
    location <- phi + attraction * dphi
    
    # pick a new absolute direction ... but MUST BE BETWEEN -pi and pi
    phi <- rwrpcauchy(1, location, rho) - 2*pi
    if(phi > pi) phi <- phi-2*pi
    if(phi < -pi) phi <- phi+2*pi
    
    Z[i] <- Z[i-1] + complex(arg = phi, mod = rweibull(1, a, b))
  }
  return(Z)
}

require(magrittr)
BCRW(a = 2, b = 1, rho = 0.7, Z.center = 10, attraction = 0, n = 2000) %>% 
  plot(type="o", asp=1, pch = 21, bg= grey(seq(0,1,length = 2000)),
       main = "a = 2, b = 1, rho = 0.2, attraction = 0")

BCRW(a = 2, b = 1, rho = 0.7, Z.center = 10, attraction = 0.5, n = 2000) %>% 
  plot(type="o", asp=1, pch = 21, bg= grey(seq(0,1,length = 200)),
       main = "a = 2, b = 1, rho = 0.7, attraction = 0.5")

BCRW(a = 2, b = 1, rho = 0.7, Z.center = 10, attraction = 0.8, n = 2000) %>% 
  plot(type="o", asp=1, pch = 21, bg= grey(seq(0,1,length = 200 )),
       main = "a = 2, b = 1, rho = 0.7, attraction = 0.8")


######


setwd("/Users/mark.hebblewhite/Box Sync/Teaching/UofMcourses/WILD562/Spring2019/Labs/Lab9/2019/")

fisher <- read.csv("Martes pennanti LaPoint New York.csv")
head(fisher)
str(fisher)

ggplot(fisher, aes(location.long, location.lat, colour = individual.local.identifier)) + geom_point()

## lets turn fisher into a spatial points data frame for use in raster operations later,
## First we need to remove NA's
fisher1<-fisher[complete.cases(fisher[4:5]),]
head(fisher1)

xy <- fisher1[, c(4,5)]
fisherSP <- SpatialPointsDataFrame(coords = xy, data = fisher1, proj4string = sp::CRS("+init=epsg:4326"))
str(fisherSP)

mapview(fisherSP, zcol="individual.local.identifier", legend = TRUE, cex=5, lwd=2, map.type = c("OpenStreetMap.DE", "Esri.WorldShadedRelief"))


##### Bringing Fisher data from ID 1016, Fisher M1, into a MOVE object
dat <- read_csv("Martes pennanti LaPoint New York.csv") %>%
   filter(!is.na(`location-lat`)) %>%
   dplyr::select(x = `location-long`, y = `location-lat`,
           t = `timestamp`, id = `tag-local-identifier`) %>%
    filter(id %in% c(1465, 1466, 1072, 1078, 1016, 1469)) # for example 2
   dat_1 <- dat %>% filter(id == 1016)

dat_1 <- amt::make_track(dat_1, x, y, t, crs = sp::CRS("+init=epsg:4326")) %>%
     amt::transform_coords(sp::CRS("+init=epsg:5070"))

summarize_sampling_rate(dat_1)


stps <- amt::track_resample(dat_1, rate = minutes(10), tolerance = minutes(1)) %>%
  filter_min_n_burst(min_n = 3) %>% steps_by_burst() %>%
  time_of_day(include.crepuscule = FALSE)

#str(stps, width = 80, strict.width = "no", nchar.max = 80, give.attr = FALSE)

### Obtaining the NLCD data from FedData
install.packages("FedData")
library(FedData)

## Create a Mask Raster
extent(fisherSP)
fisherSP@proj4string

fisherSP2 <-spTransform(fisherSP, CRS("+init=epsg:5070"))
extent(fisherSP2)
mask.raster<-raster()
extent(mask.raster) <- c(xmin=1770000, xmax=1830000, ymin=2390000 , ymax=2430000)

### NEED TO MAKE Raster mask bigger to prevent error later TO DO
res(mask.raster) = 30
#match projection to elc_habitat shapefile
projection(mask.raster)<- "+init=epsg:5070"
#set all values of mask.raster to zero
mask.raster[]<-0

plot(mask.raster)
plot(fisherSP2, add = TRUE)

#get_nlcd(mask.raster, label="landuse", year = 2011, dataset = "landcover", raw.dir = "/Users/mark.hebblewhite/Box Sync/Teaching/UofMcourses/WILD562/Spring2019/Labs/Lab9/2019/Fisher/raw/", extraction.dir = "/Users/mark.hebblewhite/Box Sync/Teaching/UofMcourses/WILD562/Spring2019/Labs/Lab9/2019/Fisher/extract/", raster.options = c("COMPRESS=DEFLATE", "ZLEVEL=9", "INTERLEAVE=BAND"), force.redo = F)

## Or try the default settings
get_nlcd(mask.raster, label="landuse", year = 2011, dataset = "landcover")



land_use <- raster("/Users/mark.hebblewhite/Box Sync/Teaching/UofMcourses/WILD562/Spring2019/Labs/Lab9/2019/EXTRACTIONS/landuse/NLCD/landuse_NLCD_2011_landcover.tif")

extent(land_use)
#land_use
## the whole point here is to ensure you do NOT clip too closely to the extent of the spatial points dataframe - because of your availability sample. 

plot(land_use)
plot(fisherSP2, add=TRUE, type="p", color = "blue25", pch=12, cex = 0.5)

```
str(land_use)
land_use@data@attributes
levels(land_use)
```
#This tells us there are a whole bunch, ~ 250, or so unnamed NLCD landcover classes that do not apply to our 1 tile of data from New York State. That makes sense. The Landcover categories we do have are

# ```
# ID OID Value      Count Red Green Blue    NLCD.2011.Land.Cover.Class
# 1     0   0     0 7854240512   0     0    0                  Unclassified
# 12   11  11    11  469012527   0     0    0                    Open Water
# 13   12  12    12    1599206   0     0    0            Perennial Snow/Ice
# 22   21  21    21  292251633   0     0    0         Developed, Open Space
# 23   22  22    22  131633826   0     0    0      Developed, Low Intensity
# 24   23  23    23   59456652   0     0    0   Developed, Medium Intensity
# 25   24  24    24   21426522   0     0    0     Developed, High Intensity
# 32   31  31    31  110507264   0     0    0                   Barren Land
# 42   41  41    41  973617734   0     0    0              Deciduous Forest
# 43   42  42    42 1037912310   0     0    0              Evergreen Forest
# 44   43  43    43  179845520   0     0    0                  Mixed Forest
# 53   52  52    52 1940362409   0     0    0                   Shrub/Scrub
# 72   71  71    71 1306961628   0     0    0                   Herbaceuous
# 82   81  81    81  597234572   0     0    0                   Hay/Pasture
# 83   82  82    82 1392218141   0     0    0              Cultivated Crops
# 91   90  90    90  347144473   0     0    0                Woody Wetlands
# 96   95  95    95  116679631   0     0    0 Emergent Herbaceuous Wetlands
# ```

wet <- land_use == 90
names(wet) <- "wet"

##Lets zoom into Ricky T
rickyT.raster <- raster()
extent(rickyT.raster) <- c(xmin=1776000, xmax=1778000, ymin=2410500, ymax=2412500)
plot(wet, extent = rickyT.raster)
plot(fisherSP2, add=TRUE, type="p", color = "blue25", pch=12, cex = 0.5, extent = rickyT.raster)

####

eda1 <- stps %>% extract_covariates(wet, where = "start") %>% mutate(landuse = factor(wet, levels = c(0, 1), labels = c("other", "forested wetland")))


## plots
p1 <- eda1 %>% dplyr::select(landuse, tod = tod_end_, sl_, ta_) %>%
  gather(key, val, -landuse, -tod) %>%
  filter(key == "sl_") %>%
  ggplot(., aes(val, group = tod, fill = tod)) + geom_density(alpha = 0.5) +
  facet_wrap(~ landuse, nrow = 2) +
  xlab("Step length [m]") + theme_light() +
  ylab("Density") +
  theme(legend.title = element_blank())

p1

p2 <- eda1 %>% dplyr::select(landuse, tod = tod_end_, sl_, ta_) %>%
  gather(key, val, -landuse, -tod) %>%
  filter(key == "ta_") %>%
  ggplot(., aes(val, group = tod, fill = tod)) + geom_density(alpha = 0.5) +
  facet_wrap(~ landuse, nrow = 2) +
  xlab("Turn angle") + theme_light() +
  theme(legend.title = element_blank(),
  axis.title.y = element_blank())

p2


require(cowplot)
pg1 <- plot_grid(
  p1 + theme(legend.position = "none"),
  p2 + theme(legend.position = "none"), rel_widths = c(1, 1)
  )
leg <- get_legend(p1)
plot_grid(pg1, leg, rel_widths = c(1, 0.1))

ggsave("fig_eda_1_animal.pdf", width = 20, height = 18, units = "cm")

#####################################

# fit the model
m1 <-stps %>% amt::random_steps(n = 9) %>%
  amt::extract_covariates(wet) %>%
  amt::time_of_day(include.crepuscule = FALSE) %>%
  mutate(log_sl_ = log(sl_)) -> d1

## lets see what we just created
head(m1, n=18)
## How to visualize the SSF point generation process
m1$caseF <-as.factor(m1$case_)
ggplot(m1, aes(x1_ ,y1_, colour = caseF)) + geom_point(aes(size = caseF, colour = caseF))
xy2 <- m1[, c(6,7)]
fisherSSFPlot <- SpatialPointsDataFrame(coords = xy2, data = m1, proj4string = sp::CRS("+init=epsg:5070"))
mapview(fisherSSFPlot, zcol="caseF", legend = TRUE, cex="caseF", lwd=2, map.type = c("OpenStreetMap.DE", "Esri.WorldShadedRelief"))


m3 <- d1 %>% amt::fit_issf(case_ ~ wet + sl_ + wet:tod_end_+ sl_:tod_end_ + strata(step_id_))
m2 <- d1 %>% amt::fit_issf(case_ ~ wet + log_sl_ + wet:tod_end_+ log_sl_:tod_end_ + strata(step_id_))
m1 <- d1 %>% amt::fit_issf(case_ ~ wet + log_sl_ + sl_ + wet:tod_end_+ log_sl_:tod_end_ + sl_:tod_end_ + strata(step_id_))

AIC(m1$model, m2$model, m3$model)
summary(m1)

s <- summary(m1$model)$coefficients
s

print(xtable::xtable(s, digits = 4,type = "latex", caption.placement = "top"))

### Exploring shape of coefficients?
shape <- sl_shape(m1)
scale <- sl_scale(m1)

shape_adj_day <- amt::adjust_shape(shape, coef(m1)["log_sl_"])
shape_adj_night <- amt::adjust_shape(shape, coef(m1)["log_sl_"]) +
coef(m1)["log_sl_:tod_end_night"]

scale_adj_day <- amt::adjust_scale(scale, coef(m1)["sl_"])
scale_adj_night <- amt::adjust_scale(scale, coef(m1)["sl_"]) + coef(m1)["sl_:tod_end_night"]

# speed
speed_day <- shape_adj_day * scale_adj_day
speed_night <- shape_adj_night * scale_adj_night

scale
shape

shape_adj_day
shape_adj_night

x <- seq(1, 500, 1)
plot(x, dgamma(x, shape = shape_adj_night, scale = scale_adj_night), type = "l", col = "red")
lines(x, dgamma(x, shape = shape_adj_day, scale = scale_adj_day), type = "l", col = "blue")

# Bootstrap everything to estimate movement rates adjusted for different day and night movement rates


mod_data <- stps %>% amt::random_steps(n = 9) %>%
 amt::extract_covariates(wet, where = "end") %>%
 amt::time_of_day(include.crepuscule = FALSE) %>%
 mutate(log_sl_ = log(sl_), cos_ta_ = cos(as_rad(ta_)))

strata <- unique(mod_data$step_id_)
n <- length(strata)

## note we are only going to do this 100 times

bt <- replicate(100, {
    m_boot <- mod_data[mod_data$step_id_ %in% sample     (strata, n, TRUE), ] %>%
    amt::fit_clogit(case_ ~ wet + log_sl_ + sl_ + wet     :tod_end_ + sl_:tod_end_ + log_sl_:tod_end_ +        strata(step_id_))

    scale_adj_day <- amt::adjust_scale(shape, coef       (m_boot)["sl_"])
    scale_adj_night <- amt::adjust_scale(shape, coef     (m_boot)["sl_"]) + coef(m_boot)["sl_:tod_end_night"]

   shape_adj_day <- amt::adjust_shape(shape, coef
   (m_boot)["log_sl_"])
   shape_adj_night <- amt::adjust_shape(shape, coef
   (m_boot)["log_sl_"]) +

   coef(m_boot)["log_sl_:tod_end_night"]

## speed
c(shape_adj_day * scale_adj_day,
 shape_adj_night * scale_adj_night)})

bt2 <- data_frame(
rep = 1:ncol(bt),
 day = bt[1, ],
 night = bt[2, ]
) %>% gather(key, val, -rep)


bt2 %>% group_by(key) %>% summarise(lq = quantile(val, 0.025),me = median(val), mean = mean(val),uq = quantile(val, 0.975))

# m/min
bt2 %>% group_by(key) %>% summarise(lq = quantile(val, 0.025) / 10,me = median(val) / 10, mean = mean(val) / 10,uq = quantile(val, 0.975) / 10)


#### Movement and habitat distributions
 ## Simulate ud
wet_c <- crop(wet, amt::bbox(dat_1, spatial = TRUE, buff = 1e3))


#movement_kernel(): calculates a movement kernel from a fitted (i)SSF. The method is currently only implemented for the gamma distribution.
mk <- amt::movement_kernel(scale, shape_adj_day, wet_c)
plot(mk)

#The habitat kernel is calculated by multiplying resources with their corresponding coefficients from the fitted (i)SSF.
hk <- amt::habitat_kernel(list(wet = coef(m1)["wet"]), wet_c)
plot(hk)


# simulate_ud(): simulates a utilization distribution (UD) from a fitted Step-Selection Function.
system.time(ssud_day <- amt::simulate_ud(
  mk, hk,
  as.numeric(stps[1, c("x1_", "y1_")]),
   n = 1e7))
 plot(ssud_day)

 ## simulate_tud(): Is a conviencience wrapper arround simulate_ud to simulate transition UDs
 #(i.e., starting at the same position many times and only simulate for a short time).
system.time(tud_day <- amt::simulate_tud(mk, hk, as.numeric(stps[150, c("x1_", "y1_")]), n = 72, n_rep = 5e3))
 plot(tud_day)


 # night
 mk <- amt::movement_kernel(scale, shape_adj_night, wet_c)
 hk <- amt::habitat_kernel(list(wet = coef(m1)["wet"] + coef(m1)["wet:tod_end_night"]), wet_c)

system.time(ssud_night <- amt::simulate_ud(
   mk, hk, as.numeric(stps[1, c("x1_", "y1_")]), n = 1e7))
plot(ssud_night)

system.time(tud_night <- amt::simulate_tud(mk, hk, as.numeric(stps[150, c("x1_", "y1_")]), n = 72, n_rep = 5e3))
plot(tud_day)
plot(tud1 <- crop(tud_day, extent(c(1778000, 1782000, 2412000, 2415000))))
plot(tud2 <- crop(tud_night, extent(c(1778000, 1782000, 2412000, 2415000))))

pllog <- list(
  geom_raster(),
   coord_equal(),
  scale_fill_continuous(low = "white", high = "red", tran = "log10", na.value = "white"),
   scale_y_continuous(expand = c(0, 0)),
   scale_x_continuous(expand = c(0, 0)),
   theme_light(),
   theme(legend.position = "none"))

 pl <- list(
   geom_raster(),
   coord_equal(),
   scale_fill_continuous(low = "white", high = "red", na.value = "white"),
 scale_y_continuous(expand = c(0, 0)),
   scale_x_continuous(expand = c(0, 0)),
   theme_light(),
   theme(legend.position = "none"))

r1 <- data.frame(rasterToPoints(mk))
p1 <- ggplot(r1, aes(x, y, fill = d)) + pllog + ggtitle("Movement kernel (night)")

r2 <- data.frame(rasterToPoints(hk))
p2 <- ggplot(r2, aes(x, y, fill = layer)) + pl + ggtitle("Habitat kernel (night)")

r1 <- data.frame(rasterToPoints(tud1))
p3 <- ggplot(r1, aes(x, y, fill = layer)) + pllog + ggtitle("Transient UD (day)")

 r2 <- data.frame(rasterToPoints(tud2))
p4 <- ggplot(r2, aes(x, y, fill = layer)) + pllog + ggtitle("Transient UD (night)")


r1 <- data.frame(rasterToPoints(ssud_day))
p5 <- ggplot(r1, aes(x, y, fill = layer)) + pl + ggtitle("Steady state UD (day)")

r2 <- data.frame(rasterToPoints(ssud_night))
p6 <- ggplot(r2, aes(x, y, fill = layer)) + pl + ggtitle("Steady state UD (night)")

cowplot::plot_grid(p1, p2, p3, p5, p4, p6, ncol = 2, labels = "AUTO")



ggsave("img/fig_one_animal1.pdf", height = 20, width = 24, units = "cm")


#################################################
### Example 2: Many animals
library(ggplot2)
library(raster)
library(lubridate)
library(amt)
library(parallel)

dat <- read_csv("Martes pennanti LaPoint New York.csv") %>%
 filter(!is.na(`location-lat`)) %>%
  dplyr::select(x = `location-long`, y = `location-lat`,
              t = `timestamp`, id = `tag-local-identifier`) %>%
   filter(id %in% c(1465, 1466, 1072, 1078, 1016, 1469))

 dat_all <- dat %>% nest(-id)
dat_all$sex <- c("f", "f", "f", "m", "m", "m")
 dat_all <- dat_all %>%
  mutate(trk = map(data, function(d) {
     amt::make_track(d, x, y, t, crs = sp::CRS("+init=epsg:4326")) %>%
       amt::transform_coords(sp::CRS("+init=epsg:5070"))}))

dat_all %>% mutate(sr = lapply(trk, summarize_sampling_rate)) %>%
  dplyr::select(id, sr) %>% unnest
  land_use <- raster("/Users/mark.hebblewhite/Box Sync/Teaching/UofMcourses/WILD562/Spring2019/Labs/Lab9/2019/EXTRACTIONS/landuse/NLCD/landuse_NLCD_2011_landcover.tif")
  rcl <- cbind(c(11, 12, 21:24, 31, 41:43, 51:52, 71:74, 81:82, 90, 95),c(1, 1, 2, 3, 3, 3, 2, 5, 5, 5, 5, 5, 5, 5, 5, 5, 8, 8, 1, 1))
# water, dev open, dev, barren, forest, shrub and herb, crops, wetlands
# 1: water, wetlands
# 2: developed (open)
# 3: developed (other)
# 5: forest, herbaceouse
 # 8: crops
 lu <- reclassify(land_use, rcl, right = NA)
 names(lu) <- "landuse"
plot(lu)

 m1 <- dat_all %>%
   mutate(steps = map(trk, function(x) {
     x %>% amt::track_resample(rate = minutes(10), tolerance = seconds(120)) %>%
      amt::filter_min_n_burst() %>%
       amt::steps_by_burst() %>% amt::random_steps() %>%
       amt::extract_covariates(lu, where = "both") %>%
       mutate(landuse_end = factor(landuse_end))
     })) 
 
 m1 <- m1 %>% mutate(fit = map(steps, ~ amt::fit_issf(., case_ ~ landuse_end +
                                                           strata(step_id_))))

 m1
 
d2 <- m1 %>% mutate(coef = map(fit, ~ broom::tidy(.x$model))) %>%
   dplyr::select(id, sex, coef) %>% unnest %>%
  mutate(id = factor(id)) %>% group_by(term) %>%
  summarize(
     mean = mean(estimate),
    ymin = mean - 1.96 * sd(estimate),
     ymax = mean + 1.96 * sd(estimate))

d2$x <- 1:nrow(d2)


 p1 <- m1 %>% mutate(coef = map(fit, ~ broom::tidy(.x$model))) %>%
   dplyr::select(id, sex, coef) %>% unnest %>%mutate(id = factor(id)) %>%
   ggplot(., aes(x = term, y = estimate, group = id, col = id, pch = sex)) +
   geom_rect(mapping = aes(xmin = x - .4, xmax = x + .4, ymin = ymin, ymax = ymax), data = d2, inherit.aes = FALSE,fill = "grey90") +geom_segment(mapping = aes(x = x - .4, xend = x + .4,y = mean, yend = mean), data = d2, inherit.aes = FALSE, size = 1) +
 geom_pointrange(aes(ymin = conf.low, ymax = conf.high),position = position_dodge(width = 0.7), size = 0.8) + geom_hline(yintercept = 0, lty = 2) +
   labs(x = "Habitat", y = "Relative Selection Strength") + theme_light() +
  scale_x_discrete(labels = c("Developed (open)", "Developed (other)", "Natural", "Crops"))

p1
ggsave("img/fig_all_animals.pdf", width = 24, height = 12, units = "cm")

############################################################

# Mixed-effect cLogit Models

In the next set of exercises, I will build on the SSF case study of Singer et al., and conduct mixed-effects clogit analyses accounting for individual fishers as random effects similar to Lab 7.  Conceptually, adding random effects to conditional logistic regression models was challenging because there is no intercept.  Here are the first 2 papers that figured out how to add a random intercept for each individual animal (e.g.), however, it did so in MATLAB. So, its mostly inaccessible to biologists. 

Craiu, R. V., T. Duchesne, D. Fortin, and S. Baillargeon. 2011. Conditional Logistic        Regression With Longitudinal Follow-up and Individual-Level Random Coefficients: A Stable   and Efficient Two-Step Estimation Method. Journal of Computational and Graphical            Statistics 20:767-784.

Duchesne, T., D. Fortin, and N. Courbin. 2010. Mixed conditional logistic regression for    habitat dplyr::selection studies. Journal of Animal Ecology 79:548-555.

Since these initial papers, however, there have been a few big breakthrough’s lately with the mclogit package http://cran.r-project.org/web/packages/mclogit/mclogit.pdf  or the coxme package here http://cran.r-project.org/web/packages/coxme/coxme.pdf I just played around with both of these packages and they are actually. 

We will first need to 'unpack' the nested data frame from section 3 above into an expanded dataframe using the unnest command.  Then we will progress through a set of 3-4 different models and compare model interpretations to naive GLM models of the same kind. 
```{r}
fisher6 <- dat_all %>%
  mutate(steps = map(trk, function(x) {
    x %>% amt::track_resample(rate = minutes(10), tolerance = seconds(120)) %>%
      amt::filter_min_n_burst() %>%
      amt::steps_by_burst() %>% amt::random_steps() %>%
      amt::extract_covariates(lu, where = "both") %>%
      mutate(landuse_end = factor(landuse_end))
  })) %>%
  dplyr::select(id, steps) %>%
  unnest()

fisher6
head(fisher6)
```


Next, we will add a String variable for landuse_end recalling that, earlier, we defined the NLCD landcover according to:
  1: water, wetlands
2: developed (open)
3: developed (other)
5: forest, herbaceouse
8: crops
```{r}
head(fisher6$landuse_end)
fisher6$landuseName = ifelse(fisher6$landuse_end == 1, "Wet Forests", 
                             ifelse(fisher6$landuse_end == 2, "Developed Open", 
                                    ifelse(fisher6$landuse_end == 3, "Developed Other", 
                                           ifelse(fisher6$landuse_end == 5, "Natural", "Crops"))))
table(fisher6$landuseName, fisher6$landuse_end)
```

## Fit a naive GLM
First, we will fit a 'naive' GLM only focusing on the habitat processes, that is, habitat dplyr::selection for the landcover covaraites and compare them to the coefficients from the SSF fit to each individual Fisher and their two-stage population-level averages. 
```{r}
model1 <- glm(case_~ I(landuse_end), data=fisher6,family=binomial(link="logit"))
## I commented out these next few versions of the models fit to landuseName to make comparisons to the previously fit 6 fisher two-step models more comparable, though we have to then keep track of which landovers 2, 3, 5, and 8 are. 
#model1 <- glm(case_~ I(landuseName), data=fisher6,family=binomial(link="logit"))
#model1 <- glm(case_~ I(landuseName=="Developed Open") + I(landuseName=="Developed Other") +I(landuseName=="Natural")+I(landuseName=="Crops"), data=fisher6,family=binomial(link="logit"))
summary(model1)
```
This model gives us an intercept, which is interpreted as wet-forests, and we note that we also did not get an intercept in the SSF model above.  Lets now compare the coefficients to above:
  ```{r}
coef(model1)
naive_glm <- broom::tidy(model1) %>% 
  filter(!term=="(Intercept)") 

figNaive <- naive_glm %>%
  ggplot(., aes(x = term, y = estimate)) +
  geom_pointrange(aes(ymin = estimate - 1.96*std.error, ymax = estimate +1.96*std.error)) +
  labs(x = "Habitat", y = "Relative selection Strength") + 
  theme_light() +
  scale_x_discrete(labels = c("Dev(open)", "Dev(other)", "Natural", "Crops")) + geom_hline(yintercept = 0, lty = 2) + ylim(c(-3.75,1.5))
figNaive
```
First, we recall we are comparing the two-stage averaged SSF coefficients from the fisher SSF model above extracted in the object d2. This is simply the arithmetic mean of the 6 individual coefficients.  Second, note that the ORDER of landcover categories on the X axis are changed now, which is slightly annoying, and tough to remedy. But keep that in mind when looking at the next figure. 

```{r}
fig5 <- plot_grid(p1, figNaive)
fig5
```

We note that there are some similarities between coefficients, but, differences. 

Developed Open is   -0.49 from the Naive Logit, and   -1.00 from the SSF
Developed Other is  -1.82 from the Naive Logit, and   -2.20 from the SSF
Natural is          +0.033 from the Naive Logit, and  +0.044 from the SSF
CropsDeveloped Open -0.25 from the Naive Logit, and   -0.69 from the SSF

The difference in the interpretation from the different parameters highlights the 'effect' of movement, so to speak, on dplyr::selection for covariates. Again, here, note we are not considering any differences between male or female, or night or day.  For example, one could conclude that the biggest difference is in the dplyr::selection of Developed Open, which we woudl underestimate the avoidance of if we failed to consider the movement processes. 

## Fitting a 'Naive' cLogit Model 

Next, we will fit a 'naive' clogit model, that is, a model that does not account for any differences between individuals and treats all step_id_'s as independent. Basically ignoring any random effects structure of individual fishers in this case. 
_from https://rdrr.io/cran/survival/man/clogit.html_ 
It turns out that the loglikelihood for a conditional logistic regression model = loglik from a Cox model with a particular data structure. Proving this is a nice homework exercise for a PhD statistics class; not too hard, but the fact that it is true is surprising.

When a well tested Cox model routine is available many packages use this ‘trick’ rather than writing a new software routine from scratch, and this is what the clogit routine does. In detail, a stratified Cox model with each case/control group assigned to its own stratum, _time set to a constant_, status of 1=case 0=control, and using the exact partial likelihood has the same likelihood formula as a conditional logistic regression. The clogit routine creates the necessary dummy variable of times (all 1) and the strata, then calls coxph.

The computation of the exact partial likelihood can be very slow, however. If a particular strata had say 10 events out of 20 subjects we have to add up a denominator that involves all possible ways of choosing 10 out of 20, which is 20!/(10! 10!) = 184756 terms. Gail et al describe a fast recursion method which partly ameliorates this; it was incorporated into version 2.36-11 of the survival package. The computation remains infeasible for very large groups of ties, say 100 ties out of 500 subjects, and may even lead to integer overflow for the subscripts – in this latter case the routine will refuse to undertake the task. The Efron approximation is normally a sufficiently accurate substitute.

First we have to create a unique stratum ID for each set of steps for each individual animal. Right now, there is a case_ field, step_id_ field, and id_ field - but, the step_id_ field repeats for each animal ID. We do this by creating a new stratum field called stratum by pasting together fisher ID and step ID. 
```{r}
require(survival)
head(fisher6)
## Look at the number of step_id_'s for each id
fisher6 %>% group_by(step_id_) %>% summarize(n=n())
fisher6$stratum <- paste(fisher6$id, fisher6$step_id_)
```
So indeed, we see that there are 66 rows of data for each step ID because there are 6 individuals. 

```{r}
clogit1 <- clogit(case_ ~ I(landuse_end) + strata(stratum), data = fisher6)
#clogit1 <- clogit(case_ ~ I(landuseName=="Developed Open") + I(landuseName=="Developed Other") +I(landuseName=="Natural")+I(landuseName=="Crops") + strata(stratum), data = fisher6)
summary(clogit1)
coef(clogit1)
# tidy up coefficients
clogit_1 <- broom::tidy(clogit1) %>% 
  filter(!term=="(Intercept)") 
## make a figure
figclogit1 <- clogit_1 %>%
  ggplot(., aes(x = term, y = estimate)) +
  geom_pointrange(aes(ymin = estimate+ 1.96*std.error, ymax = estimate-1.96*std.error)) +
  labs(x = "Habitat", y = "Relative selection Strength") + 
  theme_light() +
  scale_x_discrete(labels = c("Dev(open)", "Dev(other)", "Natural", "Crops")) + geom_hline(yintercept = 0, lty = 2) + ylim(c(-3.75,1.5))
figclogit1
plot_grid(p1, figclogit1)
```

The coefficients are a bit different, but still, quite close even with the cluster for each individual Stratum. 

## Mixed-effect cLogit Models

#We can currently fit mixed-effects clogit models using two R packages.
#coxme and mclogit. 

#First, we will use the coxme package to fit a mixed-effects conditional logistic regression model, accounting for the random effect of individual Fisher ID in this case. First, we ensure we have the coxme package loaded. See here for more information: https://cran.r-project.org/web/packages/coxme/vignettes/coxme.pdf 
#```{r}
require(coxme)
#```
#There is no 'convenient' wrapper around the coxme(Surv()) function like in clogit above.  Thus, first, we need to make a 'fake' time variable to trick the Cox-proportional hazards model that time is irrelevant in your conditional logistic model. We do this by adding a new variable, time_ to the fisher6 data frame above. This is actually what the clogit wrapper around survival is doing, we just don't know it. 
#```{r}
fisher6$time_ <- ifelse(fisher6$case_ == 0, 2, 1)   #2 for control, 1 for case
table(fisher6$time_, fisher6$case_)

clogitM1<- coxme(Surv(time_,case_) ~ I(landuse_end) + strata(stratum) + (1|id), data=fisher6)
AIC(clogitM1)
summary(clogitM1)
#```

#We note, importantly, that the random effects variance is quite low, nearly zero. This means there is very little variation between individual Fishers in selection.  This becomes important later. 

#Second, I tried to fit a similar model using the mclogit package. However, the current implementation of random effects is limited to the PQL technique, which requires large cluster sizes. Thus, here, we do not have large enough clusters with only 9 random points. We see an error message accordingly. 

#```{r, eval = FALSE, echo =FALSE}
#The first column contains the choice counts or choice indicators (alternative is chosen=1, is not chosen=0). The second column contains unique numbers for each choice set.
# create a used / avail 1, 0 variable
#fisher6$used_ <- ifelse(fisher6$case_ == 0, 0, 1) 
#table(fisher6$time_, fisher6$used_)

# convert stratum to a number
#fisher6$stratumN <- as.factor(fisher6$stratum)
#levels(fisher6$stratumN) <- 1:length(levels(fisher6$stratumF))
#fisher6$stratumN <- as.numeric(fisher6$stratumN)

# first fit a mclogit Test model wtih no random effect. 
#mclogitTest <- mclogit(cbind(used_, stratumN) ~I(landuse_end), data=fisher6)

#mclogitTest2 <- mclogit(cbind(used_, stratumN) ~I(landuse_end), random=~ 1|id, data=fisher6)
#summary(mclogitTest2)
#str(mclogitTest2)
#```

#Plotting the coefficients from coxme (or mclogit) are more difficult because there is no tidy approach for objects generated by coxme models, but we can quickly compare the coefficients directly here, where column 1, 2, and 3 are the naive GLM, naive clogit, and mixed-effect cLogit models. And compare them to the coefficients for the two-step models from Singer et al. 
#```{r}
v1<-model1$coefficients[2:5]
v2<-coef(clogit1)
v3<-coef(clogitM1)
v5<-d2$mean
coefSum <- as.data.frame(cbind(v1, v2, v3, v4))
names(coefSum) <- c("Naive", "clogit", "coxme", "two-stage iSSF")
coefSum
#```
#Recall that landuse_end 2, 3, 5 and 8 correspond to developed (open), developed (other), natural, and cropland. And that the forested wet areas are the 'reference' category. 

#We note that there are some important differences here between the 'naive' , clogit/coxme/mcclogit, and  two-stage iSSF model. The reason why there are few differences between clogit, the coxme or mcclogit is because of the rather low variance in the summary of clogitM1 for individual Fisher's we saw above. 

#However, there are - just like Lab 7 - differences between the two-staged model, d2, and the coxme/clogit models for landcover types 3 and 5, in particular.  

## Model Selection
#```{r}
AIC(clogit1, clogitM1, mclogitTest)
#```
#This clearly confirms that really, the mixed-effect model structure is not necessary and we are fine making inferences using the 'naive' cLogit Model. Finally, we should compare manually the sum of the individual model AIC values from the two-stage modeling to really undestand if this clogit model is better than the two-step. However, these results may be idiosyncratically dependent on the small sample size of Fisher's here, 6, and the limited variation in response to just one categorical covariate.  Often, addition of mixed-effects models improves model fit substantially. 

# Homework

#Conduct an SSF model for JUST wolves in the Cascade, Red Deer and Bow Valley wolf packs for some covariates that we have used this semester.  Pick one season as well, and test whether there are differences in movement during day and night. 

#```{r}
wolfGPS <- read.csv("wolfGPS.csv")
head(wolfGPS)
ggplot(wolfGPS, aes(X_COORD1, Y_COORD1, colour = WOLFNAME)) +geom_point()
ggplot(wolfGPS, aes(X_COORD1, Y_COORD1, colour = PACK)) +geom_point()
#```