#' ---
#' title: "WILD 562: Intro to step selection functions"
#' author: "Eric Palm"
#' date: "12 March 2019"
#' output:
#'   html_document:
#'     theme: simplex
#'     toc: true
#'     toc_float: true
#'     number_sections: true
#'     self_contained: true
#' ---
#'
## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, cache = TRUE)
knitr::opts_knit$set(root.dir = "C:/Users/Eric/Desktop/SSF_intro/")

#'
#'
#' # Loading necessary packages
#'
#' The `amt` package (animal movement tools) is a new package that is very handy for a lot of resource selection analyses.
#' [Here's a link](https://www.researchgate.net/publication/331112461_Animal_movement_tools_amt_R_package_for_managing_tracking_data_and_conducting_habitat_selection_analyses) 
#' to a recently published paper about `amt`.
## ---- warning = FALSE, message=F, results=F------------------------------
require(raster)
require(amt)
require(mapview)
require(tidyverse)
require(survival)
require(sjPlot)

#' # Loading and importing data
#'
#' First let's load our raster stack of habitat covariates. I don't know how Mark wants to collapse all the landcover 
#' data into different classes, so for this lab we'll keep it simple (and a bit boring) with just human access and topography layers.
## ------------------------------------------------------------------------
load("./data/habitat_stack.rda")
habitat_stack

#'
#' And then our elk telemetry data:
## ------------------------------------------------------------------------
elk_df <- read_csv("./data/elk_df.csv")

#'
#' For some reason the `read_csv` function didn't parse the timestamp column as "datetime" format, so we'll manually convert 
#' it to POSIXct format, which is the date-time format that `amt` likes:
## ------------------------------------------------------------------------
elk_df$timestamp <- as.POSIXct(elk_df$timestamp, format = "%m/%d/%y %H:%M")
elk_df

#' Now the "timestamp" column is formatted as a "datetime".
#'
#' # Data visualization and exploration
#' It's good to look at your data on a map and make sure nothing looks ridiculous. First, let's convert the data frame to an 
#' `sp` object so we can plot it a few different ways.
## ------------------------------------------------------------------------
elk_sp <- SpatialPointsDataFrame(coords = elk_df[, c("lon", "lat")], data = elk_df, proj4string = sp::CRS("+init=epsg:4326"))
elk_sp_UTM <- spTransform(elk_sp, habitat_stack@crs)

#'
#' If we want to take a quick look at our elk data on an interactive map, we can plot it using `mapview` with a basemap of our 
#' choosing (there are a lot more basemap options and other ways to make this prettier.)
## ------------------------------------------------------------------------
mapview(elk_sp, zcol = "id", legend = TRUE, cex = 5, lwd = 2, map.type = "Esri.DeLorme")

#' Let's just check to see what our raster stack of habitat covariates looks like:
## ------------------------------------------------------------------------
plot(habitat_stack)

#'
#' We can overlay our elk telemetry locations on the elevation raster:
## ------------------------------------------------------------------------
plot(habitat_stack$elev)
points(elk_sp_UTM, pch = 20, col = c("blue", "red", "green", "purple", "navy", "darkgreen")[as.factor(elk_sp_UTM$id)])

#'
#' To get an idea of how many locations we have per individual:
## ------------------------------------------------------------------------
table(elk_df$id)

#'
#' # Creating and nesting an `amt` track
#' We'll use the make_track function to do this. Our data is in WGS 84, so we'll specify that as the coordinate reference system
#' ("+init=epsg:4326"), and then transform it to UTM because `amt` requires projected data rather than lon lat.
## ------------------------------------------------------------------------
elk_trk <- amt::make_track(elk_df, .x = lon, .y = lat, .t = timestamp, id = id, crs = sp::CRS("+init=epsg:4326")) %>%
  amt::transform_coords(habitat_stack@crs)
elk_trk

#' Notice here that `amt` only requires fields for **x**, **y** and **time**, but all other fields, including animal **id** are 
#' optional. I think this is to allow flexibility on how you want to analyze the data. Obviously, because we are going to do SSF 
#' analyses, which are based on individual animal movement paths, we need to make sure we keep track of animal id. We can do this
#' by nesting our data frame.
#'
#' Nesting our data frame creates list-columns, which may be difficult to get used to at first but are quite handy. Here's we'll 
#' nest by animal id. However, if we had already broken our data into seasons and wanted to model resource selection by season, 
#' we could nest by both animal id and season, like this: `nest(-id, -season)`
## ------------------------------------------------------------------------
elk_trk_nested <-
  elk_trk %>%
  nest(-id)

#'
#' This shows the first element in the "data" list-column, which is the location data for the first individual.
## ------------------------------------------------------------------------
elk_trk_nested$data[[1]]

#'
#' ## amt data summaries and visualization
#' Before we create random "available" steps for each individual to use in an SSF analysis, we need to decide at what 
#' spatiotemporal scale we're going to do our analysis. To help us make this decision, we need to know our sampling rate, 
#' or the amount of time between successive locations. In many studies, GPS collars or tags will be set to different duty 
#' cycles. Some animals may have locations every 2 hours, while others every 13 hours, etc. These discrepancies can be 
#' challenging for modelling, because we'd like to model using a consistent spatiotemporal scale across individuals.
#'
#' Let's see what our elk data look like. `amt`'s `summarize_sampling_rate` function gives us more information than we would
#' ever want to know about the sampling interval, or time lag, between consecutive GPS locations. We'll add a new column 
#' called "lags".
## ------------------------------------------------------------------------
elk_trk %>%
  nest(-id) %>%
  mutate(lags = map(data, summarize_sampling_rate))

#' But we're just interested in seeing what's actually in the "lags" list-column. Because this column is nested, we can now 
#' "unnest" it to see our sampling rate summary.
#'
#' Let's keep the "id" column too so we can see our animal ids.
## ------------------------------------------------------------------------
elk_trk %>%
  nest(-id) %>%
  mutate(lags = map(data, summarize_sampling_rate)) %>%
  select(id, lags) %>%
  unnest()

#' The "median" column is most useful here. Luckily we have pretty consistent time lags of 2 hours across all individuals. 
#' So, the finest scale we can do an analysis would be at the 2-hr scale, which is probably something like Johnson's 3rd to 
#' 4th order of selection.
#'
#' Here, we can use some more `amt` functions to calculate some simple movement statistics from our data. The `amt` package 
#' has a cool function called `time_of_day` which calculates whether a location is during the day or night based on the angle 
#' of the sun at the particular coordinates and the timestamp.
## ---- warning=F----------------------------------------------------------
elk_trk_stats <-
  elk_trk %>%
  nest(-id) %>%
  mutate(
    speed = map(data, speed),
    step_length = map(data, step_lengths),
    turn_angle = map(data, direction_rel),
    time_of_day = map(data, time_of_day)
  ) %>%
  unnest()

#' The warnings here just let us know that we have a lot of different units in the summary statistics we've created. Step 
#' lengths are in meters, speeds in meters/second, and turn angles in degrees bound between -180 and 180. Notice the first 
#' turning angle is NA. This is because we need three consecutive locations to calculate a relative turning angle.
#'
#' You can see there are NAs in "speed", "step_length" and "turn_angle":
## ------------------------------------------------------------------------
summary(elk_trk_stats)

#'
#'
#' ## Summary plots
## ---- warning=F----------------------------------------------------------
elk_trk_stats %>%
  ggplot(., aes(x = turn_angle, fill = id)) +
  geom_histogram(breaks = seq(-180, 180, by = 10)) +
  theme_classic() +
  ylab("Count") +
  ggtitle("Relative turn angles") +
  scale_x_continuous("",
    limits = c(-180, 180), breaks = seq(-180, 180, by = 60),
    labels = seq(-180, 180, by = 60)
  ) +
  facet_wrap(~id, scales = "free") +
  theme(legend.position = "none")

#'
#' If you plot turn angles by individual, you might see a lot of irregularities, especially for those animals with relatively 
#' few locations, but if you pool turn angles across individuals, it should be a cleaner plot with a definite hump in the 
#' middle centered around 0, meaning the animal moves straight ahead more often than other directions. To see what it looks 
#' like, try running all but the last two lines of the previous code.
#'
#' Now we can plot histograms of step lengths faceted by individual.
## ---- warning=F----------------------------------------------------------
elk_trk_stats %>%
  ggplot(., aes(x = step_length, fill = id)) +
  geom_histogram(breaks = seq(0, 4000, by = 250)) +
  theme_classic() +
  ylab("Count") +
  ggtitle("Step lengths (m)") +
  scale_x_continuous("",
    limits = c(0, 4000), breaks = seq(0, 4000, by = 1000),
    labels = seq(0, 4000, by = 1000)
  ) +
  facet_wrap(~id, scales = "free") +
  theme(legend.position = "none")

#'
#' This is the typical distribution we see for step lengths. Usually the animal takes shorter steps, and more rarely takes 
#' longer ones. The `amt` package fits a *gamma* distribution, which is a very flexible distribution, to step lengths and 
#' ranodmly samples from this distribution when creating "available" steps. However, there is some debate about whether it's 
#' more appropriate to randomly draw from the empirical (observed) step lengths rather than from a distributions fitted to 
#' those step lengths. Realistically the results are probably very similar with large enough sample sizes, but they could be 
#' very different with small sample sizes.
#'
#' How about we see if animals move faster during the day versus at night.
## ---- warning=F----------------------------------------------------------
ggplot(elk_trk_stats, aes(x = tod_, y = speed, fill = tod_)) +
  geom_violin() +
  theme_bw() +
  facet_wrap(~id, scales = "free") +
  theme(legend.position = "none") +
  ylab("speed (m/s)") +
  xlab("time of day")

#'
#' It's hard to see the differences because the data are so right skewed, so let's take the log of speed:
## ---- warning=F----------------------------------------------------------
ggplot(elk_trk_stats, aes(x = tod_, y = log(step_length), fill = tod_)) +
  geom_violin() +
  theme_bw() +
  facet_wrap(~id, scales = "free") +
  theme(legend.position = "none") +
  ylab("log(speed)") +
  xlab("time of day")

#' Seems reasonable that they move a bit faster during the day.
#'
#' # Prepare SSF data frame by individual
#' Now we could create "available" steps by sampling from the distributions of step lengths and turning angles in the `elk_trk` 
#' object but if we did that, we'd be assuming that each individual had similar step length and turn angle distributions, and 
#' we'd wash over any **individual variation**. If we want to capture individual variation, we should probably create 
#' **separate tracks** for each individual, which means that we will then fit separate distributions to turn angles and step 
#' lengths for **each individual**. To do this, we will nest the dataframe **BEFORE** we use the amt `make_track` function. 
#' We're merging a few steps into a function (with argument "d") and adding the output from this function (our `amt` track) 
#' in a new column called "trk".
## ------------------------------------------------------------------------
elk_trk_id <-
  elk_df %>%
  nest(-id) %>%
  mutate(trk = map(data, function(d) {
    make_track(d, lon, lat, timestamp, crs = sp::CRS("+init=epsg:4326")) %>%
      transform_coords(habitat_stack@crs)
  }))

#'
#' Now we've made six tracks, one for each individual.
## ------------------------------------------------------------------------
elk_trk_id

#'
#' The "data" list-column has all our original data, and the "trk" list-column has our data in `amt`'s `track_xyt` format. 
#' Just to remind ourselves, let's look at the first element (first animal) in the "trk" list-column.
## ------------------------------------------------------------------------
elk_trk_id$trk[[1]]

#'
#'
#' ## Create available steps and extract covariates
#' Alright, there's a lot going on in this next chunk of code, so we'll go throughout it piece by piece. Within the mutate 
#' call, we use `purrr::map` to apply a number of `amt` functions to the "trk" list column we created above.
#'
#' In order, the functions are:
#' (1) Resampling (or regularizing) our track so we have regular intervals between consecutive locations (in this case every 
#' two hours, with a 20 minute tolerance). When there is a interval between successive locations that is more or less than 
#' that 2 hour period, locations before and after that time gap will be placed into different **bursts**. Step lengths and 
#' turning angles will not be calculated across bursts, only within them.
#' (2) Only retain bursts with at least 3 locations, because we need a minimum of three locations to calculate relative 
#' turn angles.
#' (3) Creates steps (only within a burst!) between used locations. This automatically calculates step lengths and turn 
#' angles.
#' (4) Creates 3 random "available" steps per used step. You could choose more but 3 is good for now.
#' (5) Extracts the covariate values (for all layers in the raster stack) at the endpoint of each step. You could extract 
#' values at the beginning point of each step too. It shouldn't make much difference (but you could try it and see!)
## ---- warning=F----------------------------------------------------------
ssf_2_hr <- elk_trk_id %>%
  mutate(steps_2_hr = map(trk, function(x) {
    x %>%
      track_resample(rate = minutes(120), tolerance = minutes(20)) %>%
      filter_min_n_burst(min_n = 3) %>%
      steps_by_burst(diff_time_units = "hours") %>%
      random_steps(n = 3) %>%
      extract_covariates(habitat_stack, where = "end")
  })) %>%
  select(id, steps_2_hr) %>%
  unnest()

#' So, we are doing a *point-based* SSF rather than a *path-based* SSF. [Daniel Fortin et al.'s SSF paper from 2005]
#' (https://esajournals.onlinelibrary.wiley.com/doi/full/10.1890/04-0953) is an example of a path-based SSF. Hopefully 
#' the figure below helps you visualize the "available" steps being created.  "Used" points or steps are are in black, 
#' while "available" are in gray.
#'
#' <center>
#' <img src="./images/SSF.png" width ="300"><br>
#' </center>
#'
#'
#' Take a look at what we just created above with that monster chunk of code.
## ------------------------------------------------------------------------
print(ssf_2_hr, width = Inf)

#' There seems to be a lot of zeros in the "d_high_human" column, which makes me think something might not be right with 
#' that layer, so I'm just not going to include it in our simple model below.
#'
#' Now that we have available steps, we can do a quick plot of slope for "used" versus available "points"
## ---- warning=F----------------------------------------------------------
ggplot(ssf_2_hr, aes(x = case_, y = slope, fill = case_)) +
  geom_violin() +
  theme_bw() +
  theme(legend.position = "none") +
  ylab("slope (m)") +
  xlab("")

#'
#' Looks like they might barely be selecting lower slopes, which is probably what we'd expect...
#'
#' # Running an SSF model
#'
#' Before running our model, we need to create a "stratum" field, which is a unique identifier for each set of "used" 
#' and "available" points. This is because conditional logistic regression estimates relative probability of use conditioned 
#' on resources available at a given time step. To create this "stratum" field we'll just combine the animal "id" field" with 
#' the "step_id_" field.
#'
## ------------------------------------------------------------------------
ssf_2_hr$stratum <- paste(ssf_2_hr$id, ssf_2_hr$step_id_)

#'
#'
#' We can simplify our dataframe and name it our "raw" dataframe because has unscaled covariate values. We might fit a model 
#' using this dataframe if we were making a predictive map of selection (which is definitely a challenge in an SSF framework)!
## ------------------------------------------------------------------------
ssf_2_hr_raw <-
  ssf_2_hr %>%
  select(id, case_, t2_, elev, slope, d_human, d_high_human, step_id_, stratum)

#'
#' Then we can scale and center our variables so it's easier to interpret selection coefficients across continuous covariates 
#' with different units (in our simple analysis, they're all in meters, but that's not always the case).
## ------------------------------------------------------------------------
ssf_2_hr_scaled <-
  ssf_2_hr_raw %>%
  mutate(
    elev = as.numeric(scale(elev)),
    slope = as.numeric(scale(slope)),
    d_human = as.numeric(scale(d_human)),
    d_high_human = as.numeric(scale(d_high_human))
  )

#'
#' Here's a simple model with three covariates (no distance to high human access). You'll see the "strata" argument in here, 
#' where we tell `clogit` what field has the unique id for matched sets of used and available points.
#' We are using the "cluster" argument in `survival::clogit` to cluster locations by individual id. Mark may talk about this 
#' more, but basically this is a very conservative way to account for non-independence of observations within an individual. 
#' It calculates robust standard errors for coefficient estimates. These robust standard errors are larger than the normal 
#' standard errors. A more rigorous analysis would be to do a mixed effects SSF and have a random effect for individual, but 
#' that's for Mark to teach!
## ------------------------------------------------------------------------
ssf_model <-
  clogit(case_ ~ elev + slope + d_human +
    strata(stratum) + cluster(id), method = "approximate", data = ssf_2_hr_scaled)
summary(ssf_model)

#' You can see the robust standard errors in the summary output.
#'
#' And finally, a very quick and dirty plot of the coefficients using the `sjPlot` package. Note that by default, `plot_model` 
#' is plotting the exponentiated coefficients, so 1 is the cutoff between selection and avoidance, rather than 0. We will plot 
#' the raw coefficients by specifying no transformation:
## ------------------------------------------------------------------------
plot_model(ssf_model, title = "SSF Coefficients", transform = NULL)

#' Obviously, this isn't an exciting model, but you could infer that elk are selecting areas closer to humans (negative 
#' coefficent for distance) and lower slopes, but the elevation results are pretty equivocal. You could try adding quadratic 
#' terms and interactions to represent different hypotheses, but you'd probably want to add some better covariates to make any 
#' stronger inference about elk resource selection with these data.
#'
#'
#'
