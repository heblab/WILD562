# ------------------------------------------------------------------------------
# Tidy code snippets for lab 4
# ------------------------------------------------------------------------------

#' Here are a few ways to simplify code as you are working through the lab
#' again. Hopefully there are some new tricks for you in here!

# load magrittr (or dplyr) so we can use the pipe opperator
require(magrittr)         

# Lets start with the wolfkde CSV we produced in lab 
wolfkde <- readr::read_csv("new/wolfkde.csv")

# and create a copy that we won't change
wolfkde2 <- wolfkde


# Adding a column with factor names ---------------------------------------------

#' In the lab we added labels back using a series of ifelse statements which
#' works but isn't very easy to follow

wolfkde$habitatType = 
  ifelse(wolfkde$landcover16 == 0, "NA", 
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

#' dplyr offers a vectorized version of the ifelse function: case_when. 

#' This isn't anything magical, but my impression is that most are not familiar 
#' with this function - and its a lot cleaner!

tmp <- 
  wolfkde2 %>%                                        # pass data
  dplyr::mutate(                                      # add column
    habitatType = dplyr::case_when(                   # "HabitatType"
      landcover16 == 0 ~ "NA",                          # defined by case_when
      landcover16 == 1 ~ "Open Conifer",
      landcover16 == 2 ~ "Moderate Conifer",
      landcover16 == 3 ~ "Closed Conifer",
      landcover16 == 4 ~ "Deciduous",
      landcover16 == 5 ~ "Mixed",
      landcover16 == 6 ~ "Regen",
      landcover16 == 7 ~ "Herbaceous",
      landcover16 == 8 ~ "Shrub",
      landcover16 == 9 ~ "Water",
      landcover16 == 10 ~ "Rock-Ice",
      landcover16 == 11 ~ "Cloud",
      landcover16 == 12 ~ "Burn-Forest",
      landcover16 == 13 ~ "Burn-Grassland",
      landcover16 == 14 ~ "Burn-Shrub",
      landcover16 == 15 ~ "Alpine Herb",
      landcover16 == 16 ~ "Alpine Shrub"
    )
  ) 

# Now lets compare the columns we created!
identical(wolfkde$habitatType, tmp$habitatType)       # prints TRUE if identical


# Dummy indicators -------------------------------------------------------------

# In lab we manually created dummy columns for each variable
wolfkde$closedConif = ifelse(wolfkde$habitatType == "Closed Conifer", 1, 0)
wolfkde$modConif = ifelse(wolfkde$habitatType == "Moderate Conifer", 1, 0)
wolfkde$openConif = ifelse(wolfkde$habitatType == "Open Conifer", 1, 0)
wolfkde$decid = ifelse(wolfkde$habitatType == "Deciduous", 1, 0)
wolfkde$regen = ifelse(wolfkde$habitatType == "Regen", 1, 0)
wolfkde$mixed = ifelse(wolfkde$habitatType == "Mixed", 1, 0)
wolfkde$herb = ifelse(wolfkde$habitatType == "Herbaceous", 1, 0)
wolfkde$shrub = ifelse(wolfkde$habitatType == "Shrub", 1, 0)
wolfkde$water = ifelse(wolfkde$habitatType == "Water", 1, 0)
wolfkde$rockIce = ifelse(wolfkde$habitatType == "Rock-Ice", 1, 0)
wolfkde$burn = ifelse(wolfkde$habitatType == "Burn-Grassland", 1, ifelse(wolfkde$habitatType == "Burn-Shrub", 1, ifelse(wolfkde$habitatType == "Burn-Forest", 1,0 )))
wolfkde$alpineHerb = ifelse(wolfkde$habitatType == "Alpine Herb", 1, 0)
wolfkde$alpineShrub = ifelse(wolfkde$habitatType == "Alpine Shrub", 1, 0)
# alpine col that we use later in the model calls
wolfkde$alpine = wolfkde$alpineHerb + wolfkde$alpineShrub


# ... but we can also do it automatically using model.matrix!

# first lets get the name of each variable
var_names <- sort(unique(wolfkde$habitatType))        # var names

# create dummy vars
dummy <- 
  wolfkde %>%                                         # original data
  model.matrix(~habitatType - 1, data = .) %>%        # create model matrix
  magrittr::set_colnames(var_names)                   # rename cols w/ var_names

# create dataframe for models
df <- 
  cbind(used = wolfkde$used, dummy) %>%               # add 'used' column back
  tibble::as_tibble() %>%                             # cnvrt to tibble
  print()                                             # print combined df

# Tidy model results -----------------------------------------------------------

# lets run a couple random models form the lab so we have something to tidy
oc.intercept.model = glm(used~closedConif + modConif + decid+ regen+mixed+herb+water+rockIce+burn+alpine, data = wolfkde, family = binomial(logit))
rockintercept.alpine.model = glm(used~closedConif + openConif + modConif + decid+ regen+mixed+herb+water+burn+alpine, data = wolfkde, family = binomial(logit))

# Here is the code for a summary table from lab.
rockintercept.alpine.model.df <- data.frame(summary(rockintercept.alpine.model)$coefficients[,1:2])
oc.intercept.model.df <- data.frame(summary(oc.intercept.model)$coefficients[,1:2])
coef.table <- rbind(rockintercept.alpine.model.df,oc.intercept.model.df)
coef.table$habitatType <- c(row.names((summary(rockintercept.alpine.model)$coefficients[,1:2])),row.names(summary(oc.intercept.model)$coefficients[,1:2]))
coef.table$habitatType[1] <- "rockIce"
coef.table$habitatType[12] <- "openConif"
coef.table$model <-c(rep("Open Conif Intercept",11),rep( "RockIce Intercept",11))  
# NOTE: the names are backwards here and in the original lab code ^
coef.table


#' Alternatively, we can create this with a lot less work using broom::tidy()
#' This function takes a lot of different model outputs as the argument and 
#' converts them into a standardized dataframe - cool!

mod1 <- 
  oc.intercept.model %>%                              # model output
  broom::tidy() %>%                                   # 'tidy' results
  dplyr::mutate(model = "Open Conif Intercept")       # add model column

mod2 <-  
  rockintercept.alpine.model %>%                      # model output
  broom::tidy() %>%                                   # 'tidy' results
  dplyr::mutate(model = "RockIce Intercept")          # add model column

dplyr::bind_rows(mod1, mod2) %>%                      # bind rows
  dplyr::select(-statistic, -p.value) %>%             # remove extra cols
  print(n = 22)                                       # print entire df

#' looks a lot like the table we created above! With the exception of the
#' additional habitatType column Mark used to explicitely define the intercept.

#' Note that there is also an error in the lab code and the model names are
#' backwards! Therefore the model results here appear flipped.
 