#####################################################################################

#### WILD 562 - Lab 6: Evaluating RSF models

##http://neondataskills.org/R/Raster-Data-In-R/

#####################################################################################
## Preliminaries: 0.0 Installing Packages

#function to install and load required packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

#load or install these packages:
packages <- c("sp", "raster", "plyr", "ks", "adehabitatHR", "maptools", "rgdal", "sp", "raster","ggplot2","colorRamps","rgeos", "VGAM", "AICcmodavg", "MuMIn", "effects", "corrgram", "GGally","caret", "DescTools", "car")

#run function to install packages
ipak(packages)

########################################################################################################
##### 0.1 Preliminaries: setting working directory #######################################################################

## define working directory on each computer
marksComputer <- "/Users/mark.hebblewhite/Box Sync/Teaching/UofMcourses/WILD562/Spring2019/Labs/lab6" 

## automatically set working directory depending which computer you're on
setwd(marksComputer) 

##### 0.2 Saving and loading data and shapefile data of Kernel home range from Lab 2#######################################################################

wolfkde2 <- read.csv("wolfkde.csv", header=TRUE, sep = ",", na.strings="NA", dec=".")
wolfkde3 <-na.omit(wolfkde2)
wolfkde3$usedFactor <-as.factor(wolfkde3$usedFactor) ## make sure usedFactor is a factor
# head(wolfkde3)
length(wolfkde3$used)

#source("/Users/mark.hebblewhite/Dropbox/WILD 562/Spring2017/lab4/new/Lab2NeededforLab5.R", verbose = FALSE)
# plot(homerangeALL)
#writeOGR(homerangeALL, dsn=wd_laptop, layer = "homerangeALL", driver = "ESRI Shapefile", overwrite_layer=TRUE)

kernelHR <- readOGR(dsn=wd_laptop, "homerangeALL")
plot(kernelHR)
extent(kernelHR)
kernels <- raster()
extent(kernels) <- c(xmin=546836, xmax=612093, ymin=5662036, ymax=5748911) 	

########################################################################################################
##### 0.3 Loading raster's needed for mapping RSF models later ######################################################################

setwd("/Users/mark.hebblewhite/Dropbox/WILD 562/Spring2017/lab6/rasters") ## wherever your files are
#list.files()
deer_w<-raster("deer_w2.tif")
moose_w<-raster("moose_w2.tif")
elk_w<-raster("elk_w2.tif") # already brought in above
sheep_w<-raster("sheep_w2.tif")
goat_w<-raster("goat_w2.tif")
wolf_w<-raster("wolf_w2.tif")
elevation2<-raster("Elevation2.tif") #resampled
disthumanaccess2<-raster("DistFromHumanAccess2.tif") #resampled in lab 4
disthhu2<-raster("DistFromHighHumanAccess2.tif") #resampled in lab 4
landcover2 <- raster("landcover2.tif") ## resampled to same extent as lab 4
## but note we need to repopulate the fields with the habitat legend information
landcover2@data@values <- getValues(landcover2)

## note that the extents are all different for human access and elc_habitat-derived layers, so need to recreate a new extent
#create an empty raster
mask.raster <- raster()

#set extent (note that I customized this extent so it covered both elc_habitat and humanacess)
extent(mask.raster) <- c(xmin=443680.6, xmax=650430.4, ymin=5618405, ymax=5789236) 	

#set the resolution to 30 m 
res(mask.raster)<-30

#match projection to elc_habitat shapefile
projection(mask.raster)<- "+proj=utm +zone=11 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

#set all values of mask.raster to zero
mask.raster[]<-0

########################################################################################################### 0.3 Creating 'dummy' variable landcover rasters for use in mapping later ######################################################################

# duplicate mask.raster in the creation of new 'dummy' rasters
alpine <- mask.raster
burn <- mask.raster
closedConif <- mask.raster
herb <- mask.raster
mixed <- mask.raster
rockIce <- mask.raster
water <- mask.raster
modConif <- mask.raster
decid <- mask.raster

#set values for empty rasters based on landcover2 values of Habitat classification variables
alpine@data@values <- ifelse(landcover2@data@values== 15 | landcover2@data@values == 16, 1, ifelse(is.na(landcover2@data@values)==T,NA,0))
burn@data@values <- ifelse(landcover2@data@values == 12 | landcover2@data@values == 13 | landcover2@data@values == 14, 1, ifelse(is.na(landcover2@data@values)==T,NA,0))
closedConif@data@values <- ifelse(landcover2@data@values == 3, 1, ifelse(is.na(landcover2@data@values)==T,NA,0))
herb@data@values <- ifelse(landcover2@data@values == 7, 1, ifelse(is.na(landcover2@data@values)==T,NA,0))
mixed@data@values <- ifelse(landcover2@data@values == 5, 1, ifelse(is.na(landcover2@data@values)==T,NA,0))
rockIce@data@values <- ifelse(landcover2@data@values == 10, 1, ifelse(is.na(landcover2@data@values)==T,NA,0))
water@data@values <- ifelse(landcover2@data@values == 9, 1, ifelse(is.na(landcover2@data@values)==T,NA,0))
modConif@data@values <- ifelse(landcover2@data@values == 2, 1, ifelse(is.na(landcover2@data@values)==T,NA,0))
decid@data@values <- ifelse(landcover2@data@values == 10, 1, ifelse(is.na(landcover2@data@values)==T,NA,0))
plot(rockIce)
plot(kernelHR, add=TRUE)
plot(closedConif)
plot(kernelHR, add=TRUE)
# note that open conifer as intercept

########################################################################################################
##### 0.3 Creating a raster stack ## not really needed this lab. 

#stack raster layers (i.e., create raster stack for sampling; must have same extent and resolution)
all_rasters<-stack(deer_w, moose_w, elk_w, sheep_w, goat_w, wolf_w,elevation2, disthumanaccess2, disthhu2, landcover2, alpine, burn, closedConif, modConif, herb, mixed, rockIce, water, decid)

plot(all_rasters) ## note limit of plotting 9 layers

#names = c("deer_w", "moose_w", "elk_w", "sheep_w", "goat_w", "wolf_w","elevation2", "disthumanaccess2", "disthhu2", "landcover2", "alpine", "burn", "closedConif", "modConif", "herb", "mixed", "rockIce", "water", "decid")

#writeRaster(all_rasters,"/Users/mark.hebblewhite/Box Sync/Teaching/UofMcourses/WILD562/Spring2019/Labs/lab6/rasterstack/lab6Stack.tif", bylayer = TRUE,suffix = 'names', format="GTiff")


########################################################################################################
##### 1.0 Evaluating Predictions from Competing Models

##### Running the 'top' models from last week

## top Biotic model was model 41
top.biotic <- glm(used ~ DistFromHumanAccess2+deer_w2 + goat_w2, family=binomial(logit), data=wolfkde3)
summary(top.biotic)
#double check the VIF for each final model, just to be sure
vif(top.biotic)

# Environmental Model - top model is model 11
top.env <- glm(used ~ Elevation2 + DistFromHighHumanAccess2 + openConif+modConif+closedConif+mixed+herb+shrub+water+burn, family=binomial(logit), data=wolfkde3)
summary(top.env)
vif(top.env)

# Calculate model selection AIC table
require(AICcmodavg)
models = list(top.biotic, top.env)
modnames = c("envfinal", "bioticmodel")
aictab(models, modnames)

##### 1.1 Evaluating predictions from residual plots for top.env model

# First review here in Lab http://www.statmethods.net/stats/rdiagnostics.html for Linear Regression (Normal regression)
par(mfrow = c(2,2))
plot(top.env)

## this cylces through residual plots which look decidedly different from 'normal' residual plots

##### Saving predictions manually, an example with the environment model
wolfkde3$fitted.top.env <- fitted(top.env)
#### this is the predicted probability from the model

wolfkde3$residuals.top.env <- residuals(top.env)
## these are the deviations from the predictions for each row (data point)

wolfkde3$rstudent.top.env <- rstudent(top.env)
## This is a standardized residual - the studentized residual

wolfkde3$hatvalues.top.env <- hatvalues(top.env)
#### this is the first of the leverage statistics, the larger hat value is, the bigger the influence on the fitted value

wolfkde3$cooks.distance.top.env <- cooks.distance(top.env)
#### this is the Cooks leverage statistic, the larger hat value is, the bigger the influence on the fitted value

wolfkde3$obsNumber <- 1:nrow(wolfkde3) ## just added a row number for plotting

## Making manual residual verus predicted plots
plot(wolfkde3$fitted.top.env, wolfkde3$residuals.top.env)

ggplot(wolfkde3, aes(fitted.top.env, residuals.top.env)) + geom_point() + geom_text(aes(label = obsNumber, colour = used))
## Manual residual plot

ggplot(wolfkde3, aes(wolfkde3$residuals.top.env, wolfkde3$cooks.distance.top.env)) + geom_point() + geom_text(aes(label = obsNumber, colour = used))
## shows us some points at high cooks values that might be having a big influence

ggplot(wolfkde3, aes(wolfkde3$cooks.distance.top.env, wolfkde3$hatvalues.top.env)) + geom_point() + geom_text(aes(label = obsNumber, colour = used))
## shows us some points at high cooks values that might be having a big influence
## this helps identify some locations that have high leverage that are used (1 - blue) and available (0=black) points

## e.g., datapoint 16
wolfkde3[16,]
## is a red deer wolf used point at high elevtion in open conifer far from human access. 
## Does not seem to be a data entry error - a REAL data point


#Evaluating model fit graphically
# first lets make a plot of Y against X predictions...
plot(wolfkde3$Elevation2, wolfkde3$fitted.top.env)

##### Another way of looking at it
scatterplot(fitted.top.env~Elevation2, reg.line=lm, smooth=TRUE, spread=TRUE, boxplots='xy', 
            span=0.5, xlab="elevation", ylab="residual", cex=1.5, cex.axis=1.4, cex.lab=1.4, 
            data=wolfkde3)

hist(wolfkde3$fitted.top.env, scale="frequency", breaks="Sturges", col="darkgray")

##### This plot is VERY important - it is the predicted probability of a location being a wolf used location given your top model
##### But how would we divide this into wolf habitat and wolf available? 

ggplot(wolfkde3, aes(x=wolfkde3$fitted.top.env, fill=usedFactor)) + geom_histogram(binwidth=0.05, position="identity", alpha=0.7) + xlab("Predicted Probability of Wolf Use") + theme(axis.title.x=element_text(size=16)) #+ facet_grid(pack ~ ., scales="free")

#### This plot shows that somewhere around 0.25 - 0.40 eyeballing it it looks like we could 'cut' used and available points? 

ggplot(wolfkde3, aes(x=fitted.top.env, y=..density.., fill=usedFactor)) + geom_histogram(binwidth=0.05, position="identity", alpha=0.7) + xlab("Predicted Probability of Wolf Use") + theme(axis.title.x=element_text(size=16)) + facet_grid(pack ~ ., scales="free")
#### But note this 'cut' point looks different for both wolf packs?

##### This introduces the basic problem of dividing continuous predictions from logistic regression into categories of habitat (1) and available (0)

##### 1.2 Evaluating predictions from residual plots for top.biotic model
par(mfrow = c(2,2))
plot(top.biotic)

##### Saving predictions manually, an example with the environment model
wolfkde3$fitted.top.biotic <- fitted(top.biotic)
#### this is the predicted probability from the model

wolfkde3$residuals.top.biotic <- residuals(top.biotic)
## these are the deviations from the predictions for each row (data point)

wolfkde3$rstudent.top.biotic <- rstudent(top.biotic)
## This is a standardized residual - the studentized residual

wolfkde3$hatvalues.top.biotic <- hatvalues(top.biotic)
#### this is the first of the leverage statistics, the larger hat value is, the bigger the influence on the fitted value

wolfkde3$cooks.distance.top.biotic <- cooks.distance(top.biotic)
#### This isthe Cooks leverage statistic


## Making manual residual verus predicted plots
plot(wolfkde3$fitted.top.biotic, wolfkde3$residuals.top.biotic)

ggplot(wolfkde3, aes(wolfkde3$cooks.distance.top.biotic, wolfkde3$hatvalues.top.biotic)) + geom_point() + geom_text(aes(label = obsNumber, colour = used))
## this helps identify some locations that have high leverage that are used (1 - blue) and available (0=black) points

## e.g., datapoint 16
wolfkde3[13,]
## is a red deer wolf used point at high elevtion in open conifer far from human access that is being classified as an AVAILABLE location. 
wolfkde3[30,]
## another high wolf used point that is being classified as an AVAILABLE location

#Evaluating model fit graphically
# first lets make a plot of Y against X predictions...
plot(wolfkde3$Elevation2, wolfkde3$fitted.top.biotic)

##### Another way of looking at it
scatterplot(fitted.top.biotic~Elevation2, reg.line=lm, smooth=TRUE, spread=TRUE, boxplots='xy', 
            span=0.5, xlab="elevation", ylab="residual", cex=1.5, cex.axis=1.4, cex.lab=1.4, 
            data=wolfkde3)

hist(wolfkde3$fitted.top.biotic, scale="frequency", breaks="Sturges", col="darkgray")


hist(wolfkde3$fitted.top.biotic, scale="frequency", breaks="Sturges", col="darkgray")

##### This plot is VERY important - it is the predicted probability of a location being a wolf used location given your top model
##### But how would we divide this into wolf habitat and wolf available? 

ggplot(wolfkde3, aes(x=wolfkde3$fitted.top.biotic, fill=usedFactor)) + geom_histogram(binwidth=0.05, position="identity", alpha=0.7) + xlab("Predicted Probability of Wolf Use") + theme(axis.title.x=element_text(size=16)) #+ facet_grid(pack ~ ., scales="free")

#### This plot shows that somewhere around 0.25 - 0.40 eyeballing it it looks like we could 'cut' used and available points? 

ggplot(wolfkde3, aes(x=fitted.top.biotic, y=..density.., fill=usedFactor)) + geom_histogram(binwidth=0.05, position="identity", alpha=0.7) + xlab("Predicted Probability of Wolf Use") + theme(axis.title.x=element_text(size=16)) + facet_grid(pack ~ ., scales="free")
#### But note this 'cut' point looks different for both wolf packs?

##### This introduces the basic problem of dividing continuous predictions from logistic regression into categories of habitat (1) and available (0)

#### 1.3. Comparing the 'fit' of the biotic and environmental models

ggplot(wolfkde3, aes(x=fitted.top.biotic, y=fitted.top.env, fill = pack)) + geom_point() + stat_smooth(method="lm")

## there is quite a bit of scatter here in the predictions between the two models, but in general, they are highly correlated

ggplot(wolfkde3, aes(x=fitted.top.biotic, y=fitted.top.env, fill = pack)) + geom_point() + stat_smooth()
## but some evidence that the top environmental model is not succesfulyl predicting the 'best' wolf habitat especially in the bow valley wolf pack compared to the top biotic model


########################################################################################################
##### 2.0 Classification Tables from top models 

#https://www.r-bloggers.com/evaluating-logistic-regression-models/
#https://rpubs.com/ryankelly/ml_logistic 

## 2.1 Pseuod R-squared 
require(DescTools)
PseudoR2(top.biotic, c("McFadden", "CoxSnell", "Nagel"))

# 2.2 Classification table for top biotic  model
# First we will arbitrarily define the cutpoint between 1 and 0's using p = 0.5
ppused = wolfkde3$fitted.top.biotic>0.5
table(ppused,wolfkde3$used)

### now go through Hosmer and Lemeshow chapter 5 to calculate our classification success for 0's?
167/(167+229)
## so when wolf telemetry locations were known = 1, the model classified 42% as 'used'. This is pretty terrible?
## This is also called the Specificity of a Model

1655/(1655+67)
## But when the points were really 0's, available, we classified them 96% of the time correctly.
## This is called the Sensitivity of a model 

## Now ltes do this manually using cutpoints of 0.25 and 0.1
ppused = wolfkde3$fitted.top.biotic>0.25
table(ppused,wolfkde3$used)

#### Now, what is specificity? (i.e., the probability of classifying the 1's correctly?)
304/(304+92)
#### about 76% - Great! But - what happened to our sensitivity (i.e., the probability of classifying the 0's correctly?)
1376 / (1376+346)
#### so our probability of classifying 0's correctly decreases to ~ 80% with our sensitivity

### now lets try a p = 0.10
ppused = wolfkde3$fitted.top.biotic>0.10
table(ppused,wolfkde3$used)
#### Now, what is specificity? (i.e., the probability of classifying the 1's correctly?)
357/(357+39)
#### about 90% - Great! But - what happened to our sensitivity (i.e., the probability of classifying the 0's correctly?)
1001 / (1001+721)
#### so our probability of classifying 0's correctly decreases with our sensitivity

require(caret)

wolfkde3$pr.top.biotic.used <- ifelse(wolfkde3$fitted.top.biotic>0.5, 1, 0)
xtab1<-table(wolfkde3$pr.top.biotic.used, wolfkde3$used)
xtab1

#?confusionMatrix
confusionMatrix(xtab1)

### This reveals a LOT of information - lets compare these values to the table in the ? confusionMatrix help file, and in the lab manual. 

## Excercise - redo for top enviornmental model

########################################################################################## 3.0 ROC curves

# https://www.r-bloggers.com/illustrated-guide-to-roc-and-auc/
  
# ROCR package help here: https://rocr.bioinf.mpi-sb.mpg.de 

## Sensitivity and Specificity

#Remember that sensitivity is the True Positive Rate, or, the classification success of 1's when they are truly 1. And that Specificity is the Ture Negative rate. 1 - TNR is known as the False Negative Rate, something we also need to think of. 

require(ROCR)
pp = predict(top.biotic,type="response")
pred = prediction(pp, wolfkde3$used)

perf3 <- performance(pred, "sens", x.measure = "cutoff")
plot(perf3)

#Look at what happens to our Sensitivity as we change the cutoff value. Remember that sensitivity is the True Positive Rate, or, the classification success of 1's when they are truly 1. Looking at the graph, we see that we classify everything as a wolf used location when the cutoff is really low. 

#Next, lets examine the relationship between the cutoff value and Specificity, or, the True Negative Rate - the rate we correctly classify 0's.  
perf4 <- performance(pred, "spec", x.measure = "cutoff")
plot(perf4)
#Similarly, if we use a really low threshold cutoff value between 0's and 1's, we see that we have the lowest Specificity - because basically we are calling everything a 1, and misclassifiying the true 0's. As the cutoff increases, we see that there is a sharp increase in our Specificity, approaching 100% of all 0's correctly classified by a cutoff of about 0.5. 

#Obviously, in most cases we want to maximize both Sensitivity and Specificity for a model with what could be called the 'optimal' cutpoint. 

## Estimating the Optimal Cutpoint
#Next, we will calculate the Maximum for the sum of sensitivity and specificity to calculate the optimal cutpoint probability. 

perfClass <- performance(pred, "tpr","fpr") # change 2nd and/or 3rd arguments for other metrics
fpr <- perfClass@x.values[[1]]
tpr <- perfClass@y.values[[1]]
sum <- tpr + (1-fpr)
index <- which.max(sum)
cutoff <- perfClass@alpha.values[[1]][[index]]
cutoff
#Thus the cutpoint that maximizes the overall classification succes is 0.236. Note that this is VERY different from our naive starting value of 0.5. 

#Now, lets overlay the sensitivity, specificity, and optimal cutoff curves together. 
plot(perf3, col="blue") # Sensitivity
plot(perf4, add = TRUE) # Specificity
abline(v=cutoff, col="red") ## optimal cutpoint

## ROC Plot 
#Now we will put the TPR and FPR (1 - Specificity) together to estimate the Receiver Operating Characteristic Curve (ROC plot). Receiver Operating Characteristic(ROC) summarizes the model’s performance by evaluating the trade offs between true positive rate (sensitivity) and false positive rate(1- specificity). For plotting ROC, it is advisable to assume p > 0.5 since we are more concerned about success rate. ROC summarizes the predictive power for all possible values of p > 0.5.  The area under curve (AUC), referred to as index of accuracy(A) or concordance index, is a perfect performance metric for ROC curve. Higher the area under curve, better the prediction power of the model. Below is a sample ROC curve. The ROC of a perfect predictive model has TP equals 1 and FP equals 0. This curve will touch the top left corner of the graph.

plot(perfClass)
abline(a=0, b= 1)

#This plot shows the trade off between the True Positive Rate versus the False Positive Rate for our top model. 

#Next, we will proceed to calculate the area under the curve, or, the AUC. 
BMauc <- performance(pred, measure="auc") 
str(BMauc)
auc <- as.numeric(BMauc@y.values)
auc

#This is the sum of the area under the predicted performance curve we just plotted, showing that about ~ 86% of the time, we are correctly classifying the 1's.  But this does not capture the 0's.  For this, we need to look at the entire ROC plot. 

plot(perfClass, colorize = T, lwd = 5, print.cutoffs.at=seq(0,1,by=0.1),
     text.adj=c(1.2,1.2),
     main = "ROC Curve")
text(0.5, 0.5, "AUC = 0.867")
abline(v=cutoff, col = "red", lwd = 3)

#Another cost measure that is popular is overall accuracy. 
acc.perf = performance(pred, measure = "acc")
plot(acc.perf)


## Manually Changing Cutoff Values
### now lets try a p = of our cutoff
ppused = wolfkde3$fitted.top.biotic>cutoff
table(ppused,wolfkde3$used)
#### Now, what is specificity? (i.e., the probability of classifying the 1's correctly?)
320/(320+76)
#### about 80% - Great! But - what happened to our sensitivity (i.e., the probability of classifying the 0's correctly?)
1344 / (1344+378)
#### so our probability of classifying 0's correctly is about 78%
#We see that the trade off between TPR and FPR at the optimal cutpoint leads to a much higher rate of TPR, 1's, but, at the expense of a reduced rate of TNR, or, the true negative rates. 

#Lets look at the confusion matrix now for the optimal cutpoint. 
wolfkde3$pr.top.biotic.used2 <- ifelse(wolfkde3$fitted.top.biotic>cutoff, 1, 0)
xtab2<-table(wolfkde3$pr.top.biotic.used2, wolfkde3$used)
xtab2

#?confusionMatrix
confusionMatrix(xtab2)

#Now lets use this cutoff to classify used and avail locations into 1 and 0's, and make a plot of where this cutoff is using geom_vline() in ggplot
## this is our best model classifying used and avail locations into 1 and 0's. 
ggplot(wolfkde3, aes(x=wolfkde3$fitted.top.biotic, fill=usedFactor)) + geom_histogram(binwidth=0.05, position="identity", alpha=0.7) + xlab("Predicted Probability of Wolf Use") + theme(axis.title.x=element_text(size=16)) + geom_vline(xintercept = cutoff, col="red")
#This graph shows the optimal cutpoint based on our data, and illustrates the problem of confusion and asymmetry between the 0's and 1's. 

#Finally, this next step calculates the default expected prevalence of 1's and 0's in our sample; compare that to the optimal cutpoint. 
table(wolfkde3$used)
396/(1722+396)

##### 3.2 Evaluating the top Environmental Model - on your own. 


###################### 4.0 k-folkds cross validation

##### We are going to load the custom function kxv.R from source
##### See the kxv.R details for information about this function. It was basically programmed for Boyce for the 2002 paper

source("/Users/mark.hebblewhite/Dropbox/WILD 562/Spring2017/lab6/kxv.R", verbose = FALSE)

##### 4.1 evaluating the top Environmental Model

# Kfolds with a 'fuzz' factor
kxvPrintFlag=FALSE
kxvPlotFlag=TRUE
kxvFuzzFactor = 0.01
kfolds = kxvglm(top.env$formula, data=wolfkde3, k=5, nbin=10)
kfolds

##### These values tell you the spearman rank correlation between every subset of the data 1 - 5 and the predicted correlation between the # of observations in ranked categories of habitat from 1, 10. 

##### But note that this K-folds cross validation does not address the structure of the data within wolf packs
##### NExt step is to subset by wolf pack and see how well the overall wolf model predicts wolf use by both wolf packs
# Kfolds by each pack with a 'fuzz' factor
kxvPrintFlag=FALSE
kxvPlotFlag=TRUE
kxvFuzzFactor = 0.01
kfolds2 = kxvglm(top.env$formula, data=wolfkde3, k=5, nbin=10, partition="pack")
kfolds2

##### So the answer is that the overall pooled model predicts the Bow Valley pack really well, but fails to predict the Red Deer pack any better, essentially, than random

# Manual k-folds cross-validation

# Vector of random "folds"
wolfkde3$rand.vec = sample(1:5,nrow(wolfkde3),replace=TRUE)

#Run the model for random subset of the data
top.env.1= glm(used ~ Elevation2 + DistFromHighHumanAccess2 + openConif+modConif+closedConif+mixed+herb+shrub+water+burn, family=binomial(logit), data=wolfkde3, subset=rand.vec==1)

#Make predictions for points not used to fit the model.
pred.prob = predict(top.env.1,newdata=wolfkde3[wolfkde3$rand.vec!=1,],type="response")

#Make quantiles for the predictions
q.pp = quantile(pred.prob,probs=seq(0,1,.1))


bin = rep(NA,length(pred.prob))
for (i in 1:10){
	bin[pred.prob>=q.pp[i]&pred.prob<q.pp[i+1]] = i
}

used1 = wolfkde3$used[wolfkde3$rand.vec!=1]

rand.vec.1.table <- table(used1,bin)
rand.vec.1.table
cor.test(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), c(0,2,0,8,6,15,24,50,99,110), method="spearman") 
## which suggests that in this random fold of the data, the model predicted habitat use well



########################################################################################################
##### 5.0 Mapping spatial predictions of 'top' model
par(mfrow = c(1,1))


###### 5.1 Easy spatial predictions
ggplot(wolfkde3, aes(EASTING, NORTHING, col = fitted.top.biotic)) + geom_point(size=5) + coord_equal() +  scale_colour_gradient(low = 'yellow', high = 'red')
ggplot(wolfkde3, aes(EASTING, NORTHING, col = fitted.top.env)) + geom_point(size=5) + coord_equal() +  scale_colour_gradient(low = 'yellow', high = 'red')


#### 5.2 Raster predictions for top Biotic Model
par(mfrow = c(1,1))
summary(top.biotic)

#> top.biotic$coefficients
#(Intercept) DistFromHumanAccess2              deer_w2              goat_w2 
#-3.553037526         -0.001421547          0.898069385         -0.333539833 

biotic.coefs <- top.biotic$coefficients[c(1:4)]
names(all_rasters)

rast.top.biotic <- exp(biotic.coefs[1] + biotic.coefs[2]*disthumanaccess2 + biotic.coefs[3]*deer_w + biotic.coefs[4]*goat_w) / (1 +exp(biotic.coefs[1] + biotic.coefs[2]*disthumanaccess2 + biotic.coefs[3]*deer_w + biotic.coefs[4]*goat_w ))
## need to use the names of the raster layers we brought in up above. Note that they are not the same names as stored in the Raster stack

# lets bring in the wolfyht shapefile to overlap to also aid our model evaluation
wolfyht<-shapefile("/Users/mark.hebblewhite/Box Sync/Teaching/UofMcourses/WILD562/Spring2019/Labs/lab6/Materials/wolfyht.shp")

# plot predicted raster
plot(rast.top.biotic, col=colorRampPalette(c("yellow", "orange", "red"))(255))
plot(rast.top.biotic, col=colorRampPalette(c("yellow", "orange", "red"))(255), ext=kernels)
plot(kernelHR, add=TRUE)
plot(wolfyht, col='blue', pch = 16, add=TRUE)

#look at histogram of predicted values
#hist(rast.top.biotic@data@values)  # for some reasons I kept getting errors here.  Try it yourself. 


bv.raster<-raster()
extent(bv.raster) <- c(xmin=570000, xmax=600000, ymin=5665000, ymax=5685000) 
plot(rast.top.biotic, col=colorRampPalette(c("yellow", "orange", "red"))(255), ext=bv.raster)
plot(kernelHR, add=TRUE)
plot(wolfyht, col='blue', pch = 16, add=TRUE)


Lets zoom in to a specific area in the Red Deer Pack, and examine how the spatial predictions are performing. 

##
rd.raster<-raster()
extent(rd.raster) <- c(xmin=540000, xmax=600000, ymin=5700000, ymax=5730000) 
plot(rast.top.biotic, col=colorRampPalette(c("yellow", "orange", "red"))(255), ext=rd.raster)
plot(kernelHR, add=TRUE)
plot(wolfyht, col='blue', pch = 16, add=TRUE)


#### To do 5.2  Making maps in 10 equal area quantiles based JUST on the deciles of the AVAILABILITY points
##### not the USED points. Hmm. How to do. 


#### To do 5.3 Extrapolation

## what we have just done is make an extrapolation beyond the realm of observed data that went into the model by predicting to the entire extent of the rasters
## How to make a prediction for JUST the extent of homerangeALL
## 2 ways to do it - make a new prediction just within the raster with extent = homerangeALL
### or?



### To do 5.4 - Mapping JUST the numerator from the RSF from a Used-Available Design - and then comparing with some random points 

rast.top.biotic.RSF <- exp(biotic.coefs[1] + biotic.coefs[2]*disthumanaccess2 + biotic.coefs[3]*deer_w + biotic.coefs[4]*goat_w) 
plot(rast.top.biotic.RSF, col=colorRampPalette(c("yellow", "orange", "red"))(255), extent=kernels)

### how to compare the predictions quantitatively between 'maps' - could easily do it in the dataframe just on the data ,but... nice to generate random points in R here?


### To do 5.5 Redo for top Environmental Model

summary(top.env)

env.coefs <- top.env$coefficients[c(1:11)]
env.coefs

rast.top.env <- exp(env.coefs[1] + env.coefs[2]*elevation2 + env.coefs[3]*disthumanaccess2 + env.coefs[4]*openConifer + env.coefs[5]*modConif + env.coefs[6]*closedConif + env.coefs[7]*mixed + env.coefs[8]*herb + env.coefs[9]*shrub + env.coefs[10]*water + env.coefs[11]*burn) /
  (1 + exp(env.coefs[1] + env.coefs[2]*elevation2 + env.coefs[3]*disthumanaccess2 + env.coefs[4]*openConifer + env.coefs[5]*modConif + env.coefs[6]*closedConif + env.coefs[7]*mixed + env.coefs[8]*herb + env.coefs[9]*shrub + env.coefs[10]*water+ env.coefs[11]*burn))

