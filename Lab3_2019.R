##WILD 562 Lab 3 Logistic Regression"
##author: "Mark Hebblewhite"
## date: "January 29 2019"

  ## 0.1 Preliminaries: getting started, loading packages, setting working directory
#function to install and load required packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

#load or install these packages:
packages <- c("ggplot2","foreign", "lattice", "psych", "effects", "plyr")

#run function to install packages
ipak(packages)
setwd("/Users/mark.hebblewhite/Box Sync/Teaching/UofMcourses/WILD562/Spring2019/Labs/lab3")

## 0.2 Preliminaries: Merging the wolf availabiltiy sample from the KDE's from last week. 
rdavail <- as.data.frame(cov.availRD)
str(rdavail)
rdavail$pack <- c("Red Deer")
str(rdused)

## repeat for Bow Valley pack
bvavail <- as.data.frame(cov.availBV)
str(bvavail)
bvavail$pack <- c("Bow Valley")
str(bvavail)

# merge the two availability samples together
wolfavail <- rbind(rdavail, bvavail)
str(wolfavail)

# and for next week, lets add a new column for a 1=used 0 = avail
#wolfavail$used <- 0

write.table(wolfavail, file = "/Users/mark.hebblewhite/Dropbox/WILD 562/Spring2017/lab2/new/wolfavail.csv", row.names=FALSE, na="", col.names=TRUE, sep=",")


## Objective 1.0 Merging wolf USED and wolf AVAIL datasets

wolfused <-read.csv("wolfused.csv", header = TRUE)
wolfavail <-read.csv("wolfavail.csv", header = TRUE)
str(wolfavail)

wolfkde <- rbind(wolfused, wolfavail)
str(wolfkde)
table(wolfkde$used, wolfkde$pack)
table(wolfkde$used, wolfkde$deer_w2)

## next we will create a new variable called usedFactor and graphically compare USED and AVAIL locations for prey
wolfkde$usedFactor <- factor(wolfkde$used, labels=c('0','1'))
str(wolfkde)

### Objective 1.1. Graphical data exploration for all wolves


par(mfrow = c(2,3))
boxplot(deer_w2~usedFactor, data=wolfkde, main = "Deer Used-Avail", xlab="usedFactor", ylab="deer")
boxplot(elk_w2~usedFactor, main = "Elk Used-Avail", ylab="elk_w2", xlab="usedFactor", data=wolfkde)
boxplot(moose_w2~usedFactor, main = "Moose Used-Avail", ylab="moose_w2", xlab="usedFactor", data=wolfkde)
boxplot(goat_w2~usedFactor, main = "Goat Used-Avail", ylab="goat_w2", xlab="usedFactor", data=wolfkde)
boxplot(sheep_w2~usedFactor, main = "Sheep Used-Avail", ylab="sheep_w2", xlab="usedFactor", data=wolfkde)

par(mfrow = c(1,2))
boxplot(Elevation2~usedFactor, data=wolfkde, main = "Elevation Used-Avail", xlab="usedFactor", ylab="deer")
boxplot(DistFromHumanAccess2~usedFactor, data=wolfkde, main = "Human Access Used-Avail", ylab="elk_w2", xlab="usedFactor")

## subset for Bow Valley Pack
bvkde<- subset(wolfkde, subset=pack =="Bow Valley")
par(mfrow = c(2,3))
boxplot(deer_w2~usedFactor, data=bvkde, main = "Deer Used-Avail", xlab="usedFactor", ylab="deer")
boxplot(elk_w2~usedFactor, data=bvkde, main = "Elk Used-Avail", ylab="elk_w2", xlab="usedFactor")
boxplot(moose_w2~usedFactor, data=bvkde, main = "Moose Used-Avail", ylab="moose_w2", xlab="usedFactor")
boxplot(goat_w2~usedFactor, data=bvkde, main = "Goat Used-Avail", ylab="goat_w2", xlab="usedFactor")
boxplot(sheep_w2~usedFactor, data=bvkde, main = "Sheep Used-Avail", ylab="sheep_w2", xlab="usedFactor")
## Now lets do for Elevation and Distance from Human Access2
par(mfrow = c(1,2))
boxplot(Elevation2~usedFactor, data=bvkde, main = "Elevation Used-Avail", xlab="usedFactor", ylab="deer")
boxplot(DistFromHumanAccess2~usedFactor, data=bvkde, main = "Human Access Used-Avail", ylab="elk_w2", xlab="usedFactor")

## subset for Red Deer Wolf
rdkde <- subset(wolfkde, subset=pack=="Red Deer")
table(rdkde$used, rdkde$pack)
par(mfrow = c(2,3))
boxplot(deer_w2~usedFactor, data=rdkde, main = "Deer Used-Avail", xlab="usedFactor", ylab="deer")
boxplot(elk_w2~usedFactor, data=rdkde, main = "Elk Used-Avail", ylab="elk_w2", xlab="usedFactor")
boxplot(moose_w2~usedFactor, data=rdkde, main = "Moose Used-Avail", ylab="moose_w2", xlab="usedFactor")
boxplot(goat_w2~usedFactor, data=rdkde, main = "Goat Used-Avail", ylab="goat_w2", xlab="usedFactor")
boxplot(sheep_w2~usedFactor, data=rdkde, main = "Sheep Used-Avail", ylab="sheep_w2", xlab="usedFactor")
## Now lets do for Elevation and Distance from Human Access2
par(mfrow = c(1,2))
boxplot(Elevation2~usedFactor, data=rdkde, main = "Elevation Used-Avail", xlab="usedFactor", ylab="deer")
boxplot(DistFromHumanAccess2~usedFactor, data=rdkde, main = "Human Access Used-Avail", ylab="elk_w2", xlab="usedFactor")

par(mfrow = c(1,1))
boxplot(Elevation2~used+pack, data = wolfkde, main = "Bow Valley vs. Red Deer Elevation Used-Avail")
boxplot(DistFromHumanAccess2~used+pack, data = wolfkde, main = "Bow Valley vs. Red Deer Human Access Used-Avail")
boxplot(deer_w2~used+pack, data = wolfkde, main = "Bow Valley vs. Red Deer Elevation Used-Avail")
boxplot(moose_w2~used+pack, data = wolfkde, main = "Bow Valley vs. Red Deer Elevation Used-Avail")
boxplot(elk_w2~used+pack, data = wolfkde, main = "Bow Valley vs. Red Deer Elevation Used-Avail")
boxplot(goat_w2~used+pack, data = wolfkde, main = "Bow Valley vs. Red Deer Elevation Used-Avail")
boxplot(sheep_w2~used+pack, data = wolfkde, main = "Bow Valley vs. Red Deer Elevation Used-Avail")

## using lattice package
bwplot(sheep_w2+ goat_w2 + elk_w2+moose_w2+ deer_w2~as.factor(usedFactor)|pack, data = wolfkde, layout = c(2,5), pch = "|", outer = TRUE)

## Objective 1.2 - Numerical Summary Statistics
## numerical summary statistics
aggregate(wolfkde[1:8], by=list(wolfkde$pack, wolfkde$used), FUN=mean, na.rm=TRUE)

# psych package
describeBy(wolfkde, wolfkde$pack)
tapply(wolfkde$pack, wolfkde$used, summary)

### Introduction to the plyr package

ddply(wolfkde, c("pack", "used"), summarize, mean=mean(elk_w2, na.rm=TRUE), sd=sd(elk_w2, na.rm=TRUE))
ddply(wolfkde, c("pack", "used"), summarize, mean=mean(deer_w2, na.rm=TRUE), sd=sd(deer_w2, na.rm=TRUE))
ddply(wolfkde, c("pack", "used"), summarize, mean=mean(goat_w2, na.rm=TRUE), sd=sd(goat_w2, na.rm=TRUE))
ddply(wolfkde, c("pack", "used"), summarize, mean=mean(moose_w2, na.rm=TRUE), sd=sd(moose_w2, na.rm=TRUE))
ddply(wolfkde, c("pack", "used"), summarize, mean=mean(sheep_w2, na.rm=TRUE), sd=sd(sheep_w2, na.rm=TRUE))
ddply(wolfkde, c("pack", "used"), summarize, mean=mean(Elevation2, na.rm=TRUE), sd=sd(Elevation2, na.rm=TRUE))
ddply(wolfkde, c("pack", "used"), summarize, mean=mean(DistFromHumanAccess2, na.rm=TRUE), sd=sd(DistFromHumanAccess2, na.rm=TRUE))

## Graphical exploration with ggplot2

wolfyht <-read.csv("wolfyht.csv", header = TRUE)
str(wolfyht)
# download the ggplot2 pdf manual and make sure you have the R Graphics Cookbook open too
# chapter 6 in R Graphics cookbook
# Data exploration
ggplot(wolfyht, aes(x=EASTING, y = NORTHING)) + geom_point() + stat_density2d() + facet_grid(Pack ~ ., scales="free")
# lets subset the data
ggplot(wolfyht, aes(x=EASTING, y = NORTHING)) + geom_point() + stat_density2d(aes(alpha=..density..), geom="tile", contour=FALSE) + facet_grid(Pack ~ .)
ggplot(wolfyht, aes(x=EASTING, y = NORTHING)) + geom_point() + stat_density2d(aes(fill=..density..), geom="raster", contour=FALSE)

# 2. Univariate Logistic Regression
 
## Objective 2.1 First Simulating Binomial Distribution Data to Understand the Binomial Model
x = c(-50:50) ## just creating a uniform vector from -50 to 50. 
y = rbinom(length(x), 1, plogis(1+0.07*x) )
## unpack this line by ?rbinom
## and ? plogis

plot( y ~ x)
abline(lm((y~x)))
wrong = lm(y~x)
summary(wrong)
res = glm( y~x, family=binomial(link="logit"))
summary(res)

yLogit=predict(res)
plot( yLogit ~ x )
yhat=predict(res, type="response")
plot( y ~ x)
lines(yhat~x)


## Objective 2.1 Univariate logistic regression with glm
#?glm
elev <- glm(used ~ Elevation2, family=binomial(logit), data=wolfkde)
summary(elev)
str(elev)
## exploring univarite logistic regression
## how to obtain 95% confidence intervals? Where are they in the output?
## CI's using profile log-likelihood's
confint(elev)
## CI's using standard errors
confint.default(elev)

## odds ratio's
exp(coefficients(elev))
## how to obtain 95% CI's on odds ratio's
exp(cbind(OR=coef(elev), confint(elev)))

## rescaling beta coefficients and odds ratio's 
## note that for elevation, the change in odds is for every 1 meter change in elevation. Perhaps this is not useful.

wolfkde$elev100 <- wolfkde$Elevation2 / 100
elev100 <- glm(used ~ elev100, family=binomial(logit), data=wolfkde)
summary(elev100)
exp(coef(elev100))
## therefore the interpretation of the odds ratio is scale dependent
## Unpacking logistic regression - interpreting coefficients
## excercise in EXCEL (argh!)
## repeat EXCEL excercise in R. 
elevBnp = 0:3000 ## creates a new vector elevBnp with ranges from 0 - 3000 in it. 

#Lets make sure we undestand the structure of the logistic regression model we just estimated using the str() command
'str(elevBnp)'

elevPred = predict(elev, newdata=data.frame(Elevation2=elevBnp), type = "response") ## uses the predict function to predict Y values given the model object elev
hist(elevPred)
plot(elevBnp, elevPred, type="l", ylim = c(0,1.0), ylab= "Pr(Used)")
# but were there elevations from 0 - 1300m in Banff?
plot(wolfkde$Elevation2, wolfkde$used)
lines(elevBnp, elevPred, type="l", ylab= "Pr(Used)", add = TRUE)


  ## Objective 2.2 Interpreting Coefficients in Logistic Models
 
## next human use
distHuman <- glm(used ~ DistFromHumanAccess2, family=binomial(logit), data=wolfkde)
summary(distHuman)
hist(wolfkde$DistFromHumanAccess2)
disthumanBnp = 0:7000
disthumanPred = predict(distHuman, newdata=data.frame(DistFromHumanAccess2=disthumanBnp), type="response")
hist(disthumanPred)
plot(disthumanBnp, disthumanPred, type="l", ylab= "Pr(Used)")
plot(wolfkde$DistFromHumanAccess2, wolfkde$used)
lines(disthumanBnp, disthumanPred, type="l", ylab= "Pr(Used)", add = TRUE)

# now lets do all at once for ungulate HSI models
sheep <- glm(used ~ sheep_w2, family=binomial(logit), data=wolfkde)
summary(sheep)
habvalues = 0:7
deer <- glm(used ~ deer_w2, family=binomial(logit), data=wolfkde)
summary(deer)
elk <- glm(used ~ elk_w2, family=binomial(logit), data=wolfkde)
summary(elk)
moose <- glm(used ~ moose_w2, family=binomial(logit), data=wolfkde)
summary(moose)
goat <- glm(used ~ goat_w2, family=binomial(logit), data=wolfkde)
summary(goat)

habvalues = 0:7 ## making a vector of hsi values
sheeppred = predict(sheep, newdata = data.frame(sheep_w2 = habvalues), type = "response")
goatpred = predict(goat, newdata = data.frame(goat_w2 = habvalues), type = "response")
moosepred = predict(moose, newdata = data.frame(moose_w2 = habvalues), type = "response")
elkpred = predict(elk, newdata = data.frame(elk_w2 = habvalues), type = "response")
deerpred = predict(deer, newdata = data.frame(deer_w2 = habvalues), type = "response")
sheeppred = predict(sheep, newdata = data.frame(sheep_w2 = habvalues), type = "response")

plot(habvalues, elkpred, type ="l", ylim = c(0,1.0), ylab = "Pr(Used)", col = "green")
lines(habvalues, goatpred, col = "blue")
lines(habvalues, moosepred, col = "red") 
lines(habvalues, sheeppred, col = "black") 
lines(habvalues, deerpred, col = "gray") 
legend(x="topleft", legend= c("Elk","Mountain Goat", "Moose", "Sheep", "Deer"), lty=1, col = c("green", "blue", "red", "black", "gray"), bty = "n")

## back to elevation
elev <- glm(used ~ Elevation2, family=binomial(logit), data=wolfkde)
summary(elev)
wolfkde$fitted.Elev <- fitted(elev)
head(wolfkde)
hist(wolfkde$fitted.Elev)
plot(wolfkde$fitted.Elev, wolfkde$Elevation2)

# ggplot 2 explore basic histogram functio
ggplot(wolfkde, aes(x=wolfkde$fitted.Elev)) + geom_histogram()
# lets explore faceting
ggplot(wolfkde, aes(x=wolfkde$fitted.Elev)) + geom_histogram(binwidth=0.05, fill="gray70", colour="black") + facet_grid(used ~ .)
ggplot(wolfkde, aes(x=wolfkde$fitted.Elev)) + geom_histogram(binwidth=0.05, fill="gray70", colour="black") + facet_grid(used ~ ., scales = "free")
ggplot(wolfkde, aes(x=wolfkde$fitted.Elev, fill=usedFactor)) + geom_histogram(binwidth=0.05, position="identity", alpha=0.7) + xlab("Predicted Probability of Wolf Use") + theme(axis.title.x=element_text(size=16))

# lets redo this graph using faceting by pack
ggplot(wolfkde, aes(x=wolfkde$fitted.Elev, y=..density.., fill=usedFactor)) + geom_histogram(binwidth=0.05, position="identity", alpha=0.7) + xlab("Predicted Probability of Wolf Use") + theme(axis.title.x=element_text(size=16)) + facet_grid(pack ~ ., scales="free")


# Now lets explore fitting functions to the distributions
ggplot(wolfkde, aes(x=wolfkde$fitted.Elev)) + geom_density()
ggplot(wolfkde, aes(x=wolfkde$fitted.Elev), fill=usedFactor) + geom_density(alpha=0.5) + xlim(0,1)+xlab("Predicted Probability of Wolf Use") + theme(axis.title.x=element_text(size=16)) 
# kernel lines
ggplot(wolfkde, aes(x=wolfkde$fitted.Elev, y=..density.., fill=usedFactor)) + geom_histogram(binwidth=0.05) + geom_density(alpha = 0.5) + facet_grid(pack ~ .)


## Plotting Logistic Regresson with ggplot2
ggplot2 has a great handy function called + stat_smooth for plotting logistic regression. 

# Exploring Predictions as a function of covariates
#___________
# this fits a univariate glm as a function of elevation and predicts
ggplot(wolfkde, aes(x=Elevation2, y=used)) + geom_point() + stat_smooth(method="glm", method.args = list(family="binomial"))

#
ggplot(wolfkde, aes(x=DistFromHumanAccess2, y=used)) + geom_point() + stat_smooth(method="glm", method.args = list(family="binomial"))
ggplot(wolfkde, aes(x=elk_w2, y=used)) + geom_point() + stat_smooth(method="glm", method.args = list(family="binomial"))
## but whats up with the dots? - lets jitter and see
ggplot(wolfkde, aes(x=elk_w2, y=used)) + geom_point() +geom_jitter(aes(colour = used), width=0.25, height = 0.05) + stat_smooth(method="glm", method.args = list(family="binomial"))


## lets redo elevation jittered by used
ggplot(wolfkde, aes(x=Elevation2, y=used)) + geom_point() +geom_jitter(aes(colour = used), width=0.25, height = 0.05)+ stat_smooth(method="glm", method.args = list(family="binomial"))

# Splitting by wolf pack and change the confidence interval to 99th
ggplot(wolfkde, aes(x=Elevation2, y=used, colour=pack)) + geom_point() + stat_smooth(method="glm", method.args = list(family="binomial"), level=0.99)

ggplot(wolfkde, aes(x=Elevation2, y=used, colour=pack)) + geom_point() + geom_jitter(width=0.25, height = 0.05) +stat_smooth(method="glm", method.args = list(family="binomial"), level=0.90)


ggplot(wolfkde, aes(x=moose_w2, y=used, colour=pack)) + stat_smooth(method="glm", method.args = list(family="binomial"))

ggplot(wolfkde, aes(x=sheep_w2, y=used, colour=pack)) + stat_smooth(method="glm", method.args = list(family="binomial"))

# versus faceting by wolf pack
ggplot(wolfkde, aes(x=Elevation2, y=used)) + geom_point() + stat_smooth(method="glm", method.args = list(family="binomial"), level=0.90) + facet_grid(pack~.)

# this function plots predictions from the previously fitted best model
ggplot(wolfkde, aes(x=Elevation2, y=fitted.Elev)) + geom_point() + stat_smooth(method=lm) + ylim(0, 0.8)

plot(effect("Elevation2", elev), grid=TRUE) 
plot(effect("deer_w2", deer), grid=TRUE)
## but note the scales are stretched to linearize the response

## Objective 3.3 Saving graphics 

#Printing PDFs from R
pdf("wolf_elev.pdf", width=4, height=4)
print(ggplot(wolfkde, aes(x=Elevation2, y=used)) + geom_point(colour="gray") + stat_smooth(method="glm", method.args = list(family="binomial")) + xlab("Prey H.S.I") + ylab("Predicted Probability of Wolf Use"))
dev.off()
# then go and look in the active directory for wolf_elev.pdf
#or
ggsave("elev_wolf2.pdf", width=4, height=4)
#

fig3<-ggplot(wolfkde, aes(x=elk_w2, y=used)) + geom_smooth(data = wolfkde, aes(x=elk_w2, y=used, col="Elk"),method="glm", method.args = list(family="binomial")) + geom_smooth(data = wolfkde, aes(x=deer_w2, y=used, col="Deer"),method="glm", method.args = list(family="binomial"))+ geom_smooth(data = wolfkde, aes(x=moose_w2, y=used, col="Moose"),method="glm", method.args = list(family="binomial"))+ geom_smooth(data = wolfkde, aes(x=sheep_w2, y=used, col="Sheep"),method="glm", method.args = list(family="binomial"))+ geom_smooth(data = wolfkde, aes(x=goat_w2, y=used, col="Goat"),method="glm", method.args = list(family="binomial")) + xlab("Relative probability of summer prey resource selection") + ylab("Relative probability of summer wolf resource selection") + theme(axis.title.y=element_text(size=18), axis.text.y=element_text(size=18)) + theme(axis.title.x=element_text(size=18), axis.text.x=element_text(size=18))+ labs(fill="Prey Species")
## lets save this
pdf("fig3.pdf", width=4, height=4)
fig3
dev.off()
