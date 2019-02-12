## title: 'WILD 562 Lab5 : Multiple Logisitic Regression & Model Selection'
## author: "Mark Hebblewhite"
## date: "`r format(Sys.time(), '%B %d, %Y')`"

####################################################################
# Lab5: Multiple Logisitic Regression & Model Selectionn

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, repos = "http://cran.us.r-project.org", dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

#load or install these packages:
packages <- c("car", "lattice", "ggplot2", "MASS", "AICcmodavg", "MuMIn", "effects", "corrgram", "GGally", "bootStepAIC", "broom", "plyr", "magrittr")

#run function to install packages
ipak(packages)

## 0.2 Preliminaries: setting working directories and importing data
### Define a text string that defines where your folder is
laptop = "/Users/mark.hebblewhite/Box Sync/Teaching/UofMcourses/WILD562/Spring2019/Labs/lab5"
setwd(laptop)
wolfkde <- read.csv("wolfkde.csv", header=TRUE, sep = ",", na.strings="NA", dec=".")

################################################################################
# Multiple Logistic Regression & Collinearity

## First lets fit Univariate models of these 2 covariates
elev <- glm(used~Elevation2, data =wolfkde, family= binomial(logit))
disthhacc <-  glm(used~DistFromHighHumanAccess2, data =wolfkde, family= binomial(logit))

# Next, fit both in our first multiple logistic regression model
elev.disthhacc <- glm(used~Elevation2 +DistFromHighHumanAccess2 , data =wolfkde, family= binomial(logit))
summary(elev.disthhacc)
# now lets extract coefficients

summary(elev)$coefficients[,1:2]
summary(disthhacc)$coefficients[,1:2]
summary(elev.disthhacc)$coefficients[,1:2]


## lets visually explore differences
disthumanBnp = 0:7000
prDisthhacc <- predict(disthhacc, newdata=data.frame(DistFromHighHumanAccess2=disthumanBnp), type="response")
head(prDisthhacc)

plot(wolfkde$DistFromHighHumanAccess2, wolfkde$used)
lines(disthumanBnp, prDisthhacc, type="l", ylab= "Pr(Used)")

summary(wolfkde$Elevation2)
## ok - lets evaluate the probability of use at 1931 meters from the elev.disthhacc model
meanElev = 1931
prElevMedian.Disthhacc <- predict(elev.disthhacc, newdata=data.frame(DistFromHighHumanAccess2=disthumanBnp, Elevation2=meanElev), type="response")


plot(wolfkde$DistFromHighHumanAccess2, wolfkde$used, xlim=(c(0,10000)))
lines(disthumanBnp, prElevMedian.Disthhacc, type="l", ylab= "Pr(Used)")
lines(disthumanBnp, prDisthhacc, type="l", ylab= "Pr(Used)")

##########
##  Partial Regression Coefficients 
 ## using the pretty() function 
#? pretty
newdata <- expand.grid(Elevation2 = pretty(wolfkde$Elevation2, 5), DistFromHighHumanAccess2 = pretty(wolfkde$DistFromHighHumanAccess2, 10))
head(newdata)
newdata$prElev.Disthha <-predict(elev.disthhacc, newdata, type="response")

ggplot(newdata, aes(x = DistFromHighHumanAccess2, y = prElev.Disthha)) + geom_line() + facet_wrap(~Elevation2)

## Can we hold effects of X1 constant while varying X1?
## Collinearity and Correlations
cor.test(wolfkde$Elevation2, wolfkde$DistFromHighHumanAccess2)
elev_disthha <- lm(DistFromHighHumanAccess2~Elevation2, data=wolfkde)
summary(elev_disthha)

plot(wolfkde$Elevation2,wolfkde$DistFromHighHumanAccess2, type="p")
abline(lm(DistFromHighHumanAccess2~Elevation2, data=wolfkde), col="red")
## Can we hold effects of X1 constant while varying X1?
## showing it both ways

pairs(~Elevation2+DistFromHighHumanAccess2, data=wolfkde, main="Scatterplot Matrix")

##########
## Confounding
summary(elev)$coefficients[,1:2]
summary(disthhacc)$coefficients[,1:2]
summary(elev.disthhacc)$coefficients[,1:2]

## lets test 2 other variables, elk and deer...
deer <- glm(used~deer_w2, data =wolfkde, family= binomial(logit))
elk <-  glm(used~elk_w2, data =wolfkde, family= binomial(logit))

# Next, fit both in our first multiple logistic regression model
deer.elk <- glm(used~deer_w2 + elk_w2, data =wolfkde, family= binomial(logit))

# now lets extract coefficients
summary(deer)$coefficients[,1:2]
summary(elk)$coefficients[,1:2]
summary(deer.elk)$coefficients[,1:2]

cor.test(wolfkde$deer_w2, wolfkde$elk_w2)
plot(wolfkde$deer_w2,wolfkde$elk_w2, type="p")
abline(lm(elk_w2~deer_w2, data=wolfkde), col="red")

##########
#  Screening for Collinearity
## Continuous variables
plot(wolfkde$Elevation2 ,wolfkde$goat_w2, type="p")
abline(lm(goat_w2~Elevation2, data=wolfkde), col="red")
## graphically examining collinearity

## Scatterplot Matrices
pairs(~Elevation2+DistFromHumanAccess2+DistFromHighHumanAccess2, data=wolfkde, main="Scatterplot Matrix")
pairs(~deer_w2+moose_w2+elk_w2+sheep_w2+goat_w2+Elevation2, data=wolfkde, main="Scatterplot Matrix")

### ScatterplotMatrix from Library car
## using car library
scatterplotMatrix(~deer_w2+moose_w2+elk_w2+sheep_w2+goat_w2+Elevation2, data=wolfkde, main="Scatterplot Matrix")

scatterplotMatrix(~Elevation2+DistFromHumanAccess2+DistFromHighHumanAccess2, data=wolfkde, main="Scatterplot Matrix")

scatterplotMatrix(~deer_w2+moose_w2+elk_w2+sheep_w2+goat_w2+Elevation2+DistFromHumanAccess2+DistFromHighHumanAccess2, data=wolfkde, main="Scatterplot Matrix")


### Using Library corrgram
?corrgram()
corrgram(wolfkde[1:9], order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Correlations in the Wolf Data")

corrgram(wolfkde[1:9], order=TRUE, lower.panel=panel.pts,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Correlations in the Wolf Data")

corrgram(wolfkde[1:9], order=TRUE, lower.panel=panel.ellipse,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Correlations in the Wolf Data")

### ggcorr
## using the ggcorr package
ggcorrplot <- ggcorr(wolfkde[1:9], label = TRUE)
ggcorrplot
## GGally package with ggpairs()
ggpairplot<-ggpairs(wolfkde[1:9])
ggpairplot


### Multicollinearity Function

cor.prob <- function(X, dfr = nrow(X) - 2) {
  R <- cor(X, use="complete.obs")
  above <- row(R) < col(R)
  r2 <- R[above]^2
  Fstat <- r2 * dfr / (1 - r2)
  R[above] <- 1 - pf(Fstat, 1, dfr)
  R
}

cor.prob(as.matrix(wolfkde[,c("deer_w2","elk_w2", "moose_w2", "sheep_w2", "goat_w2", "Elevation2", "DistFromHumanAccess2", "DistFromHighHumanAccess2")]))

cor.prob2 <- function(X, dfr = nrow(X) - 2) {
R <- cor(X, use="complete.obs")
above <- row(R) < col(R)
r2 <- R[above]^2
Fstat <- r2 * dfr / (1 - r2)
R[above] <- 1 - pf(Fstat, 1, dfr)
Rstar = ifelse(R[above]<0.05, "***", "NS")
R[above]=paste(R[above],Rstar)
R
}

cor.prob2(as.matrix(wolfkde[,c("deer_w2","elk_w2", "moose_w2", "sheep_w2", "goat_w2", "Elevation2", "DistFromHumanAccess2", "DistFromHighHumanAccess2")]))

### Variance Inflation FActors

?vif()

full.model = glm(used~deer_w2 + elk_w2 +moose_w2 +sheep_w2+goat_w2+Elevation2+DistFromHumanAccess2+DistFromHighHumanAccess2, data =wolfkde, family= binomial(logit))
summary(full.model)

vif(full.model)


## Categorical variables
cor.test(wolfkde$alpine, wolfkde$Elevation2)
cor.test(wolfkde$burn, wolfkde$Elevation2)
cor.test(wolfkde$closedConif, wolfkde$Elevation2)
cor.test(wolfkde$herb, wolfkde$Elevation2)
cor.test(wolfkde$goat_w2, wolfkde$Elevation2)

cor.prob(as.matrix(wolfkde[,c("Elevation2", "DistFromHumanAccess2", "openConif", "closedConif", "modConif", "burn", "herb", "decid", "burn", "alpine")]))

corrgram(wolfkde[c(7, 18:29)], order=TRUE, lower.panel=panel.fill,
upper.panel=panel.pie, text.panel=panel.txt,
main="Landcover Correlations with Elevation")
## so nothing too egregious 

## next human access[8]
corrgram(wolfkde[c(8, 18:29)], order=TRUE, lower.panel=panel.cor,
upper.panel=panel.pie, text.panel=panel.txt,
main="Landcover Correlations with Distance from Human Access")

## again, only issue is Rock and Ice but even then its not a huge effect. 

## we can essentially see this here: 
boxplot(Elevation2~landcov.f, ylab="Elevation (m)", data=wolfkde, las=3)
boxplot(DistFromHumanAccess2~landcov.f, ylab="Elevation (m)", data=wolfkde, las=3)

## Interaction Between Categorical Factors and Continuous
wolfkde$closed = 0
wolfkde$closed <- wolfkde$closedConif + wolfkde$modConif + wolfkde$openConif + wolfkde$decid + wolfkde$mixed + wolfkde$burn
## note I considered burn here as 'closed' - could change. 

wolfkde$closedFactor <-as.factor(wolfkde$closed)

ggplot(wolfkde, aes(x=DistFromHighHumanAccess2, y=used, fill=closedFactor)) + stat_smooth(method="glm", method.args = list(family="binomial"), level=0.95) #+ facet_grid(closed~.)

boxplot(DistFromHighHumanAccess2~closed, ylab="Distance from High Human (m)", data=wolfkde)
cor.test(wolfkde$closed, wolfkde$DistFromHighHumanAccess2)
cor.test(wolfkde$closed, wolfkde$Elevation2)


ggplot(wolfkde, aes(x=Elevation2, y=used, fill=closedFactor)) + stat_smooth(method="glm", method.args = list(family="binomial"), level=0.95) 

disthha.cover <-  glm(used~closed + DistFromHighHumanAccess2 + closed*DistFromHighHumanAccess2, data =wolfkde, family= binomial(logit))
summary(disthha.cover)
boxplot(DistFromHighHumanAccess2~closedFactor, ylab="Distance From High Human Access (m)", data=wolfkde)

#Lets try again with distance to human access

ggplot(wolfkde, aes(x=DistFromHumanAccess2, y=used, fill=closedFactor)) + stat_smooth(method="glm", method.args = list(family="binomial"), level=0.95) #+ facet_grid(closed~.)

#Note, there is a bit stronger evidence of an interaction here (the lines cross)

boxplot(DistFromHumanAccess2~closedFactor, ylab="Distance From High Human Access (m)", data=wolfkde)
distha.cover <-  glm(used~closed + DistFromHumanAccess2 + closed*DistFromHumanAccess2, data =wolfkde, family= binomial(logit))
summary(distha.cover)

################################################################################
# Model Building and Model Selection
################################################################################

## Cleaning Up Missing Data
length(wolfkde$Elevation2)
wolfkde2 <- na.omit(wolfkde)
length(wolfkde2$Elevation2)

##################################################
# Akaike Information Critera
cover <-  glm(used~closed, data =wolfkde2, family= binomial(logit))
## Estimating AIC manually

logLik(cover) ## this is the log likelihood
2*(length(cover$coefficients)) ## this is the number of parameters

## But note that we can calcualte AIC manually using -2* LL + 2*K where LL is logLik and K is # of parameters (without considering the small sample size correction)
-2*as.numeric(logLik(cover))+2*(length(cover$coefficients))
## Note we don't have to do this all manually, in the model str(cover) we see
#str(cover)
cover$aic

## Lets use using AIC to select interactions...
distha <-  glm(used~DistFromHumanAccess2, data =wolfkde2, family= binomial(logit))
distha.cover <-  glm(used~closed + DistFromHumanAccess2, data =wolfkde2, family= binomial(logit)) ## Main effects only

disthaXcover <-  glm(used~closed + DistFromHumanAccess2 + closed*DistFromHumanAccess2, data =wolfkde2, family= binomial(logit))

AIC(cover, distha, distha.cover, disthaXcover)

#Lets redo with distance to high human access

disthha <-  glm(used~DistFromHighHumanAccess2, data =wolfkde2, family= binomial(logit))
disthha.cover <-  glm(used~closed + DistFromHighHumanAccess2, data =wolfkde2, family= binomial(logit)) ## Main effects only
disthhaXcover <-  glm(used~closed + DistFromHighHumanAccess2 + closed*DistFromHighHumanAccess2, data =wolfkde2, family= binomial(logit))

AIC(cover, disthha, disthha.cover, disthhaXcover)

## Stepwise Model Selection
# Lets review the full.model again
full.model = glm(used~deer_w2 + elk_w2 +moose_w2 +sheep_w2+goat_w2+Elevation2+DistFromHumanAccess2+DistFromHighHumanAccess2 +closed + closed*DistFromHighHumanAccess2, data =wolfkde2, family= binomial(logit))

## Backwards selection
stepAIC(full.model, direction = "backward")

top.backwards = glm(used ~ deer_w2 + elk_w2 + moose_w2 + sheep_w2 + goat_w2 + Elevation2 + DistFromHumanAccess2 + DistFromHighHumanAccess2, data=wolfkde2,family=binomial(logit))
summary(top.backwards)


#Next, lets do Forward Stepwise model selection 
{r, include=FALSE}
# Forwards selection - First define a NULL model as the starting place
null.model = glm(used~1,data=wolfkde2,family=binomial(logit))
### This time with output from stepAIC supressed 
stepAIC(null.model, scope=list(upper=full.model, lower= null.model),direction="forward")
## lots of output supressed in Rmarkdown
top.forward <- glm(used ~ Elevation2 + DistFromHighHumanAccess2 + 
                     moose_w2 + elk_w2 + goat_w2 + DistFromHumanAccess2 + sheep_w2 + 
                     deer_w2 + closed, family = binomial(logit), data = wolfkde2)
summary(top.forward)

#Lets screen for collinearity. 
vif(top.forward)
#vif(top.backwards)


full.model.landcov = glm(used~ closedConif +modConif+openConif+decid+regen+mixed+herb+shrub+water+burn+alpine, data =wolfkde2, family= binomial(logit))
stepAIC(full.model.landcov, direction = "backward")

top.model.landcov = glm(used~openConif+modConif+closedConif+mixed+herb+shrub+water+burn, data =wolfkde2, family= binomial(logit))
summary(top.model.landcov)
vif(top.model.landcov)

#  Model selection using the AICcmodavg package
# Biotic Model List

m.biotic <- list()
head(m.biotic)

#lets fit our a-priori list of models 
## Model set 1: Biotic
m.biotic[[1]] <- glm(used ~ 1, family=binomial(logit), data=wolfkde2)
m.biotic[[2]] <- glm(used ~ elk_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[3]] <- glm(used ~ deer_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[4]] <- glm(used ~ moose_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[5]] <- glm(used ~ sheep_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[6]] <- glm(used ~ goat_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[7]] <- glm(used ~ moose_w2 + sheep_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[8]] <- glm(used ~ deer_w2 + sheep_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[9]] <- glm(used ~ elk_w2 + sheep_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[10]] <- glm(used ~ elk_w2 + goat_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[11]] <- glm(used ~ deer_w2 + goat_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[12]] <- glm(used ~ moose_w2 + goat_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[13]] <- glm(used ~ sheep_w2 + goat_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[14]] <- glm(used ~ DistFromHighHumanAccess2, family=binomial(logit), data=wolfkde2)
m.biotic[[15]] <- glm(used ~ DistFromHighHumanAccess2+deer_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[16]] <- glm(used ~ DistFromHighHumanAccess2+moose_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[17]] <- glm(used ~ DistFromHighHumanAccess2+sheep_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[18]] <- glm(used ~ DistFromHighHumanAccess2+goat_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[19]] <- glm(used ~ DistFromHighHumanAccess2+moose_w2 + sheep_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[20]] <- glm(used ~ DistFromHighHumanAccess2+deer_w2 + sheep_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[21]] <- glm(used ~ DistFromHighHumanAccess2+elk_w2 + sheep_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[22]] <- glm(used ~ DistFromHighHumanAccess2+elk_w2 + goat_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[23]] <- glm(used ~ DistFromHighHumanAccess2+deer_w2 + goat_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[24]] <- glm(used ~ DistFromHighHumanAccess2+moose_w2 + goat_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[25]] <- glm(used ~ DistFromHighHumanAccess2+sheep_w2 + goat_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[26]] <- glm(used ~ DistFromHighHumanAccess2, family=binomial(logit), data=wolfkde2)
m.biotic[[27]] <- glm(used ~ DistFromHighHumanAccess2+deer_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[28]] <- glm(used ~ DistFromHighHumanAccess2+moose_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[29]] <- glm(used ~ DistFromHighHumanAccess2+sheep_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[30]] <- glm(used ~ DistFromHighHumanAccess2+goat_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[31]] <- glm(used ~ DistFromHighHumanAccess2+moose_w2 + sheep_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[32]] <- glm(used ~ DistFromHumanAccess2, family=binomial(logit), data=wolfkde2)
m.biotic[[33]] <- glm(used ~ DistFromHumanAccess2+deer_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[34]] <- glm(used ~ DistFromHumanAccess2+moose_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[35]] <- glm(used ~ DistFromHumanAccess2+sheep_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[36]] <- glm(used ~ DistFromHumanAccess2+goat_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[37]] <- glm(used ~ DistFromHumanAccess2+moose_w2 + sheep_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[38]] <- glm(used ~ DistFromHumanAccess2+deer_w2 + sheep_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[39]] <- glm(used ~ DistFromHumanAccess2+elk_w2 + sheep_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[40]] <- glm(used ~ DistFromHumanAccess2+elk_w2 + goat_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[41]] <- glm(used ~ DistFromHumanAccess2+deer_w2 + goat_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[42]] <- glm(used ~ DistFromHumanAccess2+moose_w2 + goat_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[43]] <- glm(used ~ DistFromHumanAccess2+sheep_w2 + goat_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[44]] <- glm(used ~ DistFromHumanAccess2, family=binomial(logit), data=wolfkde2)
m.biotic[[45]] <- glm(used ~ DistFromHumanAccess2+deer_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[46]] <- glm(used ~ DistFromHumanAccess2+moose_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[47]] <- glm(used ~ DistFromHumanAccess2+sheep_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[48]] <- glm(used ~ DistFromHumanAccess2+goat_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[49]] <- glm(used ~ DistFromHumanAccess2+moose_w2 + sheep_w2, family=binomial(logit), data=wolfkde2)


## then name our models .
## note you can name your models with a command like this
# model.names <-  ("null", "disthha", "distacc", "sheepwi", "goatwin", "elkwint", "moosewin", "deerwin") but in this case there were 49 models
model.names.biotic <-c("m0","m1","m2","m3","m4","m5","m6","m7","m8","m9","m10","m11","m12","m13","m14","m15","m16","m17","m18","m19","m20","m21","m22","m23","m24","m25","m26","m27","m28","m29","m30","m31","m32","m33","m34","m35","m36","m37","m38","m39","m40","m41","m42","m43","m44", "m45","m46","m47","m48")
model.names.biotic <-1:49

aictab(cand.set = m.biotic, modnames = model.names.biotic)

## OK so the top model was model 41

top.biotic <- glm(used ~ DistFromHumanAccess2+deer_w2 + goat_w2, family=binomial(logit), data=wolfkde2)
summary(top.biotic)
vif(top.biotic)
## so - not too badly collinear. 


## and the 2nd ranked top biotic model was  model 40
second.biotic <- glm(used ~ DistFromHumanAccess2+elk_w2 + goat_w2, family=binomial(logit), data=wolfkde2)
summary(second.biotic)
vif(second.biotic)


## Model set 2: Environmental Covariates Only

m.env <- list()
head(m.env)

## Model set 1: Biotic
m.env[[1]] <- glm(used ~ 1, family=binomial(logit), data=wolfkde2)
m.env[[2]] <- glm(used ~ Elevation2, family=binomial(logit), data=wolfkde2)
m.env[[3]] <- glm(used ~ DistFromHighHumanAccess2, family=binomial(logit), data=wolfkde2)
m.env[[4]] <- glm(used ~ DistFromHumanAccess2, family=binomial(logit), data=wolfkde2)
m.env[[5]] <- glm(used ~ openConif+modConif+closedConif+mixed+herb+shrub+water+burn, family=binomial(logit), data=wolfkde2)
m.env[[6]] <- glm(used ~ Elevation2 + DistFromHumanAccess2, family=binomial(logit), data=wolfkde2)
m.env[[7]] <- glm(used ~ DistFromHighHumanAccess2 + openConif+modConif+closedConif+mixed+herb+shrub+water+burn, family=binomial(logit), data=wolfkde2)
m.env[[8]] <- glm(used ~ DistFromHumanAccess2 + openConif+modConif+closedConif+mixed+herb+shrub+water+burn, family=binomial(logit), data=wolfkde2)
m.env[[9]] <- glm(used ~ Elevation2 + openConif+modConif+closedConif+mixed+herb+shrub+water+burn, family=binomial(logit), data=wolfkde2)
m.env[[10]] <- glm(used ~ Elevation2 + DistFromHumanAccess2 + openConif+modConif+closedConif+mixed+herb+shrub+water+burn, family=binomial(logit), data=wolfkde2)
m.env[[11]] <- glm(used ~ Elevation2 + DistFromHighHumanAccess2 + openConif+modConif+closedConif+mixed+herb+shrub+water+burn, family=binomial(logit), data=wolfkde2)
m.env[[12]] <- glm(used ~ Elevation2 + DistFromHighHumanAccess2 + closed + closed*DistFromHighHumanAccess2, family=binomial(logit), data=wolfkde2)
m.env[[13]] <- glm(used ~ Elevation2 + DistFromHumanAccess2 + closed + closed*DistFromHumanAccess2, family=binomial(logit), data=wolfkde2)
m.env[[14]] <- glm(used ~ DistFromHighHumanAccess2 + closed + closed*DistFromHighHumanAccess2, family=binomial(logit), data=wolfkde2)
m.env[[15]] <- glm(used ~ DistFromHumanAccess2 + closed + closed*DistFromHumanAccess2, family=binomial(logit), data=wolfkde2)


model.names.env <-1:15

aictab(cand.set = m.env, modnames = model.names.env)

#OK - top model is model 11
top.env <- glm(used ~ Elevation2 + DistFromHighHumanAccess2 + openConif+modConif+closedConif+mixed+herb+shrub+water+burn, family=binomial(logit), data=wolfkde2)
summary(top.env)
vif(top.env)


Now - which 'set' of covariates is best? Env? or Biotic?
  
  
AIC(top.env, top.biotic)

## Environmental model HANDS DOWN. 

## now go back and compare 'top' model to top model selected by AIC

AIC(top.forward, top.biotic, second.biotic, top.env)


################################################################################
  # Model Selection using the MuMIn Package
  
 # re-run FULL logistic regression model
top.forward = glm(used ~ deer_w2 + elk_w2 + moose_w2 + sheep_w2 + goat_w2 + Elevation2 + DistFromHumanAccess2 + DistFromHighHumanAccess2 + closed + DistFromHighHumanAccess2*closed, data=wolfkde2,family=binomial(logit), na.action ="na.fail")
summary(top.forward)

#install and load MuMIn package
require(MuMIn)

#use dredge function to get all possible models
x1<-dredge(top.forward)

## x1 - looking at all 
## there are over 1000 models fit! 10! models = ? models
## note dredge has fit XX models in total out of this candidate set of 19 candidate variables

head(x1, n = 10) ## only shows top 10 models fit
plot(x1)
x2<- subset(x1, delta < 2)
plot(x2)

#get top models with AICc <2
top.models<-get.models(x1, subset=delta<2)
#top.models

#model average covariate effects
x6<-model.avg(top.models)
summary(x6)

## next lets do landcover 
top.dredge.lc = glm(used ~ openConif+modConif+closedConif+mixed+herb+shrub+water+burn+decid+regen+alpine, data=wolfkde2,family=binomial(logit), na.action ="na.fail")
x2<-dredge(top.dredge.lc)
head(x2, n=10)
plot(x2)

######################################################################
## Manipulating Coefficients from Dredge
coefficients(top.models[[1]])

top.model.coef <- lapply(top.models, coefficients)
str(top.model.coef)

#require(plyr)
ldply(top.models, function(l) as.data.frame(t(coefficients(l))))

summary(top.models[[1]])$coefficients

tidyList1<- ldply(top.models, function(l) as.data.frame(t(summary(l)$coefficients[,4])))
head(tidyList1)

#There are your p-values.  

#Next we will use the `broom` package, which has a `tidy` command which might automate everything for you.  Check this out:
  
  
require(broom)
tidy(top.models[[1]])
tidyList2 <- ldply(top.models, tidy)
head(tidyList2)

require(magrittr)
(CI.table <- ldply(top.models, tidy) %>%  mutate(CI.low = estimate - 2*std.error, CI.high = estimate + 2*std.error))

ggplot(mutate(CI.table, model = .id), aes(model, estimate)) + 
  geom_errorbar(aes(ymin = CI.low, ymax = CI.high)) + 
  facet_wrap(.~term, scales = "free_y") + theme(axis.text.x = element_text(angle = 90))


################################################################################
#  Model Selection using BIC

## First manually
AIC(top.forward, top.biotic, second.biotic, top.env)
BIC(top.forward, top.biotic, second.biotic, top.env)
## OK - so not much difference in top models using BIC and AIC in this dataset



## BIC with Dredge
Now lets use the dredge function with BIC

x1.bic<-dredge(top.forward, rank=BIC) ## note this now ranks using BIC
plot(x1.bic)

## x1.bic - look at all 

head(x1.bic, n = 10) ## only shows top 10 models fit
# lets compare the top model from AIC and BIC
head(x1.bic, n = 1) ## only shows top 1 models fit with BIC
head(x1, n = 1) ## only shows top 1 models fit with AIC


#So AIC is overfitting here potentially, selecting a model with 11 parameters versus 7 parameters with BIC. Take note.  This is a theme. 

Next, lets take a look at the top models. 

#get top models with BIC <2
top.models.bic<-get.models(x1.bic, subset=delta<2)
top.models.bic 

Note there is only 1 top model here using BIC, thus, there is no need to model average covariate effects using this command. 

x.top.bic<-model.avg(top.models.bic) ## only 1 top model, so this doesnt work

Lets run the 'top' model selected using BIC for next week. 

## Lets run the 'top' model selected using BIC for next week
top.model.bic = glm(used ~ DistFromHighHumanAccess2 + DistFromHumanAccess2+Elevation2+elk_w2+goat_w2+moose_w2, data=wolfkde2,family=binomial(logit), na.action ="na.fail")
summary(top.model.bic)
## compare to top AIC model
summary(top.forward)

################################################################################
# Variable reduction using PCA - Principle Components Analysis
head(wolfkde2)
pcawolf <-princomp(na.omit(wolfkde2[1:9]), cor=TRUE)
summary(pcawolf)
loadings(pcawolf)
plot(pcawolf, type="lines")
biplot(pcawolf, xlim =c(-0.06, 0.04))

wolfkde2$Comp.1 <- -0.406*wolfkde2$deer_w2 - 0.370*wolfkde2$moose_w2 - 0.402*wolfkde2$elk_w2 +0.182*wolfkde2$goat_w2 - 0.415*wolfkde2$wolf_w2 + 0.408*wolfkde2$Elevation2 + 0.318*wolfkde2$DistFromHumanAccess2 + 0.233*wolfkde2$DistFromHighHumanAccess2

wolf_comp1 <- glm(used ~ Comp.1, family=binomial (logit), data=wolfkde2)
wolfkde2$fitted1 <- fitted(wolf_comp1)
hist(wolfkde2$fitted1)
plot(wolfkde2$fitted1, wolfkde2$Comp.1)

figPCA <- ggplot(wolfkde2, aes(x=Comp.1, y=used)) + stat_smooth(method="glm", method.args = list(family="binomial"))
x.axis = "-0.41*deer - 0.37*moose - 0.4*elk +0.18*goat - 0.42*wolf + 0.41*Elev + 0.32*DistHum + 0.23*DistHighHum"
figPCA2 <- figPCA + xlab(x.axis)
figPCA2

#The label on the Axis now explains the problem with PCA and why its such a bugbear. How do you explain component 1 to a manager?
  
######################################################################
# Caterpillar plots of coefficients
  
require(ggplot2)

# run logistic regression model
summary(full.model)

B<-summary(full.model)$coefficient[1:length(summary(full.model)$coefficient[,1]),1]
#create margin of error (ME) for 95% CI
ME <- summary(full.model)$coefficient[1:length(summary(full.model)$coefficient[,1]),2]*1.96
lower<-B - ME
upper<-B + ME


# bundle into data frame
logisData<-data.frame(B, lower, upper, names(summary(full.model)$coefficient[,2]))
names(logisData) <- c("Coefficient", "lower.ci", "upper.ci", "Variable")
levels(logisData$Variable)[1] <- "Intercept"
logisData$Variable <- relevel(logisData$Variable, ref="Intercept")

pd <- position_dodge(0.6) # move them .05 to the left and right
x1<-ggplot(data=logisData, aes(x=Variable,y=Coefficient)) +
  geom_errorbar(data=logisData,aes(ymin=lower.ci, ymax=upper.ci), width=.4,position=pd,size=1) +
  geom_point(size=3, col="blue") 

p6<-x1+theme(axis.text.y = element_text(size=14, family="Times"),axis.text.x = element_text(size=14, family="Times"),text = element_text(size=16, family="Times"),axis.title.x=element_text(size=16, family="Times"),axis.title.y=element_text(size=16, family="Times",vjust=1))
p7<-p6+theme(axis.line.x = element_line(color="black", size = 0.25),
             axis.line.y = element_line(color="black", size = 0.25),legend.title=element_blank(),legend.text=element_text(size=16, family="Times"))+ylab("Estimate") + xlab("Coefficient") #+ theme(axis.text.x = element_text(angle = 90))

p7

tiff("coefPlot.tiff", res=600, compression = "lzw", height=5, width=7, units="in")
p7
dev.off()



##  8.1  GGally package -  
Alternatively, check out the cool GGally package!!
  
require(GGally)
ggcoef(full.model)


