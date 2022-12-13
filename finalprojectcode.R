## code for final project, working with my data

#survival will be a logistic regression, 1 or 0 alive or died
#test for multicollinearity?

all.krla<-read.csv("mainkrla.csv")


head(all.krla)
library(tidyverse)
library(dplyr)

##making it so that i work with the populations
#converting chr to factor (with 15 levels)
class(all.krla$population)
all.pop <- as.factor(all.krla$population)
tray <- as.factor(all.krla$tray)

## extracting the other columns into easier names
pot<-(all.krla$pot)
emerged <-(all.krla$emerged.N.Y)
hours.emerged <-(all.krla$hours.to.emerge)
survived <- (all.krla$survived.N.Y)
lat <- (all.krla$Latitude)
long <- (all.krla$Longitude)
elevation <- (all.krla$Elevation)
precip <- (all.krla$ppt)
tmin <- (all.krla$tmin)
tmax <- (all.krla$tmax)
tmean <-(all.krla$tmean)

##make a data frame with  data i need
df <-data.frame(tray, pot, all.pop, emerged, hours.emerged, survived, long, lat, elevation, precip, tmean, tmin, tmax)
summary(df)

## emergence assumption 1: checking to see if data are normally distributed--------
hist(hours.emerged~all.pop) ## not normally distributed
hist((log(hours.emerged)~all.pop)) ## a bit more normally distributedd 

qqnorm(hours.emerged) ##results: data in highly non normal, need to use non-parametric tests
qqnorm(log(hours.emerged))
qqline((log(hours.emerged)),col = "steelblue", lwd = 2) ## better, more linear
loghours<-log(hours.emerged)

shapiro.test(hours.emerged) ## results: reject null and therefore there is a difference btw my distribution and a normal distribution

#emergence assumptions 2: equal variance? need to nonparametric test bc non-normal data
library(car)
leveneTest(hours.emerged~all.pop) ## results: variances among populations are not equal


  #plot hours.to.emerge ~ population
plot(hours.emerged~ all.pop, xlab="Populations", ylab = "Hours to emergence") #there are clear outliers


## is there a multicollinearity problem with predictor variables?? not doing GLMM on emergence anymore
library(car)
pairs(df[,c("elevation", "lat", "long", "precip", "tmean")])

mod<- lm(hours.emerged~elevation+lat+long+precip+tmean)
car::vif(mod)## very high values, try to take out elevation

mod2<-lm(hours.emerged~lat+long+precip+tmean)
car::vif(mod2) ## taking out elevation, there is no multicollinearity problem. 


## question 1: do populations of KRLA differ in emergence? ------

 ## is the mean hours to emergence for each population different? Run anova to compare means
kruskal.test(hours.emerged~all.pop) ## yes all populations differ in emergence
library(FSA)
dt <- dunnTest(hours.emerged~all.pop, method = "bh")
print(dt, dunn.test.results = TRUE) ##### cathy ID the strongest pairwise comparrsions in report

  ## is survival different for each population?  ### i dont think an anova is best for binomial data?
kruskal.test(survived~all.pop) ## all pops differ in their survival. does this method work?
dt2 <- dunnTest(survived~all.pop, method = "bh")
print(dt2, dunn.test.results = TRUE)


### question 2.1 : do spatial/envionmental gradients affect survival?------

###### Logistic regression ---- for survival------
#assumptions: 
  #response variable is binary -yes
  #independent observations -yes
  #little to no mulitcolinarity - take out elevation

## checking for multicollinearity 
allmodel<- glm(survived~lat+long+precip+elevation+tmean, family = binomial(link = "logit"), data = df)
summary(allmodel)
car::vif(allmodel) ## very large VIF's

allmodel2<- glm(survived~lat+long+precip+tmean, family = binomial(link = "logit"), data = df)
summary(allmodel2)
car::vif(allmodel2) ## much better!! all below 4

  ## longitude
par(mfrow=c(2,2))

model <- glm(survived~long, family = binomial(link = "logit"), data = df) 
summary(model) ## median is not around 0...and it should be?

nd <- data.frame(long=seq(-130,-110),length=100)
mypred <- predict(model, newdata = nd, type = "response", se.fit = T)
plot(survived~long, xlab= "Longitude", ylab = "Survived", main="A")

lines(nd$long,mypred$fit,col="blue")
lines(nd$long,mypred$fit+2*mypred$se.fit,col="blue",lty=2) ## adding standard errors, how much uncertainty there is
lines(nd$long,mypred$fit-2*mypred$se.fit,col="blue",lty=2)
## the graph shows survival decreases as you move east. or western population have higher survival. 

    ## latitude
model.lat <- glm(survived~lat, family = binomial(link = "logit"), data = df) 
summary(model.lat) 

nd.lat <- data.frame(lat=seq(30,45),length=100)
mypred.lat <- predict(model.lat, newdata = nd.lat, type = "response", se.fit = T)
plot(survived~lat, xlab="Latitude", ylab="Survived", main="B")

lines(nd.lat$lat,mypred.lat$fit,col="blue")
lines(nd.lat$lat,mypred.lat$fit+2*mypred.lat$se.fit,col="blue",lty=2) ## adding standard errors, how much uncertainty there is
lines(nd.lat$lat,mypred.lat$fit-2*mypred.lat$se.fit,col="blue",lty=2)
  ## Graph shows that along a latitudinal gradient, there is no noticeable change in survival. 

  ##precipitation 
model.precip <- glm(survived~precip, family = binomial(link = "logit"), data = df) 
summary(model.precip) 

nd.precip <- data.frame(precip=seq(100,400),length=100)
mypred.precip <- predict(model.precip, newdata = nd.precip, type = "response", se.fit = T)
plot(survived~precip, xlab="Precipitation (mm)", ylab = "Survived", main="C")

lines(nd.precip$precip,mypred.precip$fit,col="blue")
lines(nd.precip$precip,mypred.precip$fit+2*mypred.precip$se.fit,col="blue",lty=2) ## adding standard errors, how much uncertainty there is
lines(nd.precip$precip,mypred.precip$fit-2*mypred.precip$se.fit,col="blue",lty=2)
## graph shows that populations that received more precipitation had higher survival


  ## mean temperature
model.tmean <- glm(survived~tmean, family = binomial(link = "logit"), data = df) 
summary(model.tmean) 

nd.tmean <- data.frame(tmean=seq(0,20),length=100)

mypred.tmean <- predict(model.tmean, newdata = nd.tmean, type = "response", se.fit = T)
plot(survived~tmean, xlab="Mean Temperature (C)", ylab = "Survived", main="D")

lines(nd.tmean$tmean,mypred.tmean$fit,col="blue")
lines(nd.tmean$tmean,mypred.tmean$fit+2*mypred.tmean$se.fit,col="blue",lty=2) ## adding standard errors, how much uncertainty there is
lines(nd.tmean$tmean,mypred.tmean$fit-2*mypred.tmean$se.fit,col="blue",lty=2)
## graph shoes that populations that were found in warmer sites had higher survival




  ##every population
model.pop<- glm(survived~all.pop, family = binomial(link = "logit"), data = df)
summary(model.pop)

nd.pop<- data.frame(all.pop)
mypred.pop <-predict(model.pop, type = "response", se.fit = T, data=nd.pop)
plot(survived~all.pop) ### looks super weird, not a good way to show this data



### question 2: do populations differ in their survival?------

# use emmeans to pairwise comparisons among populations, need to check residuals: 

library(DHARMa)
resids <- simulateResiduals(model.pop)
plot(resids)  ### results: graph shows that we meet the assumption that there is equal variances among all the populations. AKA the the residuals are normally distributed. 

  ## pairwise comparison 
library(emmeans)
emm<- emmeans(model.pop, specs = c("all.pop"))
pairs(emm) ### results: none of the populations differ from one another in their survival
#i think i did this right


#does tray have an influence?
library(lme4)
library(Matrix)

model.pop2<- glmer(survived~all.pop+(1|tray), family = binomial(link = "logit"), data = df)
summary(model.pop2)




##### question : how does emergence differ with  spatial variation? 
    ## a GLMM?? dont have to report this if i dont want to
#report the differences for a pairwise comparison 
#notes from kevin: dont need to do a glmm for the sake of this class; 
  #GLMM with a gamma distribution, random effect: tray, fixed effect: lat long













