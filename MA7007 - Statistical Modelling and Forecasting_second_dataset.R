install.packages("gamlss")

library(gamlss)
library(gamlss.data)
library(gamlss.dist)
library(MASS)


# Data Set Two

data(grip)

set.seed(1024)
index <- sample(3766, 1000)
mydata <- grip[index, ]
dim(mydata)

plot(mydata$age, mydata$grip, main = "Handgrip strength vs Age", 
     xlab = "Age", ylab = "Handgrip Strength")

gbccg <- gamlss(grip ~ pb(age), sigma.formula = ~pb(age), 
                data = mydata, family = BCCG)
edfAll(gbccg)

gbct <- gamlss(grip ~ pb(age), sigma.formula = ~pb(age), nu.formula = ~pb(age),
         tau.formula = ~pb(age), data = mydata, family = BCT,
         start.from = gbccg)

edfAll(gbct)

gbcp <- gamlss(grip ~ pb(age), sigma.formula = ~pb(age),
               nu.formula = ~pb(age), tau.formula = ~pb(age),
               data = mydata, family = BCPE, start.from = gbccg)
edfAll(gbcp)

GAIC(gbccg, gbct, gbcp)

fittedPlot(gbccg, gbct, x = mydata$age)


centiles(gbct, xvar = mydata$age)
centiles(gbcp, xvar = mydata$age)
centiles(gbccg, xvar = mydata$age)

plot(gbccg, resid.type = "person")
plot(gbct, resid.type = "person")
plot(gbcp, resid.type = "person")

wp(gbccg, ylim.all = 2)
wp(gbct, ylim.all = 2)
wp(gbcp, ylim.all = 2)

Q.stats(gbccg)
Q.stats(gbct)
Q.stats(gbcp)

summary(gbccg)
summary(gbct)
summary(gbcp)




















