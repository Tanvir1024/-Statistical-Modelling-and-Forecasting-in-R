install.packages("gamlss")


library(gamlss)

library(gamlss)
library(gamlss.ggplots)

install.packages("gamlss.util")

#Data Set one

data(dbbmi)
old <- 12
da<- with(dbbmi, subset(dbbmi, age>old & age<old+1))
bmi12<-da$bm
library(MASS)
truehist(bmi12, nbins = 20, col = "lightblue", 
         main = "Histogram of BMI for Age 12-13", xlab = "BMI")


# Fit different distributions
norm_fit <- gamlss(bmi12 ~ 1, family = NO)
gam_fit  <- gamlss(bmi12 ~ 1, family = GA)
lognorm_fit <- gamlss(bmi12 ~ 1, family = LOGNO)
weibull_fit <- gamlss(bmi12 ~ 1,family=WEI)
bcpe_fit <- gamlss(bmi12~1,family=BCPE)
exgaus_fit <- gamlss(bmi12~1,family=exGAUS())
bccg_fit <- gamlss(bmi12~1,family=BCCG())

# Calculate AIC and BIC value
aic_value <- data.frame(df = numeric(0), 
                         AIC = numeric(0))
bic_value <- data.frame(df = numeric(0), 
                         BIC = numeric(0))


aic_value[1,] <- c(2, AIC(weibull_fit))
bic_value[1,] <- c(2, BIC(weibull_fit))
aic_value[2,] <- c(2, AIC(lognorm_fit))
bic_value[2,] <- c(2, BIC(lognorm_fit))
aic_value[3,] <- c(2, AIC(gam_fit))
bic_value[3,] <- c(2, BIC(gam_fit))
aic_value[4,] <- c(2, AIC(norm_fit))
bic_value[4,] <- c(2, BIC(norm_fit))
aic_value[5,] <- c(2, AIC(bcpe_fit))
bic_value[5,] <- c(2, BIC(bcpe_fit))
aic_value[6,] <- c(2, AIC(exgaus_fit))
bic_value[6,] <- c(2, BIC(exgaus_fit))
aic_value[7,] <- c(2, AIC(bccg_fit))
bic_value[7,] <- c(2, BIC(bccg_fit))

summary(bccg_fit)
summary(exgaus_fit)

aic_value
bic_value

#Histogram Lognorm

histDist(bmi12, family = LOGNO(), freq = NULL, density = FALSE, 
         nbins = 10, xlim = NULL, ylim = NULL, main = NULL, 
         xlab = NULL, ylab = NULL, data = NULL, 
         col.hist = "blue", border.hist = "red", 
         fg.hist = rainbow(12)[9], line.wd = 2, 
         line.ty = c(1, 2), line.col = c(2, 3), 
         col.main = "blue4", col.lab = "blue4", 
         col.axis = "red")

# Histogram of bcpe
histDist(bmi12, family = BCPE(), freq = NULL, density = FALSE, 
         nbins = 10, xlim = NULL, ylim = NULL, main = NULL, 
         xlab = NULL, ylab = NULL, data = NULL, 
         col.hist = "blue", border.hist = "red", 
         fg.hist = rainbow(12)[9], line.wd = 2, 
         line.ty = c(1, 2), line.col = c(2, 3), 
         col.main = "blue4", col.lab = "blue4", 
         col.axis = "red")

#Histogram of exgaus
histDist(bmi12, family = exGAUS(), freq = NULL, density = FALSE, 
         nbins = 10, xlim = NULL, ylim = NULL, main = NULL, 
         xlab = NULL, ylab = NULL, data = NULL, 
         col.hist = "blue", border.hist = "red", 
         fg.hist = rainbow(12)[9], line.wd = 2, 
         line.ty = c(1, 2), line.col = c(2, 3), 
         col.main = "blue4", col.lab = "blue4", 
         col.axis = "red")

# Histogram of bccg
histDist(bmi12, family = BCCG(), freq = NULL, density = FALSE, 
         nbins = 10, xlim = NULL, ylim = NULL, main = NULL, 
         xlab = NULL, ylab = NULL, data = NULL, 
         col.hist = "blue", border.hist = "red", 
         fg.hist = rainbow(12)[9], line.wd = 2, 
         line.ty = c(1, 2), line.col = c(2, 3), 
         col.main = "blue4", col.lab = "blue4", 
         col.axis = "red")


wp(bccg_fit,ylim.all=2)
wp(exgaus_fit,ylim.all=2)
plot(bccg_fit, resid.type="pearson")
plot(exgaus_fit, resid.type="pearson")





#Data Set Two


















