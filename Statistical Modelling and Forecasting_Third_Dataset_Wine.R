install.packages("gamlss")
install.packages("ggplot2")
install.packages("ggcorrplot")

library(gamlss)
library(ggplot2)
library(ggcorrplot)

wine_data <- read.csv("winequality-red.csv")
set.seed(1039)
sample_rows <- sample(nrow(wine_data), round(0.8*nrow(wine_data)))

subset_data <- wine_data[sample_rows, ]
nrow(subset_data)
test_data <- wine_data[-sample_rows, ]
dim(wine_data)
dim(subset_data)

#histogram of quality
ggplot(subset_data, aes(x = quality)) +
  geom_histogram(binwidth = 1, color = "black", fill = "blue") +
  labs(title = "Histogram of Wine Quality", x = "Quality Score", y = "Frequency")

# Create a scatter plot matrix of the explanatory variables
ggplot(subset_data, aes(x = fixed.acidity, y = volatile.acidity)) +
  geom_point(aes(color = quality)) +
  labs(title = "Scatter Plot Matrix of Red Wine Explanatory Variables", x = "Fixed Acidity", y = "Volatile Acidity")

# Create a scatter plot matrix of the explanatory variables
ggplot(subset_data, aes(x = alcohol, y = residual.sugar)) +
  geom_point(aes(color = quality)) +
  labs(title = "Scatter Plot Matrix of Red Wine Explanatory Variables", x = "alcohol", y = "residual sugar")

# Create a scatter plot matrix of the explanatory variables
ggplot(subset_data, aes(x = citric.acid, y = chlorides)) +
  geom_point(aes(color = quality)) +
  labs(title = "Scatter Plot Matrix of Red Wine Explanatory Variables", x = "citric acid", y = "chlorides")

# Create a scatter plot matrix of the explanatory variables
ggplot(subset_data, aes(x = density, y = pH)) +
  geom_point(aes(color = quality)) +
  labs(title = "Scatter Plot Matrix of Red Wine Explanatory Variables", x = "density", y = "pH")

#correlation heatmap
ggcorrplot(cor(subset_data), hc.order = TRUE, type = "lower", lab = TRUE, insig = "blank")

#box plot for each variable
par(mfrow = c(2,6), mar = c(3,3,3,3))

oldpar = par(mfrow = c(2,6))
for ( i in 1:11 ) {
  boxplot(subset_data[[i]])
  mtext(names(subset_data)[i], cex = 0.8, side = 1, line = 2)
}
par(oldpar)
summary(subset_data)

Y<-cbind(subset_data$quality,10-da$quality)
m01 <- gamlss(Y~1, data=subset_data, family=BI)
m02 <- gamlss(Y~1, data=subset_data, family=ZABI)

m03 <- gamlss(Y~1, data=subset_data, family=ZIBI)
m04 <- gamlss(Y~1, data=subset_data, family=DBI)
#m05 <- gamlss(Y~1, data=da, family=BB) #
GAIC(m01,m02,m03,m04)

form <- formula(Y~fixed.acidity+volatile.acidity+citric.acid+chlorides+total.sulfur.dioxide+density+sulphates+ 
                  alcohol+pH+free.sulfur.dioxide+residual.sugar) 
form1 <- formula(~fixed.acidity+volatile.acidity+citric.acid+chlorides+total.sulfur.dioxide+density+sulphates+ 
                   alcohol++pH+free.sulfur.dioxide+residual.sugar) 
# only mu model 
m1 <- gamlss(form, data=subset_data, family=DBI)

# mu and sigma model
m2 <- gamlss(form,form1, data=subset_data, family=DBI)
AIC(m04, m1, m2)
wp(m2,ylim.all =2)
summary(m2)
drop1(m2)

form <- formula(Y~fixed.acidity+volatile.acidity+chlorides+total.sulfur.dioxide+density+sulphates+ alcohol) 
form1 <- formula(~fixed.acidity+volatile.acidity+chlorides+total.sulfur.dioxide+density+sulphates+ alcohol) 

# only mu model 
m3 <- gamlss(form, data=subset_data, family=DBI)

# mu and sigma model
m5 <- gamlss(form,form1, data=subset_data, family=DBI)

AIC(m04, m3, m5)
wp(m5,ylim.all =2)
summary(m5)
plot(m5)

# Find the index of the first out-of-sample observation
index <- sample(c(TRUE, FALSE), nrow(wine_data), replace = TRUE, prob = c(0.8, 0.2))
out_of_sample_index <- wine_data[which(index == FALSE), ]
first_observation <- out_of_sample_index[1, ]

# Use the model to predict the distribution for the first out-of-sample observation
prediction <- predict(m5, newdata = first_observation, type = "response")
print(prediction)




