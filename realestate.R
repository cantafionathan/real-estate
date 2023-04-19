library(ggplot2)
library(tidyverse)
library(data.table)
library(ggmap)
library(leaps)
library(readxl)

### LOAD DATA
df <- read_excel("STAT 306/Project/Real estate valuation data set.xlsx", 
                 col_types = c("skip", "numeric", "numeric",
                               "numeric", "numeric", "numeric",
                               "numeric", "numeric"))
colnames(df) <- c("date", "age", "mrt", "stores",
                  "latitude", "longitude", "price")

### Location Characterization

# is.urban feature
r = 0.0175 # center radius
location <- data.frame(lon = df$longitude, lat = df$latitude)
distance <- sqrt((location[1]-121.537)^2+(location[2]-24.9775)^2)
df[,"is.urban"] <- ifelse(distance <= r, "urban", "rural")

map <- ggmap(get_map(location = c(121.46, 24.92, 121.6, 25.02), 
                     maptype ='satellite', color = 'color'))

# regions
map + geom_point(aes(x = longitude, y = latitude, colour = factor(is.urban)),
                 data = df, size=3, alpha=1) + 
  scale_colour_discrete(name = "Location", labels = c("Rural", "Urban"))

# ppua map
map + geom_point(aes(x = longitude, y = latitude, colour = log(price)), 
                 data = df, size=3, alpha=1) + 
  scale_color_gradient(name = "ppua", low = "blue", high = "yellow")

# boxplot
ggplot(df, aes(is.urban, price)) + geom_boxplot(alpha=0) + 
  geom_jitter(alpha=0.3, color="tomato") + 
  labs(x="Location",
       y="Price")

df$price <- log(df$price)

### TRENDS

# Seasonal effect/monthly influence
monthly.ppua = c(
  mean(df$price[as.logical(df$date == 2013)]), 
  mean(df$price[as.logical(df$date == 2013.0833333)]),
  mean(df$price[as.logical(df$date == 2013.1666667)]), 
  mean(df$price[as.logical(df$date == 2013.25)]),
  mean(df$price[as.logical(df$date == 2013.3333333)]), 
  mean(df$price[as.logical(df$date == 2013.4166667)]),
  mean(df$price[as.logical(df$date == 2013.5)]), 
  mean(df$price[as.logical(df$date == 2013.5833333)]),
  mean(df$price[as.logical(df$date == 2012.6666667)]), 
  mean(df$price[as.logical(df$date == 2012.75)]),
  mean(df$price[as.logical(df$date == 2012.8333333)]), 
  mean(df$price[as.logical(df$date == 2012.9166667)]))

monthly.urban.ppua = c(
  mean(df$price[as.logical((df$date == 2013) & df$is.urban == 'urban')]), 
  mean(df$price[as.logical((df$date == 2013.0833333) & df$is.urban == 'urban')]),
  mean(df$price[as.logical((df$date == 2013.1666667) & df$is.urban == 'urban')]), 
  mean(df$price[as.logical((df$date == 2013.25) & df$is.urban == 'urban')]),
  mean(df$price[as.logical((df$date == 2013.3333333) & df$is.urban == 'urban')]), 
  mean(df$price[as.logical((df$date == 2013.4166667) & df$is.urban == 'urban')]),
  mean(df$price[as.logical((df$date == 2013.5) & df$is.urban == 'urban')]), 
  mean(df$price[as.logical((df$date == 2013.5833333) & df$is.urban == 'urban')]),
  mean(df$price[as.logical((df$date == 2012.6666667) & df$is.urban == 'urban')]), 
  mean(df$price[as.logical((df$date == 2012.75) & df$is.urban == 'urban')]),
  mean(df$price[as.logical((df$date == 2012.8333333) & df$is.urban == 'urban')]), 
  mean(df$price[as.logical((df$date == 2012.9166667) & df$is.urban == 'urban')]))

monthly.rural.ppua = c(
  mean(df$price[as.logical((df$date == 2013) & df$is.urban == 'rural')]), 
  mean(df$price[as.logical((df$date == 2013.0833333) & df$is.urban == 'rural')]),
  mean(df$price[as.logical((df$date == 2013.1666667) & df$is.urban == 'rural')]), 
  mean(df$price[as.logical((df$date == 2013.25) & df$is.urban == 'rural')]),
  mean(df$price[as.logical((df$date == 2013.3333333) & df$is.urban == 'rural')]), 
  mean(df$price[as.logical((df$date == 2013.4166667) & df$is.urban == 'rural')]),
  mean(df$price[as.logical((df$date == 2013.5) & df$is.urban == 'rural')]), 
  mean(df$price[as.logical((df$date == 2013.5833333) & df$is.urban == 'rural')]),
  mean(df$price[as.logical((df$date == 2012.6666667) & df$is.urban == 'rural')]), 
  mean(df$price[as.logical((df$date == 2012.75) & df$is.urban == 'rural')]),
  mean(df$price[as.logical((df$date == 2012.8333333) & df$is.urban == 'rural')]), 
  mean(df$price[as.logical((df$date == 2012.9166667) & df$is.urban == 'rural')]))

df.s <- data.frame(x = seq(1:12),
                   monthly.ppua = monthly.ppua,
                   monthly.rural.ppua = monthly.rural.ppua,
                   monthly.urban.ppua = monthly.urban.ppua)

#seasonal mean
ggplot(df.s, aes(x)) + 
  geom_line(aes(y = monthly.ppua, colour = "Total")) + 
  geom_line(aes(y = monthly.urban.ppua, colour = "Urban")) + 
  geom_line(aes(y = monthly.rural.ppua, colour = "Rural")) +
  scale_x_continuous(breaks = df.s$x) +
  labs(x = "Month", y = "ppua")

# seasonal
ggplot(df, aes(x=12*date %% 1, y=price, col=is.urban)) + 
  geom_point() + 
  labs(x="Month", y = "ppua") + 
  scale_colour_discrete(name='Location')

# age
ggplot(df, aes(x=age, y=price, col=is.urban)) +
  geom_point() +
  labs(x="Age", y = "ppua") + 
  scale_colour_discrete(name='Location')

#ggplot(df[df$is.urban == 'urban',], aes(x=age, y=price)) +
#  geom_point() +
#  labs(x="Age", y = "ppua")

#ggplot(df[df$is.urban == 'rural',], aes(x=age, y=price)) +
#  geom_point() +
#  labs(x="Age", y = "ppua")

# mrt
ggplot(df, aes(x=mrt, y=price, col=is.urban)) +
  geom_point() +
  labs(x="Distance to Nearest MRT", y = "ppua") + 
  scale_colour_discrete(name='Location')

#ggplot(df[df$is.urban == 'urban',], aes(x=mrt, y=price)) +
#  geom_point() +
#  labs(x="Distance to Nearest MRT", y = "ppua")

#ggplot(df[df$is.urban == 'rural',], aes(x=mrt, y=price)) +
#  geom_point() +
#  labs(x="Distance to Nearest MRT", y = "ppua")

# stores
ggplot(df, aes(x=stores, y=price, col=is.urban)) +
  geom_point() +
  labs(x="Number of Nearby Convenience Stores", y = "ppua") + 
  scale_colour_discrete(name='Location')

#ggplot(df[df$is.urban == 'urban',], aes(x=stores, y=price)) +
#  geom_point() +
#  labs(x="Number of Nearby Convenience Stores", y = "ppua")

#ggplot(df[df$is.urban == 'rural',], aes(x=stores, y=price)) +
#  geom_point() +
#  labs(x="Number of Nearby Convenience Stores", y = "ppua")

### RELATIONSHIPS BETWEEN VARIABLES

df[,"is.urban"] <- distance <= r
df$sin.season <- sin(2 * pi * (df$date %% 1))

# date and age :: no obvious connection
ggplot(df, aes(x=date, y=age, col=price, pch=is.urban)) + geom_point()

# date and mrt :: no obvious connection
ggplot(df, aes(x=date, y=mrt, col=price, pch=is.urban)) + geom_point()

# date and stores :: two discrete categories
grouped.df <- df %>% group_by(date, stores) %>% summarize(mean(price), mean(is.urban))
colnames(grouped.df) <- c("date", "stores", "price", "is.urban")
ggplot(grouped.df, aes(x=date, y=stores, fill=price)) + geom_tile()
ggplot(grouped.df, aes(x=date, y=stores, fill=is.urban)) + geom_tile()

# age and mrt :: no obvious connection
ggplot(df, aes(x=age, y=mrt, col=price, pch=is.urban)) + geom_point()

# age and stores :: no obvious connection
ggplot(df, aes(x=age, y=stores, col=price, pch=is.urban)) + geom_point()

# mrt and stores :: no obvious connection
ggplot(df, aes(x=mrt, y=stores, col=price, pch=is.urban)) + geom_point()

### MODEL SELECTION

full.df <- data.frame(df[,c(1:4,7:9)]) # exclude latitude and longitude because we have is.urban
non.date.location.features <- c("age", "mrt", "stores")
date.features <- c("date", "sin.season")
location.features <- c("is.urban")
date.location.features <- c(date.features, location.features)
add.interactions <- function(features1, features2) {
  for (feature1 in features1) {
    for (feature2 in features2) {
      full.df[,paste(feature1, feature2, sep=":")] <- full.df[,feature1] * full.df[,feature2]
    }
  }
  assign('full.df',full.df,envir=.GlobalEnv)
}
add.interactions(date.features, location.features)
add.interactions(non.date.location.features, 
                 c(date.location.features, "date:is.urban", "sin.season:is.urban"))

# Model baseline
model.baseline <- lm(
  price ~ .,
  full.df
)
summary(model.baseline)

# Forwards/Backwards selection
# plots suggest that variable counts between 7-9 are reasonable
models.forward <- regsubsets(price ~ ., data=full.df, method="forward", nvmax = 24)
plot(1:23, summary(models.forward)$cp, xlab="Variable Count", ylab="Mallow's Cp", 
     main="Variable Count vs Mallow's Cp (Forward Selection)")
plot(1:23, summary(models.forward)$bic, xlab="Variable Count", ylab="Bayesian information criterion (BIC)", 
     main="Variable Count vs BIC (Forward Selection)")
plot(1:23, summary(models.forward)$adjr2, xlab="Variable Count", ylab="Adjusted RSQ",
     main="Variable Count vs Adjusted RSQ (Forward Selection)")
models.backward <- regsubsets(price ~ ., data=full.df, method="backward", nvmax = 24)
plot(1:23, summary(models.backward)$cp, xlab="Variable Count", ylab="Mallow's Cp", 
     main="Variable Count vs Mallow's Cp (Backward Selection)")
plot(1:23, summary(models.backward)$bic, xlab="Variable Count", ylab="Bayesian information criterion (BIC)", 
     main="Variable Count vs BIC (Backward Selection)")
plot(1:23, summary(models.backward)$adjr2, xlab="Variable Count", ylab="Adjusted RSQ",
     main="Variable Count vs Adjusted RSQ (Backward Selection)")
# and forwards/backwards methods give slightly different results
show.best.variables <- function(subsets, n) {
  summary.row <- summary(subsets)$which[n,-1]
  return(paste(names(summary.row)[summary.row]))
}

# Cross validation
n <- nrow(full.df)
set.seed(306)
validation.indices <- sample.int(n, floor(n * 0.2))
full.df.train <- full.df[-validation.indices,]
full.df.validation <- full.df[validation.indices,]

best.model <- NULL
best.rmse <- .Machine$double.xmax
for (subset in list(models.forward, models.backward)) {
  for (variable.count in 7:9) {
    model <- lm(
      as.formula(
        paste("price ~", paste(show.best.variables(subset, variable.count), collapse="+"))
      ),
      full.df.train
    )
    preds <- predict(model, full.df.validation)
    rmse <- sqrt(mean((preds - full.df.validation$price)^2))
    if (rmse < best.rmse) {
      best.model <- model
      best.rmse <- rmse
    }
  }
}
summary(best.model)

### LEVERAGE AND INFLUENCE

outliers <- which(abs(rstandard(best.model)) > 3)
print(outliers)
# uncomment at most one of the following lines to exclude an extreme outlier 
# or all outliers -- also change in the plotting
full.df <- full.df[-114,]
#full.df <- full.df[-outliers,]

best.model.full <- lm(as.formula(best.model), full.df)
summary(best.model.full)

# leverage
map + geom_point(aes(x = longitude, y = latitude, colour = hatvalues(best.model.full)), 
                 data = df[-114,], size=3, alpha=0.8) + 
  scale_color_gradient(low = "blue", high = "yellow", name="P_ii") + 
  ggtitle("Leverages of Data Points")

# cook's distance
map + geom_point(aes(x = longitude, y = latitude, colour = cooks.distance(best.model.full)),
                 data = df[-114,], size=3, alpha=0.8) + 
  scale_color_gradient(low = "blue", high = "yellow", name="Cook's Distance") + 
  ggtitle("Cook's Distances of Data Points")

print(which.max(cooks.distance(best.model.full)))

# standard residuals
map + geom_point(aes(x = longitude, y = latitude, colour = abs(rstandard(best.model.full))), 
                 data = df[-114,], size=3, alpha=0.8) + 
  scale_color_gradient(low = "blue", high = "yellow", name="Standardized residuals") + 
  ggtitle("Standardized residuals")
