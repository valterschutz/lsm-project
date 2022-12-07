library(tidyverse)
library(chron)
library(car)
library(MASS)
library(akima)
library(stats)
library(nlstools)

rm(list = ls())
df = tibble(read.csv("C:\\Users\\elias\\IPS\\lsm-project\\kc_house_data.csv"))
df = df[,-c(1)]
df$date = as.POSIXct(df$date, format="%Y%m%d")
df$zipcode = factor(df$zipcode)
df$gradeCategory = factor(df$grade)
#df$yr_renovated_or_built = pmax(df$yr_renovated, df$yr_built)

#df_test = sample_n(df, 10000)
#pairs(df_test[,c("price","sqft_above","sqft_basement", "bathrooms","bedrooms","yr_built","view","condition","grade","waterfront")])
m1 = lm(log(price) ~ bedrooms + bathrooms + zipcode + sqft_above + sqft_lot + sqft_basement + floors + waterfront + view + condition + gradeCategory + yr_built + sqft_lot15 + sqft_living15 + long + lat, data=df)
#stepAIC(m1, direction="both")
#Remove the least significant covariates
m2 = lm(log(price) ~ zipcode + sqft_above + sqft_basement + gradeCategory, data=df)

df$price_per_sqft <- df$price/df$sqft_living

#df$distance_from_most_expensive <- log((df$long+122)^2+(df$lat-47.5)^2)
m3 = lm(log(price) ~ zipcode + sqft_above + I(sqft_above^2) + sqft_basement + grade + I(grade^2), data=df)
summary(m3)

vif(m1)
vif(m2)
vif(m3)
ggplot(df) +
  geom_point(aes(x = long, y = lat, color = price_per_sqft), alpha = 0.05) +
  scale_colour_gradient(low = "blue", high = "red")

#Outliers?
#df[4025, ]
#df[12778, ]
#df[1720, ]
#df[6244, ]
#df[13630, ]
