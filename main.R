library(tidyverse)
library(chron)
library(car)
df = tibble(read.csv("kc_house_data.csv"))
df = df[,-c(1)]
df$date = as.POSIXct(df$date, format="%Y%m%d")
#df$waterfront = factor(df$waterfront)
#df$view = factor(df$view)
#df$condition = factor(df$condition)
#df$grade = factor(df$grade)

df_test = sample_n(df, 1000)
pairs(df_test[,c("price","sqft_above","sqft_basement", "bathrooms","bedrooms","yr_built","view","condition","grade","waterfront")])
# 
plot(df$price, ylim=c(0,1e5))
m = lm(price ~ date + bedrooms + bathrooms + sqft_lot + floors + waterfront + view + condition + grade + sqft_above + sqft_basement + yr_built, data=df)
vif(m)
