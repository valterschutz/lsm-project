library(chron)
library(car)
library(MASS)
library(akima)
library(stats)
library(nlstools)
library(leaps)
library(tidyverse)
library(GGally)
library(caret)
library(olsrr)
library(plyr)
library(gridExtra)
rm(list = ls())

# Modify data?
df = tibble(read.csv("kc_house_data.csv"))
df$date = as.POSIXct(df$date, format="%Y%m%d")
df$zipcode = factor(df$zipcode)
df_red = select(df, -c(id,date,long,lat,sqft_living))
df_red_cont = select(df_red, -c(bedrooms,bathrooms,floors,waterfront,view,condition,grade,yr_built,yr_renovated,zipcode))
df_red_cont_sample <- sample_n(df_red_cont, 500)

# Pairs plot
ggpairs(df_red_cont_sample, mapping = aes(alpha = 0.001)) +
  theme_minimal() +
  theme(axis.text.x=element_blank(), axis.text.y=element_blank())

# Boxplots of categorical variables

# Not significant
p1 <- ggplot(df) +
  geom_boxplot(aes(x = factor(bedrooms), y = price)) +
  theme_minimal() +
  theme(axis.text.x=element_blank(), axis.text.y=element_blank())
p2 <- ggplot(df) +
  geom_boxplot(aes(x = factor(floors), y = price)) +
  theme_minimal() +
  theme(axis.text.x=element_blank(), axis.text.y=element_blank())
p3 <- ggplot(df) +
  geom_boxplot(aes(x = factor(condition), y = price)) +
  theme_minimal() +
  theme(axis.text.x=element_blank(), axis.text.y=element_blank())
# Significant
p4 <- ggplot(df) +
  geom_boxplot(aes(x = factor(grade), y = price)) +
  theme_minimal() +
  theme(axis.text.x=element_blank(), axis.text.y=element_blank())
p5 <- ggplot(df) +
  geom_boxplot(aes(x = factor(bathrooms), y = price)) +
  theme_minimal() +
  theme(axis.text.x=element_blank(), axis.text.y=element_blank())
fig.combined1 <- grid.arrange(p1,p2,p3,p4,p5, ncol = 2)

# Same thing but log(price)
# Not significant
p1 <- ggplot(df) +
  geom_boxplot(aes(x = factor(bedrooms), y = log(price))) +
  theme_minimal() +
  theme(axis.text.x=element_blank(), axis.text.y=element_blank())
p2 <- ggplot(df) +
  geom_boxplot(aes(x = factor(floors), y = log(price))) +
  theme_minimal() +
  theme(axis.text.x=element_blank(), axis.text.y=element_blank())
p3 <- ggplot(df) +
  geom_boxplot(aes(x = factor(condition), y = log(price))) +
  theme_minimal() +
  theme(axis.text.x=element_blank(), axis.text.y=element_blank())
# Significant
p4 <- ggplot(df) +
  geom_boxplot(aes(x = factor(grade), y = log(price))) +
  theme_minimal() +
  theme(axis.text.x=element_blank(), axis.text.y=element_blank())
p5 <- ggplot(df) +
  geom_boxplot(aes(x = factor(bathrooms), y = log(price))) +
  theme_minimal() +
  theme(axis.text.x=element_blank(), axis.text.y=element_blank())
fig.combined1 <- grid.arrange(p1,p2,p3,p4,p5, ncol = 2)

# Backward search
m0 = lm(price ~ ., data=df_red)
bc <- boxcox(m0)  # log is reasonable
m1 <- lm(log(price) ~ ., data=df_red)
stepAIC(m1, direction="both")
# Most important variables
m2 <- lm(log(price) ~ sqft_basement + grade + sqft_above + zipcode + view, data=df_red)
# Even simpler model
m3 <- lm(log(price) ~ sqft_basement + grade + sqft_above * zipcode + view , data=df_red)


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

