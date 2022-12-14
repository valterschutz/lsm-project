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
rm(list = ls())

# Modify data?
df = tibble(read.csv("kc_house_data.csv"))
df$date = as.POSIXct(df$date, format="%Y%m%d")
df$zipcode = factor(df$zipcode)
df$sqft_price <- df$price / df$sqft_living
df_red = select(df, -c(id,date,long,lat,sqft_living))
#dfcont <- dplyr::select(df, price, sqft_above, sqft_basement, lat, long)
#dfconttest <- sample_n(dfcont, 100)

# Pairs plot
ggpairs(dfconttest, mapping = aes(alpha = 0.01))

# Boxplots of categorical variables

# Not significant
ggplot(df) +
  geom_boxplot(aes(x = factor(bedrooms), y = price))
ggplot(df) +
  geom_boxplot(aes(x = factor(floors), y = price))
ggplot(df) +
  geom_boxplot(aes(x = factor(condition), y = price))
# Significant
ggplot(df) +
  geom_boxplot(aes(x = factor(grade), y = price))
ggplot(df) +
  geom_boxplot(aes(x = factor(bathrooms), y = price))

# Big model to see what is significant
m_big <- lm(price ~ ., data=df_red)

# Backward search
#df_search <- sample_n(df, 100)
#df_search <- df_red
#df_search = select(df_search, !c(id,date,sqft_living,long,lat))
m_backward = lm(price ~ ., data=df_red)
bc <- boxcox(m_backward)  # log is reasonable
m_backward = lm(log(price) ~ ., data=df_red)
stepAIC(m_backward, direction="both")
# From this we get two models, one simple and one more complex
m1 <- lm(log(price) ~ sqft_living15 + condition + waterfront + view + sqft_basement + grade + sqft_above + zipcode, data=df_red)
m2 <- lm(log(price) ~ sqft_basement + grade + sqft_above + zipcode - 1, data=df_red)
zipcode_score <- as.numeric(m2$coefficients[4:length(m2$coefficients)])
a <- data.frame(zipcode_score)
zipcode_strings = row.names(a)
a$zipcode <- factor(substr(zipcode_strings, 8, 12))
rownames(a) <- NULL
a
df #Hur får vi in zipcode_values i df?
which(df$zipcode == a$zipcode[20])
# Try to create a very small model

# Exhaustive model search
df_search <- sample_n(df, 1000)
df_search = select(df_search, !c(id,date,sqft_living,long,lat,bedrooms,bathrooms,floors,waterfront,condition,grade))
model <- lm(price ~ . - 1, data = df_search)
k <- ols_step_all_possible(model)
plot(k)
#
#response = df_exhaustive$price
#traincontrol = trainControl(method = "cv", number = 10)
#model = train(x = predictors, y = response, method = "lm", trControl = traincontrol)
#ggplot(model$results, aes(x = Complexity, y = PMSE)) +
#  geom_line() +
#  labs(title = "PMSE vs. Number of Included Covariates",
#       x = "Number of Included Covariates",
#       y = "PMSE")

#df_test = sample_n(df, 10000)
#pairs(df_test[,c("price","sqft_above","sqft_basement", "bathrooms","bedrooms","yr_built","view","condition","grade","waterfront")])
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
