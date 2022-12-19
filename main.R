library(chron)
library(car)
library(MASS)
library(akima)
library(stats)
library(nlstools)
library(leaps)
library(tidyverse)
library(modelr)
library(GGally)
library(caret)
library(olsrr)
library(plyr)
library(gridExtra)
rm(list = ls())

pmse = function(trainmodel, testmodel, ytest) {
  Xtest = testmodel$x
  coeffs = as.matrix(as.vector(trainmodel$coefficients))
  ypred = Xtest%*%coeffs
  pmse = sum((ytest-ypred)^2)/length(ytest)
  R2train = summary(trainmodel)$r.squared
  R2test = 1-sum((ytest-ypred)^2)/sum((ytest-mean(ytest))^2)
  MAE <- mean(abs(exp(ypred) - exp(ytest)))
  
  print(ggplot() +
          geom_point(aes(x=ypred, y=ytest), alpha=0.1, color="blue") +
          geom_abline(aes(intercept=0, slope=1), color="red"))
  
  c(pmse, R2train, R2test, MAE)
}

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

# price vs sqft_above
ggplot(df_red) +
  geom_bin2d(aes(x = sqft_above, y = price), bins=50)

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


# Training/testing
df_red$id <- 1:nrow(df_red)
df_red_train <- df_red %>% dplyr::sample_frac(0.5)
df_red_test  <- dplyr::anti_join(df_red, df_red_train, by = 'id')
m0_train = lm(price ~ ., data=df_red_train)
m1_train <- lm(log(price) ~ ., data=df_red_train)
m2_train <- lm(log(price) ~ sqft_basement + grade + sqft_above + zipcode + view, data=df_red_train)
m3_train <- lm(log(price) ~ sqft_basement + grade + sqft_above * zipcode + view , data=df_red_train)

m0_test = lm(price ~ ., data=df_red_test, x=TRUE)
m1_test <- lm(log(price) ~ ., data=df_red_test, x=TRUE)
m2_test <- lm(log(price) ~ sqft_basement + grade + sqft_above + zipcode + view, data=df_red_test, x=TRUE)
m3_test <- lm(log(price) ~ sqft_basement + grade + sqft_above * zipcode + view , data=df_red_test, x=TRUE)

# Model evaluation, R2
pmse(m0_train, m0_test, df_red_test$price)
pmse(m1_train, m1_test, log(df_red_test$price))
pmse(m2_train, m2_test, log(df_red_test$price))
pmse(m3_train, m3_test, log(df_red_test$price))

# Counting predictions
m0_predict <- as.tibble(predict(m0_test, interval = "predict", level = 0.95))
m0_predict <- mutate(m0_predict, width = upr-lwr)
m1_predict <- as.tibble(predict(m1_test, interval = "predict", level = 0.95))
m1_predict <- mutate(m1_predict, width = upr-lwr)
m2_predict <- as.tibble(predict(m2_test, interval = "predict", level = 0.95))
m3_predict <- as.tibble(predict(m3_test, interval = "predict", level = 0.95))
sum(df_red_test$price < m0_predict$upr & df_red_test$price > m0_predict$lwr) / dim(df_red_test)[1]
sum(log(df_red_test$price) < m1_predict$upr & df_red_test$price > m1_predict$lwr) / dim(df_red_test)[1]
sum(log(df_red_test$price) < m2_predict$upr & df_red_test$price > m2_predict$lwr) / dim(df_red_test)[1]
sum(log(df_red_test$price) < m3_predict$upr & df_red_test$price > m3_predict$lwr) / dim(df_red_test)[1]
  
ggplot(predict(m0_test, interval = "predict")) +
  geom_line(aes(y = fit))

ggplot(m1_predict) +
  geom_point(aes(y = width, x = log(df_red_test$price)))

# Check distribution of residuals
ggplot() +
  geom_freqpoly(aes(x = m2$residuals))
p1 <- ggplot() +
  geom_qq(aes(sample = m0_test$residuals), alpha=0.2)
thing <- df_red_test %>%
  gather_residuals(m1_test,m2_test,m3_test)
p2 <- ggplot(thing) +
  geom_qq(aes(sample = resid, color = model), alpha = 0.2)
fig.combined <- grid.arrange(p1,p2, ncol = 2, widths = c(1,1.25))
# Residuals for different categorical variables
ggplot(thing) +
  geom_boxplot(aes(x = factor(grade), y = resid, color = model))
ggplot(thing) +
  geom_boxplot(aes(x = factor(bedrooms), y = resid, color = model))
ggplot(thing) +
  geom_boxplot(aes(x = factor(floors), y = resid, color = model))
ggplot(thing) +
  geom_boxplot(aes(x = factor(round(bathrooms)), y = resid, color = model))
ggplot(thing) +
  geom_boxplot(aes(x = factor(zipcode), y = resid, color = model))
ggplot(thing) +
  geom_boxplot(aes(x = factor(condition), y = resid, color = model))

ggplot() +
  geom_qq(aes(sample = m2_test$residuals), alpha=0.2)
ggplot() +
  geom_qq(aes(sample = m3_test$residuals), alpha=0.2)


vif(m1)
vif(m2)
vif(m3)

#Outliers?
#df[4025, ]
#df[12778, ]
#df[1720, ]
#df[6244, ]
#df[13630, ]

