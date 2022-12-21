library(tidyverse)
library(modelr)
library(AER)
library(EnvStats)
library(MASS)
library(xtable)

df <- read_table("controllers.dat", col_names = c("experience","age","errors","size"), col_type = "iiii")
df <- df %>%
  mutate(error_rate = errors/size) %>%
  mutate(experienced = factor(experience != "<= 3 years")) %>%
  mutate(age = factor(age, labels = c("<= 55 years", ">= 56 years")), experience = factor(experience, labels = c("<= 3 years", "4-9 years", "10-14 years", "15-19 years", "20-24 years", ">= 25 years")))
df$experience <- relevel(df$experience, ">= 25 years")

# Plot errors against size and categorical variables
ggplot(df) +
  geom_point(aes(x = size, y = errors, color = experience, shape = age), size = 3)
# Plot error rate against categorical variables
ggplot(df) +
  geom_point(aes(y = error_rate, x = experience)) +
  facet_wrap(vars(age))

# Check if categories are worth considering (yes they are)
lambda <- sum(df$errors) / sum(df$size)
#df <- df %>%
#  mutate(errors_lwr = qpois(0.01, size*lambda), errors_upr = qpois(0.99, size*lambda))
df %>%
  arrange(errors_lwr) %>%
  ggplot() +
  geom_line(aes(x = seq(1,length(errors)), y = errors / size)) +
  geom_line(aes(x = seq(1,length(errors)), y = errors_lwr / size), color = "red", linetype = "dashed") +
  geom_line(aes(x = seq(1,length(errors)), y = errors_upr / size), color = "blue", linetype = "dashed")


for (i in 1:nrow(df)) {
  temp <- df[-i,]
  df[i,"lambda_excluded"] <- sum(temp$errors) / sum(temp$size)
}
for (i in 1:nrow(df)) {
  temp <- df[-i,]
  z <- -qnorm(0.05/2)
#  s <- 1/(length(temp)-1) * sum((temp$lambda_excluded-mean(temp$lambda_excluded))^2)
  s <- size*sqrt(var(temp$lambda_excluded))
  lambda <- mean(temp$lambda_excluded)
  df[i,"pred_int_lwr"] <- lambda - (z+1)*s
  df[i,"pred_int_upr"] <- lambda + (z+1)*s
}

ggplot(df) +
  geom_line(aes(x = seq(1,length(errors)), y = errors / size)) +
  geom_line(aes(x = seq(1,length(errors)), y = pred_int_lwr), color = "red", linetype = "dashed") +
  geom_line(aes(x = seq(1,length(errors)), y = pred_int_upr), color = "blue", linetype = "dashed")


pois_model_intercept <- glm(errors ~ 1, offset = log(size), data = df, family = "poisson", x = TRUE)
pois_model_exp <- glm(errors ~ experience, offset = log(size), data = df, family = "poisson", x = TRUE)
pois_model_full <- glm(errors ~ experience + age, offset = log(size), data = df, family = "poisson", x = TRUE)

dispersiontest(pois_model_intercept,alternative="two.sided")
dispersiontest(pois_model_exp,alternative="two.sided")
dispersiontest(pois_model_full,alternative="two.sided")

# Predictions
df <- df %>%
  gather_predictions(pois_model_intercept, pois_model_exp, pois_model_full, type = "response") %>%
  mutate(rate_pred = pred / size)
# Predictions plot
df %>%
  mutate(experience = relevel(experience, "<= 3 years")) %>%
  ggplot() +
  geom_line(aes(y = rate_pred, x = experience, group = age), color = "blue") +
  geom_line(aes(y = error_rate, x = experience, group = age), color = "black") +
  facet_grid(rows = vars(model), cols = vars(age)) +
  labs(x = "Experience", y = "Error rate")

beta <- model$coefficients
design <- model$x
df[["pred_error_rate"]] <- exp(design%*%beta)
# Plot error rate against categorical variables, now with predictions
ggplot(df) +
  geom_point(aes(y = error_rate, x = experience), color = "red") +
  geom_point(aes(y = pred_error_rate, x = experience), color = "blue") +
  facet_wrap(vars(age))
# We see again that age was not very significant for predictions. The predictions for age >= 56 has the same shape as age <= 55 but is a bit lower.


# Simplified model for experience
model_simple <- glm(errors ~ experienced + age, offset = log(size), data = df, family = "poisson", x = TRUE)
model_simple <- glm(errors ~ experienced, offset = log(size), data = df, family = "poisson", x = TRUE)

# Negative binomial stuff
nb_model_full <- glm.nb(errors ~ experience + age + offset(log(size)), data = df, x = TRUE)