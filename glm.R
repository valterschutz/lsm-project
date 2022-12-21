library(tidyverse)
library(AER)
library(EnvStats)

df <- read_table("controllers.dat", col_names = c("experience","age","errors","size"), col_type = "iiii")
df <- df %>%
  mutate(error_rate = errors/size) %>%
  mutate(age = factor(age, labels = c("<= 55 years", ">= 56 years")), experience = factor(experience, labels = c("<= 3 years", "4-9 years", "10-14 years", "15-19 years", "20-24 years", ">= 25 years")))

ggplot(df) +
  geom_point(aes(x = size, y = errors, color = experience, shape = age), size = 3)
ggplot(df) +
  geom_point(aes(y = error_rate, x = experience)) +
  facet_wrap(vars(age))

# Check if categories are worth considering (yes they are)
lambda <- sum(df$errors) / sum(df$size)
df <- df %>%
  mutate(errors_lwr = qpois(0.01, size*lambda), errors_upr = qpois(0.99, size*lambda))
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

model <- glm(errors ~ experience + age, offset = log(size), data = df, family = "poisson", x = TRUE)
dispersiontest(model,alternative="two.sided") # <-- ok, we fail to reject H0:mean=variance

# Interpret coefficients
# Base error rate for age <= 55 years and <= 3 years experience is approx 0.0136.
# Error rate for experience 4-9 years is approx. 4.94 times larger than for experience <= 3 years.
# Error rate for age >= 56 years is approx 0.849 of that for age <= 55.
