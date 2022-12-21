library(tidyverse)
library(AER)

df <- read_table("controllers.dat", col_names = c("experience","age","errors","size"), col_type = "iiii")
df <- df %>%
  mutate(error_rate = errors/size) %>%
  mutate(age = factor(age, labels = c("<= 55 years", ">= 56 years")), experience = factor(experience, labels = c("<= 3 years", "4-9 years", "10-14 years", "15-19 years", "20-24 years", ">= 25 years")))

ggplot(df) +
  geom_point(aes(x = size, y = errors, color = experience, shape = age), size = 3)
ggplot(df) +
  geom_point(aes(y = error_rate, x = experience)) +
  facet_wrap(vars(age))

model <- glm(errors ~ experience + age, offset = log(size), data = df, family = "poisson")
model <- glm(errors ~ experience + age + size, data = df, family = "poisson")
dispersiontest(model, alternative="two.sided") # we get a significant p-value and a dispersion > 1 -->overdispersion! here we should better use negative binomial!
