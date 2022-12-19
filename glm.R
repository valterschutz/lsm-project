library(tidyverse)

df <- read_table("controllers.dat", col_names = c("experience","age","errors","size"))
df <- df %>%
  mutate(error_rate = errors/size) %>%
  mutate(age = factor(age), experience = ordered(experience))

ggplot(df) +
  geom_point(aes(x = experience, y = age, size = error_rate))

model <- glm(errors ~ experience + age, offset = log(size), data = df, family = "poisson")
