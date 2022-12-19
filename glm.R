library(tidyverse)
library(modelr)

df_raw <- read.table("controllers.dat")
colnames(df_raw)<-c("experience","age","errors","size")
df_raw <- tibble(df_raw)
df <- df_raw %>% mutate(error_rate = errors/size) %>%
  mutate(age = factor(age), experience = ordered(experience))

ggplot(df) +
  geom_point(aes(x = experience, y = age, size = error_rate))

model <- glm(errors ~ experience + age, offset = log(size), data = df, family = "poisson")
