library(tidyverse)

MuscleMass <- read_csv("Dataset/MuscelMassData.csv", show_col_types = F)

head(MuscleMass)

# 2
MuscleMass %>% 
  ggplot(aes(x = X, y = y)) +
  geom_point() +
  geom_smooth(method = lm, se = F, formula = 'y ~ x')
