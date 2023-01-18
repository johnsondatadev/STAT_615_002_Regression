library(tidyverse)

route <- c(1, 0, 2, 0, 3, 1, 0, 1, 2, 0)
ampules <- c(16, 9, 17, 12, 22, 13, 8, 15, 19, 11)

df = tibble(route, ampules)

df

lm.fit <- lm(ampules ~ route)
summary(lm.fit)



# The formula is given as 