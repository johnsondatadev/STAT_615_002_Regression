library(tidyverse)

X = c(1, 0, 2, 0, 3, 1, 0, 1, 2, 0)
Y = c(16, 9, 17, 12, 22, 13, 8, 15, 19, 11)

d.data <- tibble(X, Y)

lm.fit <- lm(Y ~ X, data = d.data)
lm.fit

summary(lm.fit)

cor(Y, X)

qt(p=.025, df=8, lower.tail = FALSE)
# 4.00 +/- 2.306(0.469)
upper_bound <- 4.00 + 2.306 * 0.469
lower_bound <- 4.00 - 2.306 * 0.469

conf.int <- c(upper_bound, lower_bound)

df <- data.frame(d.data, predict(lm.fit, interval = "prediction"))
