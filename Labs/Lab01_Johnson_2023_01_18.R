library(tidyverse)

route <- c(1, 0, 2, 0, 3, 1, 0, 1, 2, 0)
ampules <- c(16, 9, 17, 12, 22, 13, 8, 15, 19, 11)

df = tibble(route, ampules)

df %>% 
  ggplot(aes(x = route, y = ampules)) +
  geom_point(color = "red") +
  geom_smooth(method = "lm", se = F, formula = y ~ x)

lm.fit <- lm(ampules ~ route)
summary(lm.fit)


# The formula is given as 
# y(hat) = 10.2 + 4x

# when x = 1
y_hat_1 <- 10.2 + 4*1

y_hat_1


y_hat_2 <- 10.2 + 4*2

y_hat_2

slope <- y_hat_2 - y_hat_1
slope

yhat <- 10.2 + 4 * 3
residual <- 22 - yhat # Where 22 is the observed value
residual

Xbar = mean(route)
Ybar = mean(ampules)
# To verify that the fitted regression line goes through the point, we substitute x in the equation for Xbar, that is, 1 and verify if it is 14.2

yhat <- 10.2 + 4 * 1
print(paste("Xbar = ", Xbar, ", Ybar = ", Ybar, "Yhat at points(1, 14.2) = ", yhat))

