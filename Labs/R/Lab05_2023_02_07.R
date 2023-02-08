library(tidyverse)

x <- c(5, 8, 11, 7, 13, 12, 12, 6)

y <- c(63, 67, 74, 64, 75, 69, 90, 60)

df <- tibble(x, y)

lm.fit <- lm(y ~ x)

plot(df$x, lm.fit$residuals)
abline(0, 0)

xnew <- x[-7]
ynew <- y[-7]
df1 <- tibble(xnew, ynew)

lm.fit1 <- lm(ynew ~ xnew)
plot(df1$xnew, lm.fit1$residuals)
abline(0, 0)

qqnorm(model$residuals) 
qqline(model$residuals)


qqnorm(new_model$residuals) 
qqline(new_model$residuals)

x_consumption <- c(2:11)
err_consumption <- c(3.2, 2.9, -1.7, -2.0, -2.3, -1.2, -0.9, 0.8, 0.7, 0.5)

plot(x_consumption, err_consumption)
abline(0, 0)

x3 <- c(1:9)
y3 <- c(2, 1, 6, 14, 15, 30, 40, 74, 75)

lm.fit3 <- lm(y3 ~ x3)
plot(x3, y3)

plot(x3, lm.fit3$residuals)
abline(0, 0)

y3sqrt <- sqrt(y3)
plot(x3, y3sqrt)

lm.fit4 <- lm(y3sqrt ~ x3)
summary(lm.fit4)
plot(x3, lm.fit4$residuals)
abline(0, 0)

x4 <- c(7, 7, 8, 3, 2, 4, 4, 6, 6, 7, 5, 3, 3, 5, 8)
y4 <- c(1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 6, 7, 8)

boxcox <- data.frame(x4, y4)

model <- lm(y4 ~ x4) 
model
library(MASS)
box_cox <- boxcox(y4 ~ x4) 
box_cox

lambda <- box_cox$x[which.max(box_cox$y)] 
lambda

new_model <- lm(((y4^lambda-1)/lambda) ~ x4) 
new_model

qqnorm(model$residuals) 
qqline(model$residuals)


qqnorm(new_model$residuals) 
qqline(new_model$residuals)
