library(MASS)

y <- c(1, 1, 2, 2, 2, 2, 3, 3, 5, 6) # dependent variable y 

x <- c(8, 7, 3, 2, 3, 4, 5, 3, 4, 7) # independent variable x

plot(y ~ x)

model <- lm(y~x) 
model

box_cox <- boxcox(y ~ x) 
box_cox

lambda <- box_cox$x[which.max(box_cox$y)] 
lambda

new_model <- lm(((y^lambda-1)/lambda) ~ x) 
new_model

qqnorm(model$residuals) 
qqline(model$residuals)


qqnorm(new_model$residuals) 
qqline(new_model$residuals)

X <- list(12, 14, 15, 18, 19, 22,10,18,18)
Mean <- list(16, 16, 16, 16, 16,16,16,16,16)
sd <- list(2, 2, 2, 2, 2,2,2,2,2)

# zscore = (x - Mean) / sd

output2 <- vector("double") 
for (i in seq_along(X)) {
  output2[[i]] <- X[[i]] - (Mean[[i]] / sd[[i]])
}
output2