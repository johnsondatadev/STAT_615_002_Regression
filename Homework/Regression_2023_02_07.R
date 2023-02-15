library(tidyverse)

MuscleMass <- read_csv("Dataset/MuscelMassData.csv", show_col_types = F)

head(MuscleMass)

## 2
MuscleMass %>% 
  ggplot(aes(x = X, y = y)) +
  geom_point() +
  geom_smooth(method = lm, se = F, formula = 'y ~ x')

# There appears to be a negative relationship between the dependent variable X and the independent variable Y and the points are close to the line indicating that this relationship is linear.

## 3
lm.MM <- lm(y ~ X, data = MuscleMass)
summary(lm.MM)

## The equation is given as Yhat = 113.35 - 0.628X
# For every increase in 1 unit of X, y is estimated to decrease by 0.628

## 4
lm.MM$residuals


## 5
qplot(x = cty, y = hwy, data = mpg, geom = "point") +
  geom_smooth(method = lm, se = FALSE)

qqnorm(lm.MM$residuals)
qqline(lm.MM$residuals)

# Most of the data points appear to be on the line, showing that the data is somewhat normal with only few deviations from the line observed.


## 6
lm.resid <- resid(lm.MM)
lm.fitted <- fitted(lm.MM)



lm.outlier <- 

ggplot(data = lm.MM) +
  geom_point(aes(x = .fitted, y = .resid)) +
  geom_hline(yintercept = 0) 


plot(MuscleMass$X, lm.resid,
     xlab = "X",
     ylab = "residuals",
     main = "Residual plot")
text(MuscleMass$X, lm.resid, n = 2, col = 'red')
abline(0, 0)


## 7
plot(lm.fitted, lm.resid)
abline(0, 0)

# Looking at the residual plot, it shows that the linear model is appropriate. The is equal spread, that is, the variance seems constant. The points do not have a particular pattern.