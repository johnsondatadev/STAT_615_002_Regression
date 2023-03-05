
library(tidyverse)
library(dplyr)
library(ggplot2)

# Residuals   Residual Analysis  Chapter 3

mtcars

mtcars%>%
  select(hp,wt) -> mtcars4
mtcars4

as_tibble(mtcars4) -> mtcars4tibble
mtcars4tibble

lm(hp~wt, mtcars4tibble)

# The linear regression model
#  hp = -1.821 + 46.160wt 


# Now let's expand the table to include a column variable for
# yhat values and residual values.

mtcars4tibble%>%
  mutate(yhat = -1.821 + 46.160*wt)%>%
  mutate(residuals = hp -(-1.821 + 46.160*wt))-> mtcars4tibble1
mtcars4tibble1

# The mean of the residuals should be approximately or exactly
# 0.  Let's confirm this property.
mean(mtcars4tibble1$residuals)

# The distribution of the residuals should be normal, if the
# a linear model is appropriate. Let's investigate this 
# requirement by looking at a qq plot.

qqnorm(mtcars4tibble1$residuals)

# comment: the data points do align somewhat in the middle
# of the plot, but the tailing of at the ends raises concerns
# suggesting that the model may be unstable.



# Now investigate the plot: residuals vs the predictor values
ggplot(data = mtcars4tibble1) +
  geom_point(mapping = aes(y = residuals, x = wt))

# Comments and Interpretation:

# Although the plot does not show a definitive pattern to
# speak of , it does suggest non-constant variation.
# Non-constant variation does not speak well for you model.

# Note that a residual plot, Resisuals vs yhat(fitted values)
# generates the same plot, and hence the same interpretation
# and model appropriateness applies.

ggplot(data = mtcars4tibble1) +
  geom_point(mapping = aes(y = residuals, x = yhat))

# Example 1   Constant Variance, No definitive pattern shown.
mtcars        # Strong support for a linear model for the
              # data
model <- lm(mpg ~ qsec, data=mtcars) 
model         # mpg  =  -5.114 + 1.412qsec

#create residual plot  (New Method)
ggplot(model, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0)


# Example 2    A definitive pattern shown in the plot
               # S curve 
X <- c(1,2,3,4,5,6,7,8,9,10,11,12,13)
X

length(X)

Y <- c(10,16,8,11,32,54,66,88,100,95,87,110,120)
Y

length(Y)

plot(Y~X)

lm(Y~X) -> model2
model2

# Lets look at the Residual Plot

ggplot(model2, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0)

# Example3   Unequal Variances


X <- c(1,2,3,4,5,6,7,8,9,42,61,78,99,12,15,12,84,90,110,130)
X

length(X)

Y <- c(10,16,8,11,32,54,66,88,100,95,87,110,120,8,15,20,90,81,100,170)
Y

length(Y)

plot(Y~X)

lm(Y~X) -> model3
model3

ggplot(model3, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0)


# More Practice
iris


plot(iris$Sepal.Length ~ iris$Sepal.Width)

# No definitive linear relationship is shown between
# the two variables.

# Now lets generate a linear model for Sepal.Length vs 
# Sepal.Width

lm(Sepal.Length~Sepal.Width , data = iris) -> model6
model6

# Finding the residuals for the model

resid(model6) -> residuals
residuals

# We will now plot the residuals against the fitted values.

plot(fitted(model6), residuals)

#add a horizontal line at 0
abline(0,0)

# A case can be made for non constant variance

# Normality Check for the residuals (qqplot)

qqnorm(residuals)

hist(residuals)
boxplot(residuals)



mpg

# Lets generate a scatter plot for hwy vs cty and 
# investigate a possible linear relationship between 
# the variables.

plot(mpg$hwy ~ mpg$cty)

# The scatter plot does suggest a positive linear
# relationship


# Now lets generate a linear model for depth vs x.

lm(mpg$hwy ~ mpg$cty) ->  modelz
modelz
     
#        or

lm(hwy ~ cty, data = mpg) -> modelz
modelz

# Finding the residuals for the model

resid(modelz) -> residuals1
residuals1

# We will now plot the residuals against the fitted values.
plot(fitted(modelz), residuals1)

abline(0,0)

# The residual plot shows no distinct pattern, further
# a strong case for constant variance can be made.

# If a linear regression model is appropriate, the 
# residuals should be normally distributed.

# Let's use a qq plot to investigate normality.

resid(modelz) -> residuals1
residuals1

qqnorm(residuals1)


