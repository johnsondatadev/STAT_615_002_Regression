# Model Selection Chapter 9

# The Mallows's Cp indicator
# This criterion is concerned with the total mean squared error 
# of the n fitted values for each subset regression model. The
# mean squared error concept involves the total error in each
# fitted value. For model evaluation, the lower the better.

# Formula: Cp =  (SSEp/(MSE(X1..., Xp)) - (n - 2p)


# The AIC Indicator  AIC -> Akaike's Information Criterion
# This criteria indicator penalizes models that have too many
# predictor variables. For model evaluation, the smaller the
# value the better.

# Formula: AIC = n*ln(SSE) - n*ln + 2*n


# The SBC Indicator  SBC -> Schwarz' Bayesian Criterion (SBC) 
# This criteria indicator also penalizes models that have too 
# many predictor variables. For model evaluation, the smaller 
# the value the better.

# Formula: SBC = n*ln(SSE) - n*ln + [ln(n)]*p

# Other model evaluators that we will rely on for model evaluation
# are the familiar R squared and Adj R squared. For both, the 
# closer the value is to 1 the better.


# Example (mtcars) Selecting the best model for predicting mpg.

install.packages("olsrr") # Install the required package.
library(olsrr) # Load the library

library(tidyverse)
library(dplyr)

# We will explore procedures for model building by using the 
# embedded data table mtcars.

mtcars

# We will however limit available variables to mpg, cyl, disp, hp,
# and drat.

# base r coding to select variables

df <- mtcars[, c("mpg", "cyl", "disp", "hp","drat")]
df

as_tibble(df)

# Linear Regression Model to predict mpg

lin_mod_1 <- lm(mpg ~ ., data = df) 
lin_mod_1

# Let's produce a correlation matrix and investigate possible
# multicollinearity issues.

cor(df)

View(cor(df))


# Let's examine a scatter plot matrix to investigate 
# linearity among predictor variables and the response
# variable.

# Full Version
pairs(df[,1:5], pch = 19)

# Upper Panel Version

pairs(df[,1:5], pch = 19, lower.panel = NULL)

# We now produce numeric indicators of error, fit and prediction
# performance for our model.

# If p total variables are present, then 2^(p - 1) models can be generated.
# For this example, there are 4 predictor variables, hence 2^4 - 1 models
# can be generated.

k <- ols_step_all_possible(lin_mod_1)
k

library(tidyverse)

as_tibble(k)

View(k)

plot(k)



# Diamonds Example

library(tidyverse)
library(dplyr)

diamonds

# Using dplyr coding to select quantitative variables from diamonds.

diamonds%>%
  select(price, carat, x, y, z) -> diamonds2
diamonds2

 # Creating the full linear regression model.

lm(price ~ carat + x + y + z, data = diamonds2) -> diamondsmodel
diamondsmodel

ols_step_all_possible(diamondsmodel) ->  allmodels
allmodels

as_tibble(allmodels)

View(allmodels)

plot(allmodels)

allmodels %>% 
  arrange(desc(rsquare), desc(adjr), desc(cp), desc(aic)) %>% 
  View()


# STEPWISE REGRESSION :An iterative process  used to produce the best 
# Regression model

# Stepwise regression is a procedure we can use to build a regression model 
# from a set of predictor variables by entering and removing predictors in a 
# stepwise manner into the model until there is no statistically valid reason
# to enter or remove any more.

# The goal of stepwise regression is to build a regression model that 
# includes all of the predictor variables that are statistically 
# significantly related to the response variable.

# Again, we will use the mtcars data table to evplore this process. The following
# procedure is refered to as forward STEPWISE REGRESSION.

head(mtcars)

intercept_only <- lm(mpg ~ 1, data=mtcars)
intercept_only

#define model with all predictors
all <- lm(mpg ~ ., data=mtcars)
all

#perform forward stepwise regression
forward <- step(intercept_only, direction='forward', scope=formula(all), trace=0)
forward


#view results of forward stepwise regression
forward$anova

View(forward$anova)

lm(mpg ~ wt + cyl + hp, data = mtcars)

# The best model is

# mpg = 38.75179 -3.16697wt -0.94162cyl -0.01804hp


# Lab

# 1 Using the diamonds data table, create a data table having only the
# variables price, carat, x, y, and z,
diamonds.dt <- diamonds[, c("price", "carat", "x", "y", "z")]
# 2 Produce a multiple linear regression model using x, y, and z to predict
# price.
fit.mlr <- lm(price ~ x + y + z, data = diamonds.dt)
summary(fit.mlr)
# 3 Produce a multiple linear regression model using carat, x, and z to
# predict price.
fit.mlr.c <- lm(price ~ carat + x + y + z, data = diamonds.dt)
summary(fit.mlr.c)
# 4 Produce summary tables for the models that you generated for # 2 and # 3.
#  Determine which does a better predicting price.  Justify your answer.
Both models have all predictors as significant. However, the second model that includes carat, x, y, and z as the predictors is better because it has the higher multiple R-squared and adjusted r-squared of 0.8541 and 0.8541 respectively compared to the other model that has 0.7825 and 0.7825 respectively. Both models are significant.
# 5 Use the stepwise regression method and code demonstrated to find the best
# model for predicting price..


# SplitTing (Training) Data

 
#load iris dataset

iris

#make this example reproducible
set.seed(1)


#Use 70% of dataset as training set and remaining 30% as testing set
sample <- sample(c(TRUE, FALSE), nrow(iris), replace=TRUE, prob=c(0.7,0.3))
sample


train  <- iris[sample, ]
train


test   <- iris[!sample, ]
test

#view dimensions of training set
dim(train)

[1] 106   5

#view dimensions of test set
dim(test)

[1] 44 5

# Model for the training data set

lm(Sepal.Length ~ Sepal.Width + Petal.Length, data = train) -> trainmodel
trainmodel


summary(trainmodel)


Coefficients:
              Estimate Std. Error
(Intercept)   2.33295    0.30352
Sepal.Width   0.55462    0.08517
Petal.Length  0.48263    0.02072
t value Pr(>|t|)    
(Intercept)    7.686 9.27e-12 ***
  Sepal.Width    6.512 2.75e-09 ***
  Petal.Length  23.296  < 2e-16 ***
  ---


# Model for the test data set

lm(Sepal.Length ~ Sepal.Width + Petal.Length, data = test) -> testmodel
testmodel

summary(testmodel)

Coefficients:
              Estimate  Std. Error
(Intercept)   2.10362    0.42517
Sepal.Width   0.67588    0.11733
Petal.Length  0.44277    0.03068
t value Pr(>|t|)    
(Intercept)    4.948 1.33e-05 ***
  Sepal.Width    5.761 9.48e-07 ***
  Petal.Length  14.434  < 2e-16 ***

library(tidyverse)
library(dplyr)
as_tibble(train)


as_tibble(train) -> traintibble
traintibble

# Lets find Yhat values for the training data

traintibble%>%
mutate(Yhatvalues = 2.33295 + 0.55462*Sepal.Width +
      0.48263*Petal.Length)-> traintibble2
traintibble2

View(traintibble2)


# Lets find Yhat values for the test data

as_tibble(test)


as_tibble(test) -> testtibble
testtibble

testtibble%>%
  mutate(Yhatvalues = 2.33295 + 0.55462*Sepal.Width +
           0.48263*Petal.Length)-> testtibble2
testtibble2

View(testtibble2)


summary(traintibble2$Yhatvalues)

summary(testtibble2$Yhatvalues)


summary(testmodel)

summary(trainmodel)


