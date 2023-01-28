
library(tidyverse)
library(broom)
library(dplyr)
library(ggplot2)

# Linear Regression (Continued)  Inference

# True (Population Equations)  
E(Y) = B(0) + B(1)X(i)  # This equation produces data points that are
# exactly on the regression line for an X input value

E(Y) = B(0) + B(1)X(i) + E(i)  # This equation produces data points that
# may or may not be on the regression line for an X input value and a given
# error value.  Note that if E(i) = 0, the data point is on the line.

# Different Models:


# Deterministic Model:   A deterministic model, as the relationship between 
# the variables is known exactly. A deterministic model is one in which 
# there is no error in the prediction of one variable from the others. 
# Each value of x corresponds to a unique value of y.

# Consider the standard equation for finding the area of a circle;

# A = pir^2   a specific value of r will output a unique value of A.


# Probabilistic Model :  Probabilistic models are statistical models that 
# include one or more probability distributions.
# this linear trend is, in fact, a straight line probabilistic model of the 
# data. The individual data points do not lie exactly on the line, and so 
# this linear model is not deterministic. There is some error in the predictive
# ability of our model.
# Associate Probabilistic Models to linear regression models that account for
# variable distributions and error.
# The model, E(Y) = B(0) + B(1)X(i) gives estimates and approximations, not 
# necessarily exact outcomes for varying x input values.

#Example 1  (A perfect Fit!!)

S<-c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50)
S  # (Dependent or Response)
U<-c(60,57,54,51,48,45,42,39,36,33)
U  #(Independent or Explanatory)

# Let's examine the scatter plot

plot(S~U)

# Lets find the correlation coefficient  (r or R)
cor(S, U)  

# Wow !!  Perfect correlation !!

# Now let's produce a linear model

lm(S~U)->k
k

summary(k)

# The linear regression model is:  S =  105.000 - 1.667U

# Let's  graph the regression line through the data points
scatter.smooth(S~U)

# Let's examine the output.

# Why is the Residual standard error nearly 0 ?
# All the data points fall on the line of regression

# What is the typical distance between a sampled slope estimate
# and the true slope from the population ?
# The standard error

# Interpret the Multiple R-squared value.
# The amount of variability (variation) in the response variable
# that can be explained by our model.

# Should the null hypothesis that the Population slope equals 
# zero be rejected ?
# Yes, the null hypothesis should be rejected because it is less than 0.005

# Is the relationship between the response variable and the 
# explanatory variable of the population dependent or independent ?
# The relationship is dependent

# Example 2  Textbook Problem

# The prediction equation relating the  x = years of education and 
# y = annual income in dollars is  y(hat) = -20,000 +  4000x, 
# and the correlation = .50.  The standard deviations were 2.0 for
# x and 16,000 for y.

#   a) Interpret the slope
# On average, for every increase in 1 year of education, your salary (is predicted to increase) 
# increases by $4000.

#   b) For a correlation of .50, do you expect a strong or weak 
# linear pattern shown by the scatter plot?
# It's neither strong nor weak, it's rather moderate.

#   c) Find the proportion of the variability in y that is explained
# by the linear regression model. What proportion of the variability
# is due to other factors or chance?

#   d) Show how to find the correlation from the slope.

#       r = b(s(x)/s(y))
# Where s is SD


# Example 3

tribble(~Home,  ~SellingPrice,  ~Size,  ~Taxes,  ~Bedrooms,   ~Bathrooms,  ~New,
           1,       279900,      2048,    3104,       4,          2,       "No",
           2,       146500,       912,    1173,       2,          1,       "No",
           3,       237700,      1654,    3076,       4,          2,       "No",
           4,       200000,      2068,    1608,       3,          2,       "No",
           5,       159900,      1477,    1454,       3,          3,       "No",
           6,       499900,      3153,    2997,       3,          2,       "Yes",
           7,       265500,      1355,    4054,       3,          2,       "No",
           8,       289900,      2075,    3002,       3,          2,       "Yes"
        )-> Homes
Homes

# Let's examine the scatter plot

ggplot(data = Homes) +
  geom_point(mapping = aes(y = SellingPrice,  x = Size)) +
  geom_smooth(method = lm, mapping =  aes(y = SellingPrice,  x = Size),
  se = FALSE)

# Based on the scatter plot how would you rate the linear pattern of the data points?
#   poor   moderate   strong    very strong ?
# There is a moderate evidence of positive association between selling price and size.

# Check the boxplots for outliers
# Useful to ensure that the model is reliable to make predictions

boxplot(Homes$SellingPrice)
boxplot(Homes$Size)

# Check the qq plots for normality
qqnorm(Homes$SellingPrice)
qqnorm(Homes$Size)

# Let's produce the linear regression model and look at important 
# summary measures

lm(Homes$SellingPrice~Homes$Size)

lm(Homes$SellingPrice~Homes$Size)-> xx
xx
summary(xx)

cor(Homes$SellingPrice,Homes$Size)

# Simple Linear Regression: Inference  (Chapter 2)

# Testing for B(1)

# For a simple linear regression model  Y = B(0) + B(1)X + e(i)

# We execute the following test:
  Ho :  B(1) equals 0  # (Yi and Xi are not linearly related)
  Ha :  B(1) does not equal 0 # (Yi and Xi are linearly related)
# Generally if the p value of the summary table is extremely small,
# the null hypothesis should be rejected.
# Again, consider the summary table for the model given above:
  
# SellingPrice = -7717.03 + 145.23Size
  
  lm(Homes$SellingPrice~Homes$Size)-> xx
  xx
  summary(xx)
  
# Note that the p value for Size is extremely small, in fact, we have
# significance well below the .05 level.
  
# Confidence intervals for B(1)
  

# Find the 95% confidence interval of the population slope B(1)
  
# Method 1

# The formula is   b(1) +/- t(se)
# b(1) is the point estimate
# t is the t statistic
# se is the standard error 
# We can find b and se from the output table. 

# We will use R coding to find t.

qt(p=.025, df=6, lower.tail = FALSE)
# p = 0.25 because 
# Degrees of freedom for linear regression is n-2

# We now find the confidence interval as follows.
#   b +/-  t(se)
#   145.23 +/- 2.446912(32.35)  =  145.23 +/-  79.1576
#   145.23 - 79.1576 = 66.0724
#   145.23 + 79.1576 = 224.3876

# LB 66.0724        UB 224.3876 

# Method 2  (Using the tidy function of the Broom package)

HomesData <- lm(SellingPrice ~ Size, data = Homes)
HomesData

HomesData1 <- tidy(HomesData, conf.int = TRUE)
select(HomesData1, term, estimate, p.value, conf.low, conf.high)


# Interpretation: We are 95% confident that the true population slope 
# falls between 66.0724 and 224.3876


# A confidence interval for B(0) can be obtain using a similar
# approach.

# Let's look at two types of Interval Bands


HomesData1 <- lm(Taxes ~ SellingPrice, data = Homes)
HomesData1


  # Scatter Plot and Confidence Interval Band for the Response Variable
ggplot(Homes, aes(x=SellingPrice, y=Taxes))+
  geom_point()+
  geom_smooth(method=lm, se=TRUE)



  # # Scatter Plot and Predictor Interval Band for the Response Variable
HomesRegression <- lm(Taxes ~ SellingPrice, data = Homes)
HomesRegression
Homesdf = data.frame(Homes, predict(HomesRegression, interval = "prediction"))
Homesdf


ggplot(Homesdf, aes(SellingPrice, Taxes)) + 
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill='prediction'), 
              fill = "black", alpha = 0.2) +
  geom_smooth(method = "lm", se = FALSE, col = "black") +
  geom_point(size = 3, col = "firebrick") + 
  labs(x = "Taxes", y = "SellingPrice") + 
  theme_classic()


# Confidence Interval for the Response Variable

# A confidence interval for the mean response at a given predictor 
# level E[Yi|Xi].

# For the same linear model, we will find a confidence interval for
# the expected response for the fixed value of 2500.

# In other words, for the model E[Yi] = -7717.0 + 145.2SIZE evaluated
# at 2500 yields 355283.  We will find a 95% confidence interval
# about 355283.

# Create a new data frame that contains the desired level(s) of x.
# Step 1
HomesData <- lm(SellingPrice ~ Size, data = Homes)
HomesData
#Step 2
newdf <- data.frame(Size = 2500)
newdf

# The confidence interval is not for the input but for the output.

# Use predict() with the interval = "confidence" argument to obtain 
# confidence intervals. They are in the lwr and upr columns.
# Step 3
predict(object = HomesData, newdata = newdf, interval = "confidence") %>% 
cbind(newdf)   # cbind means column bind and is used to organize data
               # by columns


# Prediction Interval for Y(hat)i(new) given Xi(new)

# What if, instead of estimating the mean at a given Xi, we want to
# estimate the value of a single observational/experimental unit 
# at Xi.

# Prediction: Estimating the value of a single 
# experimental/observational unit.



# So "estimation" is reserved for parameters, and "prediction" 
# is reserved for unit values.

# In linear regression, the predicted value at Xi is the same 
# as the estimated mean at Xi.

# The steps are the exact same as the confidence intervals for 
# the mean, except we use the interval = "prediction" argument 
# in predict().

HomesData <- lm(SellingPrice ~ Size, data = Homes)
HomesData

newdf <- data.frame(Size = 2500)
newdf

predict(object = HomesData, newdata = newdf, interval = "prediction") %>% 
  cbind(newdf)
