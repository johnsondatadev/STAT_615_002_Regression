
library(tidyverse)
library(dplyr)
# STAT 615
# Linear Regression Basics and Preliminaries
# Introduction to Basic R coding for Regression

# Linear regression is a process that attempts to model the 
# relationship between two variables by fitting a linear equation
# ( a straight line) to the observed data. ... If you have a hunch 
# that the data follows a straight line trend, linear regression 
# can give you quick and reasonably accurate results.

# The process of linear regression helps us to explore the possibility
# of a linear relationship existing between two quantitative variables.

# A linear regression model will have two variables: 
# an independent variable (normally designated as x), 
# and a dependent variable(normally designated as y).  
# Typically, the Linear Regression model has the following basic form:   
# y = b + ax.  
#    a is the slope of the linear model
#    b is the y intercept.

# More General Linear Regression Models and Representations are offered
# below.

#   POPULATION (Theoretical Model)      SAMPLE Model

#   E(Y) = B0 + B1X                      y = b + ax 
#                                       y = bo + b1x    
#                                      y(hat) = bo + b1x

# Models that contain the error terms and residual terms

#  POPULATION                          SAMPLE
#  E(Y) = B0 + B1X(i) + E(i)           y(hat) = bo + b1x(i) + e(i)

# Analysis and interpretations
# The parameters of the sample equation estimate the parameters of the
# population equation. That is bo estimates B0 and b1 estimates B1.

# e(i) the residual of the sample equation estimates E(i) the error for
# the theoretical population equation, likewise y(hat) estimates E(Y).

# Why are we using y(hat) and what does it mean ?

# y(hat) is the predicted value or the regression line value for a 
# designated x(i). y(hat) is also referred to as the mean and is gene-
# rated by bo + b1x(i).

# Let's investigate further by examining the data table mtcars.
mtcars

# I do not need the entire data table. I want to consider the relationship
# between mpg and hp.

mtcars%>%
  select(mpg, hp)%>%
  arrange(mpg) ->  mtcars1
mtcars1
  
# Let's look at the scatter plot and the regression line.

# First let's use ggplot coding.
ggplot(data = mtcars1) +
  geom_point(mapping = aes(x = mpg , y = hp)) +
  geom_smooth(method = lm,mapping = aes(x = mpg , y = hp), se=FALSE)+
  ggtitle("                               Scatter Plot (hp vs. mpg)")

# We can also us qplot coding to generate a scatter plot.
qplot(x = mpg, y = hp, data = mtcars1, geom = "point")

# Now attach the regression line to the scatter plot using qplot coding.

qplot(x = mpg, y = hp, data = mtcars1, geom = "point") +
  geom_smooth(method = lm, se = FALSE)
  

# Let's find the regression equation:
lm(hp ~ mpg, mtcars1)

# y(hat) = 324.08 - 8.83x

# Comment and Observation

# In the modified mtcars table, note that the mpg designation of 10.4
# yields two different values for hp, namely 205 and 215.
# Hence, for the mpg value of 10.4, we calculate the mean by the following
# process:

# y(hat) = 324.08 - 8.83(10.4)
# y(hat) =  232.248

# at 10.4, the model gives a value of 232.248 that is on the line !!


#  Consider the table of values in the table below:

              tribble(~ClubheadSpeadmphx,  ~Distanceydy,
                                  100 ,             257,
                                  102 ,             264,
                                  103 ,             274,
                                  101 ,             266,
                                  105 ,             277,
                                  100 ,             263,
                                   99 ,             258,
                                  105 ,             275
                      )
# The equation below is the linear regression model that offers 
# a linear relationship between Club Speed x and distance y.
#             y = -55.7966 + 3.1661x
#             slope = 3.1661, and the y intercept is -55.7966
              
# We can use our Regression model to make predictions for distance y,
# given a club speed x.

# Let's consider two strategies for using R to produce the 
# Regression Equation;

# Method 1
  x<- c(100,102,103,101,105,100,99,105)
  x
  
  y<- c(257,264,274,266,277,263,258,275)
  y
  
# Let's generate the Intercept and the slope for the Regression 
# Equation
  
  lm(y~x)
  
  
# Let's now generate more descriptive information for the Regression
# process
  
  lm(y~x)->xx
  xx
  summary(xx)
  
  
  anova(xx)
#     or  
  
  # anova(lm(y ~ x))  # Used for variance analysis and the F test.
  
# Method 2
  tribble(~ClubheadSpeadmphx,  ~Distanceydy,
          100 ,             257,
          102 ,             264,
          103 ,             274,
          101 ,             266,
          105 ,             277,
          100 ,             263,
          99 ,              258,
          105 ,             275
  ) ->xxx
  xxx

# We will produce the slope and the y intercept
  
  lm(xxx$Distanceydy ~  xxx$ClubheadSpeadmphx)
  
# We now generate additional descriptive information for the 
# regression process
  
  lm(xxx$Distanceydy ~  xxx$ClubheadSpeadmphx)->xz
  xz
  
  summary(xz)
  
# More practice !!

# Using the mpg data set, let's first determine if a linear 
# relationship exists between city and hwy. Let cty be 
# independent and let hwy be dependent.  We will make this 
# determination by looking at a scatter plot.

mpg

  
# Scatter Plot (Tidyverse Method)
ggplot(data = mpg) +
  geom_point(mapping = aes(x = cty , y = hwy)) +
  ggtitle("                          Scatter Plot (hwy vs. cty)")

# We can also us qplot coding to generate a scatter plot.
qplot(x = cty, y = hwy, data = mpg, geom = "point")

# Now attach the regression line to the scatter plot using qplot coding.

qplot(x = cty, y = hwy, data = mpg, geom = "point") +
  geom_smooth(method = lm, se = FALSE)

# Often the jitter plotting version is used to accommodate overlapping 
# points.

qplot(x = cty, y = hwy, data = mpg, geom = "jitter") +
  geom_smooth(method = lm, se = FALSE)

# All scatter plots indicate the same linear relationship.

# Let's find the correlation coefficient (the value that gives the
# strength and the direction of the scatter plot)

cor(mpg$hwy , mpg$cty)

# The output indicates that the  correlation coeffcient is
# .9559159. A very strong positive correlation. 
# (The closer this value is to 1 or -1 the stronger the
# relationship is.)

# We now check boxplots for outliers. ( Too many extreme outliers
# have a negative impact on our regression equation's capacity 
# to predict)

# Boxplots (Tidyverse Methods)
ggplot(data = mpg) +
  geom_boxplot(mapping = aes(y = cty))

ggplot(data = mpg) +
  geom_boxplot(mapping = aes(y = hwy))


# Boxplots (qplot coding)
qplot(y = hwy, data = mpg, geom = "boxplot")

qplot(y = cty, data = mpg, geom = "boxplot")

qplot(x = class, y = hwy, data = mpg, geom = "boxplot")


# We now look at density plots or histograms to investigate the
# distributions of both variables. (The closer to normal
# the better)

# Histograms (Tidyverse Methods)

ggplot(data = mpg) +
  geom_histogram(mapping = aes(x = cty))


ggplot(data = mpg) +
  geom_histogram(mapping = aes(x = hwy))

#  Histograms (qplot coding)

qplot(x = hwy, data = mpg, geom = "histogram")

qplot(x = cty, data = mpg, bins = 10, fill = I("green"), 
      geom = "histogram", color = I("black"))


# The histograms are not quite normal and the boxplots do show 
# some outliers, but the scatter plot does show a strong linear 
# relationship between city and highway mileage.  Hence we will 
# generate the slope and y intercept of the regression equation.  

lm(hwy ~ cty, data=mpg)


#  Let's use R coding to graph the scatter plot and the Regression 
# line

ggplot(data = mpg) +
  geom_point(mapping = aes(x = cty , y = hwy)) +
  geom_smooth(method = lm,mapping = aes(x = cty , y = hwy), se=FALSE)+
  ggtitle("Scatter Plot (hwy vs. cty)")



# Hence the regression equation is ;

#  hwy = .892 + 1.337cty

# We will use this equation to predict highway mileage based on a 
# given city mileage value.

# How should you interpret the slope ?
# Recall that slope is (change in y) / (change in x)  1.337 / 1
# Answer:  For every increase of one mile per gallon for city
# driving, highway driving increases by 1.33 miles per gallon on
# average

# Finding a Residual
# By definition, a residual is found by subtracting the expected
# value from the observed value. If the residual is a positive
# number the observed value is above average. If the residual is a
# negative number the observed value is below average.

# Example
mpg

# For the mpg data set, find the residual for the observational 
# cty value of 29.


mpg%>%
  select(cty,hwy)%>%
  filter(cty == 29)
# Step 1: Find the observed value (in the table) for 29. In the
# table a cty value of 29 corresponds to a hwy value of 41.
# Step 2: Find the expected (or fitted value) for 29. Put 29 into
# your equation.    Expected value = .892 + 1.337(29) = 39.665
# Step 3: Now find the residual by subtracting. (Observed - Expected)
# 41-39.665 = 1.335  (The residual is 1.335)
# Step 4: Since the residual is a positive number, the value of 41
# is above average.

# We now look at output that is more comprehensive and informative
# regarding the regression analysis

lm(hwy ~ cty, data=mpg) -> x
x

summary(x)


# Note that the p value for the cty coefficient is well below .05,
# hence we can reject the null hypothesis that the population or 
# theoretical slope is equal to 0.  You cannot reject the null
# hypothesis that the intercept of the theoretical model is equal to
# 0 for its p value is higher than .05.  Also, take note of the
# t values.  High absolute t values are good. Low absolute t values are 
# bad. There is a high t value for the coefficient for cty, so we are
# confident that it is significantly different from 0. But that is
# not the case for the intercept.

# How much of the variation in hwy is explained by our model ?  That
# is what the Multiple R-squared value tells us.  Our model explains
# 91.38% of the variation in hwy.  That is pretty good.


# Another Example:

# We will investigate the relationship between the quantitative 
#variables Age and Total Cholesterol by using the collected sample 
# data below.

Agex <-c(25,25,28,32,32,32,38,42,48,51,51,58,62,65)
Agex

TCy <-c(180,195,186,180,210,197,239,183,204,221,243,208,228,269)
TCy
# We will check the scatter plot (Base R method)
plot(TCy~Agex)


# Now graph the regression line through the scatter plot
abline(lm(TCy ~ Agex))

# Produce the same plots using the qplot method.

# Now let us use Tidyverse (ggplot coding)
#  Create a data table (get a better looking scatterplot)
tribble(~TCy,      ~Agex,
          25,       180,
          25,       195,
          28,       186,
          32,       180,
          32,       210,
          32,       197,
          38,       239,
          42,       183,
          48,       204,
          51,       221,
          51,       243,
          58,       208,
          62,       228,
          65,       269
          ) ->  aa
aa

# Produce a tidyverse scatter plot

ggplot(data = aa) +
  geom_point(mapping = aes(x = Agex , y = TCy)) +
  geom_smooth(method = lm,mapping = aes(x = Agex , y = TCy), se=FALSE)
  

# Now produce the model

lm(TCy~Agex) -> zz
zz

summary(zz)

# y(hat) = 151.3537 + 1.3991X

# Let's find all residuals

residuals(zz)

# Another example

tribble(~pointspergamey,    ~hoursofpracticeperweekx,
        48,                     6,
        57,                     7.5,
        63,                      8,
        71,                     8.75,
        77,                     9.2,
        83,                     10,
        87,                     10.6,
        93,                     11.4,
        93,                     11.8,
        98,                     12.45
        ) -> S
S

ggplot(data = S) +
  geom_point(mapping = aes(x = hoursofpracticeperweekx , 
                           y = pointspergamey))+
  geom_smooth(method = lm, mapping = aes(x = hoursofpracticeperweekx,
                           y = pointspergamey))

lm(S) -> I
I

summary(I)

residuals(I)



round(residuals(I), 3)

# Add the residuals.  What is the answer ?



q()
y
