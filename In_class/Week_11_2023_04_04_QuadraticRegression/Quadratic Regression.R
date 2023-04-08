

# Now lets explore Quadratic Regression Models

Hours <- c(6,9,12,14,30,35,40,47,51,55,60)
Hours

Happiness <- c(14,28,50,70,89,94,90,75,59,44,27)
Happiness

data <- data.frame(Hours, Happiness)
data

plot(Happiness ~ Hours) # Do the data points suggest that a linear
# model is appropriate ? No


# Let's generate a linear model and investigate important features
# of the linear model.

linearmodel <- lm(Happiness ~ Hours, data)
linearmodel

# Linear Model:  Happiness = 48.4531 + .2981Hours

summary(linearmodel)

# Note that the Multiple R Squared Value is only 0.0446.

# What is the Multiple R Squared value ?  Is the independent
# variable significant ?



# Lets create a quadratic model !! y = ax + bx^2 + c

#create a new variable for hours^2
data$Hourssq <- data$Hours^2

#fit the quadratic regression model
lm(Happiness ~ Hours + Hourssq, data=data) -> QuadraticModel
QuadraticModel

summary(QuadraticModel)


# How does the Multiple R squared value for the Quadratic model differ
# from the Multiple R squared value for the quadratic model ?

# Note that the Multiple R Squared Value has increased to 0.9502 

#create sequence of hour values
HourValues <- seq(0, 60, 0.1)
HourValues


#create list of predicted happines levels using quadratic model
HappinessPredict <- predict(QuadraticModel,list(Hours = HourValues,
                                                Hourssq=HourValues^2))
HappinessPredict


#create scatterplot of original data values
plot(data$Hours, data$Happiness)

#add predicted lines based on quadratic regression model
lines(HourValues, HappinessPredict, col='red')

# The quadratic model is therefore

Happiness = -0.1012*(Hours)^2 + 6.7444*(Hours) - 18.2536

# We can use this equation to find the predicted happiness of an 
# individual, given the number of hours they work per week.

# For example, an individual that works 60 hours per week is predicted 
# to have a happiness level of 22.09:

Happiness = -0.1012*(60)^2 + 6.7444*(60) - 18.2536 = 22.09

# Conversely, an individual that works 30 hours perk week is predicted to 
# have a happiness level of 92.99:

Happiness = -0.1012*(30)^2 + 6.7444*(30) - 18.2536 = 92.99



# Another Example

# Data Source


Time <-  c(0, 1, 2, 4, 6, 8, 9, 10, 11, 12, 13, 
           14, 15, 16, 18, 19, 20, 21, 22, 24, 25, 26, 27, 28, 29, 30)
Time

Counts <- c(126.6, 101.8, 71.6, 101.6, 68.1, 62.9, 45.5, 41.9, 
           46.3, 34.1, 38.2, 41.7, 24.7, 41.5, 36.6, 19.6, 
           22.8, 29.6, 23.5, 15.3, 13.4, 26.8, 9.8, 18.8, 25.9, 19.3)
Counts

# Let's create a data frame

Data <- data.frame(Counts,Time)
Data


# Let's generate a plot

plot(Time, Counts)

#Which is a better fit for the scatter plot, linear or quadratic ?

#Let's look at specifics of the linear model.
  
lm(Counts ~ Time) -> linearmodel
linearmodel

summary(linearmodel)

# We now code to create a quadratic model
Timesq <- Time^2
Timesq

quadraticmodel <-lm(Counts ~ Time + Timesq)
quadraticmodel

summary(quadraticmodel)

# Multiple R squared:   linear model     quadratic model

# Set specific parameters for 
timevalues <- seq(0, 30, 0.1)
timevalues

# Find fitted/predicted values for the quadratic model

predictedcounts <- predict(quadraticmodel,list(Time=timevalues, 
                          Timesq=timevalues^2))
predictedcounts




plot(Time, Counts, pch=16, xlab = "Time (s)", ylab = "Counts", 
     cex.lab = 1.3, col = "blue")

# Use the lines() command to graph the curve through the data points

lines(timevalues, predictedcounts, col = "red", lwd = 3)




predictedcounts <- predict(quadraticmodel,list(Time=timevalues, 
                                               Timesq=timevalues^2))
predictedcounts



predictedcountslm <- predict(linearmodel,list(Time=timevalues
                                               ))
predictedcountslm



lines(timevalues, predictedcountslm, col = "green", lwd = 3)


# A third example  



set.seed(756328)                        # Create example data
x <- rnorm(100)
x
y <- rnorm(100) + x
y

data1 <- data.frame(y,x)
data1

plot(y ~ x)

lm(y ~ x + I(x^2) + I(x^3) + I(x^4))

lm(y ~ x + I(x^2) + I(x^3) + I(x^4)) -> polymodel
polymodel

summary(polymodel)


lm(y ~ poly(x, 4, raw = TRUE))          # Using poly function
