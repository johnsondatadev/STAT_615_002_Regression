# Multiple Linear Regression / Using Dummy Variables

#  We now consider producing Multiple Linear Regression Models 
# that have categorical variables.In order to do so, we create 
# what are called dummy or indicator variables.

# Example  

# The following table displays information regarding variables 
# that are related to the price of a home.  High School Status 
# refers to a ranking of the high school near the home 
# ( E is "exemplary",
# NE is not "exemplary") Note that the variable High School Status
# is categorical having two levels. the other explanatory variable
# sqft is quantitative and the response variable price is quantitative.


#  High School Status     sqft          price

#         NE              1872           145
#         NE              1954           69.9
#          E              4104           315
#         NE              1524           144.9
#         NE              1297           134.9
#          E              3278           369
#         NE              1192           95
#          E              2252           228.9
#         NE              1620           149
#          E              2466           295
#          E              3188           388.5
#         NE              1061           75
#         NE              1195           130
#          E              1552           174
#          E              2901           334.9 

# We now structure and prepare the data for R input 

price<- c(145,69.9,315,144.9,134.9,369,95,228.9,149,295,388.5,75,130,
          174,334.9) 
price

sqft<-c(1872,1954,4104,1524,1297,3278,1192,2252,1620,2466,3188,1061,
        1195,1552,2901)
sqft

HighSchoolStatus<- c("NE","NE","E","NE","NE","E","NE","E","NE","E",
                     "E","NE","NE","E","E")
HighSchoolStatus

# Lets create a data frame  (data table)

data.frame(sqft, HighSchoolStatus, price)-> dff
dff

# Use the factor command to view the levels (or factors) of
# the categorical variable 

factor(dff$HighSchoolStatus)

# Typically, the levels of a categorical variable are assigned values
# of 0 or 1. There are two levels.  R will assign E to 1  and NE to 0.
# The standard number assignment for levels of a categorical variable
# are processed as follows:

#   E -> 1;            NE -> 1;
#   0 otherwise         0 otherwise
# Note:You could code using the numerical assignments by hand, but R
# will do it automatically.

# We will now produce the model

lm(price ~ sqft + HighSchoolStatus , dff)->reg
reg

summary(reg)  

# The Multiple Linear Regression Model is

# E(price) = 125.72281 + 0.06207sqft - 98.64787HighSchoolStatusNE 

# Remember HighSchoolStatusNE is a categorical variable that can 
# take on a value of 0 or 1

# Use R to determine which level gets the 1 and which level gets the
# 0.

contrasts(as.factor(dff$HighSchoolStatus))

# Find the average price of a house if the HighSchool Status is 
# exemplary and the square footage is 2500

# E(price) = 125.72281 + 0.06207(2500) - 98.64787(0)
# E(price) = 280.898

# Find the average price of a house if the HighSchool Status is 
# not exemplary and the square footage is 2500

# E(price) = 125.72281 + 0.06207(2500) - 98.64787(1)
# E(price) = 182.2499



#Example 3

# We will now call the data set "Salaries" to illustrate multiple 
# regression involving a catagorical variable

# Install the following package
install.packages("car")

# Now call the library
library(car)

# Now call the data set
Salaries

# Lets produce a Linear Regression Model that predicts salary
# based on the independent variables sex and yrs,service 
# (years of service)

#First lets acknowledge that R will create the dummy variables 
# for the variable sex

contrasts(as.factor(Salaries$sex))

# Compute the model
model1 <- lm(salary ~ yrs.service + sex, data = Salaries)
model1

# The Linear Multiple Regression model is ;
#  y = 92356.9 + 747.6yrs.service + 9071.8sexMale

# If sex is Male and years of service is 7, what is the predicted
# salary ?
# Answer;   y = 92356.9 + 747.6(7) + 9071.8(1)   
#             =  106661.9    $106,661.9


# If sex is Female and years of service is 7, what is the predicted
# salary ?

# Answer;   y = 92356.9 + 747.6(7) + 9071.8(0)   
#             =  97590.1     $97,590.1



# Lets add another categorical variable to our model;  rank.
# Note that the categorical variable rank, has three levels:
# AsstProf, AssocProf , and Prof

# We can also use R to find the Levels
factor(Salaries$rank)


# Lets acknowledge that R will create the dummy variables
contrasts(as.factor(Salaries$rank))

# Lets create the new model.

model2 <- lm(salary ~ yrs.service + sex + rank,  data = Salaries)
model2

contrasts(as.factor(Salaries$rank))

# For Which Model is the coefficient for the variable yrs.service 
# not significant?  Model 1 or Model 2. Also, are both models
# significant?  Investigate by calling the summaries for both models

summary(model1)

summary(model2)


# Yes both models are significant.
# For model 2 the coefficient for yrs of service is not significant
# for the p value is .13694

# Let us look at the regression equation for model 2

# E(Salaries) = 76612.8 -171.8yrs.service + 5468.7sexMale 
# + 14702.9rankAssocProf + 48980.2rankProf

# Let's predict a salary for a male employee who is an Assistant
# Professor and who has 22 years of service.

# E(Salaries) = 76612.8 -171.8(22) + 5468.7(1) 
# + 14702.9(0) + 48980.2(0)  =  728333.2 

#  $ 72,8333.20 



# Another Example:
# Now we use R coding to produce the summary table found on page 277
# in your book.  The data is on page 274. The required reading starts
# at the bottom of page 273 to page 277.  The reading involves an 
# observational study on bats and birds.   The task is to produce a
# Linear Multiple Regression Equation that uses independent variables
# Species and Mass to predict Flight Energy for bats and birds


# The table on page 274 features three levels for the categorical
# variable Species; they are :  Echolocating bats -> elb,  
# Non-Echolocating birds ->  nelbi, and Non-Echolocating bats -> nelba

# The R coding that I developed follows

Spec<- c("elba","elba","elba","elba","nelbi","nelbi","nelbi","nelbi",
         "nelbi","nelbi", "nelbi","nelbi","nelbi", "nelbi","nelbi",
         "nelbi", "nelba", "nelba","nelba","nelba")
Spec

Mass<-c(779,628,258,315,24.3,35,72.8,120,213,275,370,384,442,412,
        330,480,93,8,6.7,7.7)
Mass

qqnorm(log(Mass))

FEE<-c(43.7,34.8,23.3,22.4,2.46,3.93,9.15,13.8,14.6,22.8,26.2,25.9,
       29.5,43.7,34,27.8,8.83,1.35,1.12,1.02)
FEE

# Lets produce a data table

data.frame(Spec, log(Mass),log(FEE))-> BB
BB


factor(BB$Spec)

# Now create the linear multiple regression model

lm(log(FEE) ~ Spec + log(Mass) , BB) -> lmodel

lmodel
summary(lmodel)


# Dummy variable construction    (number of levels - 1)

# Specnelba = 1 , 0 otherwise
# Specnelbi = 1 , 0 otherwise
# The reference level is therefore is elba  
# (If a prediction is desired for elba, set the other two
# dummy variables equal to 0)

# Again, we let R create the Dummy Variable Matrix

contrasts(as.factor(BB$Spec))

# E(logFEE) = 0.81496log(Mass) + 0.07866Specnelba + 
# 0.10226Specnelbi -1.57636 

# Use your model to predict the log(FEE) for species nelbi and
# if log(Mass) = 5.829 

# E(logFEE) = 0.81496(5.829) + 0.07866(1) + 0.10226(0) -1.57636 
# E(logFEE) = 3.25270



# CLASSWORK
# Data
Gender<-c("Female","Female","Female","Female","Female","Female","Female","Female","Female","Female",
          "Male","Male","Male","Male","Male","Male","Male","Male","Male","Male")
Gender

MRICount<-c(816932,951545,991305,833868,856472,852244,790619,866662,857782,948066,949395,1001121,
            1038437,965353,955466,1079549,924059,955003,935494,949589)
MRICount

IQ <- c(133,137,138,132,140,132,135,130,133,133,140,140,139,133,133,141,135,139,141,144)
IQ

#1 Use and show R code to Create a data frame for the collection of 
# vectors above.
#2 Use and show R code to produce a Multiple Linear Regression Model.
#3 Use and show R code to produce summary indicators for your model.
# What is the p value for your model?  What is the Multiple 
# Rsquared value for your model?
#4 Use your regression model to predict IQ for a Female who has an 
# MRICount of 855000
#  (Show all of your work.)

#5  Do you thinK that this is a good model.  Justify your answer.














# Multiple Linear Regression Interaction

library(tidyverse)

tribble(~milestraveled,  ~numdeliveries,       ~gasprice,         ~traveltime,
        89,                    4,                 3.84,               7,
        66,                    1,                 3.19,              5.4,
        78,                    3,                 3.78,              6.6,
        111,                   6,                 3.89,              7.4,
        44,                    1,                 3.57,              4.8,
        77,                    3,                 3.57,              6.4,
        80,                    3,                 3.03,               7,
        66,                    2,                 3.51,              5.6,
        109,                   5,                 3.54,              7.3,
        76,                    3,                 3.25,              6.4
        ) -> K
K



# An analysis of a full regression model  

#  Produce the full regression model for the data table K. 
#  (traveltime is the response variable)

lm(traveltime~milestraveled + numdeliveries + gasprice, K)
        

# The full multiple regression model is:
#  traveltime  =  0.01412milestraveled + 0.38315numdeliveries
#  - 0.60655gasprice  

# Interpret the coefficient for the explanatory variable milestraveled.
#      .01412/1
#      If the variables numdeliveries and gasprice are held 
#      constant,  for every increase of 1 mile the traveltime 
#      increases by 0.01412 minutes.


# Find a 95% confidence interval for the coefficient of the
# numdeliveries variable

lm(traveltime~milestraveled + numdeliveries + gasprice, K)-> AA
AA
summary(AA)

#    #   b +/-  t(se)

#     b = .38315,  df = n - (k+1) = 10 - (3+1) = 6,  se = 0.30006

#  Find t
qt(p=.025, df=6, lower.tail = FALSE)


#   .38315 +/- 2.446912(0.30006)  =  .38315 +/- .73422 
#   .38315 - .73422 = -0.35107
#   .38315 + .73422 =  1.11737

# LB -0.35107        UB 1.11737

# Interpretation: We are 95% confident that the true population 
# coefficient for numdeliveries falls between  -0.35107 and 1.11737


# Are there issue of multicollinearity among any independent variables
# ?

cor(K)

# Is the dependent variable correlated with all independent variables
# ?

# Which coefficients are significant at the level of .05 ?

summary(AA)

#  Interpret the Residual standard error

#  On average, the data points are .3447 units away from
#  the regression line.  Your model is about .3447 off (high or low)
#  on average

#  Interpret the standard error for numdeliveries
#  The standard error for numdeliveries is .30006.  This is the 
#  typical distance between a sampled numdeliveries coefficient 
#  and the true population numdeliveries coefficient. Also, .30006 is
#  the standard deviation of the sampling distribution for the
#  numdeliveries coefficient.

#  Interpret the table value .8947
#    This is the Multiple R-squared; Your model explains 89.47% of the
#    variation in the dependent variable traveltime.

#  Interpret the table value .842
#    This is the Adjusted R-squared: Your model explains 84.2% of the
#    variation in the dependent variable traveltime using only those 
#    independent variables that have an impact on the
#    dependent variable


#  Should the null hypothesis that the numdeliveries explanatory 
#  variable coefficient is equal to zero be rejected ?

#         No, because the p value is .2488 which is greater than .05.

#  Should the null hypothesis that all of the population explanatory
#  variable coefficients equal zero be rejected ?

#  Yes, because the p value of .002452 (at the very bottom of the 
#  summary table) is less than .05.  Therefore, we embrace the 
#  alternative hypothesis that at least one of the population 
#  explanatory coefficients is not equal to zero. Therefore 
#  collectively the explanatory variables have a significant linear
#  relationship with the response variable.
         
# Comparing Models

cor(K)

lm(traveltime~milestraveled, K)-> K1
K1
summary(K1)




lm(traveltime~numdeliveries, K)-> K2
summary(K2)





lm(traveltime~numdeliveries + milestraveled, K)-> K3  
summary(K3) # collinarity problem

cor(K3)






lm(traveltime~numdeliveries + gasprice, K)-> K4
summary(K4)





lm(traveltime~milestraveled + gasprice, K)-> K5
summary(K5)






lm(traveltime~milestraveled + gasprice + numdeliveries, K)-> K6
summary(K6)







# Interaction terms

#  Interaction:An interaction occurs when an explanatory variable 
#  has a different effect on the outcome depending on the values 
#  of another explanatory variable.

tribble(  ~SellingPrice,     ~Size,  ~Taxes,  ~Bedrooms, ~Bathrooms,  
        279900,               2048,    3104,       4,          2,       
        146500,                912,    1173,       2,          1,       
        237700,               1654,    3076,       4,          2,      
        200000,               2068,    1608,       3,          2,       
        159900,               1477,    1454,       3,          3,       
        499900,               3153,    2997,       3,          2,       
        265500,               1355,    4054,       3,          2,       
        289900,               2075,    3002,       3,          2,       
)-> Homes
Homes

# Example
# Suppose we have a dependent variable  Selling Price and the two
# explanatory variables are  Size and Taxes.  
# A typical Multiple Linear Regression model is given below:

#  Selling Price = -70604.09 + 128.35Size + 36.74Taxes

# This is our model if Selling price is impacted by Size independent
# of values for Taxes or Selling Price is impacted by Taxes 
# independent of values for Size. 

# If however Selling price is impacted by one of our explanatory 
# variables, but the impact depends on different values for the other
# explanatory variable , we have interaction between the 
# explanatory variables.  For example suppose Selling Price is higher
# when Size is large and Taxes are high, but Selling Price is lower 
# when Size is large and Taxes are low.  

# If interaction exists we introduce another variable called an 
# interaction term that is formed by multiplying the two variables 
# Size * Taxes.  We consider adding this term to the model if the 
# its coefficient is significant.


# Using the data table Homes we first produce summary out put for the standard multiple linear
# regression model involving Selling Price, Size and Taxes.

lm(SellingPrice~Size + Taxes, Homes)->UU
UU
summary(UU)

# Our proposed Multiple Linear Regression Model without the interaction term is:

#     Selling Price = -70604.09 + 128.35Size + 36.74Taxes

#  Suppose Size = 2995, and Taxes = 3000

#  Selling Price = -70604.09 + 128.3*2995 + 36.74*3000 = 423874.4  
#  ( A reasonable answer given the values for the table))

-70604.09 + 128.3*2995 + 36.74*3000

# Using the data table Homes we first produce summary out put for the standard multiple linear
# regression model involving Selling Price, Size and Taxes and the interaction term 
# Size * Taxes

lm(SellingPrice~Size + Taxes + Size:Taxes, Homes)->uu
uu
summary(uu)

# Our proposed Multiple Linear Regression Model with the interaction term is:

#  Selling Price = 196800 - 65.24Size - 66.69Taxes + .07231(Size*Taxes) 

# Suppose Size = 2995, and Taxes = 3000

# Our Selling Price = 196800 - 65.24*2995 - 66.69*3000
# + .07231(2995*3000) = 451041.5

196800 - 65.24*2995 - 66.69*3000 + .07231*(2995*3000)

#  Analysis  

# Our interaction term is significant, we have high R(squared). 
# It is a good idea to include the interaction term in the model
# primarily because the  model is likely to produce answers that
# are consistent with typical table results.


#  Classwork Data Table


tribble(~WorkExperience, ~LevelofEducation, ~AnnualIncomeThou,
        21,                      6,               31.7,
        14,                      3,               17.9,
        4,                       8,               22.7,
        16,                      8,               63.1,
        12,                      4,               33,
        20,                      4,               41.4,
        25,                      1,               20.7,
        8,                       3,               14.6,
        24,                      12,              97.3,
        28,                       9,              72.1,
        4,                       11,              49.1,
        15,                       4,              52
)->X
X





q()
y