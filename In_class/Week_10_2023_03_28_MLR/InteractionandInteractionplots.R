

library(tidyverse)

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

interaction.plot(lm(SellingPrice~Size + Taxes + Size:Taxes, Homes))
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



# Two Way ANOVA

# Two Way ANOVA compares population means across categories of two explanatory variables. Each null
# hypothesis states that the population means are identical across categories of one categorical
# variable, controlling for the other one.

# Requirements to perform the the Two Way ANOVA
#     1. the populations are normal
#     2. the samples are independent
#     3. the populations have the same variance

# Hypothesis Testing for Two Way ANOVA
#   H(O): There is no interaction between the factors  H(A): There is interaction between factors
#   H(O): There is no effect of factor A   H(A): There is effect of factor A
#   H(O): There is no effect of factor B   H(B): There is effect of factor B

# We introduce another factor for the MilkData data.  The new factor
# is  Brand.


#                SkinMilk     MixedMilk         WholeMilk

#    BrandA     857 , 853     1006 , 991        879 , 938
#               865 , 904     1015 , 1035       841 , 818
#
#    BrandB     916 , 886     1024 , 1013       870 , 874
#               854 , 856     1065 , 1062       881 , 836

# We arrange or structure the data as follows for R input
# and analysis

tribble(~Brand,   ~MilkType,    ~Calcium,
        "BrandA",   "SkinMilk",   857,
        "BrandA",   "SkinMilk",   853,
        "BrandA",   "SkinMilk",   865,
        "BrandA",   "SkinMilk",   904,
        "BrandA",   "MixedMilk",  1006,
        "BrandA",   "MixedMilk",  991,
        "BrandA",   "MixedMilk",  1015,
        "BrandA",   "MixedMilk",  1035,
        "BrandA",   "WholeMilk",  879,
        "BrandA",   "WholeMilk",  938,
        "BrandA",   "WholeMilk",  841,
        "BrandA",   "WholeMilk",  818,
        "BrandB",   "SkinMilk",   916,
        "BrandB",   "SkinMilk",   886,
        "BrandB",   "SkinMilk",   854,
        "BrandB",   "SkinMilk",   856,
        "BrandB",   "MixedMilk",  1024,
        "BrandB",   "MixedMilk",  1013,
        "BrandB",   "MixedMilk",  1065,
        "BrandB",   "MixedMilk",  1002,
        "BrandB",   "WholeMilk", 870,
        "BrandB",   "WholeMilk", 874,
        "BrandB",   "WholeMilk", 881,
        "BrandB",   "WholeMilk", 836
) -> Milk
Milk


as.factor(Milk$Brand) -> Milk$Brand
Milk$Brand

as.factor(Milk$MilkType) -> Milk$MilkType
Milk$MilkType

str(Milk)


aov(Calcium ~ Brand + MilkType + Brand:MilkType, 
    data = Milk)-> aovMilk
aovMilk
summary(aovMilk)


interaction.plot(Milk$Brand, Milk$MilkType, Milk$Calcium, xlab = "Brand",
                 ylab = "Calcium")

# Interpreting the output table:
#   1) Note that the factor Brand is not significant, hence  
#      Calcium is not significantly impacted by variation in Brand
#      You cannot reject the null hypothesis that there is no effect
#      for the factor Brand

#   2) Note that the factor Milktype is significant, hence Calcium
#      is significantly impacted by variation in Milktype
#      You can reject the null hypothesis that there is no effect
#      for the factor MilkType

#   3) The interaction term is not significant. This indicates that
#      the relationship between Calcium and one of the factors does
#      not depend on its relationship with the other factor. 
#      The null hypothesis that there is no interaction can not be
#      rejected





# Example 2

tribble(~Weight,    ~pH,   ~Calluna,
        2.76, "pH3.5", "Present",
        2.39, "pH3.5", "Present",
        3.54, "pH3.5", "Present",
        3.71, "pH3.5", "Present",
        2.49, "pH3.5", "Present",
        4.10, "pH3.5",  "Absent",
        2.72, "pH3.5",  "Absent",
        2.28, "pH3.5",  "Absent",
        4.43, "pH3.5",  "Absent",
        3.31,  "pH3.5",  "Absent",
        3.21, "pH5.5", "Present",
        4.10, "pH5.5", "Present",
        3.04, "pH5.5", "Present",
        4.13, "pH5.5", "Present",
        5.21, "pH5.5", "Present",
        5.92, "pH5.5", "Absent",
        7.31, "pH5.5",  "Absent",
        6.10, "pH5.5",  "Absent",
        5.25, "pH5.5",  "Absent",
        7.45, "pH5.5",  "Absent"
        
) ->festuca
festuca


as.factor(festuca$pH) -> festuca$pH
as.factor(festuca$Calluna) -> festuca$Calluna

str(festuca)


aov(Weight ~ pH + Calluna + pH:Calluna, 
    data = festuca)-> aovfestuca
aovfestuca
summary(aovfestuca)


interaction.plot(festuca$pH, festuca$Calluna, festuca$Weight, xlab = "ph",
                 ylab = "weight")


#  Example 3
warpbreaks



as.factor(warpbreaks$wool) -> warpbreaks$wool
as.factor(warpbreaks$tension) -> warpbreaks$tension

str(warpbreaks)


aov(breaks ~ wool + tension + wool:tension, data = warpbreaks) -> wp
wp

summary(wp)


interaction.plot(warpbreaks$wool, warpbreaks$tension, warpbreaks$breaks, xlab = "wool",
                 ylab = "breaks")
# note that the lines are non-parallel, hence significant interaction.

# Example 4
tribble( ~Gender,   ~Age,   ~Score,
         "boy",    "ten",     4,
         "boy",    "ten",     6,
         "boy",    "ten",     8,
         "girl",   "ten",     4,
         "girl",   "ten",     8,
         "girl",   "ten",     9,
         "boy",    "eleven",  6,
         "boy",    "eleven",  6,
         "boy",    "eleven",  9,
         "girl",   "eleven",  7,
         "girl",   "eleven",  10,
         "girl",   "eleven",  13,
         "boy",    "twelve",  8,
         "boy",    "twelve",  9,
         "boy",    "twelve",  13,
         "girl",   "twelve",  12,
         "girl",   "twelve",  14,
         "girl",   "twelve",  16 
)-> V

V   

as.factor(V$Gender) -> V$Gender

as.factor(V$Age) -> V$Age
str(V)

aov(Score ~ Gender + Age + Gender:Age, data = V) ->aovV
aovV

summary(aovV)


interaction.plot(V$Gender, V$Age, V$Score, xlab = "Gender",
                 ylab = "Score")
# The lines are close to being parallel, hence no interaction or very
# weak interaction


#  Classwork

tribble(~genotype,  ~gender,  ~activity,
        "FF", "Female", 3.34,
        "FF", "Female", 4.72,
        "FF", "Female", 3.39,
        "FO", "Female", 4.05,
        "FO", "Female", 5.06,
        "FO", "Female", 3.59,
        "OO", "Female", 4.12,
        "OO", "Female", 3.58,
        "OO", "Female", 4.09,
        "FF", "Male", 2.20,
        "FF", "Male", 2.60,
        "FF", "Male", 5.26,
        "FO", "Male", 2.72,
        "FO",  "Male", 3.28,
        "FO",  "Male", 3.43,
        "OO",  "Male", 3.12,
        "OO",  "Male", 3.74,
        "OO",  "Male", 4.60
        
) -> ac
ac


diamonds

AB <- lm(price ~ cut + color + cut*color , data = diamonds)
AB
summary(AB)
aov(AB)

interaction.plot(diamonds$cut, diamonds$color, diamonds$price, 
                 xlab = "cut",
                 ylab = "price")