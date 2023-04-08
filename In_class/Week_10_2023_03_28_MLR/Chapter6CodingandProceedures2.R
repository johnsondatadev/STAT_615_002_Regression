
# Producing a Multiple Regression Model using
# Matrices

# Data Source

x1 <- c(.8,3.9,1.8,5.1,4.9,8.4,12.9,6.0,14.6,9.3)
x1

x2 <- c(2.8,2.6,2.4,2.3,2.5,2.1,2.3,2.0,2.2,1.1)
x2

x3 <- c(2.5,5.7,7.8,7.1,5.9,8.6,9.2,1.2,3.7,5.5)
x3

y <-  c(11,10.8,10.6,10.3,10.3,10.3,10.0,9.4,8.7,8.7)
y

dataalpha <- data.frame(x1,x2,x3,y)
dataalpha # y is the dependet variable

X1 <- matrix(c(1, .8, 2.8,2.5,
               1, 3.9,2.6,5.7,
               1, 1.8,2.4,7.8,
               1, 5.1,2.3,7.1,
               1, 4.9,2.5,5.9,
               1, 8.4,2.1,8.6,
               1, 12.9,2.3,9.2,
               1, 6.0,2.0,1.2,
               1, 14.6,2.2,3.7,
               1, 9.3,1.1,5.5), ncol = 4, byrow = TRUE)
X1

# Confirm the dimensions of 
dim(X1)


Y1 <- matrix(c(11,
               10.8,
               10.6,
               10.3,
               10.3,
               10.3,
               10.0,
               9.4,
               8.7,
               8.7), ncol =1, byrow = TRUE)
Y1

# First we need X1 transpose
t(X1) -> transposeX1
transposeX1

# Now multiply the transpose of X1 by X1
transposeX1%*%X1 -> Product2
Product2

# Check to see if this matrix has a determinate of
# 0.  If not then we can continue
det(Product2)


# Next find the Inverse of the product of the 
# X1 and its transpose
solve(Product2)


# The last step: Find the coefficients of your
# model

solve(Product2)%*%transposeX1%*%Y1 -> C
C

# Your multiple regression model has the form
# y = 7.9647004 + -0.1035379x1 + 0.9359110x2 +  0.1152394x3

# Compare and confirm results by using the lm method

x1 <- c(.8,3.9,1.8,5.1,4.9,8.4,12.9,6.0,14.6,9.3)
x1

x2 <- c(2.8,2.6,2.4,2.3,2.5,2.1,2.3,2.0,2.2,1.1)
x2

x3 <- c(2.5,5.7,7.8,7.1,5.9,8.6,9.2,1.2,3.7,5.5)
x3

y <-  c(11,10.8,10.6,10.3,10.3,10.3,10.0,9.4,8.7,8.7)
y

dataalpha <- data.frame(x1,x2,x3,y)
dataalpha


lm(y ~ x1+x2+x3) -> modelalpha
modelalpha

summary(modelalpha)
# Same coefficient estimates; same model


# Now find the SSE (Sum of Squares Errors or the
# Sum of Squares Residuals)

# First create the b matrix

b <- matrix(c(7.9647,
              -.1035,
              .9359,
              .1152), ncol = 1, byrow = TRUE)
b

# Or you could simply use the coding above that created the
# coefficient solutions:  
solve(Product2)%*%transposeX1%*%Y1 -> C
C


# R coding needed to find the SSE

t(Y1)%*%Y1 - t(C)%*%t(X1)%*%Y1 # answer: 0.3605227

# Now confirm by using lm coding 
anova(modelalpha)

# note that we produced an ANOVA TABLE that shows 
# the SSE.

# Another Example (Textbook Chapter 6)

Xone <- matrix(c(1,68.5,16.7,
                 1,45.2,16.8,
                 1,91.3,18.2,
                 1,47.8,16.3,
                 1,46.9,17.3,
                 1,66.1,18.2,
                 1,49.5,15.9,
                 1,52.0,17.2,
                 1,48.9,16.6,
                 1,38.4,16.0,
                 1,87.9,18.3,
                 1,72.8,17.1,
                 1,88.4,17.4,
                 1,42.9,15.8,
                 1,52.5,17.8,
                 1,85.7,18.4,
                 1,41.3,16.5,
                 1,51.7,16.3,
                 1,89.6,18.1,
                 1,82.7,19.1,
                 1,52.3,16.0), ncol = 3, byrow = TRUE)
Xone

Yone <- matrix(c(174.4,
                 164.4,
                 244.2,
                 154.6,
                 181.6,
                 207.5,
                 152.8,
                 163.2,
                 145.4,
                 137.2,
                 241.9,
                 191.1,
                 232.0,
                 145.3,
                 161.1,
                 209.7,
                 146.4,
                 144.0,
                 232.6,
                 224.1,
                 166.5), ncol = 1, byrow = TRUE)
Yone

# Find b
t(Xone)%*%Xone -> Product3
Product3
  solve(Product3)%*%t(Xone)%*%Yone -> coefficients1
  coefficients1
  
# Find SSE
t(Yone)%*%Yone - t(coefficients1)%*%t(Xone)%*%Yone



# Now confirm the same result by using the lm method

Xa <- c(68.5,45.2,91.3,47.8,46.9,66.1,49.5,52.0,48.9,
        38.4,87.9,72.8,88.4,42.9,52.5,85.7,41.3,51.7,
        89.6,82.7,52.3)
Xa

Xb <-  c(16.7,16.8,18.2,16.3,17.3,18.2,15.9,17.2,16.6,
         16.0,18.3,17.1,17.4,15.8,17.8,18.4,16.5,16.3,
         18.1,19.1,16.0)
Xb

Yall <- c(174.4,164.4,244.2,154.6,181.6,207.5,152.8,
          163.2,145.4,137.2,241.9,191.1,232.0,145.3,
          161.1,209.7,146.4,144.0,232.6,224.1,166.5)
Yall


anova(lm(Yall ~ Xa + Xb))

# Finding Joint Bonferroni Intervals

library(tidyverse)

read_csv("Dataset/Grocerytwo.csv", show_col_types = F) -> GT
GT

lm(y~xone+xtwo+xthree, data = GT) -> modelGT
modelGT

summary(modelGT)

# Find Bonnferroni Joint Confidence Intervals for B1 and
# B3  (95%)

# b1 +/- B(se)
# b3 +/- B(se)

# Find B

.05/4

1 - .0125

# Use r coding


qt(p = .9875, df = 48, lower.tail = TRUE)

# B = 2.313899

# Let's find the interval for B1

# b1 - B(se)  (lower bound for B1)  

.0007871 - 2.313899*.0003646
# -5.654758e-05

# b1 + B(se)  (upper bound for B1)  
.0007871 + 2.313899*.0003646
# 0.001630748

        -.00005654758 < B1 < 0.001630748

# Let's find the interval for B3

# b3 +/- B(se)
# b3 - B(se)  (lower bound for B3)
623.6 - 2.313899*62.64
# 478.6574

# b3 + B(se)  (upper bound for B3)
623.6 + 2.313899*62.64
# 768.5426
           478.6574 < B3 < 768.5426
