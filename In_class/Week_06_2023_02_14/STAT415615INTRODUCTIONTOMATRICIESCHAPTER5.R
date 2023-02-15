Chapter 5

# What is a matrix ?

#  A matrix is a rectangular array of values, characterized by rows and
#  columns

# Below is a 3 by 4 matrix (3 rows and 4 columns)

#     2   4   -3   6
#     1   3    5  -1
#     6   0    3   8


# Performing operations on matrices using R code

# How to add and subtract matrices using R code

A <- matrix(c(10, 8, 
              5, 12), ncol = 2, byrow = TRUE)
A

B <- matrix(c(5, 3,
              15, 6), ncol = 2, byrow = TRUE)
B

dim(A) # 2 2
dim(B) # 2 2

A + B

A - B


C <- matrix(c(1, -4, 5,
              6, 0, -3), ncol = 3, byrow = TRUE)
C

D <- matrix(c(-3, 7, 2,
              3, -2, -10), ncol = 3, byrow = TRUE)
D

dim(C)
dim(D)

C + D

C - D


E <- matrix(c(1, -4, 5, -10,
              6, 0, -3,  6,
              12, 5,-11, -4), ncol = 4, byrow = TRUE)
E

G <- matrix(c(-3, 7, 2, 7,
              3, -2, -10, -2,
              1,  0, 5, -10), ncol = 4, byrow = TRUE)
G

E + G

E - G


# How to multiply matrices using R code

# Multiplying and dividing a matrix by a scalar

# Multiply the matrix A above by -4

  -4 * |10  8|
       | 5 12|
  
# Using R code:
  
A <- matrix(c(10, 8, 
              5, 12), ncol = 2, byrow = TRUE)
A

-4*A

  # Divide the matrix A above by 2

     |10  8|  /2
     | 5 12|

# Using R code:
A <- matrix(c(10, 8, 
              5, 12), ncol = 2, byrow = TRUE)
A

A/2

# Multiply a matrix by another matrix

A <- matrix(c(10, 8, 
              5, 12), ncol = 2, byrow = TRUE)
A


B <- matrix(c(5, 3,
              15, 6), ncol = 2, byrow = TRUE)
B


# Element wise multiplication
|10  8| * |5  3|
| 5 12|   |15 6|

|50 24|
|75 72|
  
# Using R code
A * B
  
# (row by column) multiplication
#  |10  8| * |5  3|
#  | 5 12|   |15 6|  
  
# (10*5 + 8*15) ,  (10*3 + 8*6)
# (5*5 + 12*15) ,  (5*3 + 12*6)

 #  |170 78|
#   |205 87|
  
# Using R code
A%*%B


# How to find the Transpose of a matrix

# For the matrix A  |10 8| 
#                   |5 12|,
# the Transpose of A is the following;   |10 5 |
#                                        |8  12|

# Using R code  

A <- matrix(c(10, 8, 
              5, 12), ncol = 2, byrow = TRUE)
A

t(A)

# How to find the power of a Matrix

# Consider the following coding for multiplying A by itself.

A%*%A

# The same result can be achieved by raising A to the 2nd power.
install.packages("expm")
library(expm)

A %^% 2

# Now let's raise matrix B to the 3rd power.

B <- matrix(c(5, 3,
              15, 6), ncol = 2, byrow = TRUE)
B

B %^% 3

# Which of course is the same as;

B%*%B%*%B

# What is the identity (2 by 2) matrix ?

1  0
0  1

# Using R code 

diag(2)

# what is the identity (3 by 3) matrix ?

1 0 0
0 1 0
0 0 1

# Using R code

diag(3)

# The Identity matrix multiplied by an arbitrary matrix produces the
# arbitrary matrix.

# Lets multiply matrix A by the identity matrix.
A %*% diag(2)

# How to find the Inverse of a Matrix
# By definition, a matrix multiplied by its inverse will result in
# producing the identity matrix.

# Find the Inverse matrix for matrix A

A <- matrix(c(10, 8, 
              5, 12), ncol = 2, byrow = TRUE)
A

solve(A) -> inverseA
inverseA

# Now confirm that your answer is the Inverse matrix.

A%*%inverseA   # Note that the answer is the identity matrix.


# How to determine the rank of a matrix

# By definition, the rank of a matrix is the maximum number of linearly
# independent columns in the matrix.

# Consider the following example:
     |1  2  5  1|
     |2  2  10 6|
     |3  4  15 1|
# There is only one column that is linearly dependent (on another column)
# Column 3 can be written as a linear combination of column 1.
# There is no such relationship for the other 3, hence columns 1,2, and 4
# are linearly independent.  The rank therefore is 3.


# Finding the determinant of a Matrix.

# 2 by 2
|10 8|
|5 12|

   
(10 * 12) - (8 * 5)

# Using R code

A <- matrix(c(10, 8, 
              5, 12), ncol = 2, byrow = TRUE)
A

det(A)




M <- matrix(c(1, -4, 5,
              6, 0, -3,
              12, 5,-11), ncol = 3, byrow = TRUE)
M

|1 -4  5|
|6  0 -3|     1*det|0 -3|   -  -4*det|6   -3|  + 5*det|6  0|
|12 5 -11|        |5 -11|            |12 -11|         |12 5|
  
          =     1*15         -       -4*-30       + 5*30
          =      15                   -120          150
          =      45

# Using R code

det(M)

# More Practice !!

# Find the following product
# A multiplied by B Transpose

Step 1  Lets find A

A

Step 2  Lets find B Transpose

B

t(B)

t(B) -> BTranspose
BTranspose

Step 3  Now multiply !!
  
A%*%BTranspose

#  or

A%*%t(B)

# Multiply the inverse of A by the transpose of A

Step 1 Find the inverse of A

solve(A) -> inverseA
inverseA

Step 2 Find the transpose of A
t(A)

Step 3  Now multiply !!

inverseA%*%t(A)

# An interesting matrix property in the textbook establishes that the 
# inverse of the inverse of a matrix is the matrix itself, that is
inverse(inverse of X) = X,  where X is an artitrary matrix.

Use matrix A to demonstrate the property


solve(A) -> inverseA
inverseA              # This coding gives the inverse of A

solve(inverseA)       # This coding gives the inverse of the inverse of A



