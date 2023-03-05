
# Using Matrices for Linear Regression  Chapter 5

# Review and recall:

# Consider the following matrices;


P <- matrix(c(5, -6,  7,
              3,  3,  5,
              -1, 10, -3
              ), ncol = 3, byrow = TRUE)
P



U <- matrix(c(5, 10,
              3,  6
              ), ncol = 2, byrow = TRUE)
U


I <- matrix(c(2, -1, 3,  4,
               1,  3, -4, 6), ncol = 4, byrow = TRUE)
I

# Multiplication:  U and I can be multiplied in that order for the inside
# dimensions are the same  2 by 2 times a 2 by 4

U %*% I

# But when the order is reversed, they cannot be multiplied for the inside
# dimensions are not the same.    2 by 3 times a 2 by 2

I %*% U

# Hence column row multiplication for matrices is not communicative


# In order for a matrix to have an inverse, it must be a square matrix,
# but Some square matrices have inverses, some do not.  Check the 
# determinant.  If the determinant equals zero or is practically close to
# zero, the matrix does not have a inverse.

# Lets test matrix P

det(P)

solve(P)

#

# Now lets check matrix U.

det(U)

solve(U)

# Organized bivariate data of the form;

x1  y1
x2  y2
x3  y3
x4  y4
x5  y5
.   .
.   .
.   .
xn  yn

# Can be associated with the following general system
# of equations

y1 = bo + b1x1 + e1
y2 = bo + b1x2 + e2
y3 = bo + b1x3 + e3
y4 = bo + b1x4 + e4
y5 = bo + b1x5 + e5
.
.
.
yn = bo + b1xn + en

# We now consider forming general matrices need for a
# matrix equation.

     |y1|        | 1  x1|                   |e1|
     |y2|        | 1  x2|                   |e2|
     |y3|        | 1  x3|                   |e3|
     |y4|        | 1  x4|       |bo|        |e4|
Y =  |y5|   X =  | 1  x5|   A = |  |    E = |e5|
     |. |        | .    |       |b1|        |. |
     |. |        | .    |                   |. |
     |. |        | .    |                   |. |
     }yn|        | 1  xn|                   |en|


# We now have the general matrix equation;
                Y = XA + E
# We can solve this equation for A and obtain the intercept
# and the slope for our linear regression model.  
                
# Using matrices, we can solve for A by solving the 
# following matrix equation.
                A = (X^t*X)^-1*X^tY
# or the inverse of the product of X transpose and X
# multiplied by the product of X transpose and Y.
                
# To illustrate we will consider the following bivaritate
# data collection:

x <- c(1, 3, 6, 5.5, 7.1)
x

y <- c(17.1, 15, 13.12, 11.05, 8.4)
y

#  We now consider the following matrix forms so that we can
#  apply the formula     A = (X^T*X)^-1*X^TY

      |17.1 |       |1  1   |
      |15   |       |1  3   |       |bo|
  Y = |13.2 |   X = |1  6   |   A = |  |
      |11.05|       |1  5.5 |       |b1|
      |8.4  |       |1  7.1 |      
           

  
  
  x <- c(1, 3, 6, 5.5, 7.1)
  x
  
  y <- c(17.1, 15, 13.12, 11.05, 8.4)
  y
  
  #  We now consider the following matrix forms so that we can
  #  apply the formula     A = (X^T*X)^-1*X^TY
  
    |17.1 |       |1  1   |
    |15   |       |1  3   |       |bo|
    |13.12|   X = |1  6   |   A = |  |
    |11.05|       |1  5.5 |       |b1|
    |8.4  |       |1  7.1 |      
    
    
    
    # Let's use R code to establish matrix Y :
    
    y <- c(17.1,15,13.12,11.05,8.4)
  y
  
  Ym <- matrix(c(17.1,
                15,
                13.12,
                11.05,
                8.4), ncol = 1, byrow = TRUE)
  Ym
  
 #yc <- as.matrix(y, ncol = 1, byrow = TRUE)
  
  # Let's use R code to establish matrix X :
  
  x<- c(1, 3, 6, 5.5, 7.1)
  x
  
  Xm <- matrix(c(1, 1,
                1, 3,
                1, 6,
                1, 5.5,
                1, 7.1), ncol = 2, byrow = TRUE)
  Xm
  
  # Remember, we will use the matrix construction  A = (X^T*X)^-1*X^TY
  # A is the matrix that yields b0 and b1
  
  # Lets find the transpose of matrix Xm
  t(Xm) -> transposeXm
  transposeXm
  dim(Xm)
  
  dim(transposeXm)
  
  # Lets find the product of Xm and the transpose of Xm
  transposeXm%*%Xm-> ProDuct1
  ProDuct1
  
  det(ProDuct1)
  
  solve(ProDuct1)  # this produces the inverse
  
#  A = (X^T*X)^-1*X^TY
  
  solve(ProDuct1)%*%transposeXm%*%Ym
  
  
  solve(ProDuct1)%*%transposeXm%*%Ym -> interceptandslope
  interceptandslope
  
  # Lets find the fitted values and the residuals, also using matrices.
  
  # Finding the fitted values:
  # Solve the following matrix equation for Y(hat),
  # Y(hat) =  X b  , where Y(hat) is an arbitrary n by 1 matrix,
  # X is an arbitrary  n by 2 matrix and b is a 2 by 1 matrix whose 
  # elements are bo and b1.
  
  
  |               |1  1   |
                  |1  3   |       
              X = |1  6   |   |18.664854| 
                  |1  5.5 |   |-1.267888|
                  |1  7.1 |      
    # multiply these matrices !!
    
      Xm %*% interceptandslope   # these are the fitted values !!
  
  
  # Now lets find the residuals, using the following matrix equation:
  
  # e = Y - Y(hat).   note that these are matrices, not variables
  
        Ym -  Xm %*% interceptandslope
    
  
  
  # We now confirm the results by finding them using the linear regression
  # methods that we are familiar with.
  
  
  x <- c(1, 3, 6, 5.5, 7.1)
  x
  
  y <- c(17.1, 15, 13.12, 11.05, 8.4)
  y
  
  lm(y ~ x)
  
  fitted(lm(y ~ x))
  
  resid(lm(y ~ x))
  
# Text book example
  
# R code for matrix X
  X <- matrix(c(1, 80,
                1, 30,
                1, 50,
                1, 90,
                1, 70,
                1, 60,
                1, 120,
                1, 80,
                1, 100,
                1, 50,
                1, 40,
                1, 70,
                1, 90,
                1, 20,
                1, 110,
                1, 100,
                1, 30,
                1, 50,
                1, 90,
                1, 110,
                1, 30,
                1, 90,
                1, 40,
                1, 80,
                1, 70), ncol = 2, byrow = TRUE)
  X
  
  # R code for matrix Y
  
  
  Y <- matrix(c(399,
                121,
                221,
                376,
                361,
                224,
                546,
                352,
                353,
                157,
                160,
                252,
                389,
                113,
                435,
                420,
                212,
                268,
                377,
                421,
                273,
                468,
                244,
                342,
                323), ncol = 1, byrow = TRUE)
  Y
  
  
  t(X) -> transposeX
  transposeX
  dim(X)
  
  transposeX%*%X -> Product2
  Product2
  
  det(Product2)
  
  solve(Product2)
  
  
  
  solve(Product2)%*%transposeX%*%Y
  
# Special application of the inverse matrix.
  
# Solving a system of equations
  
  2x + 4y = 20
  3x +  y = 10
  
 Q = |2 4|    R = |20|
     |3 1|        |10|
   

   
R <- matrix(c(20,
              10
                  ), ncol = 1, byrow = TRUE)
 R
 

 Q <- matrix(c(2,4,
               3,1), ncol = 2, byrow =TRUE)
 Q

 det(Q)
 
 solve(Q)
 
 solve(Q)%*%R


# Problem:  Use matrix procedures to solve the following system of 
# equations
 
 5x + 2y = 8
 23x +7y = 28
  
  
  
