###################################################################
# R Basics by Luis Urso                                           #
# Variables, Vectors and Matrix                                   #
###################################################################

## Important to notice that the names are case sensitive. 

## Common Variables
## data type are auto-associated 

var1 <- 10
var2 = 20
var3 = "Hello "
var4 = "World"

## Sum

var1_2 <- var1 + var2
print(var1_2)


# Concatenate

var3_4 <- paste(var3,var4)
print(var3_4)

## Create a vector

vector1 <- c(var1,var2)

vector2 <- c(var1,var3,var2,var4)

## Create a vector with given sequence

vector3 <- c(1:100)
vector3

vector4 <- c(10:200)
vector4

## Repeat value 33 for 10 times

vector5 <- c(rep(33,10))
vector5

## Make a matrix filling with number 3, with 5 lines and 10 columns

matrix1 <- matrix(3,5,10)
matrix1

## Transpose the matrix 

matrix2 <- t(matrix1)
matrix2

## Make a matrix with random numbers 

set.seed(0)

randomic <- runif(10,min=0,max=100)
randomic




