## Getting Help

?mean

help.search('weighted mean')

help(package='dplyr')


## Libraries and Packages 

install.packages('dplyr')

library(dplyr)

dplyr::select

data(iris)


## Directory

getwd()

setwd('C://file_path')


## Variables

var1 <- "hello"

var2 <- 10

var3 <- var2 + 30

var3


## Vectors

ve1 <- c(2,4,6)

ve1

ve2 <- c("A",4,5)

ve2

ve3 <- 2:4

ve3

ve4 <- seq(2,5,by=0.5)

ve4

ve5 <- rep(1:2, times=3)

ve5

ve6 <- rep(1:2, each=3)

ve6

## Vector Functions 

vf1 <- c('Orange','Banana','Apple','Banana','Apple','Orange','Tomato')

sort(vf1)

rev(vf1)

table(vf1)

unique(vf1)

## Vector Elements Selection

vs1 <- c(1:10)

vs1[4]

vs1[-4]

vs1[2:4]

vs1[-(2:4)]

vs1[c(1,5)]

vs2 <- rep(1:5, time=3)

vs2[vs2==3]

vs2[vs2<3]

vs2[vs2 %in% c(2,4)]

## Reading and Writing Files

library('readxl')

df1 <- read_excel('Students Details.xls')

df2 <- read.csv('Students Gender.csv')

write.csv(df1,'Students Details.csv')

save(df2, file='Students Gender.Rdata')

load('Students Gender.Rdata')

## Conditions

a <- 10
b <- 20

a==b

a!=b

a>b

a>=b

a<b

a<=b

is.na(a)

is.null(a)

## Types Conversion

tp1 <- c(T,F,T)

as.logical(tp1)

as.numeric(tp1)

as.character(tp1)

as.factor(tp1)

## MATHs Functions 

n1 <- 100

log(n1)

exp(2)

# Generating 2 vectors with 30 randomized numebers from 0 to 100 

vector1 <- round((runif(30)*100))
vector2 <- round((runif(30)*100))

round(97.5)

round(33.76543,2)

signif(27.547,3)

runif(2)

max(vector1)

min(vector1)

cor(vector1,vector2)

sum(vector2)

mean(vector2)

median(vector2)

hist(vector2)

quantile(vector2)

rank(vector2)

var(vector2)

sd(vector2)

sqrt(var(vector2))

## Matrixes 

# Matrix reference to elements are always ROW, COLUMNS

vector3 <- round((runif(9)*100))
vector3

m1 <- matrix(vector3,nrow=3,ncol=3)
m1

m1[2, ]

m1[ ,1]

m1[2,3]

m1_trans <- t(m1)
m1_trans

m1_multi <- m1 * m1_trans
m1_multi

# Equation System Solver
# Find m1 * x? = m1_trans

solve(m1,m1_trans)

# Solve usage Sample
# Imagine you have this equation system
# 2x + 3y = 10
# 5x +  y = 30

ve_right = c(2,3,5,1)
ve_left = c(10,30)

m_right=t(matrix(ve_right,nrow=2,ncol=2))
m_right

m_left=matrix(ve_left,nrow=2,ncol=1)
m_left

solve(m_right,m_left)


## Lists / Collection

# List is a collection (like a vector) but can have multiples tipes. 

l1 <- list(x=1:5,y=c('a','b'))
l1

l1$x

l1$y


l1['y']

l1[1]

l1[2]

l1[[2]]

## STRING Operations 

str1 <- "Hello"

str2 <-"World"

str_vector <- c("This","is","a Sample")

str3 <- paste(str1,str2, sep=' ')
str3

str3 <- paste(str_vector, collapse = '-')
str3

as.logical(grep("l",str1))

str4 <- gsub("a Sample","Good",str3)
str4

str5 <- toupper(str4)
str5

str6 <- tolower(str5)
str6

nchar(str6)

## Data Frames

df <- data.frame(x=1:3,y=c('a','b','c'))

df

View(df)

head(df)

df$x

df$y

df[ ,2]

df[2, ]

df[2 , 2]

df['x']

df['y']

df[2,'x']

df[2,'y']

nrow(df)

ncol(df)

dim(df)

df <- cbind(df,z=1:3)

df <- cbind(df,w=df$z*2)

df <- rbind(df,df[3,])

df <- rbind(df,df[1:3,])

v_rb<-c(4,'d',4,7)

df <- rbind(df,v_rb)

# Add Column 

df$New_Column <- as.integer(df$x)*5

df

# Remove Column 

df$New_Column <- NULL

df

## STATISTICS

library('readxl')

df_td <- read_excel('Time and Distance.xls')

summary(df_td)

hist(df_td$`Time (Y)`)

hist(df_td$`Distance (X)`)

lm_df_td <- lm(df_td$`Distance (X)` ~ df_td$`Time (Y)`,df_td)

lm_df_td$coefficients

aov(lm_df_td)

t.test(df_td$`Distance (X)`,df_td$`Time (Y)`)

plot(lm_df_td)


## PROGRAMING ROUTINES 

seq <- 1:20
acc <- 0

for (i in seq){
  acc <- acc + i
}

acc

for (i in 1:50){
  j <- i + 10
  print(j)
}


text1 <- c('a','b','c','d')

j <- ''

for (i in text1){
  j <- paste(j,i,sep='-')
}

j
