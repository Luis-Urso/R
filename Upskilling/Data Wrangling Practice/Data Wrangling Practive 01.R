## Including the Key Libraries for Data Wrangling

library("dplyr")
library("tidyr")

## Uses IRIS dataset tht is already loaded with R

## Convert to Table Class 

iris <- tbl_df(iris)

## Summary of Table

glimpse(iris)

## View the Table

View(iris)

## Making a Group by using the Piping from Dplyr %>%, and manking the average of Sepal.Width

iris %>% group_by(Species) %>% summarise(avg = mean(Sepal.Width)) %>% arrange(avg)

iris %>% group_by(Species) %>% summarise(median = median(Sepal.Length))

## Summarizing Data into a Single Row

summarise(iris,avg=mean(Sepal.Length))

summarise(iris,f=first(Sepal.Length))

summarise(iris,stdev=sd(Sepal.Length))

## Summarizing and applying function (e.g. Avegrage) for each column 

summarise_each(iris,funs(mean))


## Counting Number of Rows for each Unique Value of a Variable, with wights set in wt parameter

count(iris,Species,wt=Sepal.Length)

## Adding / Appending New Column with MUTATE

iris <- mutate(iris,sepal=Sepal.Length+Sepal.Width)

## Applying a function for each column (LAMBDA function)

mutate_each(iris,funs(min_rank))

## Make a new column and drop the previous one

transmute(iris,sepal=Sepal.Length * Sepal.Width)



