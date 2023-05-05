
##
## PACKAGES INSTALLATION
##


packages <- c("plotly","tidyverse","ggrepel","fastDummies","knitr","kableExtra",
              "splines","reshape2","PerformanceAnalytics","correlation","see",
              "ggraph","psych","nortest","rgl","car","ggside","tidyquant","olsrr",
              "jtools","ggstance","magick","cowplot","emojifont","beepr","Rcpp",
              "equatiomatic")

options(rgl.debug = TRUE)

if(sum(as.numeric(!packages %in% installed.packages())) != 0){
  installed <- packages[!packages %in% installed.packages()]
  for(i in 1:length(installed)) {
    install.packages(installed, dependencies = T)
    break()}
  sapply(packages, require, character = T) 
} else {
  sapply(packages, require, character = T) 
}
  

##
## LOAD COMPANIES DATASET / SET TUP
##

## Command to see all R Built in DataSets

data()

## Command to load a Builtin Dataset - e.g. Professors Salaries that is in the
## R Package called "carData"

data("Salaries")

# Copy to df_salaries (standardize the naming)

df_salaries <- Salaries 

# Creates a new column called Seniority

df_salaries$seniority <- round(df_salaries$yrs.service/5)

## Destroy the original Salaries

Salaries <- NULL 

## Load a CSV file and transforming it into a Dataset - e.g. Diabetes.CSV
## Make some basic data wrangling

df_diabetes <- read.csv("Diabetes.csv")

# Filter the whole data set keeping only samples with BMI > 0

df_diabetes <- filter(df_diabetes,BMI>0) 

# Change 1 to Yes and 0 to No

df_diabetes$Outcome <- ifelse(df_diabetes$Outcome==1,"Yes","No") 

## Dataset Stats Summary 

summary(df_salaries)

summary(df_diabetes)


## Basic GGPLOT Definition - Sample with Line Chart
## ggplot -> Creates the Canvas for the graph
## geom_line -> adds the Line Graph type in the Canvas
## Sintax:
##       ggplot(parameters) +
##       gem_line(parameters)
##
## Note: In the parameters, it is common to see the "aes" parm, which means Aesthetics
##       and it is used when we want to make a reference to variable constructors like
##       x,y coordinates, variable color or size depending on a data value. If using a 
##       constant value, the aes is not required for example: color = "red".

ggplot(df_diabetes,aes(x=Age,y=BMI)) +
  geom_line(color="red")

## Set Line Colors depending on another field (Outcome)

ggplot(df_diabetes,aes(x=Age,y=BMI)) +
  geom_line(aes(color=Outcome))

ggplot(df_salaries,aes(x=yrs.service,y=salary)) +
  geom_line(aes(color=sex))

## Sample of Area Graph (see "geom_area") and the "eas" was used since the color is
## variable with Outcome value.
## See also the "alpha" parameter that defines the transparency. 

ggplot(df_diabetes,aes(x=Age,y=BMI)) +
  geom_area(aes(fill=Outcome),alpha=0.5) # See that as Outcome is 0 or 1 it doesn't make too much difference.

ggplot(df_salaries,aes(x=yrs.service,y=salary)) +
  geom_area(aes(fill=sex),alpha=0.6)

## Scatter Plot - see "geom_point" 

ggplot(df_diabetes,aes(x=Age,y=BMI)) +
  geom_point(aes(color=Outcome))

ggplot(df_salaries,aes(x=yrs.service,y=salary)) +
  geom_point(aes(color=sex),alpha=0.6)


## Bubble Plot - see the parameter "size" that determines the size of
## of the bubbles and the "scale_size_area" adjust the overall graph scale

ggplot(df_salaries,aes(x=yrs.service,y=salary,size=seniority,color="Stepwise")) +
  geom_point(alpha=0.5) +
  scale_size_area()


## Bar Plot - see "geom_col"

ggplot(df_diabetes,aes(x=Age,y=BMI)) +
  geom_col(aes(color=Outcome))

ggplot(df_diabetes,aes(x=Age,y=BMI)) +
  geom_col(aes(fill=Outcome))

## Basic Line Graph

ggplot(df_diabetes,aes(x= Age, y=Glucose)) +
  geom_line()

## Basic Column Graph (info: R will make the mean of Glucose)

ggplot(df_diabetes,aes(x= Age, y=Glucose)) +
  geom_col()
  
## Lollipop Graph with Variable Size for the Points depending on Age

ggplot(df_diabetes,aes(x= Age, y=Glucose)) +
  geom_point(alpha=0.7,size = df_diabetes$Age/10,color=df_diabetes$Age) +
  geom_segment(aes(x = Age, xend = Age, y=0 , yend = Glucose)) + 
  scale_size_area()


## Same as previous one, but now using the Pip "%>%" to facilitate the Syntax
## and reduce the need to reference the dataset all the time. 
##
## O important: See that this method of passing the parameters for each Geometry
##              makes a better adjustmet of the graph.This is the most used method in
##              R world when making professional graphs. The others can be used but
##              only for explotion and non-complex graphs. 

df_diabetes %>%
  ggplot() +
    geom_point(aes(x= Age, y=Glucose, size=Age, color=Age),alpha=0.7)+
    geom_segment(aes(x = Age, xend = Age, y=0 , yend = Glucose))


df_salaries %>% filter(sex=="Male") %>% ggplot() +
  geom_point(aes(x=yrs.service,y=salary),alpha=0.6)


## Histogram Graph - see the "geom_histogram" parameter, and the "bins" defines
## the amount of vertical bars.

df_salaries %>% ggplot(aes(salary)) + geom_histogram(aes(fill=sex),bins=15)


## Box Plot Graph 

df_salaries %>% ggplot() +
  geom_boxplot(aes(x=yrs.service,y=salary)) +
  scale_size_area()


## Graph comparing Age (X asis) x Glucose (Y axis) )
## Smoothline Graph with formula definition

df_diabetes %>%
  
  ggplot() +
  
  geom_smooth(aes(x = Age, y = Glucose, color = "Stepwise"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5), size = 1.5) +
  
  geom_smooth(aes(x = Age, y = BMI, color = "Blue"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5), size =1.5) +
  
  geom_point(aes(x = Age, y = DiabetesPedigreeFunction*100),
             color = "#440154FF", alpha = 0.6, size = 2) +
  
  labs(x = "Age", y = "Glucose") +
    theme(panel.background = element_rect("white"),
          panel.grid = element_line("grey95"),
          panel.border = element_rect(NA),
          legend.position = "bottom")


