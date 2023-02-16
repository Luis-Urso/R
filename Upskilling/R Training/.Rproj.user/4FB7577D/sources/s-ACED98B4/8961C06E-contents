
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
## LOAD COMPANIES DATASET
##


df_diabetes <- read.csv("Diabetes.csv")

## Dataset Stats Summary 

summary(df_diabetes)

## Graph comparing Age (X asis) x Glucose (Y axis) )
## Smoothline Graph with formula definition

df_diabetes %>%
  
  ggplot() +
  
  geom_point(aes(x = Age, y = Glucose),
             color = "#440154FF", alpha = 0.6, size = 2) +
  
  labs(x = "Age", y = "Glucose") +
  
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")

## Basic Line Graph

ggplot(df_diabetes,aes(x= Age, y=Glucose)) +
  geom_line()

## Basic Column Graph

ggplot(df_diabetes,aes(x= Age, y=Glucose)) +
  geom_col()



## Graph comparing Age (X asis) x Glucose (Y axis) )
## Smoothline Graph with formula definition

df_diabetes %>%
  
  ggplot() +
  
  geom_smooth(aes(x = Age, y = Glucose, color = "Stepwise"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5), size = 1.5) +
  
  geom_point(aes(x = Age, y = DiabetesPedigreeFunction*100),
             color = "#440154FF", alpha = 0.6, size = 2) +
  
  labs(x = "Age", y = "Glucose") +
    theme(panel.background = element_rect("white"),
          panel.grid = element_line("grey95"),
          panel.border = element_rect(NA),
          legend.position = "bottom")


