#####################################################################
# GISS UPSKILLING PROGRAM                                           #
#                                                                   #
# MULTIPLE LINEAR & NON-LINEAR REGRESSION                           #                    
#                                                                   #
# By: Luis Urso                                                     #
# Last Update: 15-Feb-2023                                          #
#                                                                   #   
# Description:                                                      #
# In this script we will make a Multiple Regression (non-           #
# linear) applying some optimization techniques like:               #
#   - Step-Wise Optimization (Model Adjust                          #
#   - Shaphiro-Francia Test                                         #
#   - Box-Cox Transformation                                        #    
#                                                                   #
# About this Exercise:                                              #
# This is a companies database collected from Compustat             #
# Global that contains some known  companies  financial             #
# informations (fudaments) that we will use to calculate            #
# the expect return (Y) giving some parameters (X1,X2,Xn)           #
#####################################################################

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

companies <- read.csv("Companies.csv")


#####################################
## BLOCK-1                         ##  
## INITIAL EXPLORATORY ANALYSIS    ##
#####################################


##
## CHECK THE BASIC MULTIVARIATE STATISTICS 
##

summary(companies)

##
## CORRELATION ANALYSIS - SOME OPTIONS
##

## Correlation Analysis - Option 01
## Depends on the packages 'see' and 'ggraph'

companies %>%
  correlation(method = "pearson") %>%
  plot()

## Correlation Analysis - Option 02
## Depends on the package 'PerformanceAnalytics

chart.Correlation((companies[2:6]), histogram = TRUE)

## Correlation Analysis - Option 03
## Depends on the package ''psych'

pairs.panels(companies[2:6],
             smooth = TRUE,
             lm = TRUE,
             scale = FALSE,
             density = TRUE,
             ellipses = FALSE,
             method = "pearson",
             pch = 1,
             cor = TRUE,
             hist.col = "aquamarine",
             breaks = 12,
             stars = TRUE, # If TRUE, adds significance level with stars
             ci = TRUE, alpha = 0.05)

## Correlation Analysis - Option 04
## Depends on the package 'metan'
## See that this package was installed inline since sometimes it has bug
## when installing at begining of the script. 
##
## Obs: Graphics with a Light Purple background, means they have a good
##      correlations, the ones with White Background, don't have, see the Beta
##      is almost 0
##      The Gray shadows are the Confidence Interval [CI]

install.packages("metan")

library(metan)

companies %>%
  corr_plot(return, disclosure, indebtedness , assets, liquidity,
            shape.point = 21,
            col.point = "black",
            fill.point = "#FDE725FF",
            size.point = 2,
            alpha.point = 0.6,
            maxsize = 4,
            minsize = 2,
            smooth = TRUE,
            col.smooth = "black",
            col.sign = "#440154FF",
            upper = "corr",
            lower = "scatter",
            diag.type = "density",
            col.diag = "#440154FF",
            pan.spacing = 0,
            lab.position = "bl")

##############################################################################
## BLOCK-2                                                                  ##
## MAKING THE MULTIPLE CORRELATION USING LINEAR MODEL - USING ALL VARIABLES ## 
##############################################################################

## Views the Database in Report Format

companies %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)

## Creates the Multiple Correlation Model 1 
## Function: Return (Y) in function of all other columns (X), 
##           except the 'company' field. 
##           See that: the '.' means ALL Columns, and '-' means REMOVE
##                     the field. Important to know, that the function 'lm'
##                     removes automatically 'return' field from the BETAS.

model_companies_ln <- lm(formula = return ~ . - company,
                      data = companies)

## Make a summary of the model to check the statistics
## You will see that 'indebtedness' has a P-Value [Pr(.|t|)] of 0.087 
## (8.7%) which means it is not statistically significant at CI=95%.
## it is expected to have a p-Value < 5%

summary(model_companies_ln)

## So to adjust that we will make the STEP-WISE Optimization
## that will automatically define all the BETAs those are 
## statistically significant. 

#######################################################
## BLOCK-3                                           ##
## STEP-WISE - OPTIMIZING THE MODEL                  ## 
## OPTIMIZE the Xn Parameters + BETA                 ##
#######################################################

## The STEP-WISE requires the definition of degree of freedom (k parameter)
## that is acquired from CHI-SQUARE distribution. 

## Defining the Degree of Freedom for CI=95%

chi_sq <- qchisq(p = 0.05, df = 1, lower.tail = F)

## Checking the CI for CHI-SQUARE - this is only didactic

round(pchisq(chi_sq, df = 1, lower.tail = F), 7)

## MAKE the STEP-WISE
## See that it uses the previous generated model 'model_companies"
## to optimize and generate a new model called 'model_companies_sw"

model_companies_sw <- step(model_companies_ln, k = chi_sq)

## See that in the last and optimized proposed model, the function
## removed the 'indebtedness' and 'disclosure' BETAS, so optimized model 
## will be: RETURNS in function of ASSETS and LIQUIDITY 

summary(model_companies_sw)

## Makes a Summary Reports of the Model - Better Visual 

export_summs(model_companies_sw, scale = F, digits = 5)

## Confidence Interval Analysis at 95%

confint(model_companies_sw, level = 0.95)

## Plot CIs - Not Scaled (incorrect)

plot_summs(model_companies_sw, colors = "#440154FF")

## Plot CIs - Scaled (CORRECT)

plot_summs(model_companies_sw, scale=TRUE, colors = "#440154FF")

## Plot CIs - Scaled + Person Distribution 

plot_summs(model_companies_sw, scale = TRUE, plot.distributions = TRUE,
           inner_ci_level = .95, colors = "#440154FF")

## Plot CIs - Comparing the 2 models without Step-Wise and with Step-Wise

plot_summs(model_companies_sw,model_companies, scale = TRUE, plot.distributions = TRUE,
           inner_ci_level = .95, colors = c("#FDE725FF", "#440154FF"),)

###############################################################################
## BLOCK-4                                                                   ##
## SHAPHIRO-FRANCIA TEST - VERIFYING THE MODEL RESIDUALS ADHERENCE           ## 
## TO NORMALITY                                                              ##
###############################################################################

## Shapiro-Francia Test for n > 30 
## See that that the p-Value is 0.01816 and for this test
## is expected to be >= 0.05. Conclusion: The residuals are not adherent
## to the model, we will have to remake the model further. 
## The reason is that this model is non-liner and will need to be transformed
## using the BOX-COX transformation - See in the next topic.

sf_test<-sf.test(model_companies_sw$residuals)

sf_test

if (sf_test$p.value < 0.05){
  print("H1: There is no adherence to Normality, may need to optimize")
} else {
  print("H0: There is adherence to Normality")
}

## Plot the Companies Data Histogram X Step-Wise Models Residuals 
## To compare the adherence of the residuals. 
## See there are gaps, meaning the model still need to be adjusted.

companies %>%
  mutate(residuos = model_companies_sw$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "white", 
                 fill = "#440154FF", 
                 bins = 30,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(model_companies_sw$residuals),
                            sd = sd(model_companies_sw$residuals)),
                size = 2, color = "grey30") +
  scale_color_manual(values = "grey50") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme_bw()

###############################################################
## BLOCK-5                                                    #
## BOX-COX TRANSFORMATION                                     #   
## Adjust Y parameters addin a Lambda (Y^LAMBDA)              # 
## Non-Linear/Lambda Calculation                              #
###############################################################

## BOX-COX Lambra Calculation 
## Depends on the package 'car'
## This is applied only in the Y (result variable) that is 'companies$return'

lambda_BC <- powerTransform(companies$return)

lambda_BC

## Creates a new field in Companies Dataset to store the Y^LAMBDA
## The new field will be called 'bcreturn' that is the Ym Transformed
##
## This makes the transformation of Y using the fomula: 
##
## ((Yn ^ LAMBDA) - 1) / LAMBDA

companies$bcreturn <- (((companies$return ^ lambda_BC$lambda) - 1) / 
                         lambda_BC$lambda)


## Visualizing the Return (return) x Box-Cox Return (bcreturn)

companies %>%
  select(company, return, bcreturn, everything()) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 18)

## Creating a new Model, but now using the Box-Cox' - 'bcreturn' field. 

model_companies_bc <- lm(formula = bcreturn ~ . -company -return, 
                data = companies)

## Summary of the Model
## See that 'indebtedness' doesn't pass on p-value < 5%

summary(model_companies_bc)

## Makes the Step-Wise Again to Optimize the BETAs and X.
## See that the model was optimized again and the new formula
## is Return in Function of Disclosure, Assets, Liquidity
## And all parameters pass in the Pr(>|t|) < 0.05.

chi_sq <- qchisq(p = 0.05, df = 1, lower.tail = F)

model_companies_sw_bc <- step(model_companies_bc, k = chi_sq)

summary(model_companies_sw_bc)

## SHAPHIRO-FRANCIA Test
## Check the Residuals Normality
## See that p-Value >= 0.05 so H0: Statistically Not Significant,
## so there is adherence to Normality (we are measuring differences
## we don't want)

sf_test<-sf.test(model_companies_sw_bc$residuals)

sf_test

if (sf_test$p.value < 0.05){
  print("H1: There is no adherence to Normality, may need to optimize")
} else {
  print("H0: There is adherence to Normality")
}


## Plot the Companies Data Histogram X Step-Wise Models Residuals 
## To compare the adherence of the residuals. 

companies %>%
  mutate(residuos = model_companies_sw_bc$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..),
                 color = "white",
                 fill = "#287D8EFF",
                 bins = 30,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(model_companies_sw_bc$residuals),
                            sd = sd(model_companies_sw_bc$residuals)),
                size = 2, color = "grey30") +
  scale_color_manual(values = "grey50") +
  labs(x = "Residuals",
       y = "Frequency") +
  theme_bw()

## Makes a summary comparing the 2 models: Linear x Box-Cox

export_summs(model_companies_sw, model_companies_bc,
             model.names = c("Linear Model","Box-Cox MOdel"),
             scale = F, digits = 6)


## See the Confidence Interval for Box-Cox Model 
confint(model_companies_bc, level = 0.95)

## Plot the Model (Not Scaled) - This is an Error to Make !
plot_summs(model_companies_bc, colors = "#287D8EFF") 

## Plot the Model Scaled
plot_summs(model_companies_bc, scale = TRUE, colors = "#287D8EFF")

## Plot the Model Scaled with Normal Dist 
plot_summs(model_companies_bc, scale = TRUE, plot.distributions = TRUE,
           inner_ci_level = .95, colors = "#287D8EFF")

## Plot the Model Comparing the BETAs for Linear versus Box-Cox Model
plot_summs(model_companies_sw, model_companies_bc, scale = T, plot.distributions = TRUE,
           inner_ci_level = .95, colors = c("#440154FF", "#287D8EFF"))

##################################################################
## BLOCK-6                                                      ##
## MAKING THE PREDICTION WITH THE GENERATED MODEL               ##
##################################################################

## The model to be used is the Box-Cox Generated
##
## Simulation Scenario: What would be the RETURN for a company with a
##                      DISCLOSURE=50, LIQUIDITY = 14, and ASSETS=4000 

prediction <- predict(object = model_companies_sw_bc, 
              data.frame(disclosure = 50, 
                         liquidity = 14, 
                        assets = 4000),
              interval = "confidence", level = 0.95)

## Apply the Formula to Calculate the non-Scaled Predicted Result (Real)

Predicted_result = ((prediction[1] * lambda_BC$lambda) + 1) ^ (1 / lambda_BC$lambda )

Predicted_result

## Add 2 new fields to Company DataSet the fitted Return with the 
## First Step Wise Executed and the Box-Cox
## These will be used for plotting in the further step


companies$yhat_sw <- model_companies_sw$fitted.values

companies$yhat_swbc <- (((model_companies_bc$fitted.values*(lambda_BC$lambda))+
                                    1))^(1/(lambda_BC$lambda))

## Visualize the 3 FITTED models: Linear, Step Wise, and Step Wise + Box Cox

companies %>%
  ggplot() +
  geom_smooth(aes(x = return, y = yhat_sw, color = "Stepwise"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5), size = 1.5) +
  geom_point(aes(x = return, y = yhat_sw),
             color = "#440154FF", alpha = 0.6, size = 2) +
  geom_smooth(aes(x = return, y = yhat_swbc, color = "Stepwise Box-Cox"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5), size = 1.5) +
  geom_point(aes(x = return, y = yhat_swbc),
             color = "#287D8EFF", alpha = 0.6, size = 2) +
  geom_smooth(aes(x = return, y = return), method = "lm", formula = y ~ x,
              color = "grey30", size = 1.05,
              linetype = "longdash") +
  scale_color_manual("Model:", 
                     values = c("#287D8EFF", "#440154FF")) +
  labs(x = "Return", y = "Fitted Values") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")


#####################################################################
#                           T H E   E N D                           #
#####################################################################
