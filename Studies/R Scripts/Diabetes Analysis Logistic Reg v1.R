#################################################################
# Diabetes, based on Pima Indians Database                      #
# By Luis A. Urso                                               #
# Objective: Use Logistic Binary Regression to predict diabetes #
#################################################################


## Install Packages 


pkges <- c("plotly","tidyverse","knitr","kableExtra","fastDummies","rgl","car",
             "reshape2","jtools","stargazer","lmtest","caret","pROC","ROCR","nnet",
             "magick","cowplot","globals","equatiomatic")

options(rgl.debug = TRUE)

if(sum(as.numeric(!pkges %in% installed.packages())) != 0){
  installer <- pkges[!pkges %in% installed.packages()]
  for(i in 1:length(installer)) {
    install.packages(installer, dependencies = T)
    break()}
  sapply(pkges, require, character = T) 
} else {
  sapply(pkges, require, character = T) 
}

## Load the Dataset 

diabetes <- read.csv("diabetes.csv")

summary(diabetes)

model_diabetes <- glm(formula = Outcome ~ . -Insulin -Age, 
                      data = diabetes, 
                      family = "binomial")

summary(model_diabetes)

logLik(model_diabetes)

## Make a Step Wise to optmise the variables to consider 

model_diabetes_sw <- step(object = model_diabetes,
                          k = qchisq(p = 0.05, df = 1, lower.tail = FALSE))

summary(model_diabetes_sw)

## See the Equation 

extract_eq(model_diabetes_sw, use_coefs = T,
           wrap = T, show_distribution = T) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 25)

## See the Confidence Interval 

confint(model_diabetes_sw, level = 0.95)

###
### MAKING THE CONFUSION MATRIX
###

## Adding the Fitted Values (phat) into the Original Dataset (diabetes)

diabetes$phat <- model_diabetes_sw$fitted.values



## Confusion Matrix with Cutoff Definition

cutoff_factor = 0.33

confusionMatrix(table(predict(model_diabetes_sw, type = "response") >= cutoff_factor,
                      diabetes$Outcome == 1)[2:1, 2:1])

## Visualizing the Confusion Matrix Metrics

data.frame(Recall = confusionMatrix(table(predict(model_diabetes_sw,
                                                         type = "response") >= cutoff_factor,
                                                 diabetes$Outcome == 1)[2:1, 2:1])[["byClass"]][["Sensitivity"]],
           Specic = confusionMatrix(table(predict(model_diabetes_sw,
                                                          type = "response") >= cutoff_factor,
                                                  diabetes$Outcome == 1)[2:1, 2:1])[["byClass"]][["Specificity"]],
           Acc = confusionMatrix(table(predict(model_diabetes_sw,
                                                    type = "response") >= cutoff_factor,
                                            diabetes$Outcome == 1)[2:1, 2:1])[["overall"]][["Accuracy"]]) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", position = "center",
                full_width = F, 
                font_size = 27)

## Defining the best Cutoff by making Recall = Specificity  

predictions <- prediction(predictions = model_diabetes_sw$fitted.values, 
                        labels = as.factor(diabetes$Outcome))

data_roc <- performance(predictions, measure = "sens") 

pred_sensivity <- (performance(predictions, measure = "sens"))@y.values[[1]] 

pred_recall <- (performance(predictions, measure = "spec"))@y.values[[1]]

cutoffs <- data_roc@x.values[[1]]

plot_data <- cbind.data.frame(cutoffs, pred_recall, pred_sensivity)

plot_data %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)

## Ploting Comp. Recall x Sensivity 

ggplotly(plot_data %>%
           ggplot(aes(x = cutoffs, y = pred_recall)) +
           geom_line(aes(color = "Recall"),
                     size = 1) +
           geom_point(color = "#95D840FF",
                      size = 1.9) +
           geom_line(aes(x = cutoffs, y = pred_sensivity, color = "Sensivity"),
                     size = 1) +
           geom_point(aes(x = cutoffs, y = pred_sensivity),
                      color = "#440154FF",
                      size = 1.9) +
           labs(x = "Cutoff",
                y = "Sensivity/Recall") +
           scale_color_manual("Legend:",
                              values = c("#95D840FF", "#440154FF")) +
           theme_bw())

###
### Analysing Using the ROC Curve
###

ROC <- roc(response = diabetes$Outcome, 
           predictor = model_diabetes_sw$fitted.values)

#Plotagem da curva ROC propriamente dita
ggplot() +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1),
               color = "grey40", size = 0.2) +
  geom_line(aes(x = 1 - pred_recall, y = pred_sensivity),
            color = "darkorchid", size = 2) +
  labs(x = "Recall",
       y = "Sensivitye",
       title = paste("Area below ROC Curve:",
                     round(ROC$auc, 4),
                     "|",
                     "Gini Coef.:",
                     round((ROC$auc[1] - 0.5) / 0.5, 4))) +
  theme(panel.background = element_rect(NA),
        panel.border = element_rect(color = "black", fill = NA),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10)
  )


predict(object = model_diabetes_sw,
        data.frame(Pregnancies = 0,
                   Glucose = 137,
                   BloodPressure = 40,
                   BMI = 43.1,
                   DiabetesPedigreeFunction=2.288),
        type = "response")


predict(object = model_diabetes_sw,
        data.frame(Pregnancies = 10,
                   Glucose = 139,
                   BloodPressure = 80,
                   BMI = 27.1,
                   DiabetesPedigreeFunction=1.441),
        type = "response")
