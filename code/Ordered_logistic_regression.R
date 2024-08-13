options(digits=2)

## Clear R memory


## Package to load 
if(!require("readxl")){install.packages("readxl")}
if(!require("tidyverse")){install.packages("tidyverse")}
if(!require("ordinal")){install.packages("ordinal")}
if(!require("haven")){install.packages("haven")}
if(!require("broom")){install.packages("broom")}
# Import data
rm(list=ls())                                                  

data <- read_xlsx("processedData/data.xlsx", sheet="transmissionILK")
data <- na.omit(data)

# Data Wrangling

data <-  data |> 
                mutate(
                  age_cag= if_else( age >= 28 & age < 45, "28-45", 
                                    if_else( age >= 45 & age < 60, "45-60", "60-85"))) |> 
                    mutate_at(c("age_cag", 'sex', "residentialStatus","educ", "motherTongue", "mainOccu", "frequencyInteractionMangroves",
                                "learnILK_Observation", "learnILK_Listen", "learnILK_Practice", "frequencyUseKnowledge"), as.factor) |> 
                    mutate(learnILK_Observation= factor(learnILK_Observation, levels=c('1', '2', '3', '4', '5'), 
                                                        ordered = T), 
                           learnILK_Listen = factor(learnILK_Listen, levels=c('1', '2', '3', '4', '5'), 
                                                        ordered = T), 
                           learnILK_Practice= factor(learnILK_Practice, levels=c('1', '2', '3', '4', '5'), 
                                                        ordered = T))
                 
#Calculate quartiles
quartiles <- quantile(data$residenceDuration, probs = c(0, 0.25, 0.75, 1))

# Create categorical variable
data$residenceDuration_cat <- cut(data$residenceDuration, 
                             breaks = quartiles, 
                             labels = c("Short", "Medium", "Long"),
                             include.lowest = TRUE)

data$educ                          <- relevel(data$educ, ref= "None")
data$residentialStatus             <- relevel(data$residentialStatus, ref = "Non Indigenous")
data$sex                           <- relevel(data$sex, ref="Male")
data$frequencyInteractionMangroves <-  relevel(data$frequencyInteractionMangroves, ref="Once a week")
data$age_cag                       <-  relevel(data$age_cag, ref="28-45")
data$residenceDuration_cat         <-  relevel(data$residenceDuration_cat, ref="Short")
data$frequencyUseKnowledge         <-  relevel(data$frequencyUseKnowledge, ref = "Never used")


################################################################################
ctrl = clm.control(method = "optim", maxIter = 10000)

listenModel <- clm(learnILK_Listen ~ age_cag + sex + educ + residentialStatus + 
                     motherTongue + residenceDuration, 
                   data=data)

summary(listenModel)

#step(listenModel, direction = "backward" )
#finalLearnModel<- clm(learnILK_Listen ~ sex + motherTongue,
                              #data=data)
tab1 <- summary(listenModel)
stargazer(tab1$coefficients, type = "text", out="tab1.txt")

##############################################################################
library(car)
vif(lm(as.numeric(learnILK_Observation) ~ age + sex + educ + residentialStatus + residenceDuration, data=data))

observeModel <- clm(learnILK_Observation ~  age_cag + sex + educ + residentialStatus +residenceDuration, 
                   data=data)

  summary(observeModel)

#step(oberservationModel, direction = "backward" )
#finalLearnModel<- clm(learnILK_Listen ~ sex + motherTongue,
                     # data=data)
tab1 <- summary(observeModel)
stargazer(tab1$coefficients, type = "text", out="tab1.txt")

###############################################################################
practiceModel <- clm(learnILK_Practice ~ age_cag + sex + educ  + residentialStatus+ residenceDuration, 
                    data=data)

summary(practiceModel)

#step(practiceModel, direction = "backward" )
#finalLearnModel<- clm(learnILK_Listen ~ sex + motherTongue,
                      #data=data)
tab1 <- summary(finalLearnModel)
stargazer(tab1$coefficients, type = "text", out="tab1.txt")

