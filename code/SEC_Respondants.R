## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++###
##                                  CODE R                                   ###
##                                                                           ###
##                                                                           ###
##                                                                           ###
## Updated: August, 2024                                                     ###
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++###

## Clear R memory

rm(list=ls())                                                  

## Package to load 

if(!require("tidyverse")){install.packages("tidyverse")}
if(!require("readxl")){install.packages("readxl")}
if(!require("gtsummary")){install.packages("gtsummary")}
if(!require("flextable")){install.packages("flextable")}


## Import data 
data <- read_xlsx("processedData/data.xlsx", sheet="SEC")

#fisher test   
fisher.test.simulate.p.values <- function(data, variable, by, ...) {
  result <- list()
  test_results <- stats::fisher.test(data[[variable]], data[[by]], simulate.p.value = TRUE,B=10000)
  result$p <- test_results$p.value
  result$test <- test_results$method
  result
}

## Checking Assumptions for mean comparison sttistical tests (age and residenceDuration)

  #normality (H0 : is normally distributed)
  shapiro.test(data$age);shapiro.test(data$residenceDuration)

  #Homoscedasticity (H0: th two variances are the same)
  var.test(data$age~data$residentialStatus)
  var.test(data$residenceDuration~data$residentialStatus)



data |> 
      select(-village) |> 
      mutate_at(c("mainOccu" ,"sex","countryOfOrigin","residentialStatus",
                  "motherTongue","educ"), as.factor) |> 
      tbl_summary(
                by=residentialStatus,
                statistic = list(all_continuous() ~ "{mean} ({sd})"),
                digits=all_continuous()~2,
                type = list(c("age", "residenceDuration") ~ "continuous")) |> 
       add_p(test = list(all_categorical() ~ "fisher.test.simulate.p.values",
                         all_continuous()~ "wilcox.test")
             ) |> 
       as_flex_table()  |> 
       flextable::save_as_docx(path="SEC_1.docx")

                