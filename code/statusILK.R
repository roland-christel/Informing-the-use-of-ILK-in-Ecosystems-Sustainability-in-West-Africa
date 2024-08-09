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
data <- read_xlsx("processedData/data.xlsx", sheet="statusILK")



data |> 
     na.omit() |> 
     mutate_all(as.factor) |> 
     mutate(
       sex= factor(sex, levels=c("Male", "Female"))
     ) |> 
     mutate(
       across(starts_with("status"),
           ~ case_when(
             . == "1" ~ "Low",
             . == "2" ~ "Medium",
             . == "3" ~ "None",
             . == "4" ~ "Strong",
             . == "5" ~ "Very strong",
             TRUE ~ as.character(.)
           )
    )
  ) |> 
  pivot_longer(cols = starts_with("status"), 
               names_to = "status", values_to = "values") |> 
  mutate(
    status= str_replace(status , "^statusUseILK_", "")
  ) |> 
  select(sex, status, values) |> 
  group_by(sex,status, values) |> 
  summarise(
    N = n()
  ) |> 
  mutate(
    percent = N/sum(N)*100
  ) |> 
  ggplot() +
    aes(x = values, y=percent, fill = sex)+
    geom_bar(stat = "identity", position = "dodge")+
    scale_fill_manual(values=c('black', "grey"), name="Sex")+
    scale_y_continuous(labels = function(y) paste0(y, "%"))+
    facet_wrap(~status) +
    labs(
      y = "Proportion", x=""
    )+
  theme_bw()+
  theme(
    axis.text.x  = element_text(colour = "black", size = 12),
    axis.text.y  = element_text(colour = "black", size = 12),
    axis.title.y = element_text(colour = "black", size = 12,
                                vjust = 3,face = "bold"),
    axis.title.x = element_text(colour = "black", size = 12,
                                vjust = -.5, face = "bold"),
    strip.text.x = element_text(size = 13, face="bold", ),
    legend.text = element_text(face = "italic", size = 12),
    legend.title = element_text(face = "bold"),
  )
