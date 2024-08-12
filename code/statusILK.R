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
if(!require("purrr")){install.packages("purrr")}
if(!require("tidyr")){install.packages("tidyr")}


## Import data 
data <- read_xlsx("processedData/data.xlsx", sheet="statusILK")

df <- df <- data |> 
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
  group_by(sex,status, values)


# Function to perform chi-square test for a single status
perform_chi_square <- function(data) {
  contingency_table <- table(data$sex, data$values)
  chi_square_result <- chisq.test(contingency_table)
  
  # Return a list with the test statistic, p-value, and degrees of freedom
  return(list(
    statistic = chi_square_result$statistic,
    p_value = chi_square_result$p.value,
    df = chi_square_result$parameter
  ))
}

# Perform chi-square test for each status
chi_square_results <- df %>%
  group_by(status) %>%
  nest() %>%
  mutate(chi_square = map(data, perform_chi_square)) %>%
  unnest_wider(chi_square)

# Print results
print(chi_square_results)


# compute the proportions 
dff <- df |>  
            summarise(
              N = n()
            ) |> 
            mutate(
              percent = N/sum(N)*100
            ) 


# Define the positions for each status
text_positions <- data.frame(
  status = unique(df$status),
  x_pos = c(1, 4.5, 4.5,4.5,4.5,4.5,1,1,4.5),  
  y_pos = c(rep(70,9)) 
)

# Combine chi-square results with text positions
chi_square_results <- chi_square_results %>%
  left_join(text_positions, by = "status")


x11();  ggplot(dff) +
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
    axis.text.x  = element_text(colour = "black", size = 9),
    axis.text.y  = element_text(colour = "black", size = 9),
    axis.title.y = element_text(colour = "black", size = 12,
                                vjust = 3,face = "bold"),
    axis.title.x = element_text(colour = "black", size = 12,
                                vjust = -.5, face = "bold"),
    strip.text.x = element_text(size = 13, face="bold", ),
    legend.text = element_text(face = "italic", size = 12),
    legend.title = element_text(face = "bold"),
  ) +
    geom_text(
      data = chi_square_results,
      aes(x = x_pos, y = y_pos, 
          label = paste("χ² =", round(statistic, 2), "\np =", 
                        round(p_value,2))),
      hjust = 0, vjust = 1, size = 3.5, 
      inherit.aes = FALSE
    )
