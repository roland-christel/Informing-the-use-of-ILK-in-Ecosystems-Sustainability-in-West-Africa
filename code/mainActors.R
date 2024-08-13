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
dat <- read_xlsx("processedData/data.xlsx", sheet="mainActors")



df <- dat |> 
  na.omit() |> 
  mutate_all(as.factor) |> 
  mutate(
    across(starts_with("actors"),
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
  pivot_longer(cols = starts_with("actors"), 
               names_to = "actors", values_to = "values") |> 
  mutate(
    actors = str_replace(actors , "^actorsUseILK_", "")
  ) |> 
  group_by(actors, values) |> 
  summarise(
    N = n()
  ) |> 
  mutate(
    percent = N / sum(N)*100
  )

 # Load ggpattern for patterned fill
library(ggpattern)

# Create the plot with ggpattern
p <- ggplot(df) +
  aes(x = percent, y = actors, fill = values) +
  geom_bar(
    stat = "identity")+
  scale_fill_grey(start = .8, end = 0)+
  labs(
    x = "Percentage",
    y = "Actors",
  ) +
  theme(
    axis.text.x = element_text(colour = "black", size = 9),
    axis.text.y = element_text(colour = "black", size = 9),
    axis.title.y = element_text(colour = "black", size = 12, vjust = 3, face = "bold"),
    axis.title.x = element_text(colour = "black", size = 12, vjust = -.5, face = "bold"),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11, face = "bold")
  )+
  theme_bw()+
  theme(
    axis.text.x  = element_text(colour = "black", size = 9),
    axis.text.y  = element_text(colour = "black", size = 9),
    axis.title.y = element_text(colour = "black", size = 12,
                                vjust = 3,face = "bold"),
    axis.title.x = element_text(colour = "black", size = 12,
                                vjust = -.5, face = "bold"),
    legend.text = element_text( size = 12),
    legend.title = element_text(face = "bold"),
  )

###########################
library(plotly)

# Create the plot with plotly
 plot_ly(
  data = df,
  x = ~percent,
  y = ~actors,
  type = 'bar',
  orientation = 'h',  # Horizontal bar chart
  color = ~values,
  colors = grey.colors(n = length(unique(df$values)), start = 0.8, end = 0)
) %>%
  layout(
    font = list(family = "Segoe UI", size = 14, color = "black"),
    xaxis = list(
      title = "Percentage",
      titlefont = list(color = "black",size = 16, bold=TRUE),
      tickfont = list(size = 12, color = "black")
    ),
    yaxis = list(
      title = list(text= "Actors", standoff = 10L),
      titlefont = list(color = "black",size = 16, bold=TRUE),
      tickfont = list(size = 12, color = "black")
    ),
    barmode = 'stack',
    showlegend = TRUE,
    legend = list(
      title = list(text=" "),
      font = list(size = 12)
    ),
    margin = list(l = 100, r = 50, b = 100, t = 50, pad = 4)
  )

# Show the plot
plotly_plot

