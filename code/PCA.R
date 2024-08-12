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
if(!require("FactoMineR")){install.packages("FactoMineR")}

library(factoextra)
## Import data 
dat <- read_xlsx("processedData/data.xlsx", sheet="fieldsILK")


dat <- dat |> 
           na.omit()

pca_result <- PCA(dat, scale.unit = T, quali.sup = 1:3)


# Extract coordinates
var_coord <- pca_result$var$coord[, 1:2]
suppl_coord <- pca_result$quali.sup$coord[, 1:2]

pca_result$var$contrib


seuil <- 100/nrow(pca_result$var$contrib)
moda <- which(pca_result$var$contrib[, 1]>seuil | pca_result$var$contrib[, 2]>seuil)

round(pca_result$var$contrib[moda, 1:2], 2)


var_coord <- round(pca_result$var$coord[moda, 1:2], 2)
# Set up the plot
plot(var_coord, type = "n", xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2),
     xlab = paste0("Dim 1 (", round(pca_result$eig[1,2], 1), "%)"),
     ylab = paste0("Dim 2 (", round(pca_result$eig[2,2], 1), "%)"),
)
# Add grid lines
abline(h = 0, v = 0, lty = 2, col = "gray")


# Add text labels for active variables
text(var_coord[,1], var_coord[,2], labels = rownames(var_coord), 
     pos = 3, col = "red", cex = 0.8)

# Add points for supplementary variables
points(suppl_coord[1,1], suppl_coord[1,2], pch = 17, col = "blue")
points(suppl_coord[2,1], suppl_coord[2,2], pch = 17, col = "blue")
text(suppl_coord[1,1], suppl_coord[1,2], labels = rownames(suppl_coord)[1], 
     pos = 1, col = "blue", cex = 0.8)
text(suppl_coord[2,1], suppl_coord[2,2], labels = rownames(suppl_coord)[2], 
     pos = 1, col = "blue", cex = 0.8)


points(suppl_coord[3,1], suppl_coord[3,2], pch = 10, col = "blue")
points(suppl_coord[4,1], suppl_coord[4,2], pch = 10, col = "blue")
text(suppl_coord[3,1], suppl_coord[3,2], labels = rownames(suppl_coord)[3], 
     pos = 3, col = "blue", cex = 0.8)
text(suppl_coord[4,1], suppl_coord[4,2], labels = rownames(suppl_coord)[4], 
     pos = 3, col = "blue", cex = 0.8)


points(suppl_coord[5,1], suppl_coord[5,2], pch = 3, col = "blue")
points(suppl_coord[6,1], suppl_coord[6,2], pch = 3, col = "blue")
points(suppl_coord[7,1], suppl_coord[7,2], pch = 3, col = "blue")
points(suppl_coord[8,1], suppl_coord[8,2], pch = 3, col = "blue")

text(suppl_coord[5,1], suppl_coord[5,2], labels = rownames(suppl_coord)[5], 
     pos = 3, col = "blue", cex = 0.8)
text(suppl_coord[6,1], suppl_coord[6,2], labels = rownames(suppl_coord)[6], 
     pos = 2, col = "blue", cex = 0.8)
text(suppl_coord[7,1], suppl_coord[7,2], labels = rownames(suppl_coord)[7], 
     pos = 2, col = "blue", cex = 0.8)
text(suppl_coord[8,1], suppl_coord[8,2], labels = rownames(suppl_coord)[8], 
     pos = 3, col = "blue", cex = 0.8)
