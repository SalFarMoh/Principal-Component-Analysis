#===============================================================================================
# UK CURVE 
#===============================================================================================

install.packages("readxl")
install.packages("dplyr")

library(readxl)
library(dplyr)
library(tidyverse)


Evaluation_data <- read_excel("/Users/apple/Desktop/bond/Anh's models/FRED/UK_Government.xlsm", sheet = "Clean UK Data")

Evaluation_data$Date <- as.Date(Evaluation_data$Date, format = "%Y-%m-%d")

pca_result <- prcomp(Evaluation_data[, -1], scale. = TRUE)

# Summary of PCA results
summary(pca_result)

# principal components
pca_components <- pca_result$rotation

# standard deviations of the principal components
pca_sd <- pca_result$sdev

# Access the proportion of variance explained by each principal component
variance_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2)
variance_explained

# Access the cumulative proportion of variance explained
cumulative_variance_explained <- cumsum(variance_explained)

# Plot the scree plot to visualize the proportion of variance explained
plot(1:length(pca_sd), pca_sd^2 / sum(pca_sd^2), type = "b", 
     xlab = "Principal Component", ylab = "Proportion of Variance Explained",
     main = "Scree Plot")


library(ggplot2)

library(reshape2)
melted_data <- melt(Evaluation_data, id.vars = "Date")

# Plot using ggplot2
ggplot(melted_data, aes(x = Date, y = value, color = variable)) +
  geom_line(size = 1) +
  labs(title = "Maturity Over Time",
       x = "Date",
       y = "Maturities") +
  theme_minimal() +
  theme(legend.position = "bottom")  

library(psych)

#KMO statistic
numeric_data <- Evaluation_data[sapply(Evaluation_data, is.numeric)]
kmo_result <- KMO(numeric_data)
kmo_result

#######################
##########Miscellaneous



data_matrix <- as.matrix(Evaluation_data[, c("Maturity Y01", "Maturity Y02",
                                             "Maturity Y03", "Maturity Y04" ,
                                             "Maturity Y05" , "Maturity Y07",
                                             "Maturity Y10", "Maturity Y15",
                                             "Maturity Y20", "Maturity Y30")])

loadings <- pca_result$rotation

approximation <- data_matrix %*% loadings
approximation

# Assuming 'pca_result' is the result of your PCA

# Extract the loadings matrix (rotation) from the PCA result
loadings_matrix <- pca_result$rotation

# Calculate the correlation matrix of the principal components
correlation_matrix <- cor(loadings_matrix)

# Print or view the correlation matrix
print(correlation_matrix)



#============================================================================================
# US CURVE 
#============================================================================================

US_data <- read_excel("/Users/apple/Desktop/bond/Anh's models/FRED/US_Treasury.xlsm", sheet = "Clean US Data")

US_data$Date <- as.Date(US_data$Date, format = "%Y-%m-%d")

USpca_result <- prcomp(US_data[, -1], scale. = TRUE)

# Summary of PCA results
summary(USpca_result)

# principal components
USpca_components <- USpca_result$rotation

# standard deviations of the principal components
USpca_sd <- USpca_result$sdev

# Access the proportion of variance explained by each principal component
USvariance_explained <- USpca_result$sdev^2 / sum(USpca_result$sdev^2)
USvariance_explained

# Access the cumulative proportion of variance explained
UScumulative_variance_explained <- cumsum(USvariance_explained)

# Plot the scree plot to visualize the proportion of variance explained
plot(1:length(USpca_sd), USpca_sd^2 / sum(USpca_sd^2), type = "b", 
     xlab = "Principal Component", ylab = "Proportion of Variance Explained",
     main = "Scree Plot")


library(ggplot2)

library(reshape2)
USmelted_data <- melt(US_data, id.vars = "Date")

# Plot using ggplot2
ggplot(USmelted_data, aes(x = Date, y = value, color = variable)) +
  geom_line(size = 1) +
  labs(title = "Maturity Over Time",
       x = "Date",
       y = "Maturities") +
  theme_minimal() +
  theme(legend.position = "bottom") 


#kM Test
USnumeric_data <- US_data[sapply(US_data, is.numeric)]
USkmo_result <- KMO(USnumeric_data)
USkmo_result
#============================================================================================
# 
#============================================================================================

