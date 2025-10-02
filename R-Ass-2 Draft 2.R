# R Studio Code: MLR Data Analysis with R
  
library(ggplot2) # Load ggplot2 package


# ================================
# Getting started with the dataset in concrete.csv :
pacman::p_load(ggplot2) 
pacman::p_load(tidymodels)

# Data Preparation

# Trial 1 

# Load required libraries
pacman::p_load(gridExtra)

# Read the data (assuming it's in a CSV file named 'concrete.csv')
# concrete <- read.csv("concrete.csv")

# a) Histograms of the Variables
# Create histograms for each variable
p1 <- ggplot(concrete, aes(x = cement)) + 
  geom_histogram(binwidth = 10, fill = "blue", color = "black") + 
  ggtitle("Histogram of Cement") + 
  theme_minimal()

p2 <- ggplot(concrete, aes(x = wcr)) + 
  geom_histogram(binwidth = 0.01, fill = "green", color = "black") + 
  ggtitle("Histogram of WCR") + 
  theme_minimal()

p3 <- ggplot(concrete, aes(x = age)) + 
  geom_histogram(binwidth = 5, fill = "red", color = "black") + 
  ggtitle("Histogram of Age") + 
  theme_minimal()

p4 <- ggplot(concrete, aes(x = strength)) + 
  geom_histogram(binwidth = 1, fill = "purple", color = "black") + 
  ggtitle("Histogram of Strength") + 
  theme_minimal()

# Arrange histograms in a grid
grid.arrange(p1, p2, p3, p4, ncol = 2)

# b) Marginal Distributions of the Variables
# Summary statistics for marginal distributions
summary(concrete)

# Optional: Density plots for marginal distributions
p5 <- ggplot(concrete, aes(x = cement)) + 
  geom_density(fill = "blue", alpha = 0.5) + 
  ggtitle("Density Plot of Cement") + 
  theme_minimal()

p6 <- ggplot(concrete, aes(x = wcr)) + 
  geom_density(fill = "green", alpha = 0.5) + 
  ggtitle("Density Plot of WCR") + 
  theme_minimal()

p7 <- ggplot(concrete, aes(x = age)) + 
  geom_density(fill = "red", alpha = 0.5) + 
  ggtitle("Density Plot of Age") + 
  theme_minimal()

p8 <- ggplot(concrete, aes(x = strength)) + 
  geom_density(fill = "purple", alpha = 0.5) + 
  ggtitle("Density Plot of Strength") + 
  theme_minimal()

# Arrange density plots in a grid
grid.arrange(p5, p6, p7, p8, ncol = 2)

# c) Scatter Plots of Strength (Response) against each Predictor
# Scatter plot: Strength vs Cement
s1 <- ggplot(concrete, aes(x = cement, y = strength)) + 
  geom_point(color = "blue") + 
  geom_smooth(method = "lm", se = FALSE, color = "red") + 
  ggtitle("Strength vs Cement") + 
  theme_minimal()

# Scatter plot: Strength vs WCR
s2 <- ggplot(concrete, aes(x = wcr, y = strength)) + 
  geom_point(color = "green") + 
  geom_smooth(method = "lm", se = FALSE, color = "red") + 
  ggtitle("Strength vs WCR") + 
  theme_minimal()

# Scatter plot: Strength vs Age
s3 <- ggplot(concrete, aes(x = age, y = strength)) + 
  geom_point(color = "red") + 
  geom_smooth(method = "lm", se = FALSE, color = "blue") + 
  ggtitle("Strength vs Age") + 
  theme_minimal()

# Arrange scatter plots in a grid
grid.arrange(s1, s2, s3, ncol = 2)


# New Trial 2











