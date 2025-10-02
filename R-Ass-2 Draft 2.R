# R Studio Code: MLR Data Analysis with R
  
library(ggplot2) # Load ggplot2 package


# ================================
# Getting started with the dataset in concrete.csv :
pacman::p_load(ggplot2) 
pacman::p_load(tidymodels)

concrete <- read.csv(file.choose(), header = TRUE, na.strings = c("NA"))

head(concrete) # View first few rows of the dataset
summary(concrete) # Get an overview of the dataset
str(concrete)
view(concrete)
unique(concrete$age)
# unique(concrete$cement)
# unique(concrete$wcr)
# unique(concrete$strength)

# concrete$age <- as.character(concrete$age)

# ==================
# Part a): Data Preparation
# Historgrams of the Variables
par(mfrow = c(2, 2))   # 2x2 grid of plots
hist(concrete$cement, main = "Histogram of Cement", xlab = "Cement", col = "skyblue", border = "white")
hist(concrete$wcr, main = "Histogram of WCR", xlab = "WCR", col = "lightgreen", border = "white")
hist(concrete$age, main = "Histogram of Age", xlab = "Age", col = "orange", border = "white")
hist(concrete$strength, main = "Histogram of Strength", xlab = "Strength", col = "pink", border = "white")
par(mfrow = c(1, 1))

# OR
par(mfrow=c(2,2))
for (i in 1:4) hist(concrete[,i], main=names(concrete)[i])
par(mfrow = c(1, 1))



# Marginal Distributions of the Variables
# Trial 1 - density plots
par(mfrow = c(2, 2))
plot(density(concrete$cement), main = "Density of Cement", col = "blue")
plot(density(concrete$wcr), main = "Density of WCR", col = "green")
plot(density(concrete$age), main = "Density of Age", col = "orange")
plot(density(concrete$strength), main = "Density of Strength", col = "red")
par(mfrow = c(1, 1))

# Trial 1 - Boxplots
par(mfrow = c(2, 2))
boxplot(concrete$cement, main = "Cement")
boxplot(concrete$wcr, main = "WCR")
boxplot(concrete$age, main = "Age")
boxplot(concrete$strength, main = "Strength")
par(mfrow = c(1, 1))




# Scatter Plots of Strength against each Predictor
par(mfrow = c(1, 3))  # 3 plots in one row
plot(concrete$cement, concrete$strength, main = "Strength vs Cement", 
     xlab = "Cement", ylab = "Strength", col = "blue", pch = 19)
plot(concrete$wcr, concrete$strength, main = "Strength vs WCR", 
     xlab = "WCR", ylab = "Strength", col = "green", pch = 19)
plot(concrete$age, concrete$strength, main = "Strength vs Age", 
     xlab = "Age", ylab = "Strength", col = "orange", pch = 19)
par(mfrow = c(1, 1))














