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


# =============================================================================
# ==============================================================================
  
# New Trial 2
# Load the data (assuming it's in a CSV file named 'concrete.csv')
# concrete <- read.csv("concrete.csv")

# Set up a 2x2 plotting area for histograms with overlaid density plots
par(mfrow = c(2, 2))

# a & b) Histograms with Overlaid Marginal Density Distributions
# Histogram and density for cement
hist(concrete$cement, main = "Histogram of Cement with Density", 
     xlab = "Cement", col = "lightblue", probability = TRUE, breaks = 15)
lines(density(concrete$cement), col = "red", lwd = 2)

# Histogram and density for wcr
hist(concrete$wcr, main = "Histogram of WCR with Density", 
     xlab = "WCR", col = "lightgreen", probability = TRUE, breaks = 15)
lines(density(concrete$wcr), col = "red", lwd = 2)

# Histogram and density for age
hist(concrete$age, main = "Histogram of Age with Density", 
     xlab = "Age", col = "lightcoral", probability = TRUE, breaks = 15)
lines(density(concrete$age), col = "red", lwd = 2)

# Histogram and density for strength
hist(concrete$strength, main = "Histogram of Strength with Density", 
     xlab = "Strength", col = "purple", probability = TRUE, breaks = 15)
lines(density(concrete$strength), col = "red", lwd = 2)
par(mfrow = c(1, 1))


# Reset plotting parameters for scatter plots
par(mfrow = c(2, 2))

# c) Scatter Plots of Strength against each Predictor
# Scatter plot: Strength vs Cement
plot(concrete$cement, concrete$strength, main = "Strength vs Cement", 
     xlab = "Cement", ylab = "Strength", pch = 16, col = "blue")
# abline(lm(strength ~ cement, data = concrete), col = "red", lwd = 2)

# Scatter plot: Strength vs WCR
plot(concrete$wcr, concrete$strength, main = "Strength vs WCR", 
     xlab = "WCR", ylab = "Strength", pch = 16, col = "green")
# abline(lm(strength ~ wcr, data = concrete), col = "red", lwd = 2)

# Scatter plot: Strength vs Age
plot(concrete$age, concrete$strength, main = "Strength vs Age", 
     xlab = "Age", ylab = "Strength", pch = 16, col = "red")
# abline(lm(strength ~ age, data = concrete), col = "blue", lwd = 2)

# Reset plotting parameters to default
par(mfrow = c(1, 1))

# b) Summary statistics for marginal distributions
summary(concrete)


# Part B
# Plot correlation ellipses
plotcorr(cor_matrix, col = "blue", main = "Correlation Ellipse Plot")


# ===============================================================================================
# ==================================================================================================

# Model with Age as a Factor variable
concrete2 <- concrete
concrete2$age <- as.character(concrete2$age)
str(concrete2)
conc_f <- lm(strength ~ cement + wcr + age, data = concrete2)
conc_f
summary(conc_f)

# (i) Pearson correlation coefficients
cor(concrete2, method = "pearson")

# Compute the correlation matrix - Same!
cor_matx <- cor(concrete2[, c("cement","wcr","age","strength")])
print(cor_matx)


# (ii) An ellipse plot to visualise collinearity
pacman::p_load(ellipse)
plotcorr(cor(concrete2))


# (iii) Variance Inflation Factors (VIFs)
pacman::p_load(car)
conc_f <- lm(strength ~ cement + wcr + age, data = concrete2)
vif(conc_f)


