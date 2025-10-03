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
# conc_f <- lm(strength ~ cement + wcr + age, data = concrete2)
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
conc_f <- lm(strength ~ cement + wcr + age + cement:age, data = concrete2)
summary(conc_f)
vif(conc_f)

## Residual analysis
plot(conc_f, which=1)
resplot(conc_f, plots = 1)

plot(conc_f, which = 2)
resplot(conc_f, plots = 2)

## Scale-location plot
plot(conc_f, which = 3)
resplot(conc_f, plots = 3)

## Cook's Distance plot
plot(conc_f, which = 4)
plot(conc_f, which = 5)
resplot(conc_f, plots = 4)

# Part b): Variable Selection
# Backward Elimination
summary(conc_f)
drop1(conc_f, test="F")


# Part e) 5-fold cross validation

# Generic code
# spe <- c()
# folds <- 5
# sb <- round(seq(0,nrow(dat),length=(folds+1)))
# for (i in 1:folds)
# {
#   test <- (sb[((folds+1)-i)]+1):(sb[((folds+2)-i)])
#   train <- (1:nrow(dat))[-test]
#   fit <- lm(res ~ p1+..., data=dat[train,])
#   pred <- predict(fit, newdata=dat[test,])
#   spe <- c(spe,(dat$response[test]- pred)^2)
# }
# 
# spe <- data.frame(spe1, spe2, spe3)
# apply(spe,2,mean)




# ======================================
# Set seed for reproducibility
set.seed(123)

# Load data (assuming concrete is already defined)
# If not, uncomment and adjust: concrete <- read.csv("concrete.csv")

# Number of observations and folds
n <- nrow(concrete)
k <- 5
sb <- round(seq(0, n, length = (k + 1)))  # Fold boundaries

# Initialize vectors to store MSPE for each model
mspe_full <- numeric(k)
mspe_reduced <- numeric(k)

# 5-fold cross-validation for full model (strength ~ cement + wcr + age)
for (i in 1:k) {
  test <- (sb[k + 1 - i] + 1):sb[k + 2 - i]
  train <- (1:n)[-test]
  fit_full <- lm(strength ~ cement + wcr + age, data = concrete[train, ])
  pred_full <- predict(fit_full, newdata = concrete[test, ])
  mspe_full[i] <- mean((concrete$strength[test] - pred_full)^2, na.rm = TRUE)
}

# 5-fold cross-validation for reduced model (strength ~ cement + age, dropping wcr)
for (i in 1:k) {
  test <- (sb[k + 1 - i] + 1):sb[k + 2 - i]  # Same fold split for fairness
  train <- (1:n)[-test]
  fit_reduced <- lm(strength ~ cement + age, data = concrete[train, ])
  pred_reduced <- predict(fit_reduced, newdata = concrete[test, ])
  mspe_reduced[i] <- mean((concrete$strength[test] - pred_reduced)^2, na.rm = TRUE)
}

# Calculate overall MSPE for each model
mspe_full_mean <- mean(mspe_full, na.rm = TRUE)
mspe_reduced_mean <- mean(mspe_reduced, na.rm = TRUE)

# Report results
cat("MSPE for Full Model (cement + wcr + age):", mspe_full_mean, "\n")
cat("MSPE for Reduced Model (cement + age):", mspe_reduced_mean, "\n")
cat("MSPE per fold for Full Model:", mspe_full, "\n")
cat("MSPE per fold for Reduced Model:", mspe_reduced, "\n")

# Optional: Check relative increase in MSPE
relative_increase <- ((mspe_reduced_mean - mspe_full_mean) / mspe_full_mean) * 100
cat("Relative increase in MSPE (%):", relative_increase, "\n")

# Optional: Flag negative strength value
if (any(concrete$strength < 0)) {
  cat("Warning: Negative strength value (-0.7) detected, consider removing or correcting this outlier.\n")
}


# Box plots

# Using MSPEs
# Combine MSPEs into a data frame for plotting
mspe_data <- data.frame(
  MSPE = c(mspe_full, mspe_reduced),
  Model = factor(rep(c("Full", "Reduced"), each = k))
)

# Generate box plots
boxplot(MSPE ~ Model, data = mspe_data, 
        main = "MSPE Comparison: Full vs Reduced Model",
        ylab = "Mean Squared Prediction Error (MPa²)",
        col = c("lightblue", "lightgreen"),
        border = "black")

# Using the squared prediction errors
# Combine squared prediction errors into a data frame
spe_full <- vector("list", k)
spe_reduced <- vector("list", k)
spe_data <- data.frame(
  SPE = unlist(c(spe_full, spe_reduced)),
  Model = factor(rep(c("Full", "Reduced"), each = length(unlist(spe_full))))
)

# Generate box plots
boxplot(SPE ~ Model, data = spe_data, 
        main = "Squared Prediction Errors: Full vs Reduced Model",
        ylab = "Squared Prediction Error (MPa²)",
        col = c("lightblue", "lightgreen"),
        border = "black")
