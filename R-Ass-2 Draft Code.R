# Part 1: MLR

# ================================================================================

#=============================================================================



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


# Part b): Multicollinearity among predictors
# (i) Pearson correlation coefficients
cor(concrete, method = "pearson")

# Compute the correlation matrix - Same!
cor_matrix <- cor(concrete[, c("cement","wcr","age","strength")])
print(cor_matrix)


# (ii) An ellipse plot to visualise collinearity
pacman::p_load(ellipse)
plotcorr(cor(concrete))


# (iii) Variance Inflation Factors (VIFs)
pacman::p_load(car)
conc_model <- lm(strength ~ cement + wcr + age, data = concrete)
vif(conc_model)

# Part C
# Part-C-1
conc_model <- lm(strength ~ cement + wcr + age, data = concrete)
conc_model
summary(conc_model)
confint(conc_model)
confint(conc_model)["(Intercept)", ]

# Part C-2: Comment on the Model output
# -Regression coefficients
# -Model significance
# -Adequacy of fit, and
# -Appropriateness of fit

## Residual analysis
plot(conc_model, which=1)
resplot(conc_model, plots = 1)

plot(conc_model, which = 2)
resplot(conc_model, plots = 2)

## Scale-location plot
plot(conc_model, which = 3)
resplot(conc_model, plots = 3)

## Cook's Distance plot
plot(conc_model, which = 4)
plot(conc_model, which = 5)
resplot(conc_model, plots = 4)


# Part d): Variable Selection
# Backward Elimination with AIC
# drop1(conc_model, test="F")
conc.back <- stats::step(conc_model, direction="backward")
summary(conc.back)
resplot(conc.back)

# Forward Selection with AIC
# add1(conc_null, scope=conc_model, test="F")
conc_null <- lm(strength ~ 1, data = concrete) # Intercept-only model
sc <- list(lower=conc_null, upper=conc_model)
conc.forw <- stats::step(conc_null, scope=sc, direction="forward", k=2)
summary(conc.forw)
resplot(conc.forw)


# AIC Stepwise Model Search: Both Directions Approach
# starting with the null model
conc.b1 <- stats::step(conc_null, scope = sc, direction = "both")
summary(conc.b1)
resplot(conc.b1)

# starting with the full model
conc.b2 <- stats::step(conc_model, scope = sc, direction = "both")
summary(conc.b2)
resplot(conc.b2)

# starting with a model somewhere in the middle
conc_mid <- lm(strength ~  wcr + age, data = concrete)
conc.b3 <- stats::step(conc_mid, scope = sc, direction = "both")
summary(conc.b3)
resplot(conc.b3)

# AIC is used when the principal aim is the prediction




# Part e) 5-fold cross validation
# Set seed for reproducibility
set.seed(123)  # Ensures consistent fold assignment

n <- nrow(concrete) # Number of observations
k <- 5 # Number of folds
fold_size <- n %/% k  # Approximately 10 or 11 per fold (54 / 5 = 10.8)
sb <- round(seq(0, n, length = (k + 1)))  # Fold boundaries

mspe_folds <- numeric(k) # Initialize vector to store MSPE for each fold

# 5-fold cross-validation loop 
for (i in 1:k) {
  # Define test set indices for the current fold
   test <- (sb[k + 1 - i] + 1):sb[k + 2 - i]
  # Define training set indices (all except test set)
   train <- (1:n)[-test]  
  # Fit linear model on training data 
   fit <- lm(strength ~ cement + wcr + age, data = concrete[train, ]) 
  # Predict on test data
   pred <- predict(fit, newdata = concrete[test, ])
  # Calculate squared prediction errors for this fold
   spe <- (concrete$strength[test] - pred)^2
  # Store MSPE for this fold
   mspe_folds[i] <- mean(spe, na.rm = TRUE)  # na.rm handles NA from outliers
}

# Organize SPE into a data frame with 5 columns (one per fold)
n_per_fold <- length(spe) / k  # Number of observations per fold (approx. 10-11)
spe_df <- data.frame(
  spe1 = spe[1:n_per_fold],
  spe2 = spe[(n_per_fold + 1):(2 * n_per_fold)],
  spe3 = spe[(2 * n_per_fold + 1):(3 * n_per_fold)],
  spe4 = spe[(3 * n_per_fold + 1):(4 * n_per_fold)],
  spe5 = spe[(4 * n_per_fold + 1):length(spe)]
)

# Calculate mean SPE for each fold (MSPE per fold)
mspe_per_fold <- apply(spe_df, 2, mean, na.rm = TRUE)

# Calculate overall MSPE
mspe <- mean(mspe_folds, na.rm = TRUE)

# Report results
cat("Mean Squared Prediction Error (MSPE) from 5-fold cross-validation:", mspe, "\n")
cat("MSPE for each fold:", mspe_folds, "\n")
cat("Mean SPE per fold from data frame:", mspe_per_fold, "\n")

# Optional: Flag the negative strength value for review
if (any(concrete$strength < 0)) {
  cat("Warning: Negative strength value (-0.7) detected, consider removing or correcting this outlier.\n")
}

# Optional: Display the full model for reference
summary(lm(strength ~ cement + wcr + age, data = concrete))




boxplot(spe)

# Part f): Prediction
conc.str <- data.frame(cement=350, wcr=0.5, age=28)
predict(conc_model, newdata = conc.str, interval = "conf")
predict(conc_model, newdata = conc.str, interval = "pred")

#
# ====================================================================================================
# =======================================================================================================
# =======================================================================================================

# Question 2
# Energy consumption data from 80 office buildings

Part a): Multicollinearity


