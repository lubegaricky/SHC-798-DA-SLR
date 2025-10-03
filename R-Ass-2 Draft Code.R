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
# view(concrete)
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
summary(conc_model)
confint(conc_model)
confint(conc_model)["(Intercept)", ]

# Part C-2: Comment on the Model output
# -Regression coefficients
# -Model significance
# -Adequacy of fit, and
# -Appropriateness of fit

## Residual analysis
resplot(conc_model)

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

# Autocorrelation using the Durbin-Watson test
pacman::p_load(lmtest)
dwtest(conc_model)



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

# Getting started with the dataset in energy.csv :

e.consump <- read.csv(file.choose(), header = TRUE, na.strings = c("NA"))

head(e.consump) # View first few rows of the dataset
summary(e.consump) # Get an overview of the dataset
str(e.consump) # inspect the dataset and viewing column data types
# view(e.consump) # views entire dataset

# View variables
par(mfrow=c(3,2))
for (i in 1:6) hist(e.consump[,i], main=names(e.consump)[i])
par(mfrow = c(1, 1))


# Part a): Multicollinearity
# (i) Pearson correlation coefficients
cor(e.consump, method = "pearson")

# Compute the correlation matrix - Same!
cor_matrix <- cor(e.consump[, c("area","occup","climate", "glazing", "insulation", "energy")])
print(cor_matrix)


# (ii) An ellipse plot to visualise collinearity
pacman::p_load(ellipse)
plotcorr(cor(e.consump))


# (iii) Variance Inflation Factors (VIFs)
pacman::p_load(car)
engy_model <- lm(energy ~ area + occup + climate + glazing + insulation, data = e.consump)
vif(engy_model)


# Part b): Model and Predictor Linearity
# Initial Model Output
summary(engy_model)
confint(engy_model)
confint(engy_model)["(Intercept)", ]

## Residual analysis 1
plot(engy_model, which=1)
resplot(engy_model, plots = 1)

plot(engy_model, which = 2)
resplot(engy_model, plots = 2)

## Scale-location plot
plot(engy_model, which = 3)
resplot(engy_model, plots = 3)

## Cook's Distance plot
plot(engy_model, which = 4)
plot(engy_model, which = 5)
resplot(engy_model, plots = 4)

resplot(engy_model)

par(mfrow = c(1, 1))

# Linearity of each predictor - Use of Partial Residual Plots
pacman::p_load(car)
crPlots(engy_model)
crPlots(engy_model, layout = c(1,1))

# Tried Package faraway
pacman::p_load(faraway)
prplot(engy_model, 4)
# ?prplot()

# Transformed Model 1
engy_model2 <- lm(energy ~ area + log(occup) + climate + glazing + insulation, data = e.consump)
summary(engy_model2)
crPlots(engy_model2)

## Residual analysis 2
plot(engy_model2, which=1)
resplot(engy_model2, plots = 1)

plot(engy_model2, which = 2)
resplot(engy_model2, plots = 2)

## Scale-location plot
plot(engy_model2, which = 3)
resplot(engy_model2, plots = 3)

## Cook's Distance plot
plot(engy_model2, which = 4)
plot(engy_model2, which = 5)
resplot(engy_model2, plots = 4)

resplot(engy_model2)

# Transformed Model 2
engy_model3 <- lm(energy ~ log(area) + log(occup) + climate + glazing + insulation, data = e.consump)
summary(engy_model3)
crPlots(engy_model3)

## Residual analysis 3
plot(engy_model3, which=1)
resplot(engy_model3, plots = 1)

plot(engy_model3, which = 2)
resplot(engy_model3, plots = 2)

## Scale-location plot
plot(engy_model3, which = 3)
resplot(engy_model3, plots = 3)

## Cook's Distance plot
plot(engy_model3, which = 4)
plot(engy_model, which = 5)
resplot(engy_model3, plots = 4)

resplot(engy_model3)


# ===================================================================
# Part c) Variable Selection starting with the transformed model

# Backward Elimination with AIC
engy.back <- stats::step(engy_model3, direction="backward")
summary(engy.back)
resplot(engy.back)

# # Forward Selection with AIC
engy_null <- lm(energy ~ 1, data = e.consump) # Intercept-only model
sc <- list(lower=engy_null, upper=engy_model3)
# engy.forw <- stats::step(engy_null, scope=sc, direction="forward", k=2)
# summary(engy.forw)
# resplot(engy.forw)


# AIC Stepwise Model Search: Both Directions Approach
# starting with the null model
engy.b1 <- stats::step(engy_null, scope = sc, direction = "both")
summary(engy.b1)
resplot(engy.b1)

# starting with the full model
engy.b2 <- stats::step(engy_model3, scope = sc, direction = "both")
summary(engy.b2)
resplot(engy.b2)

# starting with a model somewhere in the middle
engy.mid <- lm(energy ~  climate + glazing, data = e.consump)
engy.b3<- stats::step(engy.mid, scope = sc, direction = "both")
summary(engy.b3)
resplot(engy.b3)


# ===================================================================================
# Part d) 5-fold cross validation
set.seed(123) # Set seed for reproducibility
n <- nrow(e.consump) # Number of observations and folds
k <- 5 # Number of folds
sb <- round(seq(0, n, length = (k + 1)))  # Fold boundaries

# Initialize vectors to store MSPE for each model
mspe_full <- numeric(k)
mspe_reduced <- numeric(k)

# 5-fold cross-validation for full model (engy_model3)
for (i in 1:k) {
  test <- (sb[k + 1 - i] + 1):sb[k + 2 - i]
  train <- (1:n)[-test]
  fit_full <- lm(energy ~ log(area) + log(occup) + climate + glazing + insulation, data = e.consump[train, ])
  pred_full <- predict(fit_full, newdata = e.consump[test, ])
  mspe_full[i] <- mean((e.consump$energy[test] - pred_full)^2, na.rm = TRUE)
}

# 5-fold cross-validation for reduced model (dropping glazing)
for (i in 1:k) {
  test <- (sb[k + 1 - i] + 1):sb[k + 2 - i]  # Same fold split for comparability
  train <- (1:n)[-test]
  fit_reduced <- lm(energy ~ log(area) + log(occup) + climate + insulation, data = e.consump[train, ])
  pred_reduced <- predict(fit_reduced, newdata = e.consump[test, ])
  mspe_reduced[i] <- mean((e.consump$energy[test] - pred_reduced)^2, na.rm = TRUE)
}

# Calculate overall MSPE for each model
mspe_full_mean <- mean(mspe_full, na.rm = TRUE)
mspe_reduced_mean <- mean(mspe_reduced, na.rm = TRUE)

# Report results
cat("MSPE per fold for Full Model:", mspe_full, "\n")
cat("MSPE per fold for Reduced Model:", mspe_reduced, "\n")
cat("MSPE for Full Model:", mspe_full_mean, "\n")
cat("MSPE for Reduced Model:", mspe_reduced_mean, "\n")


# Optional: Check relative increase in MSPE
relative_increase <- ((mspe_reduced_mean - mspe_full_mean) / mspe_full_mean) * 100
cat("Relative increase in MSPE (%):", relative_increase, "\n")


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
        ylab = "Mean Squared Prediction Error",
        col = c("purple", "lightgreen"),
        border = "black")


# ===============================================================================
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

# =================================================================

# My code chunk
