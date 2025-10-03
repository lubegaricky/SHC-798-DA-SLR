# R Studio Code:
# Part 1: Data Analysis with R
  
library(ggplot2) # Load ggplot2 package
head(mpg) # View first few rows of the dataset
summary(mpg) # Get an overview of the dataset

# --- Add ----
str(mpg)
view(mpg)
# -------------

#(a)
aggregate(cbind(cty, hwy) ~ class, data = mpg, mean)
# aggregate(cbind(cty, hwy) ~ manufacturer, data = mpg, mean)

#(b)
boxplot(cty ~ cyl, data = mpg, main = "City mpg by Cylinders", xlab = "Cylinders", ylab = "City MPG")
boxplot(hwy ~ cyl, data = mpg, main = "Highway mpg by Cylinders", xlab = "Cylinders", ylab = "Highway MPG")

#(c)
plot(mpg$displ, mpg$hwy, main = "Engine Displacement vs. Highway MPG", xlab = "Displacement", ylab = "Highway MPG")
cor(mpg$displ, mpg$hwy)
# [1] -0.76602

#(d)
lm_model <- lm(hwy ~ displ, data = mpg)
summary(lm_model)
plot(mpg$displ, mpg$hwy, main = "Regression: hwy ~ displ")
abline(lm_model, col = "blue")


# Part 2: Data Smoothing


# Traffic flow data
hour <- 6:18
vehicles <- c(200, 350, 500, 420, 380, 300, 250, 220, 200, 280, 400, 550, 600)
traffic <- data.frame(hour, vehicles)

#(a)
running_mean <- sapply(2:(length(vehicles)-1), function(i) mean(vehicles[(i-1):(i+1)]))
hours_smooth <- hour[2:(length(hour)-1)]
plot(hour, vehicles, type = "p", main = "Running Mean (Manual)", xlab = "Hour", ylab = "Vehicles")
lines(hours_smooth, running_mean, type = "l", col = "blue")

#(b)
ks <- ksmooth(hour, vehicles, kernel = "box", bandwidth = 1)
plot(hour, vehicles, main = "ksmooth (Box kernel)", xlab = "Hour", ylab = "Vehicles")
lines(ks, col = "blue")

#(c)
ks_gauss <- ksmooth(hour, vehicles, kernel = "normal", bandwidth = 2)
plot(hour, vehicles, main = "ksmooth (Gaussian kernel)", xlab = "Hour", ylab = "Vehicles")
lines(ks_gauss, col = "blue")

?ksmooth()
?ks.gauss()

#(d)
ks_gauss2 <- ksmooth(hour, vehicles, kernel = "normal", bandwidth = 1)
lines(ks_gauss2, col = "red") # Compare with previous

#(e)
lo <- loess(vehicles ~ hour, data = traffic, span = 0.3, degree = 2)
hour_seq <- seq(6, 18, 0.1)
lo_pred <- predict(lo, newdata = data.frame(hour = hour_seq))
plot(hour, vehicles, main = "LOESS Smoother", xlab = "Hour", ylab = "Vehicles")
lines(hour_seq, lo_pred, col = "purple")


#Part 3: Simple Regression


#Question 1

data(cars)
model_cars <- lm(dist ~ speed, data = cars)
summary(model_cars)
plot(cars$speed, cars$dist, main = "Speed vs Distance")
abline(model_cars, col = "red")

#(a)
#Multiple R-squared:  0.6511,	Adjusted R-squared:  0.6438 
#This means that 65.11% of the variation in stopping distance is explained by speed

#(b)
#For B0=-17.58, This means that at a speed of 0 mph the stopping distance is -17.58 feet. Which doesn't make any sense but it helps with determining the model.
#For B1=3.93, This means for every 1 mph increase in speed, the stopping distance is increased by 3.93 feet.
#This is significant. The p-value is basically 0 (p-value: 1.49e-12), so the relationship is statistically significant. Thus, speed has a great impact on stopping distance.

#(c)
predict(model_cars, newdata = data.frame(speed = 20), interval = "prediction")

#(d)
par(mfrow = c(2, 2))
plot(model_cars)


#Question 2
load(file.choose())
head(housing)

#(a)
model_housing <- lm(price ~ size, data = housing)
summary(model_housing)
#B0 is not relevant, B1 shows that when there is an increase in size then the rice will increase at a rate of 93.01.
#The p-value is less than 0.05 and this shows that the relastionship between size and price is significant.

#(b)
par(mfrow = c(2, 2))
plot(model_housing)
#Residuals vs Fitted: The residuals are not perfectly random; there's a slight curved trend, suggesting non-linearity in the relationship between size and price.
#Q-Q Residuals: Residuals are approximately normally distributed, but a few high values are mild outliers.
#Scale-Location: This suggests the variance of residuals may increase with fitted values.
#Residuals vs Leverage: A few observations (e.g., 64, 74, 44) are moderately influential, but none exceed Cook’s distance threshold.


#Question 3 
par(mfrow = c(1,1))
time <- 0:9
count <- c(500, 400, 320, 250, 190, 150, 120, 90, 70, 50)
bacteria <- data.frame(time, count)

#(a)
plot(bacteria$time, bacteria$count, type = "b", pch = 19,
     xlab = "Time (hours)", ylab = "Bacterial Count",
     main = "Bacterial Count Over Time")

#(b)
lm_linear <- lm(count ~ time, data = bacteria)
summary(lm_linear)
lm_exp <- lm(log(count) ~ time, data = bacteria)
summary(lm_exp)
plot(time, count, pch = 19, col = "darkblue",
     xlab = "Time (hours)", ylab = "Bacterial Count",
     main = "Linear vs Exponential Fit")
abline(lm_linear, col = "red", lwd = 2)
lines(time, exp(predict(lm_exp)), col = "blue", lwd = 2, lty = 2)
legend("topright", legend = c("Linear Fit", "Exponential Fit"),
       col = c("red", "blue"), lwd = 2, lty = c(1, 2))
#The exponential model fits better because it does have a slightly higher R-Squared value.

#(c)
log_pred <- predict(lm_exp, newdata = data.frame(time = 10))
count_pred <- exp(log_pred)
count_pred
# Time(10 hours)=41.68

#(d)
confint(lm_exp)


#Question 4
par(mfrow = c(2,2))
age <- c(5, 10, 15, 20, 25, 30)
strength <- c(48, 42, 37, 30, 27, 21)
alien <- data.frame(age, strength)

#(a)
hist(alien$age, main = "Histogram of Age", xlab = "Age (years)", col = "lightblue")
hist(alien$strength, main = "Histogram of Strength", xlab = "Strength (MPa)", col = "lightgreen")

#(b)
alien$log_age <- log(alien$age)
alien$log_strength <- log(alien$strength)
hist(alien$log_age, main = "Histogram of log(Age)", xlab = "log(Age)", col = "skyblue")
hist(alien$log_strength, main = "Histogram of log(Strength)", xlab = "log(Strength)", col = "lightpink")

#(c)
log_model <- lm(log_strength ~ log_age, data = alien)
summary(log_model)

#(d)
summary(log_model)$r.squared

#(e)
summary(log_model)$coefficients["log_age", "Pr(>|t|)"]

#(f)
residuals <- resid(log_model)
fitted_vals <- fitted(log_model)

plot(fitted_vals, residuals, pch = 19, col = "purple",
     main = "Residuals vs Fitted Values",
     xlab = "Fitted log(Strength)", ylab = "Residuals")
abline(h = 0, col = "red")



# ----- Others -----

# Option1
# Combine columns using cbind
result <- cbind(mpg$cty, mpg$hwy)
# Add column names
colnames(result) <- c("City_MPG", "Highway_MPG")
# Add row names (e.g., using row indices or another column like model names)
rownames(result) <- mpg$model
# View result
print(result)

# Option2
# Create a data frame
result <- data.frame(City_MPG = mpg$cty, Highway_MPG = mpg$hwy)
# Optionally add row names (e.g., using model names)
rownames(result) <- mpg$model
# View result
print(result)


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




