# Assignment 1
## Part 1: Data Analysis with R'

# ```{r, Part-1}
# Getting Started with the Dataset:
pacman::p_load(tidymodels)
pacman::p_load(ggplot2) # checks if ggplot2 is installed;if it's not installed, it automatically installs it first, then loads it. If it's already installed, it just loads it into your current R session

?mpg
mpg
head(mpg) # View first few rows of the dataset
summary(mpg) # Get an overview of the dataset

is.na(mpg) # any NA? Boolean
sum(is.na(mpg)) # Count total NA values across the entire dataset
sapply(mpg, function(x) sum(is.na(x))) # Count NA values by column

mpg %>% ## Count NA values by column (in dplyr)
  summarise_all(~sum(is.na(.))) 

mpg %>%  # Another alternative of the above
  summarise(across(everything(), ~sum(is.na(.))))

# --- Add ----
str(mpg)
view(mpg)
# -------------

#Analyse the mpg dataset using descriptive methods

#(a)
# average city and highway fuel economy across all vehicle classes
# average fuel economy, afe
# Examples
# Single variable
aggregate(mpg$cty, by = list(class = mpg$class), FUN = mean)
aggregate(cty ~ class, data = mpg, FUN = mean)

# Multiple variables
afe <- aggregate(cbind(cty, hwy) ~ class, data = mpg, FUN = mean)
afe

aggregate(cbind(cty, hwy) ~ class, data = mpg, mean)
# cbind(mpg$cty, mpg$hwy)
# cbind(City = mpg$cty, Highway = mpg$hwy, row.names = mpg$model)
# cbind(City = mpg$cty, Highway = mpg$hwy, row.names = mpg$model)
# cbind(CityMPG = mpg$cty, HwyMPG = mpg$hwy, row.names = paste(mpg$manufacturer, mpg$model))

aggregate(list(cty, hwy) ~ class, data = mpg, mean)
# aggregate(cbind(cty, hwy) ~ manufacturer, data = mpg, mean)
c(mpg$cty, mpg$hwy)


#(b)
# Compare the fuel efficiency (cty and hwy)
boxplot(cty ~ cyl, data = mpg, main = "City mpg by Cylinders", xlab = "Cylinders", ylab = "City mpg")
boxplot(hwy ~ cyl, data = mpg, main = "Highway mpg by Cylinders", xlab = "Cylinders", ylab = "Highway mpg")


par(mfrow = c(1, 2)) # Set up a 1x2 plot layout for side-by-side boxplots

# Boxplot for city mpg by cylinders
boxplot(cty ~ cyl, data = mpg, 
        main = "City mpg by Number of Cylinders", 
        xlab = "Cylinders", 
        ylab = "City mpg (miles per gallon)")

# Boxplot for highway mpg by cylinders
boxplot(hwy ~ cyl, data = mpg, 
        main = "Highway mpg by Number of Cylinders", 
        xlab = "Cylinders", 
        ylab = "Highway mpg (miles per gallon)")

par(mfrow = c(1, 1)) # Reset plot layout to default

# Combine plots by faceting
library(tidyr)
mpg_comb <- mpg %>%
  select(cyl, cty, hwy) %>%
  pivot_longer(cols = c(cty, hwy), names_to = "fuel_econ", values_to = "mpg")

ggplot(mpg_comb, aes(x = factor(cyl), y = mpg, fill = fuel_econ)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Fuel Efficiency by Number of Cylinders",
       x = "Number of Cylinders",
       y = "miles per gallon",
       fill = "Fuel Economy") +
  scale_fill_manual(values = c("cty" = "lightblue", "hwy" = "lightcoral"),
                    labels = c("City", "Highway")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# (c)
# Correlation
# Engine Displacement vs Highway Fuel Economy Correlation Analysis
cat("\nCorrelation: engine displacement (displ) and highway fuel economy (hwy) \n") 
# Load required libraries
# library(ggplot2)
# library(dplyr)
# library(corrplot)

# Load the mpg dataset (built-in to ggplot2)
# data("mpg")

# Display basic information about the dataset
cat("Dataset Overview:\n")
cat("Number of observations:", nrow(mpg), "\n")
cat("Number of variables:", ncol(mpg), "\n")
cat("\nFirst few rows:\n")
print(head(mpg))

# Summary statistics for displacement and highway mpg
cat("\n=== SUMMARY STATISTICS ===\n")
cat("\nEngine Displacement (displ):\n")
print(summary(mpg$displ))
cat("\nHighway Fuel Economy (hwy):\n")
print(summary(mpg$hwy))

# Calculate correlation coefficient
correlation_pearson <- cor(mpg$displ, mpg$hwy)
correlation_spearman <- cor(mpg$displ, mpg$hwy, method = "spearman")

cat("\n=== CORRELATION ANALYSIS ===\n")
cat("Pearson correlation coefficient:", round(correlation_pearson, 4), "\n")
cat("Spearman correlation coefficient:", round(correlation_spearman, 4), "\n")

# Interpretation of correlation strength
interpret_correlation <- function(r) {
  abs_r <- abs(r)
  if (abs_r >= 0.7) return("Strong")
  else if (abs_r >= 0.3) return("Moderate")
  else return("Weak")
}

cat("Correlation strength:", interpret_correlation(correlation_pearson), "\n")
cat("Direction:", ifelse(correlation_pearson > 0, "Positive", "Negative"), "\n")

# Create basic scatter plot
cat("\n=== Creating a Basic Scatter Plot ===\n")

# Basic scatter plot
plot_dh <- ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(alpha = 0.9, size = 2, color = "black") +
    labs(
    title = "Engine Displacement vs Highway Fuel Economy",
    subtitle = paste("Pearson r =", round(correlation_pearson, 3)),
    x = "Engine Displacement (L)",
    y = "Highway Fuel Economy (mpg)",
    caption = "Data source: mpg dataset"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 11),
    panel.grid.minor = element_blank()
  )

print(plot_dh)


cat("\nTest the significance of the correlation \n")
cor_test <- cor.test(mpg$displ, mpg$hwy, method = "pearson")
cat("Pearson correlation test:\n")
print(cor_test)
cat("\nSignificance level: ", ifelse(cor_test$p.value < 0.001, "p < 0.001 (highly significant)", 
                                   ifelse(cor_test$p.value < 0.01, "p < 0.01 (significant)", 
                                          ifelse(cor_test$p.value < 0.05, "p < 0.05 (significant)", "not significant"))), "\n")

# Linear regression
cat("\n Linear Regression Model")
lm_model <- lm(hwy ~ displ, data = mpg)
summary(lm_model)
plot(mpg$displ, mpg$hwy)
abline(lm_model, col = "red")
# Comment on the output and what it implies about the relationship between engine size and fuel efficiency

cat("\n Model Diagnostics \n")
# Diagnostics plots
par(mfrow = c(2,2))
plot(lm_model)

# 
# par(mfrow = c(1,2))
# plot(lm_model)
# 
# par(mfrow = c(1,2))
# #Tukey-Anscombe
# plot(lm_model$fitted,lm_model$resid, main = "City mpg by Number of Cylinders")

#Quantile-Quantile Plot
# qqnorm(lm_model$resid) #Quantile-Quantile Plot
# qqline(lm_model$resid) # adds the diagonal line
# par(mfrow = c(1,1))
# 
# par(mfrow = c(1,2))
# ## Residuals vs. Predictor
# xx <- mpg$displ
# yy <- residuals(lm_model)
# plot(xx, yy, xlab="predictor (displ)", ylab="Residuals", pch=20)
# title("Residuals vs. Predictor displ")
# lines(loess.smooth(xx,yy),col="red") +
#   abline(h=0, col="grey")
# 
#  
# ## Tukey-Anscombe Plot
# lm_dh <- fitted(lm_model)
# plot(lm-dh, yy, xlab="Fitted", ylab="Residuals", pch=20)
# title("Residuals vs. Fitted Values")
# lines(loess.smooth(lm_dh,yy),col="red") +
#   abline(h=0, col="grey")

par(mfrow = c(1,1))


# Tukey-Anscombe Plot
plot(lm_model$fitted.values, lm_model$residuals, xlab="Fitted", ylab="Residuals", pch=20) +
  title("Residuals vs. Fitted Values") +
  lines(loess.smooth(lm_model$fitted.values, lm_model$residuals),col="red") +
  abline(h=0, col="grey")

# Residuals vs. Predictor Plot
plot(mpg$displ, lm_model$residuals, xlab="predictor (displ)", ylab="Residuals", pch=20) +
  title("Residuals vs. Predictor displ") +
  lines(loess.smooth(mpg$displ, lm_model$residuals),col="red") +
  abline(h=0, col="grey")

# Quantile-Quantile Plot
qqnorm(lm_model$residuals)
qqline(lm_model$residuals) # adds the diagonal line


## ====  Part 2: Data Smoothing =====
# Traffic Flow Data Analysis
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

#(d)
ks_gauss2 <- ksmooth(hour, vehicles, kernel = "normal", bandwidth = 1)
lines(ks_gauss2, col = "red") # Compare with previous

#(e)
lo <- loess(vehicles ~ hour, data = traffic, span = 0.3, degree = 2)
hour_seq <- seq(6, 18, 0.1)
lo_pred <- predict(lo, newdata = data.frame(hour = hour_seq))
plot(hour, vehicles, main = "LOESS Smoother", xlab = "Hour", ylab = "Vehicles")
lines(hour_seq, lo_pred, col = "purple")


## Part 3: Simple regression
# Question 1

# The dataset cars
# A SLR to analyse the relationship between speed and stopping distance

cat("\n Test SLR between speed and stopping distance \n")
lm_s.sd <- lm(dist ~ speed, data = cars)
summary(lm_s.sd)

cat("\n ===  SLR Model Plot  === \n")
plot(cars$speed, cars$dist)
abline(lm_s.sd, col = "blue")

# (a)
# From the model summary, Multiple R-squared:  0.6511,	Adjusted R-squared:  0.6438
# Thus, 65.11% of the variation in stopping distance is explained by speed

# (b)
# Intercept (-17.5791): This means that for a theoretical speed of 0 mph the predicted stopping distance is -17.5791 feet (this is not practically rational but ensures the regression line fits the data best within the observed speed range. It is not meaningful to extrapolate to speed = 0.
  # It's p-value (0.0123) is small and statistically significant at the 5% level, but its practical importance is limited.

# Slope (3.9324): For every 1 mph increase in speed, stopping distance increases by about 3.9324 feet. Higher driving speeds require longer stopping distances.
  # The p-value (1.49e-12) is much smaller than 0.05 (even at a 1% significance level), so the relationship between speed and stopping distance is statistically significant.
  # We reject the null hypothesis that speed has no effect on stopping distance. Thus, speed has an considerable impact on stopping distance.

# (c)
# Predicting stopping distance for  speed = 20 mph;  compute a 95% prediction interval.

predict(lm_s.sd, newdata = data.frame(speed = 20), interval = "prediction", level = 0.95)

# (d)
cat("Evaluating Model Assumptions \n")
cat("\n ===  Model Diagnostics Plots  === \n")
# Diagnostics plots
par(mfrow = c(2,2))
plot(lm_s.sd)

par(mfrow = c(1,1))
# Tukey-Anscombe Plot
plot(lm_s.sd$fitted.values, lm_s.sd$residuals, xlab="Fitted", ylab="Residuals", pch=20) +
  title("Residuals vs. Fitted Values") +
  lines(loess.smooth(lm_s.sd$fitted.values, lm_s.sd$residuals),col="red") +
  abline(h=0, col="grey")

# Residuals vs. Predictor Plot
plot(cars$speed, lm_s.sd$residuals, xlab="predictor (speed)", ylab="Residuals", pch=20) +
  title("Residuals vs. Predictor displ") +
  lines(loess.smooth(cars$speed, lm_s.sd$residuals),col="red") +
  abline(h=0, col="grey")

# Quantile-Quantile Plot
qqnorm(lm_s.sd$residuals) #Quantile-Quantile Plot
qqline(lm_s.sd$residuals) # adds the diagonal line

# Evaluating Model Assumptions
cat("Model Assumption Evaluation \n")
cat("\n ===  Linearity  === \n")
# From the Tukey-Anscombe Plot (Residuals vs. Fitted):
  # By inspection, the residuals generally hover around the zero line which suggests that they likely have a mean of approximately zero
  # There is, however, slight curvature in the red LOESS line which implies mild (misspecified) non-linearity. This is confirmed by the systematic misprediction in the middle (overpredicting) and the extremes (underpredicting).
  # **Transformation**: Add a quadratic term (dist = β~0~ + β~1~ . speed + β~2~ . speed^2^ + E~i~) to fix this and improve the model (as the true relationship is quadratic).

cat("\n ===  Homoskedasticity  === \n")
# From the Scale-Location Plot:
  # The red line is slightly upward-trending, indicating that variance increases with fitted values (minor heteroscedasticity)
  # The Tukey-Anscombe plot also seems to indicate that the scatter is not constant for the entire range of speed/fitted values (less scatter for lower values and more scatter for higher values).
  # There is an obvious violation of homoskedasticity.
  # **Transformation**: Log-transform on dist (since stopping distance cannot be negative)

cat("\n ===  Independence  === \n")
  # Since the data is not time-dependent, independence is likely satisfied (no autocorrelation expected)
  # **Transformation**: None needed

cat("\n ===  Normality  === \n")
# From the Q-Q Plot: 
  # The bulk of the residuals (in the central region) are approximately Gaussian distributed.
  # A noticeable deviations (or outliers) at the upper tail indicates right skewness hence departure from normality.
  # The assumption of Gaussian errors is slightly violated by the model due to this moderate non-normality.
  # **Transformation**: Log-transform on dist to correct right-skewness (improve normality and heteroskedasticity)

cat("\n ===  Model Evaluation and Improvements  ==== \n")
# Therefore, this model (lm_s.sd = dist ~ speed) has minor assumption violations (non-linearity, heteroscedasticity, non-normality).
# Suggested transformations like the log(dist) ~ speed and a quadratic term can be made and the diagnostics re-checked.
# The best model is the one with the most stable residuals, best-fulfilled assumptions, and highest adjusted R².

## Part 3: Simple regression
# Question 2

# housing.rda
# pax.data <- read.delim(file.choose(), header = TRUE, na.strings = c("NA"))
# amb.data <- read.delim(file.choose(), header = TRUE, na.strings = c("NA"))
# brk.dat <- read.delim(file.choose(), header = TRUE, na.strings = c("NA"))
# load(file.choose())

load(file.choose())
head(housing)

# Explore the data
head(housing) # View first few rows of the dataset
summary(housing) # Get an overview of the dataset

is.na(housing) # any NA? Boolean
sum(is.na(housing)) # Count total NA values across the entire dataset
sapply(housing, function(x) sum(is.na(x))) # Count NA values by column

housing %>% ## Count NA values by column (in dplyr)
  summarise_all(~sum(is.na(.))) 

housing %>%  # Another alternative of the above
  summarise(across(everything(), ~sum(is.na(.))))

# --- Add ----
str(housing)
view(housing)
# -------------

# Load the housing.rda data file
load(file.choose())
head(housing) # View first few rows of the dataset
summary(housing) # Get an overview of the dataset
str(housing)

# using regression analysis to explore the relationship between house size and price.

# (a)
# Fit a simple regression model.
cat("\n A SLR between house prices and house size \n")
lm_p.s <- lm(price ~ size, data = housing)
summary(lm_p.s)

cat("\n ===  SLR Model Plot  === \n")
plot(housing$size, housing$price) +
  abline(lm_p.s, col = "red")

# Comment on the model summary


# (b) 
# Perform residual diagnostics and comment on model assumptions
cat("Performing Model Diagnostics \n")
cat("\n ===  Model Diagnostics Plots  === \n")
# Diagnostics plots
par(mfrow = c(2,2))
plot(lm_p.s)

par(mfrow = c(1,1))
# Tukey-Anscombe Plot
plot(lm_p.s$fitted.values, lm_p.s$residuals, xlab="Fitted", ylab="Residuals", pch=20) +
  title("Residuals vs. Fitted Values") +
  lines(loess.smooth(lm_p.s$fitted.values, lm_p.s$residuals),col="red") +
  abline(h=0, col="grey")

# Residuals vs. Predictor Plot
plot(housing$size, lm_p.s$residuals, xlab="predictor (size)", ylab="Residuals", pch=20) +
  title("Residuals vs. Predictor size") +
  lines(loess.smooth(housing$size, lm_p.s$residuals),col="red") +
  abline(h=0, col="grey")

# Quantile-Quantile Plot
qqnorm(lm_p.s$residuals) #Quantile-Quantile Plot
qqline(lm_p.s$residuals) # adds the diagonal line

# Evaluating Model Assumptions
cat("Model Assumption Evaluation \n")
# 1.  **Linearity**: *From the Tukey-Anscombe Plot (Residuals vs. Fitted)*:
#   
#   -   From the plot, the residuals generally hover around the zero line which suggests that the E[E~*i*~] = 0 is approximately met.
# However, LOESS smoother line has a kink in the middle and largely deviates from the horizontal.
# The residuals for low  and high house size (and respective fitted house price) values are systematically negative and positive for medium values.
# The linearity assumption is violated and a straight line is not the correct fit to the data
# The model ought to be improved by variable transformation.

# 
# 2.  **Homoskedasticity**: *From the Tukey-Anscombe plot and Scale-Location Plot*:
#   
 # -    The Tukey-Anscombe plot indicates a more or less constant scatter for the entire range of house size (& fitted) values.
#   There is no obvious violation of homoskedasticity.
#  - The red line in the Scale-Location Plot is fairly horizontal which implies constant variance with fitted values (no heteroscedasticity)

# 
# 3.  **Independence**
#   
#   -   The residuals can be considered independent and not correlated

# 
# 4.  **Normality**: *From the Q-Q Plot*:
#   
#   -   The bulk of the residuals (in the central region) lie on the line and thereby follow the Gaussian distribution.
# There are slight deviations (or outliers) at the lower and upper tail which indicate right skewness.
# The assumption of Gaussian errors is slightly violated by the model due to this moderate non-normality.
# Despite this, the approximation to normality in the center may be sufficient to validate this model.
# 
# #### **Model Evaluation and Improvements**
# 
# -   Therefore, this model (lm_s.sd = dist \~ speed) has minor assumption violations (non-linearity, heteroscedasticity, non-normality).
# 
# -   Suggested transformations like the log(dist) \~ speed and a quadratic term can be made and the diagnostics re-checked. The best model is the one with the most stable residuals, best-fulfilled assumptions, and highest adjusted R².




