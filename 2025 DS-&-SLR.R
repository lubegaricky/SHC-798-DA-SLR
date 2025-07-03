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





