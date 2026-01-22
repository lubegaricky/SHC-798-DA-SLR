# R Studio Code: MSVI 890 MEng Test File, 2026
  
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



# ****************************************************************
# Model with Age as a Factor variable
concrete
concrete2 <- concrete
concrete2$age <- as.character(concrete2$age) # character
str(concrete2)
conc_f2 <- lm(strength ~ cement + wcr + age, data = concrete2)
summary(conc_f2)


str(concrete2$age)
model.matrix(~ age, data = concrete2)[1:6, ]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
concrete3 <- concrete
concrete3$age <- as.factor(concrete3$age) # factor
str(concrete3)
conc_f3 <- lm(strength ~ cement + wcr + age, data = concrete2)
summary(conc_f3)


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
cat("MSPE per fold for Full Model:", mspe_full, "\n")
cat("MSPE per fold for Reduced Model:", mspe_reduced, "\n")
cat("MSPE for Full Model (cement + wcr + age):", mspe_full_mean, "\n")
cat("MSPE for Reduced Model (cement + age):", mspe_reduced_mean, "\n")

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



# ===============================================================
# ```{r Part-1-d, cache = FALSE}
# Part d) 5-fold cross-validation
set.seed(123) # Set seed for reproducibility
n <- nrow(e.consump) # Use e.consump for consistency
k <- 5 # Number of folds
sb <- round(seq(0, n, length = (k + 1)))  # Fold boundaries

# Initialize vectors to store MSPE for each model
mspe_full <- numeric(k)
mspe_reduced <- numeric(k)

# 5-fold cross-validation for full model
for (i in 1:k) {
  test <- (sb[k + 1 - i] + 1):sb[k + 2 - i]
  train <- (1:n)[-test]
  if (sum(complete.cases(e.consump[train, ])) == 0) {
    warning(paste("Fold", i, "has no valid cases"))
    mspe_full[i] <- NA
  } else {
    fit_full <- lm(energy ~ log(area) + log(occup) + climate + glazing + insulation, data = e.consump[train, ])
    pred_full <- predict(fit_full, newdata = e.consump[test, ])
    mspe_full[i] <- mean((e.consump$energy[test] - pred_full)^2, na.rm = TRUE)
  }
}

# 5-fold cross-validation for reduced model
for (i in 1:k) {
  test <- (sb[k + 1 - i] + 1):sb[k + 2 - i]
  train <- (1:n)[-test]
  if (sum(complete.cases(e.consump[train, ])) == 0) {
    warning(paste("Fold", i, "has no valid cases"))
    mspe_reduced[i] <- NA
  } else {
    fit_reduced <- lm(energy ~ log(area) + log(occup) + climate + insulation, data = e.consump[train, ])
    pred_reduced <- predict(fit_reduced, newdata = e.consump[test, ])
    mspe_reduced[i] <- mean((e.consump$energy[test] - pred_reduced)^2, na.rm = TRUE)
  }
}

# Calculate overall MSPE for each model
mspe_full_mean <- mean(mspe_full, na.rm = TRUE)
mspe_reduced_mean <- mean(mspe_reduced, na.rm = TRUE)

# Report results
cat("MSPE per fold for Full Model:", mspe_full, "\n")
cat("MSPE per fold for Reduced Model:", mspe_reduced, "\n")
cat("MSPE for Full Model:", mspe_full_mean, "\n")
cat("MSPE for Reduced Model:", mspe_reduced_mean, "\n")

# Checking relative increase in MSPE
relative_increase <- ((mspe_reduced_mean - mspe_full_mean) / mspe_full_mean) * 100
cat("Relative increase in MSPE (%):", relative_increase, "\n")

# Box plots
mspe_data <- data.frame(
  MSPE = c(mspe_full, mspe_reduced),
  Model = factor(rep(c("Full", "Reduced"), each = k))
)
boxplot(MSPE ~ Model, data = mspe_data, 
        main = "MSPE Comparison: Full vs Reduced Model",
        ylab = "Mean Squared Prediction Error",
        col = c("purple", "lightgreen"),
        border = "black")




 
# ============================================================================
 
# *********************************************************************
   
# =======================================================================

# ANOVA

# Load ggplot2 for better visualization
# library(ggplot2)

# Create boxplot
ggplot(timber, aes(x = species, y = stiffness, fill = species)) +
  geom_boxplot() +
  labs(title = "Bending Stiffness by Timber Species",
       x = "Timber Species",
       y = "Bending Stiffness (kN·m²)") +
  theme_minimal()


# Outliers
# Load dplyr for data manipulation
# library(dplyr)

# Summary statistics by species
summary_stats <- timber %>%
  group_by(species) %>%
  summarise(
    Mean = mean(stiffness),
    SD = sd(stiffness),
    Median = median(stiffness),
    IQR = IQR(stiffness),
    Q1 = quantile(stiffness, 0.25),
    Q3 = quantile(stiffness, 0.75),
    Min = min(stiffness),
    Max = max(stiffness)
  )

# Identify outliers using IQR method
outliers <- timber %>%
  group_by(species) %>%
  mutate(
    Q1 = quantile(stiffness, 0.25),
    Q3 = quantile(stiffness, 0.75),
    IQR = Q3 - Q1,
    Lower_Bound = Q1 - 1.5 * IQR,
    Upper_Bound = Q3 + 1.5 * IQR,
    Outlier = stiffness < Lower_Bound | stiffness > Upper_Bound
  ) %>%
  filter(Outlier) %>%
  select(species, stiffness)

# Print summary statistics and outliers
print("Summary Statistics:")
print(summary_stats)
print("Outliers:")
print(outliers)


# ========================
# Ensure species is a factor
# timber$species <- as.factor(timber$species)

# Create boxplot and store stats
bp <- boxplot(stiffness ~ species,
              data = timber,
              main = "Bending Stiffness by Timber Species",
              xlab = "Timber Species",
              ylab = "Bending Stiffness (kN·m²)",
              col = c("lightblue", "lightgreen", "lightpink"),
              border = "black",
              outline = TRUE)  # ensures outliers are shown

# bp$out contains all outlier values
# bp$group tells which species each outlier belongs to
# bp$names gives the species names in order

# Annotate outliers on the plot
text(x = bp$group, y = bp$out, labels = bp$out, pos = 3, cex = 0.7, col = "red")


# Outliers

# Create boxplot without default outliers
bp <- boxplot(stiffness ~ species,
              data = timber,
              main = "Bending Stiffness by Timber Species",
              xlab = "Timber Species",
              ylab = "Bending Stiffness (kN·m²)",
              col = c("lightblue", "lightgreen", "lightpink"),
              border = "black",
              outline = FALSE)  # hide default outliers

# Compute and plot species means
means <- tapply(timber$stiffness, timber$species, mean)
points(1:length(means), means, col = "blue", pch = 19, cex = 1.2)

# Annotate extremes (min and max) for each species
species_levels <- levels(timber$species)

for (i in seq_along(species_levels)) {
  sp <- species_levels[i]
  sp_data <- timber$stiffness[timber$species == sp]
  
  min_val <- min(sp_data)
  max_val <- max(sp_data)
  
  # Plot extremes as red dots
  points(i, min_val, col = "red", pch = 19)
  points(i, max_val, col = "red", pch = 19)
  
  # Label extremes
  text(i, min_val, labels = round(min_val,1), pos = 3, col = "red", cex = 0.7)
  text(i, max_val, labels = round(max_val,1), pos = 3, col = "red", cex = 0.7)
}

# Add legend
legend("topright", legend = c("Mean", "Min/Max"),
       col = c("blue", "red"), pch = 19, bty = "n")


# ============================================================
# GroK

# Verify data structure
cat("Data structure of timber:\n")
str(timber)
cat("First few rows of timber:\n")
print(head(timber))

# Set y-axis range to include all outliers
y_min <- 6500  # Below 6999.2
y_max <- 11500  # Above 11124.5
cat("Y-axis range: ", y_min, " to ", y_max, "\n")

# Set up the plot with base graphics
par(mar = c(5, 5, 4, 2))  # Adjust margins

# Create boxplot without default outliers
bp <- boxplot(stiffness ~ species, 
              data = timber, 
              col = c("lightblue", "lightgreen", "lightpink"),  # Colors for pine, gum, cedar
              border = "black",  # Black border
              xlab = "Timber Species",
              ylab = "Bending Stiffness (kN·m²)",
              main = "Bending Stiffness by Timber Species",
              outline = FALSE,  # Suppress default outliers
              las = 1,  # Horizontal y-axis labels
              ylim = c(y_min, y_max))  # Ensure y-axis includes outliers

# Debug: Print species order and boxplot stats
cat("Species order in boxplot:", levels(as.factor(timber$species)), "\n")
cat("Boxplot stats (min, Q1, median, Q3, max):\n")
print(bp$stats)

# Test graphics: Plot a blue point to confirm plotting works
cat("Testing graphics: Plotting blue point at (1, 7000)\n")
points(1, 7000, pch = 19, col = "blue", cex = 1.5)

# Hardcode known outliers (from prior analysis)
cat("Plotting hardcoded outliers:\n")
# Pine: outlier at 6999.2 (x=1)
points(1, 6999.2, pch = 19, col = "red", cex = 1.5)
text(1, 6999.2, labels = "6999.2", pos = 3, offset = 1.0, col = "black", cex = 1.0)
cat("  Pine: 6999.2 at x=1\n")
# Gum: outliers at 8314.9 and 11124.5 (x=2)
points(c(2, 2), c(8314.9, 11124.5), pch = 19, col = "red", cex = 1.5)
text(c(2, 2), c(8314.9, 11124.5), labels = c("8314.9", "11124.5"), 
     pos = 3, offset = 1.0, col = "black", cex = 1.0)
cat("  Gum: 8314.9, 11124.5 at x=2\n")
# Cedar: no outliers
cat("  Cedar: No outliers at x=3\n")

# **************************************
# GPT
# Ensure species is a factor
# timber$species <- as.factor(timber$species)

# Draw boxplots without default outliers
boxplot(stiffness ~ species,
        data = timber,
        col = c("lightblue", "lightgreen", "lightpink"),
        border = "black",
        main = "Bending Stiffness by Timber Species",
        xlab = "Timber Species",
        ylab = "Bending Stiffness (kN·m²)",
        outline = FALSE)

species_levels <- levels(timber$species)

# Plot and label min and max values for each species
for (i in seq_along(species_levels)) {
  sp <- species_levels[i]
  sp_data <- timber$stiffness[timber$species == sp]
  
  min_val <- min(sp_data)
  max_val <- max(sp_data)
  
  points(i, min_val, col = "red", pch = 19)
  points(i, max_val, col = "red", pch = 19)
  
  text(i, min_val, labels = round(min_val,1), pos = 3, col = "red", cex = 0.7)
  text(i, max_val, labels = round(max_val,1), pos = 3, col = "red", cex = 0.7)
}



# ===========================
# Violin Plots
# Install and load vioplot package if not already installed
# if (!require(vioplot)) install.packages("vioplot")
pacman::p_load(vioplot)

# Verify data structure
cat("Data structure of timber:\n")
str(timber)
cat("First few rows of timber:\n")
print(head(timber))

# Set y-axis range to include all outliers
y_min <- 6500  # Below 6999.2
y_max <- 11500  # Above 11124.5
cat("Y-axis range: ", y_min, " to ", y_max, "\n")

# Set up the plot with base graphics
par(mar = c(5, 5, 4, 2))  # Adjust margins

# Create violin plots
vioplot(timber$stiffness[timber$species == "pine"],
        timber$stiffness[timber$species == "gum"],
        timber$stiffness[timber$species == "cedar"],
        names = c("pine", "gum", "cedar"),
        col = c("lightblue", "lightgreen", "lightpink"),  # Fill colors
        border = "black",  # Black border
        xlab = "Timber Species",
        ylab = "Bending Stiffness (kN·m²)",
        main = "Bending Stiffness by Timber Species",
        las = 1,  # Horizontal y-axis labels
        ylim = c(y_min, y_max))  # Ensure y-axis includes outliers

# Debug: Print species order
cat("Species order in violin plot: pine, gum, cedar\n")

# Test graphics: Plot a blue point to confirm plotting works
cat("Testing graphics: Plotting blue point at (1, 7000)\n")
points(1, 7000, pch = 19, col = "blue", cex = 1.5)

# Hardcode known outliers (from prior analysis)
cat("Plotting hardcoded outliers:\n")
# Pine: outlier at 6999.2 (x=1)
points(1, 6999.2, pch = 19, col = "red", cex = 1.5)
text(1, 6999.2, labels = "6999.2", pos = 3, offset = 1.0, col = "black", cex = 1.0)
cat("  Pine: 6999.2 at x=1\n")
# Gum: outliers at 8314.9 and 11124.5 (x=2)
points(c(2, 2), c(8314.9, 11124.5), pch = 19, col = "red", cex = 1.5)
text(c(2, 2), c(8314.9, 11124.5), labels = c("8314.9", "11124.5"), 
     pos = 3, offset = 1.0, col = "black", cex = 1.0)
cat("  Gum: 8314.9, 11124.5 at x=2\n")
# Cedar: no outliers
cat("  Cedar: No outliers at x=3\n")

# +++++++++++++++++++++++++++++++++++++++++++++++++=
# Set up the plot with base graphics
par(mfrow = c(3, 1), mar = c(4, 4, 2, 2))  # 3 rows, adjust margins

# Define colors
colors <- c("lightblue", "lightgreen", "lightpink")

# Create histograms for each species
for (i in seq_along(unique(timber$species))) {
  species <- unique(timber$species)[i]
  data <- timber$stiffness[timber$species == species]
  
  # Histogram
  hist(data, breaks = seq(6500, 11500, by = 500), 
       col = colors[i], border = "black", 
       main = paste("Histogram of", species), 
       xlab = "Bending Stiffness (kN·m²)", ylab = "Frequency", 
       xlim = c(6500, 11500))
  
  # Add density curve
  lines(density(data), col = "black", lwd = 2)
}


# ============
# Set up a 3x1 plotting area for histograms with overlaid density plots
par(mfrow = c(3, 1), mar = c(4, 4, 2, 2))

# Histograms with overlaid marginal density distributions
# Histogram and density for pine
hist(timber$stiffness[timber$species == "pine"], 
     main = "Histogram of Pine Stiffness with Density", 
     xlab = "Bending Stiffness (kN·m²)", 
     col = "lightblue", border = "black", 
     probability = TRUE, breaks = seq(6500, 11500, by = 500), 
     xlim = c(6500, 11500))
lines(density(timber$stiffness[timber$species == "pine"]), col = "red", lwd = 2)

# Histogram and density for gum
hist(timber$stiffness[timber$species == "gum"], 
     main = "Histogram of Gum Stiffness with Density", 
     xlab = "Bending Stiffness (kN·m²)", 
     col = "lightgreen", border = "black", 
     probability = TRUE, breaks = seq(6500, 11500, by = 500), 
     xlim = c(6500, 11500))
lines(density(timber$stiffness[timber$species == "gum"]), col = "red", lwd = 2)

# Histogram and density for cedar
hist(timber$stiffness[timber$species == "cedar"], 
     main = "Histogram of Cedar Stiffness with Density", 
     xlab = "Bending Stiffness (kN·m²)", 
     col = "lightpink", border = "black", 
     probability = TRUE, breaks = seq(6500, 11500, by = 500), 
     xlim = c(6500, 11500))
lines(density(timber$stiffness[timber$species == "cedar"]), col = "red", lwd = 2)

# Reset plotting parameters
par(mfrow = c(1, 1))

# Summary statistics for stiffness by species
summary(timber$stiffness[timber$species == "pine"])
summary(timber$stiffness[timber$species == "gum"])
summary(timber$stiffness[timber$species == "cedar"])


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++=

# Assuming your data frame is called timber
# Set colors for each species
colors <- c("lightblue", "lightgreen", "lightpink")

# Create the boxplot
boxplot(stiffness ~ species, data = timber,
        col = colors,
        border = "black",
        main = "Timber Bending Stiffness by Species",
        xlab = "Species",
        ylab = "Stiffness (kN·m^2)")



# Colors for each species
colors <- c("lightblue", "lightgreen", "lightpink")

# Create the boxplot and store statistics
bp <- boxplot(stiffness ~ species, data = timber,
              col = colors,
              border = "black",
              main = "Timber Bending Stiffness by Species",
              xlab = "Species",
              ylab = "Stiffness (kN·m^2)",
              outline = TRUE)  # show outliers

# Loop over each species to label outliers
species_levels <- levels(factor(timber$species))  # ensure correct order

for (i in 1:length(species_levels)) {
  # Identify outliers for this species
  outliers <- timber$stiffness[timber$species == species_levels[i]]
  q <- boxplot.stats(outliers)$out
  
  # Label each outlier
  text(rep(i, length(q)), q, labels = round(q,1), pos = 3, cex = 0.7)
}

# ================================================================
# 3x1 plotting area
par(mfrow = c(3, 1), mar = c(4, 4, 2, 2))

# Histograms with density
hist(timber$stiffness[timber$species == "pine"], 
     main = "Pine Stiffness", xlab = "Bending Stiffness (kN·m²)", 
     col = "lightblue", border = "black", probability = TRUE, 
     breaks = seq(6500, 11500, 500), xlim = c(6500, 11500))
lines(density(timber$stiffness[timber$species == "pine"]), col = "red", lwd = 2)

hist(timber$stiffness[timber$species == "gum"], 
     main = "Gum Stiffness", xlab = "Bending Stiffness (kN·m²)", 
     col = "lightgreen", border = "black", probability = TRUE, 
     breaks = seq(6500, 11500, 500), xlim = c(6500, 11500))
lines(density(timber$stiffness[timber$species == "gum"]), col = "red", lwd = 2)
points(11124.5, 0, pch = 19, col = "red", cex = 1.5)
text(11124.5, 0, labels = "11124.5", pos = 3, cex = 0.7, col = "red")

hist(timber$stiffness[timber$species == "cedar"], 
     main = "Cedar Stiffness", xlab = "Bending Stiffness (kN·m²)", 
     col = "lightpink", border = "black", probability = TRUE, 
     breaks = seq(6500, 11500, 500), xlim = c(6500, 11500))
lines(density(timber$stiffness[timber$species == "cedar"]), col = "red", lwd = 2)

# Reset plot
par(mfrow = c(1, 1))



# ============================================
# 3x1 plotting area
par(mfrow = c(3, 1), mar = c(4, 4, 2, 2))

# Boxplots with R's outliers
bp_pine <- boxplot(timber$stiffness[timber$species == "pine"], 
                   col = "lightblue", border = "black", 
                   main = "Pine Stiffness", xlab = "Pine", 
                   ylab = "Bending Stiffness (kN·m²)", 
                   ylim = c(6500, 11500))
if (length(bp_pine$out) > 0) {
  text(x = bp_pine$group, y = bp_pine$out, labels = bp_pine$out, 
       pos = 3, cex = 0.7, col = "red")
}

bp_gum <- boxplot(timber$stiffness[timber$species == "gum"], 
                  col = "lightgreen", border = "black", 
                  main = "Gum Stiffness", xlab = "Gum", 
                  ylab = "Bending Stiffness (kN·m²)", 
                  ylim = c(6500, 11500))
if (length(bp_gum$out) > 0) {
  text(x = bp_gum$group, y = bp_gum$out, labels = bp_gum$out, 
       pos = 3, cex = 0.7, col = "red")
}

bp_cedar <- boxplot(timber$stiffness[timber$species == "cedar"], 
                    col = "lightpink", border = "black", 
                    main = "Cedar Stiffness", xlab = "Cedar", 
                    ylab = "Bending Stiffness (kN·m²)", 
                    ylim = c(6500, 11500))
if (length(bp_cedar$out) > 0) {
  text(x = bp_cedar$group, y = bp_cedar$out, labels = bp_cedar$out, 
       pos = 3, cex = 0.7, col = "red")
}

# Reset plot
par(mfrow = c(1, 1))

# =========================================++++++++++++++++++++++++================
# T-test

# Load dplyr for data manipulation
# library(dplyr)

# Perform pairwise t-tests with Bonferroni correction
tapply(timber$stiffness, timber$species, sd) # check for group SD

pairwise_results <- pairwise.t.test(timber$stiffness, timber$species, 
                                    p.adjust.method = "bonferroni", 
                                    pool.sd = FALSE, # Welch's t-test (unequal variances)
                                    paired = FALSE,  # Independent samples
                                    conf.level = 0.95)

# Print the results
print("Pairwise t-test results with Bonferroni correction:")
print(pairwise_results)


# *****************
# Pairwise t-tests with Bonferroni adjustment
pairwise.t.test(timber$stiffness, timber$species,
                p.adjust.method = "bonferroni")
                # pool.sd = FALSE)
                



















