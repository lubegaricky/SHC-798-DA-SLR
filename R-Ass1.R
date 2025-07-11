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
