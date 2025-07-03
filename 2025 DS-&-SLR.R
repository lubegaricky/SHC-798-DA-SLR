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