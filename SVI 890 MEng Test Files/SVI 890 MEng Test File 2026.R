# R Studio Code: MSVI 890 MEng Test File, 2026
  

# ================================
# Getting started with the dataset in concrete.csv :
pacman::p_load(ggplot2) 
pacman::p_load(tidymodels)

# Data Preparation

# Load required libraries
pacman::p_load(gridExtra)

# Read the test data (it is a .CSV file)
# Getting started with the dataset in trajectory data :
trajectory <- read.csv(file.choose(), header = TRUE, na.strings = c("NA"))
# trajectory
head(trajectory)
summary(trajectory)
str(trajectory)

traj.1 <- read.table(file.choose(),
                 sep = ";",
                 header = TRUE,
                 stringsAsFactors = FALSE,
                 strip.white = TRUE)


