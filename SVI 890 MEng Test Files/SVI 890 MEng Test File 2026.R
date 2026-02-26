# R Studio Code: MSVI 890 MEng Test File, 2026
  

# ================================
# Getting started with the dataset :
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

# Separating

traj.1 <- read.delim(file.choose(),
                   sep = ";",
                   header = TRUE,
                   stringsAsFactors = FALSE,
                   strip.white = TRUE,
                   fill = TRUE)
traj.1 <- traj.1[, names(df) != ""]
head(traj.1)
summary(traj.1)
str(traj.1)

traj.1 <- read.table(file.choose(),
                 sep = ";",
                 header = TRUE,
                 stringsAsFactors = FALSE,
                 strip.white = TRUE)
head(traj.1)


# Tidyverse alternative (recommended for robustness)

pacman::p_load(readr)

gates <- read_delim(file.choose(),
                 delim = ";",
                 trim_ws = TRUE)
gates <- gates[, names(df) != ""]
summary(gates)
head(gates)
str(gates)

# Explore
# library(dplyr)

traj.1 %>%
  summarise(across(everything(), class)) %>%
  pivot_longer(everything(),
               names_to = "Variable",
               values_to = "Class")


gates %>%
  summarise(across(everything(), class)) %>%
  pivot_longer(everything(),
               names_to = "Variable",
               values_to = "Class")


# ===============================
# 'Trials'
traj.1 <- read.table(file.choose(),
                     sep = ";",
                     header = FALSE,
                     skip = 1,
                     fill = TRUE,
                     stringsAsFactors = FALSE,
                     strip.white = TRUE)

# Add column names for the first 8 fixed columns
colnames(traj.1)[1:8] <- c("Track_ID", "Type", "Entry_Gate", "Entry_Time_s", 
                           "Exit_Gate", "Exit_Time_s", "Traveled_Dist_px", "Avg_Speed_kpxh")

# header = FALSE + skip = 1 — skips the header row manually to avoid the column count mismatch

head(traj.1)
summary(traj.1)
str(traj.1)


