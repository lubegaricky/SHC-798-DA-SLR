# R Studio Code: MSVI 890 MEng Test File, 2026
  

# ================================
pacman::p_load(tidyverse)
# pacman::p_load(tidymodels)
pacman::p_load(gridExtra)
search() #give list of attached packages


# Coordinate System Conversion
# ==============================================================================
# UTM to WGS-84
pacman::p_load(sf)
UTM <- read.csv(file.choose(), header = TRUE, na.strings = c("NA"))
head(UTM)
summary(UTM)
str(UTM)
# Convert UTM (Zone 35S) to WGS84 (Lat/Long)
# Convert to sf object
UTM_sf <- st_as_sf(UTM,
                   coords = c(UTM_X, UTM_Y),
                   crs = 32735)   # EPSG:32735 = UTM Zone 35 South (WGS84)

# Transform to WGS84 (EPSG:4326)
WGS84_sf <- st_transform(UTM_sf, crs = 4326)

# Extract coordinates
coords <- st_coordinates(WGS84_sf)

# Append to dataframe
UTM$Longitude <- coords[,1]
UTM$Latitude  <- coords[,2]

# Export to CSV with semicolon delimiter
write.table(UTM,
            file = "UTM_to_WGS84_R01.csv",
            sep = ";",
            row.names = FALSE)

# -------------------------------------------------

# New File for DFS

# Browse and load File 1 (UTM_DFS1)
UTM_DFS1 <- read.csv(file.choose(), header = TRUE, strip.white = TRUE)
U_DFS

# Browse and load File 2 (UTM_DFS2)
UTM_DFS2 <- read.csv(file.choose(), header = TRUE, check.names = FALSE, strip.white = TRUE)
head(UTM_DFS2)
summary(UTM_DFS2)
str(UTM_DFS2)

# Combine (join using Tag)
U_DFS <- UTM_DFS1 %>%
  left_join(UTM_DFS2, by = "Tag") %>%
  select(Tag, UTM_X, UTM_Y, `UTM Zone`, `UTM Hemisphere`, `Image X`, `Image Y`) %>%
  rename(`UTM X` = UTM_X,
         `UTM Y` = UTM_Y)

# View combined dataframe before exporting
head(U_DFS)
summary(U_DFS)
str(U_DFS)
print(U_DFS)
View(U_DFS)  


# Export with semicolon separator
write.table(U_DFS, "DFS_UTM_2.csv",
            sep = "; ",
            row.names = FALSE,
            quote = FALSE)

# =============================================================
# Getting started with the dataset in trajectory data
# Read the test data (a .CSV file)
traj_jul <- read.csv(file.choose(), header = TRUE, na.strings = c("NA"), check.names = FALSE, strip.white = TRUE)
head(traj_jul)
summary(traj_jul)
str(traj_jul)

traj_Anth <- read.csv(file.choose(), header = TRUE, na.strings = c("NA"), check.names = FALSE, strip.white = TRUE)
head(traj_Anth)
summary(traj_Anth)
str(traj_Anth)

my_traj <- read.csv(file.choose(), header = TRUE, na.strings = c("NA"), check.names = FALSE, strip.white = TRUE)
head(my_traj)
summary(my_traj)
str(my_traj)

# ===============================================================
# Visualisations



