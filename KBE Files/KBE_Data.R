pacman::p_load(tidyverse)
pacman::p_load(openxlsx)
search()

# ================== Claude =============================
n <- 124
zone_mat <- matrix(1, nrow = n, ncol = n)
diag(zone_mat) <- 0

zone_ftr <- zone_mat

assign_factor <- function(mat, origins, dests, ftr) {
  for (i in origins) {
    for (j in dests) {
      mat[i, j] <- ftr
      mat[j, i] <- ftr
    }
  }
  mat
}

# ---- Bombo Road ----
zone_ftr <- assign_factor(zone_ftr,
                          c(99, 102),
                          c(1,2,3,4,5,6,7,9,10,11,12,13,14,15,20,21,22,23,32,33,38,39,40,41,43,117,118),
                          0.167)

zone_ftr <- assign_factor(zone_ftr,
                          c(123,115,96,97,109,98,104),
                          c(1,2,3,4,5,6,7,9,10,11,12,13,14,15,20,21,22,23,32,33,38,39,40,41,43,117,118),
                          0.200)

zone_ftr <- assign_factor(zone_ftr,
                          c(45,46,47,48,107,55,100,101,105,106,103,95,110,124,52,51,50,49),
                          c(1,2,3,4,5,6,7,9,10,11,12,13,14,15,20,21,22,23,32,33,38,39,40,41,43,117,118),
                          0.250)

zone_ftr <- assign_factor(zone_ftr,
                          c(53,54,55,57),
                          c(1,2,3,4,5,6,7,9,10,11,12,13,14,15,20,21,22,23,32,33,38,39,40,41,43,117,118),
                          0.333)

zone_ftr <- assign_factor(zone_ftr,
                          c(58,67,88,89,82,27,26,90,24),
                          c(1,2,3,4,5,6,7,9,10,11,12,13,14,15,20,21,22,23,32,33,38,39,40,41,43,117,118),
                          0.500)

# ---- To Zirobwe ----
zone_ftr <- assign_factor(zone_ftr, c(123,115,96,97,98,103), c(108), 0.500)
zone_ftr <- assign_factor(zone_ftr, c(53,54,55,57,68,74),    c(108), 0.500)
zone_ftr <- assign_factor(zone_ftr,
                          c(45,46,47,48,100,101,105,106,103,95,110,124,52,51,50), c(108), 0.500)
zone_ftr <- assign_factor(zone_ftr, c(73,75,76,77,78),       c(108), 0.500)
zone_ftr <- assign_factor(zone_ftr,
                          c(49,66,84,80,79,112,83,81,85,34,35), c(108), 0.333)
zone_ftr <- assign_factor(zone_ftr,
                          c(58,67,88,89,82,27,26,90), c(108), 0.333)
zone_ftr <- assign_factor(zone_ftr, c(24,20,30,40), c(108), 0.250)
zone_ftr <- assign_factor(zone_ftr,
                          c(1,2,3,4,5,6,7,9,10,11,12,13,14,15,20,21,22,23,32,33,38,39,40,41,43,117,118),
                          c(108), 0.250)

# ---- Northern By-pass ----
zone_ftr <- assign_factor(zone_ftr,
                          c(10,60,61,62,63,64,65,86,93,94,111,113,117,119,121,122),
                          c(44,77,78,114,116,120), 0.333)

zone_ftr <- assign_factor(zone_ftr,
                          c(10,60,61,62,63,64,65,86,93,94,111,113,117,119,121,122),
                          c(4,5,6,7,9,11,13,15,16,38,39,66,118), 0.500)

zone_ftr <- assign_factor(zone_ftr,
                          c(10,60,61,62,63,64,65,86,93,94,111,113,117,119,121,122),
                          c(1,2,3,8,12,14,17,18,20,21,22,23,32,33,40,41,43,
                            19,24,25,26,27,28,29,30,31,34,35,36,37,38,42,54,58,67,
                            71,73,74,75,76,79,80,81,82,83,84,85,87,88,89,90,91,92,112),
                          0.333)

zone_ftr <- assign_factor(zone_ftr,
                          c(10,60,61,62,63,64,65,86,93,94,111,113,117,119,121,122),
                          c(45,46,47,48,49,50,51,52,53,55,56,57,59,68,69,70,
                            72,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,115,123,124),
                          0.250)

# ---- Other Stations in Network ----
zone_ftr <- assign_factor(zone_ftr,
                          c(66,79,80,81,83,84,85,87,91,92,112),
                          c(45,46,47,48,49,50,51,52,53,55,56,57,59,68,69,70,
                            72,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,115,123,124),
                          0.500)

zone_ftr <- assign_factor(zone_ftr,
                          c(66,79,80,81,83,84,85,87,91,92,112),
                          c(1,2,3,8,12,14,17,18,20,21,22,23,32,33,40,41,43,
                            19,24,25,26,27,28,29,30,31,34,35,36,37,38,42,54,58,67,
                            71,73,74,75,76,82,85,88,89,90),
                          0.500)

# ---- Through Matugga ----
zone_ftr <- assign_factor(zone_ftr,
                          c(48,50,45,52,50,47,46,59,56,55,69,70,71,72,75),
                          c(88,84,66,51,80,87,82,67,58), 0.500)

# ---- Through Kasangati ----
zone_ftr <- assign_factor(zone_ftr,
                          c(54,58,97,74,82,27,28,34,35,88,89,90,17),
                          c(56,70,71,44,69,107), 0.500)

# Zone labels
zone_labels <- as.character(1:n)
rownames(zone_ftr) <- zone_labels
colnames(zone_ftr) <- zone_labels

str(zone_ftr)
head(zone_ftr)
summary(zone_ftr)

# Write to Excel
wb <- createWorkbook()
addWorksheet(wb, "zone_ftr_A")
writeData(wb, "zone_ftr_A", zone_ftr, rowNames = TRUE)
saveWorkbook(wb, "D:/2025 MEng Transportation/SHC-798-DA-SLR/KBE Files/zone_ftr_A.xlsx", overwrite = TRUE)

cat("Done. Matrix written to zone_ftr.xlsx\n")
cat("Non-1.0 factor count:", sum(zone_ftr != 1 & zone_ftr != 0), "\n")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# ========================= Grok ===============================
# # 1. Create 124 x 124 matrix
n_zones <- 124

zone_ftr <- matrix(1, nrow = n_zones, ncol = n_zones)
diag(zone_ftr) <- 0

# Add proper row and column names
rownames(zone_ftr) <- paste("Zone", 1:n_zones)
colnames(zone_ftr) <- paste("Zone", 1:n_zones)

# Helper function
set_factor <- function(origins, destinations, factor) {
  origins <- unique(unlist(origins))
  destinations <- unique(unlist(destinations))
  for (o in origins) {
    for (d in destinations) {
      if (o != d) {
        zone_ftr[o, d] <<- factor
        zone_ftr[d, o] <<- factor
      }
    }
  }
}

# Apply Duplicate Factors

# Bombo Road
set_factor(99:102, c(1:7,9:13,14,15,20,21,22,23,32,33,38:41,43,117,118), 0.167)
set_factor(c(123,115,96,97,109,98,104), c(1:7,9:13,14,15,20,21,22,23,32,33,38:41,43,117,118), 0.200)
set_factor(c(45,46,47,48,107,55,100,101,105,106,103,95,110,124,52,51,50,49),
           c(1:7,9:13,14,15,20,21,22,23,32,33,38:41,43,117,118), 0.250)
set_factor(c(53,54,55,57), c(1:7,9:13,14,15,20,21,22,23,32,33,38:41,43,117,118), 0.333)
set_factor(c(58,67,88,89,82,27,26,90,24),
           c(1:7,9:13,14,15,20,21,22,23,32,33,38:41,43,117,118), 0.500)

# To Zirobwe
set_factor(c(123,115,96,97,98,103), 108, 0.500)
set_factor(c(53,54,55,57,68,74), 108, 0.500)
set_factor(c(45,46,47,48,100,101,105,106,103,95,110,124,52,51,50), 108, 0.500)
set_factor(c(73,75,76,77,78), 108, 0.500)
set_factor(c(49,66,84,80,79,112,83,81,85,34,35), 108, 0.333)
set_factor(c(58,67,88,89,82,27,26,90), 108, 0.333)
set_factor(c(24,20,30,40), 108, 0.250)
set_factor(c(1:7,9:13,14,15,20,21,22,23,32,33,39:41,43,117,118), 108, 0.250)

# Northern By-pass
set_factor(c(10,60,61,62,63,64,65,86,93,94,111,113,117,119,121,122), 
           c(44,77,78,114,116,120), 0.333)
set_factor(c(10,60,61,62,63,64,65,86,93,94,111,113,117,119,121,122), 
           c(4:7,9,11,13,15,16,38,39,66,118), 0.500)
set_factor(c(10,60,61,62,63,64,65,86,93,94,111,113,117,119,121,122), 
           c(1,2,3,8,12,14,17,18,20,21,22,23,32,33,40,41,43), 0.333)
set_factor(c(10,60,61,62,63,64,65,86,93,94,111,113,117,119,121,122), 
           c(45:52,53,55,56,57,59,68:70,72,95:110,115,123,124), 0.250)

# Other Stations
set_factor(c(66,79,80,81,83,84,85,87,91,92,112),
           c(45:52,53,55,56,57,59,68:70,72,95:110,115,123,124), 0.500)
set_factor(c(66,79,80,81,83,84,85,87,91,92,112),
           c(1:3,8,12,14,17:18,20:23,32,33,40,41,43,19,24:31,34:39,42,54,58,67,71,73:76,82,85,88,89,90), 0.500)

# Through Matugga & Kasangati
set_factor(c(45,46,47,48,50,52,55,56,59,69,70,71,72,75),
           c(51,58,66,67,80,82,84,87,88), 0.500)
set_factor(c(17,27,28,34,35,54,58,74,82,88,89,90,97),
           c(44,56,69,70,71,107), 0.500)

str(zone_ftr)
head(zone_ftr)
summary(zone_ftr)
view(zone_ftr)

#  Choose save location and Save to Excel
file_path <- "D:/2025 MEng Transportation/SHC-798-DA-SLR/KBE Files/zone_ftr_Gr.xlsx"

wb <- createWorkbook()
addWorksheet(wb, "zone_ftr_Gr")   
writeData(wb, "zone_ftr_Gr", zone_ftr, 
          rowNames = TRUE, colNames = TRUE)

setColWidths(wb, "zone_ftr_Gr", cols = 1:125, widths = "auto")

saveWorkbook(wb, file_path, overwrite = TRUE)

cat("✅ Saved as zone_ftr_g.xlsx\n")
cat(file_path, "\n")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
# ====================Gemini================
# 1. Initialize the 124x124 matrix, Elements = 1, except diagonal (i=j) which equals 0
zone_mat <- matrix(1, nrow = 124, ncol = 124)
diag(zone_mat) <- 0

# Create the working matrix zone_ftr
zone_ftr <- zone_mat

# 2. Define helper function to apply factors symmetrically
apply_factor <- function(mat, origins, destinations, factor) {
  for (i in origins) {
    for (j in destinations) {
      if (i != j) { # Ensure we don't overwrite the diagonal
        mat[i, j] <- factor
        mat[j, i] <- factor
      }
    }
  }
  return(mat)
}

# --- Define Common Sets ---
dest_bomboroad <- c(1:7, 9:15, 20:23, 32:33, 38:41, 43, 117:118)
orig_north_bypass <- c(10, 60:65, 86, 93:94, 111, 113, 117, 119, 121, 122)
dest_other_stns <- c(45:53, 55:57, 59, 68:70, 72, 95:110, 115, 123:124)
dest_network_broad <- c(1:3, 8, 12, 14, 17:18, 20:23, 32:33, 40:41, 43, 
                        19, 24:31, 34:38, 42, 54, 58, 67, 71, 73:76, 79:85, 87:92, 112)

# --- Apply Factors ---

# Bombo Road Section
zone_ftr <- apply_factor(zone_ftr, c(99, 102), dest_bomboroad, 0.167)
zone_ftr <- apply_factor(zone_ftr, c(123, 115, 96, 97, 109, 98, 104), dest_bomboroad, 0.200)
zone_ftr <- apply_factor(zone_ftr, c(45:48, 107, 55, 100:101, 105:106, 103, 95, 110, 124, 52, 51, 50, 49), dest_bomboroad, 0.250)
zone_ftr <- apply_factor(zone_ftr, c(53, 54, 55, 57), dest_bomboroad, 0.333)
zone_ftr <- apply_factor(zone_ftr, c(58, 67, 88, 89, 82, 27, 26, 90, 24), dest_bomboroad, 0.500)

# To Zirobwe Section (Destination 108)
zone_ftr <- apply_factor(zone_ftr, c(123, 115, 96:98, 103), 108, 0.500)
zone_ftr <- apply_factor(zone_ftr, c(53:55, 57, 68, 74), 108, 0.500)
zone_ftr <- apply_factor(zone_ftr, c(45:48, 100:101, 105:106, 103, 95, 110, 124, 52, 51, 50), 108, 0.500)
zone_ftr <- apply_factor(zone_ftr, c(73, 75:78), 108, 0.500)
zone_ftr <- apply_factor(zone_ftr, c(49, 66, 84, 80, 79, 112, 83, 81, 85, 34, 35), 108, 0.333)
zone_ftr <- apply_factor(zone_ftr, c(58, 67, 88:89, 82, 27, 26, 90), 108, 0.333)
zone_ftr <- apply_factor(zone_ftr, c(24, 20, 30, 40), 108, 0.250)
zone_ftr <- apply_factor(zone_ftr, dest_bomboroad, 108, 0.250)

# Northern By-pass Section
zone_ftr <- apply_factor(zone_ftr, orig_north_bypass, c(44, 77, 78, 114, 116, 120), 0.333)
zone_ftr <- apply_factor(zone_ftr, orig_north_bypass, c(4:7, 9, 11, 13, 15, 16, 38, 39, 66, 118), 0.500)
zone_ftr <- apply_factor(zone_ftr, orig_north_bypass, dest_network_broad, 0.333)
zone_ftr <- apply_factor(zone_ftr, orig_north_bypass, dest_other_stns, 0.250)

# Other Stations / Network (Red text)
orig_other_stns <- c(66, 79:81, 83:85, 87, 91:92, 112)
zone_ftr <- apply_factor(zone_ftr, orig_other_stns, dest_other_stns, 0.500)
zone_ftr <- apply_factor(zone_ftr, orig_other_stns, dest_network_broad, 0.500)

# Matugga & Kasangati
zone_ftr <- apply_factor(zone_ftr, c(48, 50, 45, 52, 47, 46, 59, 56, 55, 69:72, 75), 
                         c(88, 84, 66, 51, 80, 87, 82, 67, 58), 0.500)
zone_ftr <- apply_factor(zone_ftr, c(54, 58, 97, 74, 82, 27, 28, 34, 35, 88:90, 17), 
                         c(56, 70, 71, 44, 69, 107), 0.500)
str(zone_ftr)
head(zone_ftr)
summary(zone_ftr)
view(zone_ftr)

# 3. Write to Excel
# Define the full file path
file_path <- "D:/2025 MEng Transportation/SHC-798-DA-SLR/KBE Files/zone_ftr_ge.xlsx"
# Write the matrix to the specified location
write.xlsx(as.data.frame(zone_ftr), 
           file = file_path, 
           rowNames = FALSE, 
           colNames = FALSE)

# ------- Saving file with rows and columns as numbers --------------
# 1. Convert the matrix to a data frame
final_df <- as.data.frame(zone_ftr)

# 2. Assign the numbers 1 to 124 as both Row and Column names
rownames(final_df) <- 1:124
colnames(final_df) <- 1:124

# 3. Define the file path
file_path <- "D:/2025 MEng Transportation/SHC-798-DA-SLR/KBE Files/zone_ftr_gem.xlsx"

# 4. Write to Excel, ensuring Row and Column names are included
write.xlsx(final_df, 
           file = file_path, 
           rowNames = TRUE,   # This adds the zone numbers to the first column
           colNames = TRUE)   # This adds the zone numbers to the first row
