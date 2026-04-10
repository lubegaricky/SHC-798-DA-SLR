pacman::p_load(tidyverse)
pacman::p_load(openxlsx)
search()

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
addWorksheet(wb, "zone_ftr")
writeData(wb, "zone_ftr", zone_ftr, rowNames = TRUE)
saveWorkbook(wb, "D:/2025 MEng Transportation/SHC-798-DA-SLR/KBE Files/zone_ftr.xlsx", overwrite = TRUE)

cat("Done. Matrix written to zone_ftr.xlsx\n")
cat("Non-1.0 factor count:", sum(zone_ftr != 1 & zone_ftr != 0), "\n")
