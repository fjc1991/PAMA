## Logistic Regression
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(car)
library(MASS)
library(lme4)
library(lmerTest)
library(lmtest)

data <- read.csv(file.choose())


###################################
####### Logistic Regression #######
###################################

# Electronic
# M1
summary(M1 <- glm(data$Electronics ~ 
                      
                      # IVs
                      data$Big5_O_PosLexCountAvg +
                      data$Big5_C_PosLexCountAvg +
                      data$Big5_E_PosLexCountAvg +
                      data$Big5_A_PosLexCountAvg +
                      data$Big5_N_PosLexCountAvg,
                    
                    # Data
                    data = data, family = binomial)
)

a1 <- summary(M1)
a2 <- round(a1$coefficients,3)
a3 <- car::vif(M1, type = 'predictor')
vif_range <- paste0(min(round(a3, 3)), " ~ ", max(round(a3, 3)))
summary_stats <- matrix(NA, nrow = 1, ncol = ncol(a2))
summary_stats[1, 3] <- paste0(min(round(a3,3)), " ~ ", max(round(a3,3)))
summary_stats[1, 4] <- round(a1$r.squared,3)
a2 <- rbind(a2, summary_stats)
write.csv(a2,"LR Elec (M1).csv")
bptest_result <- bptest(M1)
print(bptest_result)

# M2
summary(M2 <- glm(data$Electronics ~ 
                      
                      # IVs
                      data$Big5_O_PosLexCountAvg +
                      data$Big5_C_PosLexCountAvg +
                      data$Big5_E_PosLexCountAvg +
                      data$Big5_A_PosLexCountAvg +
                      data$Big5_N_PosLexCountAvg +
                      
                      # Controls
                      prod_review_count + review_count + title_count + also_buy + 
                      rank + also_view + imageURLHighRes_count, 
                    
                    # Data
                    data = data, family = binomial)
)

a1 <- summary(M2)
a2 <- round(a1$coefficients,3)
a3 <- car::vif(M2, type = 'predictor')
vif_range <- paste0(min(round(a3, 3)), " ~ ", max(round(a3, 3)))
summary_stats <- matrix(NA, nrow = 1, ncol = ncol(a2))
summary_stats[1, 3] <- paste0(min(round(a3,3)), " ~ ", max(round(a3,3)))
summary_stats[1, 4] <- round(a1$r.squared,3)
a2 <- rbind(a2, summary_stats)
write.csv(a2,"LR Elec (M2).csv")
bptest_result <- bptest(M2)
print(bptest_result)


# Mobile
# M3
summary(M3 <- glm(data$Mobile.Gadgets ~ 
                    
                    # IVs
                    data$Big5_O_PosLexCountAvg +
                    data$Big5_C_PosLexCountAvg +
                    data$Big5_E_PosLexCountAvg +
                    data$Big5_A_PosLexCountAvg +
                    data$Big5_N_PosLexCountAvg , 
                  
                  # Data
                  data = data, family = binomial)
)

a1 <- summary(M3)
a2 <- round(a1$coefficients,3)
a3 <- car::vif(M3, type = 'predictor')
vif_range <- paste0(min(round(a3, 3)), " ~ ", max(round(a3, 3)))
summary_stats <- matrix(NA, nrow = 1, ncol = ncol(a2))
summary_stats[1, 3] <- paste0(min(round(a3,3)), " ~ ", max(round(a3,3)))
summary_stats[1, 4] <- round(a1$r.squared,3)
a2 <- rbind(a2, summary_stats)
write.csv(a2,"LR Mob (M3).csv")
bptest_result <- bptest(M3)
print(bptest_result)

# M4
summary(M4 <- glm(data$Mobile.Gadgets ~ 
                      
                      # IVs
                      data$Big5_O_PosLexCountAvg +
                      data$Big5_C_PosLexCountAvg +
                      data$Big5_E_PosLexCountAvg +
                      data$Big5_A_PosLexCountAvg +
                      data$Big5_N_PosLexCountAvg +
                      
                      # Controls
                      prod_review_count + review_count + title_count + also_buy + 
                      rank + also_view + imageURLHighRes_count, 
                    
                    # Data
                    data = data, family = binomial)
)

a1 <- summary(M4)
a2 <- round(a1$coefficients,3)
a3 <- car::vif(M4, type = 'predictor')
vif_range <- paste0(min(round(a3, 3)), " ~ ", max(round(a3, 3)))
summary_stats <- matrix(NA, nrow = 1, ncol = ncol(a2))
summary_stats[1, 3] <- paste0(min(round(a3,3)), " ~ ", max(round(a3,3)))
summary_stats[1, 4] <- round(a1$r.squared,3)
a2 <- rbind(a2, summary_stats)
write.csv(a2,"LR Mob (M4).csv")
bptest_result <- bptest(M4)
print(bptest_result)


# Computer
# M5
summary(M5 <- glm(data$Computing...Software ~ 
                      
                      # IVs
                      data$Big5_O_PosLexCountAvg +
                      data$Big5_C_PosLexCountAvg +
                      data$Big5_E_PosLexCountAvg +
                      data$Big5_A_PosLexCountAvg +
                      data$Big5_N_PosLexCountAvg ,
                    
                    # Data
                    data = data, family = binomial)
)

a1 <- summary(M5)
a2 <- round(a1$coefficients,3)
a3 <- car::vif(M5, type = 'predictor')
vif_range <- paste0(min(round(a3, 3)), " ~ ", max(round(a3, 3)))
summary_stats <- matrix(NA, nrow = 1, ncol = ncol(a2))
summary_stats[1, 3] <- paste0(min(round(a3,3)), " ~ ", max(round(a3,3)))
summary_stats[1, 4] <- round(a1$r.squared,3)
a2 <- rbind(a2, summary_stats)
write.csv(a2,"LR Com (M5).csv")
bptest_result <- bptest(M5)
print(bptest_result)

# M6
summary(M6 <- glm(data$Computing...Software ~ 
                    
                    # IVs
                    data$Big5_O_PosLexCountAvg +
                    data$Big5_C_PosLexCountAvg +
                    data$Big5_E_PosLexCountAvg +
                    data$Big5_A_PosLexCountAvg +
                    data$Big5_N_PosLexCountAvg +
                    
                    # Controls
                    prod_review_count + review_count + title_count + also_buy + 
                    rank + also_view + imageURLHighRes_count, 
                  
                  # Data
                  data = data, family = binomial)
)

a1 <- summary(M6)
a2 <- round(a1$coefficients,3)
a3 <- car::vif(M6, type = 'predictor')
vif_range <- paste0(min(round(a3, 3)), " ~ ", max(round(a3, 3)))
summary_stats <- matrix(NA, nrow = 1, ncol = ncol(a2))
summary_stats[1, 3] <- paste0(min(round(a3,3)), " ~ ", max(round(a3,3)))
summary_stats[1, 4] <- round(a1$r.squared,3)
a2 <- rbind(a2, summary_stats)
write.csv(a2,"LR Com (M6).csv")
bptest_result <- bptest(M6)
print(bptest_result)


# Home
# M7
summary(M7 <- glm(data$Home...Office ~ 
                      
                      # IVs
                      data$Big5_O_PosLexCountAvg +
                      data$Big5_C_PosLexCountAvg +
                      data$Big5_E_PosLexCountAvg +
                      data$Big5_A_PosLexCountAvg +
                      data$Big5_N_PosLexCountAvg ,
                    
                    # Data
                    data = data, family = binomial)
)

a1 <- summary(M7)
a2 <- round(a1$coefficients,3)
a3 <- car::vif(M7, type = 'predictor')
vif_range <- paste0(min(round(a3, 3)), " ~ ", max(round(a3, 3)))
summary_stats <- matrix(NA, nrow = 1, ncol = ncol(a2))
summary_stats[1, 3] <- paste0(min(round(a3,3)), " ~ ", max(round(a3,3)))
summary_stats[1, 4] <- round(a1$r.squared,3)
a2 <- rbind(a2, summary_stats)
write.csv(a2,"Lr Home (M7).csv")
bptest_result <- bptest(M7)
print(bptest_result)

# M8
summary(M8 <- glm(data$Home...Office ~ 
                    
                    # IVs
                    data$Big5_O_PosLexCountAvg +
                    data$Big5_C_PosLexCountAvg +
                    data$Big5_E_PosLexCountAvg +
                    data$Big5_A_PosLexCountAvg +
                    data$Big5_N_PosLexCountAvg +
                    
                    # Controls
                    prod_review_count + review_count + title_count + also_buy + 
                    rank + also_view + imageURLHighRes_count, 
                  
                  # Data
                  data = data, family = binomial)
)

a1 <- summary(M8)
a2 <- round(a1$coefficients,3)
a3 <- car::vif(M8, type = 'predictor')
vif_range <- paste0(min(round(a3, 3)), " ~ ", max(round(a3, 3)))
summary_stats <- matrix(NA, nrow = 1, ncol = ncol(a2))
summary_stats[1, 3] <- paste0(min(round(a3,3)), " ~ ", max(round(a3,3)))
summary_stats[1, 4] <- round(a1$r.squared,3)
a2 <- rbind(a2, summary_stats)
write.csv(a2,"Lr Home (M8).csv")
bptest_result <- bptest(M8)
print(bptest_result)


# Sports
# M9
summary(M9 <- glm(data$Sports..Health...Outdoors ~ 
                      
                      # IVs
                      data$Big5_O_PosLexCountAvg +
                      data$Big5_C_PosLexCountAvg +
                      data$Big5_E_PosLexCountAvg +
                      data$Big5_A_PosLexCountAvg +
                      data$Big5_N_PosLexCountAvg ,
                    
                    # Data
                    data = data, family = binomial)
)

a1 <- summary(M9)
a2 <- round(a1$coefficients,3)
a3 <- car::vif(M9, type = 'predictor')
vif_range <- paste0(min(round(a3, 3)), " ~ ", max(round(a3, 3)))
summary_stats <- matrix(NA, nrow = 1, ncol = ncol(a2))
summary_stats[1, 3] <- paste0(min(round(a3,3)), " ~ ", max(round(a3,3)))
summary_stats[1, 4] <- round(a1$r.squared,3)
a2 <- rbind(a2, summary_stats)
write.csv(a2,"LR Sports (M9).csv")
bptest_result <- bptest(M9)
print(bptest_result)

# M10
summary(M10 <- glm(data$Sports..Health...Outdoors ~ 
                     
                     # IVs
                     data$Big5_O_PosLexCountAvg +
                     data$Big5_C_PosLexCountAvg +
                     data$Big5_E_PosLexCountAvg +
                     data$Big5_A_PosLexCountAvg +
                     data$Big5_N_PosLexCountAvg +
                     
                     # Controls
                     prod_review_count + review_count + title_count + also_buy + 
                     rank + also_view + imageURLHighRes_count, 
                   
                   # Data
                   data = data, family = binomial)
)

a1 <- summary(M10)
a2 <- round(a1$coefficients,3)
a3 <- car::vif(M10, type = 'predictor')
vif_range <- paste0(min(round(a3, 3)), " ~ ", max(round(a3, 3)))
summary_stats <- matrix(NA, nrow = 1, ncol = ncol(a2))
summary_stats[1, 3] <- paste0(min(round(a3,3)), " ~ ", max(round(a3,3)))
summary_stats[1, 4] <- round(a1$r.squared,3)
a2 <- rbind(a2, summary_stats)
write.csv(a2,"LR Sports (M10).csv")
bptest_result <- bptest(M10)
print(bptest_result)


# Arts
# M11
summary(M11 <- glm(data$Arts..Crafts..Entertainment...Toys ~ 
                      
                      # IVs
                      data$Big5_O_PosLexCountAvg +
                      data$Big5_C_PosLexCountAvg +
                      data$Big5_E_PosLexCountAvg +
                      data$Big5_A_PosLexCountAvg +
                      data$Big5_N_PosLexCountAvg ,
                    
                    # Data
                    data = data, family = binomial)
)

a1 <- summary(M11)
a2 <- round(a1$coefficients,3)
a3 <- car::vif(M11, type = 'predictor')
vif_range <- paste0(min(round(a3, 3)), " ~ ", max(round(a3, 3)))
summary_stats <- matrix(NA, nrow = 1, ncol = ncol(a2))
summary_stats[1, 3] <- paste0(min(round(a3,3)), " ~ ", max(round(a3,3)))
summary_stats[1, 4] <- round(a1$r.squared,3)
a2 <- rbind(a2, summary_stats)
write.csv(a2,"LR Art (M11).csv")
bptest_result <- bptest(M11)
print(bptest_result)

# M12
summary(M12 <- glm(data$Arts..Crafts..Entertainment...Toys ~ 
                     
                     # IVs
                     data$Big5_O_PosLexCountAvg +
                     data$Big5_C_PosLexCountAvg +
                     data$Big5_E_PosLexCountAvg +
                     data$Big5_A_PosLexCountAvg +
                     data$Big5_N_PosLexCountAvg +
                     
                     # Controls
                     prod_review_count + review_count + title_count + also_buy + 
                     rank + also_view + imageURLHighRes_count, 
                   
                   # Data
                   data = data, family = binomial)
)

a1 <- summary(M12)
a2 <- round(a1$coefficients,3)
a3 <- car::vif(M12, type = 'predictor')
vif_range <- paste0(min(round(a3, 3)), " ~ ", max(round(a3, 3)))
summary_stats <- matrix(NA, nrow = 1, ncol = ncol(a2))
summary_stats[1, 3] <- paste0(min(round(a3,3)), " ~ ", max(round(a3,3)))
summary_stats[1, 4] <- round(a1$r.squared,3)
a2 <- rbind(a2, summary_stats)
write.csv(a2,"LR Art (M12).csv")
bptest_result <- bptest(M12)
print(bptest_result)


# Books
# M13
summary(M13 <- glm(data$Books...Educational ~ 
                      
                      # IVs
                      data$Big5_O_PosLexCountAvg +
                      data$Big5_C_PosLexCountAvg +
                      data$Big5_E_PosLexCountAvg +
                      data$Big5_A_PosLexCountAvg +
                      data$Big5_N_PosLexCountAvg ,
                    
                    # Data
                    data = data, family = binomial)
)

a1 <- summary(M13)
a2 <- round(a1$coefficients,3)
a3 <- car::vif(M13, type = 'predictor')
vif_range <- paste0(min(round(a3, 3)), " ~ ", max(round(a3, 3)))
summary_stats <- matrix(NA, nrow = 1, ncol = ncol(a2))
summary_stats[1, 3] <- paste0(min(round(a3,3)), " ~ ", max(round(a3,3)))
summary_stats[1, 4] <- round(a1$r.squared,3)
a2 <- rbind(a2, summary_stats)
write.csv(a2,"LR Book (M13).csv")
bptest_result <- bptest(M13)
print(bptest_result)

# M14
summary(M14 <- glm(data$Books...Educational ~ 
                     
                     # IVs
                     data$Big5_O_PosLexCountAvg +
                     data$Big5_C_PosLexCountAvg +
                     data$Big5_E_PosLexCountAvg +
                     data$Big5_A_PosLexCountAvg +
                     data$Big5_N_PosLexCountAvg +
                     
                     # Controls
                     prod_review_count + review_count + title_count + also_buy + 
                     rank + also_view + imageURLHighRes_count, 
                   
                   # Data
                   data = data, family = binomial)
)

a1 <- summary(M14)
a2 <- round(a1$coefficients,3)
a3 <- car::vif(M14, type = 'predictor')
vif_range <- paste0(min(round(a3, 3)), " ~ ", max(round(a3, 3)))
summary_stats <- matrix(NA, nrow = 1, ncol = ncol(a2))
summary_stats[1, 3] <- paste0(min(round(a3,3)), " ~ ", max(round(a3,3)))
summary_stats[1, 4] <- round(a1$r.squared,3)
a2 <- rbind(a2, summary_stats)
write.csv(a2,"LR Book (M14).csv")
bptest_result <- bptest(M14)
print(bptest_result)
