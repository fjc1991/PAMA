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
############## HRM ################
###################################

# Fit the model using lmer from lmerTest
model1 <- lmer(Big5_O_PosLexCountAvg ~ 
                 Cognitive.Affordance +
                 Physical.Affordance +
                 Functional.Affordance +
                 Sensory.Affordance +
                 
                 # Controls
                 prod_review_count + review_count +
                 title_count + also_buy + rank + also_view +
                 imageURLHighRes_count +
                 
                 (1 | product_category), 
               data = data)

# Display the summary of the model which includes p-values
summary(model1)
model_summary <- summary(model1)$coefficients
model_summary_df1 <- as.data.frame(model_summary)
write.csv(model_summary_df1, "HRM_O.csv", row.names = TRUE)


# Fit the model using lmer from lmerTest
model2 <- lmer(Big5_C_PosLexCountAvg ~ 
                 Cognitive.Affordance +
                 Physical.Affordance +
                 Functional.Affordance +
                 Sensory.Affordance +
                 
                 # Controls
                 prod_review_count + review_count +
                 title_count + also_buy + rank + also_view +
                 imageURLHighRes_count +
                 
                 (1 | product_category), 
               data = data)

# Display the summary of the model which includes p-values
summary(model2)
model_summary1 <- summary(model2)$coefficients
model_summary_df2 <- as.data.frame(model_summary1)
write.csv(model_summary_df2, "HRM_C.csv", row.names = TRUE)

# Fit the model using lmer from lmerTest
model3 <- lmer(Big5_E_PosLexCountAvg ~ 
                 Cognitive.Affordance +
                 Physical.Affordance +
                 Functional.Affordance +
                 Sensory.Affordance +
                 
                 # Controls
                 prod_review_count + review_count +
                 title_count + also_buy + rank + also_view +
                 imageURLHighRes_count +
                 
                 (1 | product_category), 
               data = data)

# Display the summary of the model which includes p-values
summary(model3)
model_summary2 <- summary(model3)$coefficients
model_summary_df3 <- as.data.frame(model_summary2)
write.csv(model_summary_df3, "HRM_E.csv", row.names = TRUE)

# Fit the model using lmer from lmerTest
model4 <- lmer(Big5_A_PosLexCountAvg ~ 
                 Cognitive.Affordance +
                 Physical.Affordance +
                 Functional.Affordance +
                 Sensory.Affordance +
                 
                 # Controls
                 prod_review_count + review_count +
                 title_count + also_buy + rank + also_view +
                 imageURLHighRes_count +
                 
                 (1 | product_category), 
               data = data)

# Display the summary of the model which includes p-values
summary(model4)
model_summary3 <- summary(model4)$coefficients
model_summary_df4 <- as.data.frame(model_summary3)
write.csv(model_summary_df4, "HRM_A.csv", row.names = TRUE)

# Fit the model using lmer from lmerTest
model5 <- lmer(Big5_N_PosLexCountAvg ~ 
                 Cognitive.Affordance +
                 Physical.Affordance +
                 Functional.Affordance +
                 Sensory.Affordance +
                 
                 # Controls
                 prod_review_count + review_count +
                 title_count + also_buy + rank + also_view +
                 imageURLHighRes_count +
                 
                 (1 | product_category), 
               data = data)

# Display the summary of the model which includes p-values
summary(model5)
model_summary4 <- summary(model5)$coefficients
model_summary_df5 <- as.data.frame(model_summary4)
write.csv(model_summary_df5, "HRM_N.csv", row.names = TRUE)


###################################
###### Personality Regression######
###################################

# Openness

summary(M1.1 <-lm(Big5_O_PosNormCountAvg ~ 
                    
                    # IVs
                    Cognitive.Affordance + 
                    Physical.Affordance + 
                    Functional.Affordance +
                    Sensory.Affordance + 
                     
                    # Controls
                    prod_review_count + review_count +
                    title_count + also_buy + rank + also_view +
                    imageURLHighRes_count + average_rating,
                     
                     data=data))

# Conscientiousness

summary(M1.2 <-lm(Big5_C_PosNormCountAvg ~ 
                    
                    # IVs
                    Cognitive.Affordance + 
                    Physical.Affordance + 
                    Functional.Affordance +
                    Sensory.Affordance + 
                    
                    # Controls
                    prod_review_count + review_count +
                    title_count + also_buy + rank + also_view +
                    imageURLHighRes_count + average_rating,
                  
                  data=data))

# Extroversion

summary(M1.3 <-lm(Big5_E_PosNormCountAvg ~ 
                    
                    # IVs
                    Cognitive.Affordance + 
                    Physical.Affordance + 
                    Functional.Affordance +
                    Sensory.Affordance + 
                    
                    # Controls
                    prod_review_count + review_count +
                    title_count + also_buy + rank + also_view +
                    imageURLHighRes_count + average_rating,
                  
                  data=data))

# Agreeableness

summary(M1.4 <-lm(Big5_A_PosNormCountAvg ~ 
                    
                    # IVs
                    Cognitive.Affordance + 
                    Physical.Affordance + 
                    Functional.Affordance +
                    Sensory.Affordance + 
                    
                    # Controls
                    prod_review_count + review_count +
                    title_count + also_buy + rank + also_view +
                    imageURLHighRes_count + average_rating,
                  
                  data=data))

# Neuroticism

summary(M1.5 <-lm(Big5_N_PosNormCountAvg ~ 
                    
                    # IVs
                    Cognitive.Affordance + 
                    Physical.Affordance + 
                    Functional.Affordance +
                    Sensory.Affordance + 
                    
                    # Controls
                    prod_review_count + review_count +
                    title_count + also_buy + rank + also_view +
                    imageURLHighRes_count + average_rating,
                  
                  data=data))


###################################
######## Average Rating ###########
###################################


# Openness
summary(M3.1 <-lm(data$average_rating ~ 
                    
                    # IVs
                    Big5_O_PosNormCountAvg +
                     
                    Big5_O_PosNormCountAvg * Cognitive.Affordance + 
                    Big5_O_PosNormCountAvg * Physical.Affordance + 
                    Big5_O_PosNormCountAvg * Functional.Affordance +
                    Big5_O_PosNormCountAvg * Sensory.Affordance + 
                    
                    # Controls
                    prod_review_count + review_count +
                    title_count + also_buy + rank + also_view +
                    imageURLHighRes_count,
                  
                  data=data))

# Conscientiousness
summary(M3.2 <-lm(data$average_rating ~ 
                    
                    # IVs
                    Big5_C_PosNormCountAvg +
                    
                    Big5_C_PosNormCountAvg * Cognitive.Affordance + 
                    Big5_C_PosNormCountAvg * Physical.Affordance + 
                    Big5_C_PosNormCountAvg * Functional.Affordance +
                    Big5_C_PosNormCountAvg * Sensory.Affordance + 
                    
                    # Controls
                    prod_review_count + review_count +
                    title_count + also_buy + rank + also_view +
                    imageURLHighRes_count,
                  
                  data=data))

# Conscientiousness
summary(M3.3 <-lm(data$average_rating ~ 
                    
                    # IVs
                    Big5_E_PosNormCountAvg +
                    
                    Big5_E_PosNormCountAvg * Cognitive.Affordance + 
                    Big5_E_PosNormCountAvg * Physical.Affordance + 
                    Big5_E_PosNormCountAvg * Functional.Affordance +
                    Big5_E_PosNormCountAvg * Sensory.Affordance + 
                    
                    # Controls
                    prod_review_count + review_count +
                    title_count + also_buy + rank + also_view +
                    imageURLHighRes_count,
                  
                  data=data))

# Agreeableness
summary(M3.4 <-lm(data$average_rating ~ 
                    
                    # IVs
                    Big5_A_PosNormCountAvg +
                    
                    Big5_A_PosNormCountAvg * Cognitive.Affordance + 
                    Big5_A_PosNormCountAvg * Physical.Affordance + 
                    Big5_A_PosNormCountAvg * Functional.Affordance +
                    Big5_A_PosNormCountAvg * Sensory.Affordance + 
                    
                    # Controls
                    prod_review_count + review_count +
                    title_count + also_buy + rank + also_view +
                    imageURLHighRes_count,
                  
                  data=data))

# Neuroticism
summary(M3.5 <-lm(data$average_rating ~ 
                    
                    # IVs
                    Big5_N_PosNormCountAvg +
                    
                    Big5_N_PosNormCountAvg * Cognitive.Affordance + 
                    Big5_N_PosNormCountAvg * Physical.Affordance + 
                    Big5_N_PosNormCountAvg * Functional.Affordance +
                    Big5_N_PosNormCountAvg * Sensory.Affordance + 
                    
                    # Controls
                    prod_review_count + review_count +
                    title_count + also_buy + rank + also_view +
                    imageURLHighRes_count,
                  
                  data=data))

###################################
########## Helpfulness ############
###################################

# Openness
summary(M4.1 <-lm(data$weighted_helpfulness_score ~ 
                    
                    # IVs
                    Big5_O_PosNormCountAvg +
                    
                    Big5_O_PosNormCountAvg * Cognitive.Affordance + 
                    Big5_O_PosNormCountAvg * Physical.Affordance + 
                    Big5_O_PosNormCountAvg * Functional.Affordance +
                    Big5_O_PosNormCountAvg * Sensory.Affordance + 
                    
                    # Controls
                    prod_review_count + review_count +
                    title_count + also_buy + rank + also_view +
                    imageURLHighRes_count,
                  
                  data=data))

# Conscientiousness
summary(M4.2 <-lm(data$weighted_helpfulness_score ~ 
                    
                    # IVs
                    Big5_C_PosNormCountAvg +
                    
                    Big5_C_PosNormCountAvg * Cognitive.Affordance + 
                    Big5_C_PosNormCountAvg * Physical.Affordance + 
                    Big5_C_PosNormCountAvg * Functional.Affordance +
                    Big5_C_PosNormCountAvg * Sensory.Affordance + 
                    
                    # Controls
                    prod_review_count + review_count +
                    title_count + also_buy + rank + also_view +
                    imageURLHighRes_count,
                  
                  data=data))

# Conscientiousness
summary(M4.3 <-lm(data$weighted_helpfulness_score ~ 
                    
                    # IVs
                    Big5_E_PosNormCountAvg +
                    
                    Big5_E_PosNormCountAvg * Cognitive.Affordance + 
                    Big5_E_PosNormCountAvg * Physical.Affordance + 
                    Big5_E_PosNormCountAvg * Functional.Affordance +
                    Big5_E_PosNormCountAvg * Sensory.Affordance + 
                    
                    # Controls
                    prod_review_count + review_count +
                    title_count + also_buy + rank + also_view +
                    imageURLHighRes_count,
                  
                  data=data))

# Agreeableness
summary(M4.4 <-lm(data$weighted_helpfulness_score ~ 
                    
                    # IVs
                    Big5_A_PosNormCountAvg +
                    
                    Big5_A_PosNormCountAvg * Cognitive.Affordance + 
                    Big5_A_PosNormCountAvg * Physical.Affordance + 
                    Big5_A_PosNormCountAvg * Functional.Affordance +
                    Big5_A_PosNormCountAvg * Sensory.Affordance + 
                    
                    # Controls
                    prod_review_count + review_count +
                    title_count + also_buy + rank + also_view +
                    imageURLHighRes_count,
                  
                  data=data))

# Neuroticism
summary(M4.5 <-lm(data$weighted_helpfulness_score ~ 
                    
                    # IVs
                    Big5_N_PosNormCountAvg +
                    
                    Big5_N_PosNormCountAvg * Cognitive.Affordance + 
                    Big5_N_PosNormCountAvg * Physical.Affordance + 
                    Big5_N_PosNormCountAvg * Functional.Affordance +
                    Big5_N_PosNormCountAvg * Sensory.Affordance + 
                    
                    # Controls
                    prod_review_count + review_count +
                    title_count + also_buy + rank + also_view +
                    imageURLHighRes_count,
                  
                  data=data))

###################################
###################################

# a1<-summary(M1.1)
# a2<-round(a1$coefficients,3)
# a3<-car::vif(M1.1)
# #b3 <- b3[1:(length(b3) - 22)]
# a2[a2[,4] < 0.1, 3] <- paste0(a2[a2[,4] < 0.05, 3],"*")
# a2[a2[,4] < 0.01,3] <- paste0(a2[a2[,4] < 0.01,3],"*")
# a2[a2[,4] < 0.001,3] <- paste0(a2[a2[,4] < 0.001,3],"*")
# #b2 <- b2[1:(nrow(b2) - 22),c(1,3)]
# a2<-rbind(a2,
#           VIF_range = c(paste0(min(round(a3,3))," ~ ",max(round(a3,3))),""),
#           R_square = c(round(a1$r.squared,3),""),
#           Adj_R_square = c(round(a1$adj.r.squared,3),""),
#           F_value = c(paste0(round(a1$fstatistic[1],3),"***"),""))
# write.csv(a2,"AFF OLS (COG M1.1).csv")


# a1 <- summary(M1.2)
# a2 <- round(a1$coefficients,3)
# a3 <- car::vif(M1.1, type = 'predictor')
# vif_range <- paste0(min(round(a3, 3)), " ~ ", max(round(a3, 3)))
# summary_stats <- matrix(NA, nrow = 1, ncol = ncol(a2))
# summary_stats[1, 3] <- paste0(min(round(a3,3)), " ~ ", max(round(a3,3)))
# summary_stats[1, 4] <- round(a1$r.squared,3)
# a2 <- rbind(a2, summary_stats)
# write.csv(a2,"AFF OLS (COG M1.2).csv")
