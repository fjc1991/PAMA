library(car)
library(MASS)

data <- read.csv(file.choose())

names(data)

###################################
########## Helpfulness ############
########## Interaction ############
###################################

# Interaction Analysis
M1.1 <-lm(weighted_helpfulness_score ~ 
            
            # IVs
            data$Big5_O_PosLexCountAvg +
            
            data$Cognitive_Affordance +
            data$Physical_Affordance +
            data$Functional_Affordance +
            data$Sensory_Affordance + 
            
            data$Big5_O_PosLexCountAvg_x_Cognitive_Affordance + 
            data$Big5_O_PosLexCountAvg_x_Physical_Affordance +
            data$Big5_O_PosLexCountAvg_x_Functional_Affordance +
            data$Big5_O_PosLexCountAvg_x_Sensory_Affordance +
            
            # Controls
            prod_review_count + review_count +
            title_count + also_buy + rank + also_view +
            imageURLHighRes_count,
          
          data=data)

a1<-summary(M1.1)
a2<-round(a1$coefficients,3)
a3<-car::vif(M1.1)
a2[a2[,4] < 0.05, 3] <- paste0(a2[a2[,4] < 0.05, 3],"*")
a2[a2[,4] < 0.01,3] <- paste0(a2[a2[,4] < 0.01,3],"*")
a2[a2[,4] < 0.001,3] <- paste0(a2[a2[,4] < 0.001,3],"*")
#b2 <- b2[1:(nrow(b2) - 22),c(1,3)]
a2<-rbind(a2,
          VIF_range = c(paste0(min(round(a3,3))," ~ ",max(round(a3,3))),""),
          R_square = c(round(a1$r.squared,3),""),
          Adj_R_square = c(round(a1$adj.r.squared,3),""),
          F_value = c(paste0(round(a1$fstatistic[1],3),"***"),""))
write.csv(a2,"ASSIM Help (M1.1).csv")

# Interaction Analysis
M1.2 <-lm(weighted_helpfulness_score ~ 
            
            # IVs
            data$Big5_C_PosLexCountAvg +
            
            data$Cognitive_Affordance +
            data$Physical_Affordance +
            data$Functional_Affordance +
            data$Sensory_Affordance + 
            
            data$Big5_C_PosLexCountAvg_x_Cognitive_Affordance + 
            data$Big5_C_PosLexCountAvg_x_Physical_Affordance +
            data$Big5_C_PosLexCountAvg_x_Functional_Affordance +
            data$Big5_C_PosLexCountAvg_x_Sensory_Affordance +
            
            # Controls
            prod_review_count + review_count +
            title_count + also_buy + rank + also_view +
            imageURLHighRes_count,
          
          data=data)

a1<-summary(M1.2)
a2<-round(a1$coefficients,3)
a3<-car::vif(M1.2)
a2[a2[,4] < 0.05, 3] <- paste0(a2[a2[,4] < 0.05, 3],"*")
a2[a2[,4] < 0.01,3] <- paste0(a2[a2[,4] < 0.01,3],"*")
a2[a2[,4] < 0.001,3] <- paste0(a2[a2[,4] < 0.001,3],"*")
#b2 <- b2[1:(nrow(b2) - 22),c(1,3)]
a2<-rbind(a2,
          VIF_range = c(paste0(min(round(a3,3))," ~ ",max(round(a3,3))),""),
          R_square = c(round(a1$r.squared,3),""),
          Adj_R_square = c(round(a1$adj.r.squared,3),""),
          F_value = c(paste0(round(a1$fstatistic[1],3),"***"),""))
write.csv(a2,"ASSIM Help (M1.2).csv")

# Interaction Analysis
summary(M1.3 <-lm(weighted_helpfulness_score ~ 
                    
                    # IVs
                    data$Big5_E_PosLexCountAvg +
                    
                    data$Cognitive_Affordance +
                    data$Physical_Affordance +
                    data$Functional_Affordance +
                    data$Sensory_Affordance + 
                    
                    data$Big5_E_PosLexCountAvg_x_Cognitive_Affordance + 
                    data$Big5_E_PosLexCountAvg_x_Physical_Affordance +
                    data$Big5_E_PosLexCountAvg_x_Functional_Affordance +
                    data$Big5_E_PosLexCountAvg_x_Sensory_Affordance +
                    
                    # Controls
                    prod_review_count + review_count +
                    title_count + also_buy + rank + also_view +
                    imageURLHighRes_count,
                  
                  data=data))

a1<-summary(M1.3)
a2<-round(a1$coefficients,3)
a3<-car::vif(M1.3)
a2[a2[,4] < 0.05, 3] <- paste0(a2[a2[,4] < 0.05, 3],"*")
a2[a2[,4] < 0.01,3] <- paste0(a2[a2[,4] < 0.01,3],"*")
a2[a2[,4] < 0.001,3] <- paste0(a2[a2[,4] < 0.001,3],"*")
#b2 <- b2[1:(nrow(b2) - 22),c(1,3)]
a2<-rbind(a2,
          VIF_range = c(paste0(min(round(a3,3))," ~ ",max(round(a3,3))),""),
          R_square = c(round(a1$r.squared,3),""),
          Adj_R_square = c(round(a1$adj.r.squared,3),""),
          F_value = c(paste0(round(a1$fstatistic[1],3),"***"),""))
write.csv(a2,"ASSIM Help (M1.3).csv")

# Interaction Analysis
summary(M1.4 <-lm(weighted_helpfulness_score ~ 
                    
                    # IVs
                    data$Big5_A_PosLexCountAvg +
                    
                    data$Cognitive_Affordance +
                    data$Physical_Affordance +
                    data$Functional_Affordance +
                    data$Sensory_Affordance + 
                    
                    data$Big5_A_PosLexCountAvg_x_Cognitive_Affordance + 
                    data$Big5_A_PosLexCountAvg_x_Physical_Affordance +
                    data$Big5_A_PosLexCountAvg_x_Functional_Affordance +
                    data$Big5_A_PosLexCountAvg_x_Sensory_Affordance +
                    
                    # Controls
                    prod_review_count + review_count +
                    title_count + also_buy + rank + also_view +
                    imageURLHighRes_count,
                  
                  data=data))

a1<-summary(M1.4)
a2<-round(a1$coefficients,3)
a3<-car::vif(M1.4)
a2[a2[,4] < 0.05, 3] <- paste0(a2[a2[,4] < 0.05, 3],"*")
a2[a2[,4] < 0.01,3] <- paste0(a2[a2[,4] < 0.01,3],"*")
a2[a2[,4] < 0.001,3] <- paste0(a2[a2[,4] < 0.001,3],"*")
#b2 <- b2[1:(nrow(b2) - 22),c(1,3)]
a2<-rbind(a2,
          VIF_range = c(paste0(min(round(a3,3))," ~ ",max(round(a3,3))),""),
          R_square = c(round(a1$r.squared,3),""),
          Adj_R_square = c(round(a1$adj.r.squared,3),""),
          F_value = c(paste0(round(a1$fstatistic[1],3),"***"),""))
write.csv(a2,"ASSIM Help (M1.4).csv")

# Interaction Analysis
summary(M1.5 <-lm(weighted_helpfulness_score ~ 
                    
                    # IVs
                    data$Big5_N_PosLexCountAvg +
                    
                    data$Cognitive_Affordance +
                    data$Physical_Affordance +
                    data$Functional_Affordance +
                    data$Sensory_Affordance + 
                    
                    data$Big5_N_PosLexCountAvg_x_Cognitive_Affordance + 
                    data$Big5_N_PosLexCountAvg_x_Physical_Affordance +
                    data$Big5_N_PosLexCountAvg_x_Functional_Affordance +
                    data$Big5_N_PosLexCountAvg_x_Sensory_Affordance +
                    
                    # Controls
                    prod_review_count + review_count +
                    title_count + also_buy + rank + also_view +
                    imageURLHighRes_count,
                  
                  data=data))

a1<-summary(M1.5)
a2<-round(a1$coefficients,3)
a3<-car::vif(M1.5)
a2[a2[,4] < 0.05, 3] <- paste0(a2[a2[,4] < 0.05, 3],"*")
a2[a2[,4] < 0.01,3] <- paste0(a2[a2[,4] < 0.01,3],"*")
a2[a2[,4] < 0.001,3] <- paste0(a2[a2[,4] < 0.001,3],"*")
#b2 <- b2[1:(nrow(b2) - 22),c(1,3)]
a2<-rbind(a2,
          VIF_range = c(paste0(min(round(a3,3))," ~ ",max(round(a3,3))),""),
          R_square = c(round(a1$r.squared,3),""),
          Adj_R_square = c(round(a1$adj.r.squared,3),""),
          F_value = c(paste0(round(a1$fstatistic[1],3),"***"),""))
write.csv(a2,"ASSIM Help (M1.5).csv")

###################################
########## Avg. Rating ############
########## Interaction ############
###################################

# Interaction Analysis
summary(M2.1 <-lm(average_rating ~ 
                    
                    # IVs
                    data$Big5_O_PosLexCountAvg +
                    
                    data$Cognitive_Affordance +
                    data$Physical_Affordance +
                    data$Functional_Affordance +
                    data$Sensory_Affordance + 
                    
                    data$Big5_O_PosLexCountAvg_x_Cognitive_Affordance + 
                    data$Big5_O_PosLexCountAvg_x_Physical_Affordance +
                    data$Big5_O_PosLexCountAvg_x_Functional_Affordance +
                    data$Big5_O_PosLexCountAvg_x_Sensory_Affordance +
                    
                    # Controls
                    prod_review_count + review_count +
                    title_count + also_buy + rank + also_view +
                    imageURLHighRes_count,
                  
                  data=data))

a1<-summary(M2.1)
a2<-round(a1$coefficients,3)
a3<-car::vif(M2.1)
a2[a2[,4] < 0.05, 3] <- paste0(a2[a2[,4] < 0.05, 3],"*")
a2[a2[,4] < 0.01,3] <- paste0(a2[a2[,4] < 0.01,3],"*")
a2[a2[,4] < 0.001,3] <- paste0(a2[a2[,4] < 0.001,3],"*")
#b2 <- b2[1:(nrow(b2) - 22),c(1,3)]
a2<-rbind(a2,
          VIF_range = c(paste0(min(round(a3,3))," ~ ",max(round(a3,3))),""),
          R_square = c(round(a1$r.squared,3),""),
          Adj_R_square = c(round(a1$adj.r.squared,3),""),
          F_value = c(paste0(round(a1$fstatistic[1],3),"***"),""))
write.csv(a2,"ASSIM Rate (M2.1).csv")

# Interaction Analysis
summary(M2.2 <-lm(average_rating ~ 
                    
                    # IVs
                    data$Big5_C_PosLexCountAvg +
                    
                    data$Cognitive_Affordance +
                    data$Physical_Affordance +
                    data$Functional_Affordance +
                    data$Sensory_Affordance + 
                    
                    data$Big5_C_PosLexCountAvg_x_Cognitive_Affordance + 
                    data$Big5_C_PosLexCountAvg_x_Physical_Affordance +
                    data$Big5_C_PosLexCountAvg_x_Functional_Affordance +
                    data$Big5_C_PosLexCountAvg_x_Sensory_Affordance +
                    
                    # Controls
                    prod_review_count + review_count +
                    title_count + also_buy + rank + also_view +
                    imageURLHighRes_count,
                  
                  data=data))

a1<-summary(M2.2)
a2<-round(a1$coefficients,3)
a3<-car::vif(M2.2)
a2[a2[,4] < 0.05, 3] <- paste0(a2[a2[,4] < 0.05, 3],"*")
a2[a2[,4] < 0.01,3] <- paste0(a2[a2[,4] < 0.01,3],"*")
a2[a2[,4] < 0.001,3] <- paste0(a2[a2[,4] < 0.001,3],"*")
#b2 <- b2[1:(nrow(b2) - 22),c(1,3)]
a2<-rbind(a2,
          VIF_range = c(paste0(min(round(a3,3))," ~ ",max(round(a3,3))),""),
          R_square = c(round(a1$r.squared,3),""),
          Adj_R_square = c(round(a1$adj.r.squared,3),""),
          F_value = c(paste0(round(a1$fstatistic[1],3),"***"),""))
write.csv(a2,"ASSIM Rate (M2.2).csv")

# Interaction Analysis
summary(M2.3 <-lm(average_rating ~ 
                    
                    # IVs
                    data$Big5_E_PosLexCountAvg +
                    
                    data$Cognitive_Affordance +
                    data$Physical_Affordance +
                    data$Functional_Affordance +
                    data$Sensory_Affordance + 
                    
                    data$Big5_E_PosLexCountAvg_x_Cognitive_Affordance + 
                    data$Big5_E_PosLexCountAvg_x_Physical_Affordance +
                    data$Big5_E_PosLexCountAvg_x_Functional_Affordance +
                    data$Big5_E_PosLexCountAvg_x_Sensory_Affordance +
                    
                    # Controls
                    prod_review_count + review_count +
                    title_count + also_buy + rank + also_view +
                    imageURLHighRes_count,
                  
                  data=data))

a1<-summary(M2.3)
a2<-round(a1$coefficients,3)
a3<-car::vif(M2.3)
a2[a2[,4] < 0.05, 3] <- paste0(a2[a2[,4] < 0.05, 3],"*")
a2[a2[,4] < 0.01,3] <- paste0(a2[a2[,4] < 0.01,3],"*")
a2[a2[,4] < 0.001,3] <- paste0(a2[a2[,4] < 0.001,3],"*")
#b2 <- b2[1:(nrow(b2) - 22),c(1,3)]
a2<-rbind(a2,
          VIF_range = c(paste0(min(round(a3,3))," ~ ",max(round(a3,3))),""),
          R_square = c(round(a1$r.squared,3),""),
          Adj_R_square = c(round(a1$adj.r.squared,3),""),
          F_value = c(paste0(round(a1$fstatistic[1],3),"***"),""))
write.csv(a2,"ASSIM Rate (M2.3).csv")

# Interaction Analysis
summary(M2.4 <-lm(average_rating ~ 
                    
                    # IVs
                    data$Big5_A_PosLexCountAvg +
                    
                    data$Cognitive_Affordance +
                    data$Physical_Affordance +
                    data$Functional_Affordance +
                    data$Sensory_Affordance + 
                    
                    data$Big5_A_PosLexCountAvg_x_Cognitive_Affordance + 
                    data$Big5_A_PosLexCountAvg_x_Physical_Affordance +
                    data$Big5_A_PosLexCountAvg_x_Functional_Affordance +
                    data$Big5_A_PosLexCountAvg_x_Sensory_Affordance +
                    
                    # Controls
                    prod_review_count + review_count +
                    title_count + also_buy + rank + also_view +
                    imageURLHighRes_count,
                  
                  data=data))

a1<-summary(M2.4)
a2<-round(a1$coefficients,3)
a3<-car::vif(M2.4)
a2[a2[,4] < 0.05, 3] <- paste0(a2[a2[,4] < 0.05, 3],"*")
a2[a2[,4] < 0.01,3] <- paste0(a2[a2[,4] < 0.01,3],"*")
a2[a2[,4] < 0.001,3] <- paste0(a2[a2[,4] < 0.001,3],"*")
#b2 <- b2[1:(nrow(b2) - 22),c(1,3)]
a2<-rbind(a2,
          VIF_range = c(paste0(min(round(a3,3))," ~ ",max(round(a3,3))),""),
          R_square = c(round(a1$r.squared,3),""),
          Adj_R_square = c(round(a1$adj.r.squared,3),""),
          F_value = c(paste0(round(a1$fstatistic[1],3),"***"),""))
write.csv(a2,"ASSIM Rate (M2.4).csv")

# Interaction Analysis
summary(M2.5 <-lm(average_rating ~ 
                    
                    # IVs
                    data$Big5_N_PosLexCountAvg +
                    
                    data$Cognitive_Affordance +
                    data$Physical_Affordance +
                    data$Functional_Affordance +
                    data$Sensory_Affordance + 
                    
                    data$Big5_N_PosLexCountAvg_x_Cognitive_Affordance + 
                    data$Big5_N_PosLexCountAvg_x_Physical_Affordance +
                    data$Big5_N_PosLexCountAvg_x_Functional_Affordance +
                    data$Big5_N_PosLexCountAvg_x_Sensory_Affordance +
                    
                    # Controls
                    prod_review_count + review_count +
                    title_count + also_buy + rank + also_view +
                    imageURLHighRes_count,
                  
                  data=data))

a1<-summary(M2.5)
a2<-round(a1$coefficients,3)
a3<-car::vif(M2.5)
a2[a2[,4] < 0.05, 3] <- paste0(a2[a2[,4] < 0.05, 3],"*")
a2[a2[,4] < 0.01,3] <- paste0(a2[a2[,4] < 0.01,3],"*")
a2[a2[,4] < 0.001,3] <- paste0(a2[a2[,4] < 0.001,3],"*")
#b2 <- b2[1:(nrow(b2) - 22),c(1,3)]
a2<-rbind(a2,
          VIF_range = c(paste0(min(round(a3,3))," ~ ",max(round(a3,3))),""),
          R_square = c(round(a1$r.squared,3),""),
          Adj_R_square = c(round(a1$adj.r.squared,3),""),
          F_value = c(paste0(round(a1$fstatistic[1],3),"***"),""))
write.csv(a2,"ASSIM Rate (M2.5).csv")

###################################
########## Comb.Rating ############
########## Interaction ############
###################################

# Interaction Analysis
summary(M3.1 <-lm(combined_metric ~ 
                    
                    # IVs
                    data$Big5_O_PosLexCountAvg +
                    
                    data$Cognitive_Affordance +
                    data$Physical_Affordance +
                    data$Functional_Affordance +
                    data$Sensory_Affordance + 
                    
                    data$Big5_O_PosLexCountAvg_x_Cognitive_Affordance + 
                    data$Big5_O_PosLexCountAvg_x_Physical_Affordance +
                    data$Big5_O_PosLexCountAvg_x_Functional_Affordance +
                    data$Big5_O_PosLexCountAvg_x_Sensory_Affordance +
                    
                    # Controls
                    prod_review_count + review_count +
                    title_count + also_buy + rank + also_view +
                    imageURLHighRes_count,
                  
                  data=data))

a1<-summary(M3.1)
a2<-round(a1$coefficients,3)
a3<-car::vif(M3.1)
a2[a2[,4] < 0.05, 3] <- paste0(a2[a2[,4] < 0.05, 3],"*")
a2[a2[,4] < 0.01,3] <- paste0(a2[a2[,4] < 0.01,3],"*")
a2[a2[,4] < 0.001,3] <- paste0(a2[a2[,4] < 0.001,3],"*")
#b2 <- b2[1:(nrow(b2) - 22),c(1,3)]
a2<-rbind(a2,
          VIF_range = c(paste0(min(round(a3,3))," ~ ",max(round(a3,3))),""),
          R_square = c(round(a1$r.squared,3),""),
          Adj_R_square = c(round(a1$adj.r.squared,3),""),
          F_value = c(paste0(round(a1$fstatistic[1],3),"***"),""))
write.csv(a2,"ASSIM Comb (M3.1).csv")

# Interaction Analysis
summary(M3.2 <-lm(combined_metric ~ 
                    
                    # IVs
                    data$Big5_C_PosLexCountAvg +
                    
                    data$Cognitive_Affordance +
                    data$Physical_Affordance +
                    data$Functional_Affordance +
                    data$Sensory_Affordance + 
                    
                    data$Big5_C_PosLexCountAvg_x_Cognitive_Affordance + 
                    data$Big5_C_PosLexCountAvg_x_Physical_Affordance +
                    data$Big5_C_PosLexCountAvg_x_Functional_Affordance +
                    data$Big5_C_PosLexCountAvg_x_Sensory_Affordance +
                    
                    # Controls
                    prod_review_count + review_count +
                    title_count + also_buy + rank + also_view +
                    imageURLHighRes_count,
                  
                  data=data))

a1<-summary(M3.2)
a2<-round(a1$coefficients,3)
a3<-car::vif(M3.2)
a2[a2[,4] < 0.05, 3] <- paste0(a2[a2[,4] < 0.05, 3],"*")
a2[a2[,4] < 0.01,3] <- paste0(a2[a2[,4] < 0.01,3],"*")
a2[a2[,4] < 0.001,3] <- paste0(a2[a2[,4] < 0.001,3],"*")
#b2 <- b2[1:(nrow(b2) - 22),c(1,3)]
a2<-rbind(a2,
          VIF_range = c(paste0(min(round(a3,3))," ~ ",max(round(a3,3))),""),
          R_square = c(round(a1$r.squared,3),""),
          Adj_R_square = c(round(a1$adj.r.squared,3),""),
          F_value = c(paste0(round(a1$fstatistic[1],3),"***"),""))
write.csv(a2,"ASSIM Comb (M3.2).csv")

# Interaction Analysis
summary(M3.3 <-lm(combined_metric ~ 
                    
                    # IVs
                    data$Big5_E_PosLexCountAvg +
                    
                    data$Cognitive_Affordance +
                    data$Physical_Affordance +
                    data$Functional_Affordance +
                    data$Sensory_Affordance + 
                    
                    data$Big5_E_PosLexCountAvg_x_Cognitive_Affordance + 
                    data$Big5_E_PosLexCountAvg_x_Physical_Affordance +
                    data$Big5_E_PosLexCountAvg_x_Functional_Affordance +
                    data$Big5_E_PosLexCountAvg_x_Sensory_Affordance +
                    
                    # Controls
                    prod_review_count + review_count +
                    title_count + also_buy + rank + also_view +
                    imageURLHighRes_count,
                  
                  data=data))

a1<-summary(M3.3)
a2<-round(a1$coefficients,3)
a3<-car::vif(M3.3)
a2[a2[,4] < 0.05, 3] <- paste0(a2[a2[,4] < 0.05, 3],"*")
a2[a2[,4] < 0.01,3] <- paste0(a2[a2[,4] < 0.01,3],"*")
a2[a2[,4] < 0.001,3] <- paste0(a2[a2[,4] < 0.001,3],"*")
#b2 <- b2[1:(nrow(b2) - 22),c(1,3)]
a2<-rbind(a2,
          VIF_range = c(paste0(min(round(a3,3))," ~ ",max(round(a3,3))),""),
          R_square = c(round(a1$r.squared,3),""),
          Adj_R_square = c(round(a1$adj.r.squared,3),""),
          F_value = c(paste0(round(a1$fstatistic[1],3),"***"),""))
write.csv(a2,"ASSIM Comb (M3.3).csv")

# Interaction Analysis
summary(M3.4 <-lm(combined_metric ~ 
                    
                    # IVs
                    data$Big5_A_PosLexCountAvg +
                    
                    data$Cognitive_Affordance +
                    data$Physical_Affordance +
                    data$Functional_Affordance +
                    data$Sensory_Affordance + 
                    
                    data$Big5_A_PosLexCountAvg_x_Cognitive_Affordance + 
                    data$Big5_A_PosLexCountAvg_x_Physical_Affordance +
                    data$Big5_A_PosLexCountAvg_x_Functional_Affordance +
                    data$Big5_A_PosLexCountAvg_x_Sensory_Affordance +
                    
                    # Controls
                    prod_review_count + review_count +
                    title_count + also_buy + rank + also_view +
                    imageURLHighRes_count,
                  
                  data=data))

a1<-summary(M3.4)
a2<-round(a1$coefficients,3)
a3<-car::vif(M3.4)
a2[a2[,4] < 0.05, 3] <- paste0(a2[a2[,4] < 0.05, 3],"*")
a2[a2[,4] < 0.01,3] <- paste0(a2[a2[,4] < 0.01,3],"*")
a2[a2[,4] < 0.001,3] <- paste0(a2[a2[,4] < 0.001,3],"*")
#b2 <- b2[1:(nrow(b2) - 22),c(1,3)]
a2<-rbind(a2,
          VIF_range = c(paste0(min(round(a3,3))," ~ ",max(round(a3,3))),""),
          R_square = c(round(a1$r.squared,3),""),
          Adj_R_square = c(round(a1$adj.r.squared,3),""),
          F_value = c(paste0(round(a1$fstatistic[1],3),"***"),""))
write.csv(a2,"ASSIM Comb (M3.4).csv")

# Interaction Analysis
summary(M3.5 <-lm(combined_metric ~ 
                    
                    # IVs
                    data$Big5_N_PosLexCountAvg +
                    
                    data$Cognitive_Affordance +
                    data$Physical_Affordance +
                    data$Functional_Affordance +
                    data$Sensory_Affordance + 
                    
                    data$Big5_N_PosLexCountAvg_x_Cognitive_Affordance + 
                    data$Big5_N_PosLexCountAvg_x_Physical_Affordance +
                    data$Big5_N_PosLexCountAvg_x_Functional_Affordance +
                    data$Big5_N_PosLexCountAvg_x_Sensory_Affordance +
                    
                    # Controls
                    prod_review_count + review_count +
                    title_count + also_buy + rank + also_view +
                    imageURLHighRes_count,
                  
                  data=data))

a1<-summary(M3.5)
a2<-round(a1$coefficients,3)
a3<-car::vif(M3.5)
a2[a2[,4] < 0.05, 3] <- paste0(a2[a2[,4] < 0.05, 3],"*")
a2[a2[,4] < 0.01,3] <- paste0(a2[a2[,4] < 0.01,3],"*")
a2[a2[,4] < 0.001,3] <- paste0(a2[a2[,4] < 0.001,3],"*")
#b2 <- b2[1:(nrow(b2) - 22),c(1,3)]
a2<-rbind(a2,
          VIF_range = c(paste0(min(round(a3,3))," ~ ",max(round(a3,3))),""),
          R_square = c(round(a1$r.squared,3),""),
          Adj_R_square = c(round(a1$adj.r.squared,3),""),
          F_value = c(paste0(round(a1$fstatistic[1],3),"***"),""))
write.csv(a2,"ASSIM Comb (M3.5).csv")

###################################
########## Helpfulness ############
############# Ratio ###############
###################################

# Ratio Analysis
summary(M4.1 <-lm(weighted_helpfulness_score ~ 
                    
                    # IVs
                    data$Big5_O_PosLexCountAvg +
                    
                    data$Cognitive_Affordance +
                    data$Physical_Affordance +
                    data$Functional_Affordance +
                    data$Sensory_Affordance + 
     
                    
                    data$Ratio_Big5_O_PosLexCountAvg_to_Cognitive_Affordance +
                    data$Ratio_Big5_O_PosLexCountAvg_to_Physical_Affordance +
                    data$Ratio_Big5_O_PosLexCountAvg_to_Functional_Affordance +
                    data$Ratio_Big5_O_PosLexCountAvg_to_Sensory_Affordance +
                    
                    # Controls
                    prod_review_count + review_count +
                    title_count + also_buy + rank + also_view +
                    imageURLHighRes_count,
                  
                  data=data))

# Ratio Analysis
summary(M4.2 <-lm(weighted_helpfulness_score ~ 
                    
                    # IVs
                    data$Big5_C_PosLexCountAvg +
                    
                    data$Cognitive_Affordance +
                    data$Physical_Affordance +
                    data$Functional_Affordance +
                    data$Sensory_Affordance + 
                    
                    
                    data$Ratio_Big5_C_PosLexCountAvg_to_Cognitive_Affordance +
                    data$Ratio_Big5_C_PosLexCountAvg_to_Physical_Affordance +
                    data$Ratio_Big5_C_PosLexCountAvg_to_Functional_Affordance +
                    data$Ratio_Big5_C_PosLexCountAvg_to_Sensory_Affordance +
                    
                    # Controls
                    prod_review_count + review_count +
                    title_count + also_buy + rank + also_view +
                    imageURLHighRes_count,
                  
                  data=data))

# Ratio Analysis
summary(M4.3 <-lm(weighted_helpfulness_score ~ 
                    
                    # IVs
                    data$Big5_E_PosLexCountAvg +
                    
                    data$Cognitive_Affordance +
                    data$Physical_Affordance +
                    data$Functional_Affordance +
                    data$Sensory_Affordance + 
                    
                    
                    data$Ratio_Big5_E_PosLexCountAvg_to_Cognitive_Affordance +
                    data$Ratio_Big5_E_PosLexCountAvg_to_Physical_Affordance +
                    data$Ratio_Big5_E_PosLexCountAvg_to_Functional_Affordance +
                    data$Ratio_Big5_E_PosLexCountAvg_to_Sensory_Affordance +
                    
                    # Controls
                    prod_review_count + review_count +
                    title_count + also_buy + rank + also_view +
                    imageURLHighRes_count,
                  
                  data=data))

# Ratio Analysis
summary(M4.4 <-lm(weighted_helpfulness_score ~ 
                    
                    # IVs
                    data$Big5_A_PosLexCountAvg +
                    
                    data$Cognitive_Affordance +
                    data$Physical_Affordance +
                    data$Functional_Affordance +
                    data$Sensory_Affordance + 
                    
                    
                    data$Ratio_Big5_A_PosLexCountAvg_to_Cognitive_Affordance +
                    data$Ratio_Big5_A_PosLexCountAvg_to_Physical_Affordance +
                    data$Ratio_Big5_A_PosLexCountAvg_to_Functional_Affordance +
                    data$Ratio_Big5_A_PosLexCountAvg_to_Sensory_Affordance +
                    
                    # Controls
                    prod_review_count + review_count +
                    title_count + also_buy + rank + also_view +
                    imageURLHighRes_count,
                  
                  data=data))

# Ratio Analysis
summary(M4.5 <-lm(weighted_helpfulness_score ~ 
                    
                    # IVs
                    data$Big5_N_PosLexCountAvg +
                    
                    data$Cognitive_Affordance +
                    data$Physical_Affordance +
                    data$Functional_Affordance +
                    data$Sensory_Affordance + 
                    
                    
                    data$Ratio_Big5_N_PosLexCountAvg_to_Cognitive_Affordance +
                    data$Ratio_Big5_N_PosLexCountAvg_to_Physical_Affordance +
                    data$Ratio_Big5_N_PosLexCountAvg_to_Functional_Affordance +
                    data$Ratio_Big5_N_PosLexCountAvg_to_Sensory_Affordance +
                    
                    # Controls
                    prod_review_count + review_count +
                    title_count + also_buy + rank + also_view +
                    imageURLHighRes_count,
                  
                  data=data))

###################################
########## Avg. Rating ############
############# Ratio ###############
###################################

# Ratio Analysis
summary(M5.1 <-lm(average_rating ~ 
                    
                    # IVs
                    data$Big5_O_PosLexCountAvg +
                    
                    data$Cognitive_Affordance +
                    data$Physical_Affordance +
                    data$Functional_Affordance +
                    data$Sensory_Affordance + 
                    
                    
                    data$Ratio_Big5_O_PosLexCountAvg_to_Cognitive_Affordance +
                    data$Ratio_Big5_O_PosLexCountAvg_to_Physical_Affordance +
                    data$Ratio_Big5_O_PosLexCountAvg_to_Functional_Affordance +
                    data$Ratio_Big5_O_PosLexCountAvg_to_Sensory_Affordance +
                    
                    # Controls
                    prod_review_count + review_count +
                    title_count + also_buy + rank + also_view +
                    imageURLHighRes_count,
                  
                  data=data))

# Ratio Analysis
summary(M5.2 <-lm(average_rating ~ 
                    
                    # IVs
                    data$Big5_C_PosLexCountAvg +
                    
                    data$Cognitive_Affordance +
                    data$Physical_Affordance +
                    data$Functional_Affordance +
                    data$Sensory_Affordance + 
                    
                    
                    data$Ratio_Big5_C_PosLexCountAvg_to_Cognitive_Affordance +
                    data$Ratio_Big5_C_PosLexCountAvg_to_Physical_Affordance +
                    data$Ratio_Big5_C_PosLexCountAvg_to_Functional_Affordance +
                    data$Ratio_Big5_C_PosLexCountAvg_to_Sensory_Affordance +
                    
                    # Controls
                    prod_review_count + review_count +
                    title_count + also_buy + rank + also_view +
                    imageURLHighRes_count,
                  
                  data=data))

# Ratio Analysis
summary(M5.3 <-lm(average_rating ~ 
                    
                    # IVs
                    data$Big5_E_PosLexCountAvg +
                    
                    data$Cognitive_Affordance +
                    data$Physical_Affordance +
                    data$Functional_Affordance +
                    data$Sensory_Affordance + 
                    
                    
                    data$Ratio_Big5_E_PosLexCountAvg_to_Cognitive_Affordance +
                    data$Ratio_Big5_E_PosLexCountAvg_to_Physical_Affordance +
                    data$Ratio_Big5_E_PosLexCountAvg_to_Functional_Affordance +
                    data$Ratio_Big5_E_PosLexCountAvg_to_Sensory_Affordance +
                    
                    # Controls
                    prod_review_count + review_count +
                    title_count + also_buy + rank + also_view +
                    imageURLHighRes_count,
                  
                  data=data))

# Ratio Analysis
summary(M5.4 <-lm(average_rating ~ 
                    
                    # IVs
                    data$Big5_A_PosLexCountAvg +
                    
                    data$Cognitive_Affordance +
                    data$Physical_Affordance +
                    data$Functional_Affordance +
                    data$Sensory_Affordance + 
                    
                    
                    data$Ratio_Big5_A_PosLexCountAvg_to_Cognitive_Affordance +
                    data$Ratio_Big5_A_PosLexCountAvg_to_Physical_Affordance +
                    data$Ratio_Big5_A_PosLexCountAvg_to_Functional_Affordance +
                    data$Ratio_Big5_A_PosLexCountAvg_to_Sensory_Affordance +
                    
                    # Controls
                    prod_review_count + review_count +
                    title_count + also_buy + rank + also_view +
                    imageURLHighRes_count,
                  
                  data=data))

# Ratio Analysis
summary(M5.5 <-lm(average_rating ~ 
                    
                    # IVs
                    data$Big5_N_PosLexCountAvg +
                    
                    data$Cognitive_Affordance +
                    data$Physical_Affordance +
                    data$Functional_Affordance +
                    data$Sensory_Affordance + 
                    
                    
                    data$Ratio_Big5_N_PosLexCountAvg_to_Cognitive_Affordance +
                    data$Ratio_Big5_N_PosLexCountAvg_to_Physical_Affordance +
                    data$Ratio_Big5_N_PosLexCountAvg_to_Functional_Affordance +
                    data$Ratio_Big5_N_PosLexCountAvg_to_Sensory_Affordance +
                    
                    # Controls
                    prod_review_count + review_count +
                    title_count + also_buy + rank + also_view +
                    imageURLHighRes_count,
                  
                  data=data))

###################################
########## Comb.Rating ############
############# Ratio ###############
###################################

# Ratio Analysis
summary(M6.1 <-lm(combined_metric ~ 
                    
                    # IVs
                    data$Big5_O_PosLexCountAvg +
                    
                    data$Cognitive_Affordance +
                    data$Physical_Affordance +
                    data$Functional_Affordance +
                    data$Sensory_Affordance + 
                    
                    
                    data$Ratio_Big5_O_PosLexCountAvg_to_Cognitive_Affordance +
                    data$Ratio_Big5_O_PosLexCountAvg_to_Physical_Affordance +
                    data$Ratio_Big5_O_PosLexCountAvg_to_Functional_Affordance +
                    data$Ratio_Big5_O_PosLexCountAvg_to_Sensory_Affordance +
                    
                    # Controls
                    prod_review_count + review_count +
                    title_count + also_buy + rank + also_view +
                    imageURLHighRes_count,
                  
                  data=data))

# Ratio Analysis
summary(M6.2 <-lm(combined_metric ~ 
                    
                    # IVs
                    data$Big5_C_PosLexCountAvg +
                    
                    data$Cognitive_Affordance +
                    data$Physical_Affordance +
                    data$Functional_Affordance +
                    data$Sensory_Affordance + 
                    
                    
                    data$Ratio_Big5_C_PosLexCountAvg_to_Cognitive_Affordance +
                    data$Ratio_Big5_C_PosLexCountAvg_to_Physical_Affordance +
                    data$Ratio_Big5_C_PosLexCountAvg_to_Functional_Affordance +
                    data$Ratio_Big5_C_PosLexCountAvg_to_Sensory_Affordance +
                    
                    # Controls
                    prod_review_count + review_count +
                    title_count + also_buy + rank + also_view +
                    imageURLHighRes_count,
                  
                  data=data))

# Ratio Analysis
summary(M6.3 <-lm(combined_metric ~ 
                    
                    # IVs
                    data$Big5_E_PosLexCountAvg +
                    
                    data$Cognitive_Affordance +
                    data$Physical_Affordance +
                    data$Functional_Affordance +
                    data$Sensory_Affordance + 
                    
                    
                    data$Ratio_Big5_E_PosLexCountAvg_to_Cognitive_Affordance +
                    data$Ratio_Big5_E_PosLexCountAvg_to_Physical_Affordance +
                    data$Ratio_Big5_E_PosLexCountAvg_to_Functional_Affordance +
                    data$Ratio_Big5_E_PosLexCountAvg_to_Sensory_Affordance +
                    
                    # Controls
                    prod_review_count + review_count +
                    title_count + also_buy + rank + also_view +
                    imageURLHighRes_count,
                  
                  data=data))

# Ratio Analysis
summary(M6.4 <-lm(combined_metric ~ 
                    
                    # IVs
                    data$Big5_A_PosLexCountAvg +
                    
                    data$Cognitive_Affordance +
                    data$Physical_Affordance +
                    data$Functional_Affordance +
                    data$Sensory_Affordance + 
                    
                    
                    data$Ratio_Big5_A_PosLexCountAvg_to_Cognitive_Affordance +
                    data$Ratio_Big5_A_PosLexCountAvg_to_Physical_Affordance +
                    data$Ratio_Big5_A_PosLexCountAvg_to_Functional_Affordance +
                    data$Ratio_Big5_A_PosLexCountAvg_to_Sensory_Affordance +
                    
                    # Controls
                    prod_review_count + review_count +
                    title_count + also_buy + rank + also_view +
                    imageURLHighRes_count,
                  
                  data=data))

# Ratio Analysis
summary(M6.5 <-lm(combined_metric ~ 
                    
                    # IVs
                    data$Big5_N_PosLexCountAvg +
                    
                    data$Cognitive_Affordance +
                    data$Physical_Affordance +
                    data$Functional_Affordance +
                    data$Sensory_Affordance + 
                    
                    
                    data$Ratio_Big5_N_PosLexCountAvg_to_Cognitive_Affordance +
                    data$Ratio_Big5_N_PosLexCountAvg_to_Physical_Affordance +
                    data$Ratio_Big5_N_PosLexCountAvg_to_Functional_Affordance +
                    data$Ratio_Big5_N_PosLexCountAvg_to_Sensory_Affordance +
                    
                    # Controls
                    prod_review_count + review_count +
                    title_count + also_buy + rank + also_view +
                    imageURLHighRes_count,
                  
                  data=data))