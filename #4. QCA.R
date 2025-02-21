# Load required packages
library(QCA)
library(dplyr)
library(ggplot2)

#####################
# Electronic Ratings
#####################


# Set working directory and create "Electronic_rating" folder
setwd("C:/Users/joe_c/Dropbox/0_Academia/1_Current Research/#0.1. Submitted/PAMA_SR (Done)/PMAT Analysis")
if (!dir.exists("Electronic_rating")) {
  dir.create("Electronic_rating")
}
output_dir <- "Electronic_rating"

# Load data
data <- read.csv("#1. Data/#3. Modified_Main_Analysis_Cat.csv")

# Define original condition names and simplified codes
conditions <- c(
  "Big5_O_PosLexCountAvg", "Big5_C_PosLexCountAvg", "Big5_E_PosLexCountAvg",
  "Big5_A_PosLexCountAvg", "Big5_N_PosLexCountAvg",
  "Cognitive.Affordance", "Physical.Affordance", "Functional.Affordance",
  "Sensory.Affordance"
)
condition_codes <- c("O", "C", "E", "A", "N", "CA", "PA", "FA", "SA")  # Simplified names

# Exploratory analysis (for reporting)
rating_summary <- summary(data$average_rating)
write.csv(as.data.frame(t(rating_summary)), file.path(output_dir, "rating_summary.csv"))
p <- ggplot(data, aes(x = average_rating)) + 
  geom_histogram(binwidth = 0.5, fill = "green", alpha = 0.7) +
  labs(title = "Distribution of Average Rating", x = "Rating", y = "Frequency")
ggsave(file.path(output_dir, "rating_distribution.png"), p, width = 6, height = 4)

# Split conditions into Big5 and Affordances for calibration
big5_conditions <- conditions[1:5]
affordance_conditions <- conditions[6:9]
big5_codes <- condition_codes[1:5]
affordance_codes <- condition_codes[6:9]

# Calibrate Big5 conditions (continuous) with jitter
set.seed(123)  # For reproducibility
for (i in 1:length(big5_conditions)) {
  cond <- big5_conditions[i]
  code <- big5_codes[i]
  percentiles <- quantile(data[[cond]], probs = c(0.05, 0.5, 0.95), na.rm = TRUE)
  jittered_data <- data[[cond]] + runif(length(data[[cond]]), -0.001, 0.001)
  data[[code]] <- calibrate(jittered_data, type = "fuzzy", thresholds = percentiles)
}

# Calibrate Affordance conditions (discrete) with jitter
for (i in 1:length(affordance_conditions)) {
  cond <- affordance_conditions[i]
  code <- affordance_codes[i]
  thresholds <- c(min(data[[cond]], na.rm = TRUE), 
                  median(data[[cond]], na.rm = TRUE), 
                  max(data[[cond]], na.rm = TRUE))
  jittered_data <- data[[cond]] + runif(length(data[[cond]]), -0.001, 0.001)
  data[[code]] <- calibrate(jittered_data, type = "fuzzy", thresholds = thresholds)
}

# Calibrate outcome
outcome_percentiles <- quantile(data$average_rating, probs = c(0.05, 0.5, 0.95), na.rm = TRUE)
data$H <- calibrate(data$average_rating + runif(length(data$average_rating), -0.001, 0.001), 
                    type = "fuzzy", thresholds = outcome_percentiles)

# Define coded condition list
coded_conditions <- c("O", "C", "E", "A", "N", "CA", "PA", "FA", "SA")

# Verify no 0.5 values (for robustness)
sink(file.path(output_dir, "calibration_check.txt"))
for (cond in c(coded_conditions, "H")) {
  num_0.5 <- sum(data[[cond]] == 0.5, na.rm = TRUE)
  if (num_0.5 > 0) {
    cat("Condition", cond, "has", num_0.5, "values equal to 0.5\n")
  }
}
sink()

# Necessity analysis
necessity <- pof(data[, coded_conditions], data$H, relation = "necessity")
write.csv(necessity$incl.cov, file.path(output_dir, "necessity_results.csv"))
sink(file.path(output_dir, "necessity_output.txt"))
print(necessity)
sink()

# Sufficiency analysis: Truth table
tt <- truthTable(data, outcome = "H", conditions = coded_conditions, incl.cut = 0.9, n.cut=3)
write.csv(tt$tt, file.path(output_dir, "truth_table.csv"))
sink(file.path(output_dir, "truth_table_output.txt"))
print(tt)
sink()

# Minimize the truth table
solution <- minimize(tt, details = TRUE)
# Save solution components (adjust based on actual structure)
write.csv(solution$IC$incl.cov, file.path(output_dir, "solution_incl_cov.csv"))
sink(file.path(output_dir, "solution_output.txt"))
print(solution)
sink()

print(solution)