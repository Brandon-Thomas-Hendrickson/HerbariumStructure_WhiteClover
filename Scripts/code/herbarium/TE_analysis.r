####################
#TE Analysis#
####################
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggpubr)

op <- read.csv("Github_Projects/HerbariumStructure_WhiteClover/Data/herbarium/Input/TE_analysis_ready.csv", header = TRUE, sep = ",")
# Initialize a list to store the results
model_results <- list()

op_sub <- op[op$H_C == "H", ]
for (i in 15:9453) {
  # Get the column name
  col_name <- colnames(op_sub)[i]
  
  # Run the linear regression model
  model <- lm(op_sub[[i]] ~ Year + lat + Year:lat, data = op_sub)
  
  # Store the model summary in the list
  model_results[[col_name]] <- summary(model)
}

# Extract estimates and p-values into a structured dataframe
results_df <- do.call(rbind, lapply(names(model_results), function(col_name) {
  model_summary <- model_results[[col_name]]
  coefficients <- as.data.frame(coef(model_summary))
  
  # Extract relevant terms
  data.frame(
    Column = col_name,
    P_Year = coefficients["Year", "Pr(>|t|)"],
    Estimate_Year = coefficients["Year", "Estimate"],
    P_Latitude = coefficients["lat", "Pr(>|t|)"],
    Estimate_Latitude = coefficients["lat", "Estimate"],
    P_Interaction = coefficients["Year:lat", "Pr(>|t|)"],
    Estimate_Interaction = coefficients["Year:lat", "Estimate"]
  )
}))

# Initialize a list to store the ANOVA results
anova_results <- list()

# Loop through columns 12 to 9450
for (i in 15:9453) {
  # Get the column name
  col_name <- colnames(op)[i]
  
  # Run the ANOVA model
  model <- aov(op[[i]] ~ Range, data = op)
  
  # Store the model summary in the list
  anova_results[[col_name]] <- summary(model)
}

# Extract p-values and other relevant information into a structured dataframe
anova_results_df <- do.call(rbind, lapply(names(anova_results), function(col_name) {
  model_summary <- anova_results[[col_name]]
  p_value <- model_summary[[1]]$`Pr(>F)`[1]  # Extract the p-value for the RANGE term
  
  data.frame(
    Column = col_name,
    P_Range = p_value
  )
}))

