# Load necessary libraries
library(ggplot2)
library(ggpubr)
library(Rmisc)

op <- read.csv("Data/herbarium/Input/herbarium_structure_dataframe_1192026.csv",header=TRUE)

op <- op[op$H_C=="H",]

op <- op[op$Year <= 1997, ]

# Function to compare linear vs quadratic models and choose the best
compare_and_choose_model <- function(data, response_var, predictor_vars) {
  # Create formulas
  linear_formula <- as.formula(paste(response_var, "~", paste(predictor_vars, collapse = " * ")))
  quad_formula <- as.formula(paste(response_var, "~", paste(predictor_vars, collapse = " * "), "+ I(", predictor_vars[1], "^2)"))
  
  # Fit models
  linear_model <- lm(linear_formula, data = data)
  quad_model <- lm(quad_formula, data = data)
  
  # Extract p-values
  linear_p <- summary(linear_model)$coefficients[predictor_vars[1], "Pr(>|t|)"]
  quad_p <- summary(quad_model)$coefficients[paste0("I(", predictor_vars[1], "^2)"), "Pr(>|t|)"]
  
  # Compare AIC
  linear_aic <- AIC(linear_model)
  quad_aic <- AIC(quad_model)
  cat("Linear AIC:", linear_aic, "Quadratic AIC:", quad_aic, "\n")
  
  if (quad_aic < linear_aic) {
    return(list(model = quad_model, type = "quadratic", formula = quad_formula,
                linear_aic = linear_aic, quad_aic = quad_aic,
                linear_p = linear_p, quad_p = quad_p))
  } else {
    return(list(model = linear_model, type = "linear", formula = linear_formula,
                linear_aic = linear_aic, quad_aic = quad_aic,
                linear_p = linear_p, quad_p = quad_p))
  }
}

# Function to create predictions based on model type
create_predictions <- function(data, model_info, response_var, n_points = 100) {
  # Create base prediction data
  prediction_data <- data.frame(
    Year = seq(min(data$Year, na.rm = TRUE), max(data$Year, na.rm = TRUE), length.out = n_points),
    lat = mean(data$lat, na.rm = TRUE)
  )
  
  # Generate predictions using the chosen model (linear or quadratic)
  prediction_data[[paste0("Predicted_", response_var)]] <- predict(model_info$model, newdata = prediction_data)
  return(prediction_data)
}

# First, let's check what columns are available
cat("Available columns containing 'geno' or 'hb':\n")
geno_cols <- grep("geno|hb|Hb", colnames(op), value = TRUE, ignore.case = TRUE)
print(geno_cols)

# Function to add results to dataframe
add_to_results <- function(response_var, region, model_info) {
  model_results_df <<- rbind(model_results_df, data.frame(
    response = response_var,
    region = region,
    best_model = model_info$type,
    linear_aic = model_info$linear_aic,
    quadratic_aic = model_info$quad_aic,
    linear_p_value = model_info$linear_p,
    quadratic_p_value = model_info$quad_p,
    stringsAsFactors = FALSE
  ))
}

# Initialize results dataframe
model_results_df <- data.frame(
  response = character(),
  region = character(),
  best_model = character(),
  linear_aic = numeric(),
  quadratic_aic = numeric(),
  linear_p_value = numeric(),
  quadratic_p_value = numeric(),
  stringsAsFactors = FALSE
)

# Choose best models for each gene (using correct column names)
cat("Comparing models for geno_Hb7a1:\n")
hb7a1_best <- compare_and_choose_model(op, "geno_Hb7a1", c("Year", "lat"))
add_to_results("geno_Hb7a1", "Overall", hb7a1_best)

# Create predictions for overall model
prediction_data <- create_predictions(op, hb7a1_best, "geno_Hb7a1")

# Choose best models for North region
op_N <- op[op$Region=="North",]
cat("Comparing models for North region geno_Hb7a1:\n")
hb7a1_N_best <- compare_and_choose_model(op_N, "geno_Hb7a1", c("Year", "lat"))
add_to_results("geno_Hb7a1", "North", hb7a1_N_best)
prediction_data_N <- create_predictions(op_N, hb7a1_N_best, "geno_Hb7a1")

# Choose best models for South region
op_S <- op[op$Region=="South",]
cat("Comparing models for South region geno_Hb7a1:\n")
hb7a1_S_best <- compare_and_choose_model(op_S, "geno_Hb7a1", c("Year", "lat"))
add_to_results("geno_Hb7a1", "South", hb7a1_S_best)
prediction_data_S <- create_predictions(op_S, hb7a1_S_best, "geno_Hb7a1")

plot_NS_Hb7a1 <- ggplot(data = op[op$Year <= 1997, ], aes(x = Year, y = geno_Hb7a1)) +
    geom_point(data = op,aes(x = Year, y = geno_Hb7a1), color = "black", alpha = 0.5,size=2) +
    geom_line(data=prediction_data, aes( x= Year, y =Predicted_geno_Hb7a1),color="black",size=2,linetype="dashed")+
    geom_line(data = prediction_data_N, aes(x = Year, y = Predicted_geno_Hb7a1), color = "blue", size = 2) +
    geom_line(data = prediction_data_S, aes(x = Year, y = Predicted_geno_Hb7a1), color = "red", size = 2) +
    theme_light() + theme(plot.title = element_text(hjust = 0.5),axis.title.x=element_blank(),axis.text=element_text(size=12,color="black"),axis.title=element_text(size=12,color="black")) +
    labs(title = paste("Hb7a1"),y="Genotype")

# Choose best models for each gene (using correct column names)
cat("Comparing models for geno_Hb7a2:\n")
Hb7a2_best <- compare_and_choose_model(op, "geno_Hb7a2", c("Year", "lat"))
add_to_results("geno_Hb7a2", "Overall", Hb7a2_best)

# Create predictions for overall model
prediction_data <- create_predictions(op, Hb7a2_best, "geno_Hb7a2")

# Choose best models for North region
op_N <- op[op$Region=="North",]
cat("Comparing models for North region geno_Hb7a2:\n")
Hb7a2_N_best <- compare_and_choose_model(op_N, "geno_Hb7a2", c("Year", "lat"))
add_to_results("geno_Hb7a2", "North", Hb7a2_N_best)
prediction_data_N <- create_predictions(op_N, Hb7a2_N_best, "geno_Hb7a2")

# Choose best models for South region
op_S <- op[op$Region=="South",]
cat("Comparing models for South region geno_Hb7a2:\n")
Hb7a2_S_best <- compare_and_choose_model(op_S, "geno_Hb7a2", c("Year", "lat"))
add_to_results("geno_Hb7a2", "South", Hb7a2_S_best)
prediction_data_S <- create_predictions(op_S, Hb7a2_S_best, "geno_Hb7a2")

plot_NS_Hb7a2 <- ggplot(data = op[op$Year <= 1997, ], aes(x = Year, y = geno_Hb7a2)) +
    geom_point(data = op,aes(x = Year, y = geno_Hb7a2), color = "black", alpha = 0.5,size=2) +
    geom_line(data=prediction_data, aes( x= Year, y =Predicted_geno_Hb7a2),color="black",size=2,linetype="dashed")+
    geom_line(data = prediction_data_N, aes(x = Year, y = Predicted_geno_Hb7a2), color = "blue", size = 2) +
    geom_line(data = prediction_data_S, aes(x = Year, y = Predicted_geno_Hb7a2), color = "red", size = 2) +
    theme_light() + theme(plot.title = element_text(hjust = 0.5),axis.title.x=element_blank(),axis.text=element_text(size=12,color="black"),axis.title=element_text(size=12,color="black")) +
    labs(title = paste("Hb7a2"),y="Genotype")
# Choose best models for each gene (using correct column names)
cat("Comparing models for geno_Hb7b:\n")
Hb7b_best <- compare_and_choose_model(op, "geno_Hb7b", c("Year", "lat"))
add_to_results("geno_Hb7b", "Overall", Hb7b_best)

# Create predictions for overall model
prediction_data <- create_predictions(op, Hb7b_best, "geno_Hb7b")

# Choose best models for North region
op_N <- op[op$Region=="North",]
cat("Comparing models for North region geno_Hb7b:\n")
Hb7b_N_best <- compare_and_choose_model(op_N, "geno_Hb7b", c("Year", "lat"))
add_to_results("geno_Hb7b", "North", Hb7b_N_best)
prediction_data_N <- create_predictions(op_N, Hb7b_N_best, "geno_Hb7b")

# Choose best models for South region
op_S <- op[op$Region=="South",]
cat("Comparing models for South region geno_Hb7b:\n")
Hb7b_S_best <- compare_and_choose_model(op_S, "geno_Hb7b", c("Year", "lat"))
add_to_results("geno_Hb7b", "South", Hb7b_S_best)
prediction_data_S <- create_predictions(op_S, Hb7b_S_best, "geno_Hb7b")

plot_NS_Hb7b <- ggplot(data = op[op$Year <= 1997, ], aes(x = Year, y = geno_Hb7b)) +
    geom_point(data = op,aes(x = Year, y = geno_Hb7b), color = "black", alpha = 0.5,size=2) +
    geom_line(data=prediction_data, aes( x= Year, y =Predicted_geno_Hb7b),color="black",size=2,linetype="dashed")+
    geom_line(data = prediction_data_N, aes(x = Year, y = Predicted_geno_Hb7b), color = "blue", size = 2) +
    geom_line(data = prediction_data_S, aes(x = Year, y = Predicted_geno_Hb7b), color = "red", size = 2) +
    theme_light() + theme(plot.title = element_text(hjust = 0.5),axis.title.x=element_blank(),axis.text=element_text(size=12,color="black"),axis.title=element_text(size=12,color="black")) +
    labs(title = paste("Hb7b"),y="Genotype")
# Choose best models for each gene (using correct column names)
cat("Comparing models for geno_Hb9:\n")
Hb9_best <- compare_and_choose_model(op, "geno_Hb9", c("Year", "lat"))
add_to_results("geno_Hb9", "Overall", Hb9_best)

# Create predictions for overall model
prediction_data <- create_predictions(op, Hb9_best, "geno_Hb9")

# Choose best models for North region
op_N <- op[op$Region=="North",]
cat("Comparing models for North region geno_Hb9:\n")
Hb9_N_best <- compare_and_choose_model(op_N, "geno_Hb9", c("Year", "lat"))
add_to_results("geno_Hb9", "North", Hb9_N_best)
prediction_data_N <- create_predictions(op_N, Hb9_N_best, "geno_Hb9")

# Choose best models for South region
op_S <- op[op$Region=="South",]
cat("Comparing models for South region geno_Hb9:\n")
Hb9_S_best <- compare_and_choose_model(op_S, "geno_Hb9", c("Year", "lat"))
add_to_results("geno_Hb9", "South", Hb9_S_best)
prediction_data_S <- create_predictions(op_S, Hb9_S_best, "geno_Hb9")

plot_NS_Hb9 <- ggplot(data = op[op$Year <= 1997, ], aes(x = Year, y = geno_Hb9)) +
    geom_point(data = op,aes(x = Year, y = geno_Hb9), color = "black", alpha = 0.5,size=2) +
    geom_line(data=prediction_data, aes( x= Year, y =Predicted_geno_Hb9),color="black",size=2,linetype="dashed")+
    geom_line(data = prediction_data_N, aes(x = Year, y = Predicted_geno_Hb9), color = "blue", size = 2) +
    geom_line(data = prediction_data_S, aes(x = Year, y = Predicted_geno_Hb9), color = "red", size = 2) +
    theme_light() + theme(plot.title = element_text(hjust = 0.5),axis.title.x=element_blank(),axis.text=element_text(size=12,color="black"),axis.title=element_text(size=12,color="black")) +
    labs(title = paste("Hb9"),y="Genotype")
# Choose best models for each gene (using correct column names)
cat("Comparing models for geno_Hb13:\n")
Hb13_best <- compare_and_choose_model(op, "geno_Hb13", c("Year", "lat"))
add_to_results("geno_Hb13", "Overall", Hb13_best)

# Create predictions for overall model
prediction_data <- create_predictions(op, Hb13_best, "geno_Hb13")

# Choose best models for North region
op_N <- op[op$Region=="North",]
cat("Comparing models for North region geno_Hb13:\n")
Hb13_N_best <- compare_and_choose_model(op_N, "geno_Hb13", c("Year", "lat"))
add_to_results("geno_Hb13", "North", Hb13_N_best)
prediction_data_N <- create_predictions(op_N, Hb13_N_best, "geno_Hb13")

# Choose best models for South region
op_S <- op[op$Region=="South",]
cat("Comparing models for South region geno_Hb13:\n")
Hb13_S_best <- compare_and_choose_model(op_S, "geno_Hb13", c("Year", "lat"))
add_to_results("geno_Hb13", "South", Hb13_S_best)
prediction_data_S <- create_predictions(op_S, Hb13_S_best, "geno_Hb13")

plot_NS_Hb13 <- ggplot(data = op[op$Year <= 1997, ], aes(x = Year, y = geno_Hb13)) +
    geom_point(data = op,aes(x = Year, y = geno_Hb13), color = "black", alpha = 0.5,size=2) +
    geom_line(data=prediction_data, aes( x= Year, y =Predicted_geno_Hb13),color="black",size=2,linetype="dashed")+
    geom_line(data = prediction_data_N, aes(x = Year, y = Predicted_geno_Hb13), color = "blue", size = 2) +
    geom_line(data = prediction_data_S, aes(x = Year, y = Predicted_geno_Hb13), color = "red", size = 2) +
    theme_light() + theme(plot.title = element_text(hjust = 0.5),axis.title.x=element_blank(),axis.text=element_text(size=12,color="black"),axis.title=element_text(size=12,color="black")) +
    labs(title = paste("Hb13"),y="Genotype")

# Print summary of best models
cat("\n=== MODEL SUMMARY ===\n")
cat("geno_Hb7a1 - Overall:", hb7a1_best$type, ", North:", hb7a1_N_best$type, ", South:", hb7a1_S_best$type, "\n")
if (exists("Hb7a2_best")) {
  cat("geno_Hb7a2 - Overall:", Hb7a2_best$type, ", North:", Hb7a2_N_best$type, ", South:", Hb7a2_S_best$type, "\n")
}
if (exists("Hb7b_best")) {
  cat("geno_Hb7b - Overall:", Hb7b_best$type, ", North:", Hb7b_N_best$type, ", South:", Hb7b_S_best$type, "\n")
}
if (exists("Hb9_best")) {
  cat("geno_Hb9 - Overall:", Hb9_best$type, ", North:", Hb9_N_best$type, ", South:", Hb9_S_best$type, "\n")
}
if (exists("hb13_best")) {
  cat("geno_hb13 - Overall:", hb13_best$type, ", North:", hb13_N_best$type, ", South:", hb13_S_best$type, "\n")
}

# Display comprehensive results dataframe
cat("\n=== COMPREHENSIVE RESULTS DATAFRAME ===\n")
print(model_results_df)

# Round p-values for better display
model_results_df$linear_p_value <- round(model_results_df$linear_p_value, 6)
model_results_df$quadratic_p_value <- round(model_results_df$quadratic_p_value, 6)
model_results_df$linear_aic <- round(model_results_df$linear_aic, 2)
model_results_df$quadratic_aic <- round(model_results_df$quadratic_aic, 2)

cat("\n=== ROUNDED RESULTS FOR EASIER READING ===\n")
print(model_results_df)

# Combine all plots into a single figure
combined_plot <- ggarrange(plot_NS_Hb7a1, plot_NS_Hb7a2, plot_NS_Hb7b, plot_NS_Hb9, plot_NS_Hb13,
                           ncol = 2, nrow = 3, align="hv")


op <- op[,c("geno_Hb7a1","geno_Hb7a2","geno_Hb7b","geno_Hb9","geno_Hb13","Year","Region","lat","Samp")]
ggsave("SupFig5.pdf", plot = combined_plot, width = 6, height = 8, units = "in")