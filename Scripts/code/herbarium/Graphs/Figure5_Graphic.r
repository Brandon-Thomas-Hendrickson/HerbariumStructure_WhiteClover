##########################
#Figure 5: Selection Graphic
############################

####################
#AC LI Plotting ####
####################

library(ggplot2)

op <- read.csv("Data/herbarium/Input/herbarium_structure_dataframe_1192026.csv",header=TRUE)
op <- op[op$Year <= 1997, ]

op_N <- op[op$Region=="North",]

ac_model <- lm(stdAC ~ Year, data = op_N)
summary(ac_model)
# Create a new data frame for predictions
prediction_data_N <- data.frame(
  Year = seq(min(op_N$Year, na.rm = TRUE), max(op_N$Year, na.rm = TRUE), length.out = 100)
)

prediction_data_N$Predicted_stdAC <- predict(ac_model, newdata = prediction_data_N)

op_S <- op[op$Region=="South",]

ac_model <- lm(stdAC ~ Year, data = op_S)
summary(ac_model)
# Create a new data frame for predictions
prediction_data_S <- data.frame(
  Year = seq(min(op_S$Year, na.rm = TRUE), max(op_S$Year, na.rm = TRUE), length.out = 100)
)
# Generate predictions
prediction_data_S$Predicted_stdAC <- predict(ac_model, newdata = prediction_data_S)

ac_model <- lm(stdAC ~ Year * lat, data = op)
# Create a new data frame for predictions
summary(ac_model)
prediction_data <- data.frame(
  Year = seq(min(op$Year, na.rm = TRUE), max(op$Year, na.rm = TRUE), length.out = 100),
  lat = mean(op$lat, na.rm = TRUE)  # Use the mean lat or adjust as needed
)

prediction_data$Predicted_stdAC <- predict(ac_model, newdata = prediction_data)

plot_NS_AC <- ggplot(data = op[op$Year <= 1997, ], aes(x = Year, y = stdAC)) +
    geom_point(data = op, aes(x = Year, y = stdAC), alpha = 0.6, color = "darkgrey") + # Original data points
    geom_line(data = prediction_data_N, aes(x = Year, y = Predicted_stdAC), color = "blue", size = 2) +
    geom_line(data = prediction_data_S, aes(x = Year, y = Predicted_stdAC), color = "red", size = 2) +
    geom_line(data=prediction_data, aes( x= Year, y =Predicted_stdAC),color="black",size=2,linetype="dashed")+ # Predicted lines
    theme_light() + theme(plot.title = element_text(hjust = 0.5),axis.title.x=element_blank(), axis.text=element_text(size=12,color="black"),axis.title.y=element_text(size=12,color="black"))

# Fit the linear regression model
li_model <- lm(stdLI ~ Year * lat, data = op)

# Summary of the model
summary(li_model)

# Create a new data frame for predictions
prediction_data <- data.frame(
    Year = seq(min(op$Year, na.rm = TRUE), max(op$Year, na.rm = TRUE), length.out = 100),
    lat = mean(op$lat, na.rm = TRUE)  # Use the mean lat or adjust as needed
)

# Generate predicted values
prediction_data$Predicted_stdLI <- predict(li_model, newdata = prediction_data)


op_N <- op[op$Region=="North",]
# Create a new data frame for predictions

li_model <- lm(stdLI ~ Year, data = op_N)
summary(li_model)
prediction_data_N <- data.frame(
  Year = seq(min(op_N$Year, na.rm = TRUE), max(op_N$Year, na.rm = TRUE), length.out = 100)
)

prediction_data_N$Predicted_stdLI <- predict(li_model, newdata = prediction_data_N)

op_S <- op[op$Region=="South",]

li_model <- lm(stdLI ~ Year, data = op_S)
summary(li_model)
# Create a new data frame for predictions
prediction_data_S <- data.frame(
  Year = seq(min(op_S$Year, na.rm = TRUE), max(op_S$Year, na.rm = TRUE), length.out = 100)
)
# Generate predictions
prediction_data_S$Predicted_stdLI <- predict(li_model, newdata = prediction_data_S)

plot_NS_LI <- ggplot(data = op[op$Year <= 1997, ], aes(x = Year, y = stdLI)) +
    geom_point(data = op, aes(x = Year, y = stdLI), alpha = 0.6, color = "darkgrey") + # Original data points
    geom_line(data = prediction_data_N, aes(x = Year, y = Predicted_stdLI), color = "blue", size = 2) +
    geom_line(data = prediction_data_S, aes(x = Year, y = Predicted_stdLI), color = "red", size = 2) + 
    geom_line(data=prediction_data, aes( x= Year, y =Predicted_stdLI),color="black",size=2,linetype="dashed")+# Predicted lines
    theme_light() + theme(plot.title = element_text(hjust = 0.5),axis.title.x=element_blank(), axis.title.y=element_text(size=12,color="black"),axis.text=element_text(size=12,color="black"))


##########################
#Figure 5: Selection Graphic
############################

####################
#AC LI Plotting ####
####################


final_figure <- ggarrange(plot_NS_AC,plot_NS_LI,ncol=2,nrow=1,labels=c("A","B"))

ggsave("Figure5.pdf", plot = final_figure, width = 8, height = 4, units = "in")

op <- op[,c("stdAC","stdLI","Year","Region","Samp","lat","lon")]

library(dplyr)
library(broom)

# Initialize list to store model results
model_results <- list()

# ========== AC MODELS ==========

# Model 1: AC ~ Year (North)
op_N <- op[op$Region == "North", ]
ac_model_N <- lm(stdAC ~ Year, data = op_N)

model_results[["AC_North_Year"]] <- tidy(ac_model_N) %>%
  mutate(Model = "AC_North", 
         Response = "stdAC",
         Predictors = "Year",
         Region = "North")

# Model 2: AC ~ Year (South)
op_S <- op[op$Region == "South", ]
ac_model_S <- lm(stdAC ~ Year, data = op_S)

model_results[["AC_South_Year"]] <- tidy(ac_model_S) %>%
  mutate(Model = "AC_South", 
         Response = "stdAC",
         Predictors = "Year",
         Region = "South")

# Model 3: AC ~ Year * lat (All)
ac_model_all <- lm(stdAC ~ Year * lat, data = op)

model_results[["AC_All_Year_lat"]] <- tidy(ac_model_all) %>%
  mutate(Model = "AC_All", 
         Response = "stdAC",
         Predictors = "Year * lat",
         Region = "All")

# ========== LI MODELS ==========

# Model 4: LI ~ Year * lat (All)
li_model_all <- lm(stdLI ~ Year * lat, data = op)

model_results[["LI_All_Year_lat"]] <- tidy(li_model_all) %>%
  mutate(Model = "LI_All", 
         Response = "stdLI",
         Predictors = "Year * lat",
         Region = "All")

# Model 5: LI ~ Year (North)
li_model_N <- lm(stdLI ~ Year, data = op_N)

model_results[["LI_North_Year"]] <- tidy(li_model_N) %>%
  mutate(Model = "LI_North", 
         Response = "stdLI",
         Predictors = "Year",
         Region = "North")

# Model 6: LI ~ Year (South)
li_model_S <- lm(stdLI ~ Year, data = op_S)

model_results[["LI_South_Year"]] <- tidy(li_model_S) %>%
  mutate(Model = "LI_South", 
         Response = "stdLI",
         Predictors = "Year",
         Region = "South")

# ========== COMBINE ALL MODEL RESULTS ==========
all_model_results <- bind_rows(model_results)

# Reorder columns for clarity
all_model_results <- all_model_results %>%
  dplyr::select(Model, Response, Region, Predictors, term, estimate, std.error, statistic, p.value)

# ========== EXTRACT MODEL FIT STATISTICS ==========
model_fit <- data.frame(
  Model = c("AC_North_Year", "AC_South_Year", "AC_All_Year_lat", 
            "LI_All_Year_lat", "LI_North_Year", "LI_South_Year"),
  Response = c("stdAC", "stdAC", "stdAC", "stdLI", "stdLI", "stdLI"),
  Region = c("North", "South", "All", "All", "North", "South"),
  Predictors = c("Year", "Year", "Year * lat", "Year * lat", "Year", "Year"),
  R_squared = c(summary(ac_model_N)$r.squared, 
                summary(ac_model_S)$r.squared,
                summary(ac_model_all)$r.squared,
                summary(li_model_all)$r.squared,
                summary(li_model_N)$r.squared,
                summary(li_model_S)$r.squared),
  Adj_R_squared = c(summary(ac_model_N)$adj.r.squared,
                    summary(ac_model_S)$adj.r.squared,
                    summary(ac_model_all)$adj.r.squared,
                    summary(li_model_all)$adj.r.squared,
                    summary(li_model_N)$adj.r.squared,
                    summary(li_model_S)$adj.r.squared),
  F_statistic = c(summary(ac_model_N)$fstatistic[1],
                  summary(ac_model_S)$fstatistic[1],
                  summary(ac_model_all)$fstatistic[1],
                  summary(li_model_all)$fstatistic[1],
                  summary(li_model_N)$fstatistic[1],
                  summary(li_model_S)$fstatistic[1]),
  DF_Residual = c(ac_model_N$df.residual, ac_model_S$df.residual, ac_model_all$df.residual,
                  li_model_all$df.residual, li_model_N$df.residual, li_model_S$df.residual),
  N_obs = c(nobs(ac_model_N), nobs(ac_model_S), nobs(ac_model_all),
            nobs(li_model_all), nobs(li_model_N), nobs(li_model_S))
)

# ========== VIEW RESULTS ==========
print("Model Coefficients:")
print(all_model_results)
