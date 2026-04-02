############################
#Figure 3################

#Heterozygosity of All 
#####################
#Heterozygosity Graphs#
#####################
library(ggplot2)
library(ggpubr)
library(Rmisc)
library(emmeans)
library(multcomp)
herb <- read.csv("Data/herbarium/Input/herbarium_structure_dataframe_1192026.csv",header=TRUE)

herb <- herb[!is.na(herb$N_S),]

# Fit the Gamma GLM
gamma_model <- glm(Heterozygosity ~ Year * lat, data = herb[herb$Year <= 1997, ], family = gaussian)

summary(gamma_model)

# Load the package
library(DHARMa)

# Simulate residuals from your gamma model
simulation_output <- simulateResiduals(fittedModel = gamma_model, plot = TRUE)

# Optional: Perform additional tests
testDispersion(simulation_output)
testUniformity(simulation_output)
testOutliers(simulation_output)

# Create a new data frame for predictions
prediction_data <- data.frame(
  Year = seq(min(herb$Year, na.rm = TRUE), max(herb$Year, na.rm = TRUE), length.out = 100),
  lat = mean(herb$lat, na.rm = TRUE)  # Use the mean latitude or adjust as needed
)

# Generate predictions
prediction_data$Predicted_Heterozygosity <- predict(gamma_model, newdata = prediction_data, type = "response")
# Plot one for N and one for S

herb_N <- herb[herb$N_S=="N",]
gamma_model <- lm(Heterozygosity ~ Year, data = herb_N[herb_N$Year <= 1997, ])
summary(gamma_model)
simulation_output <- simulateResiduals(fittedModel = gamma_model, plot = TRUE)
# Optional: Perform additional tests
testDispersion(simulation_output)
testUniformity(simulation_output)
testOutliers(simulation_output)
# Create a new data frame for predictions
prediction_data_N <- data.frame(
  Year = seq(min(herb_N$Year, na.rm = TRUE), max(herb_N$Year, na.rm = TRUE), length.out = 100)
)

# Generate predictions
prediction_data_N$Predicted_Heterozygosity <- predict(gamma_model, newdata = prediction_data_N, type = "response")

herb_S <- herb[herb$N_S=="S",]
gamma_model <- lm(Heterozygosity ~ Year,data = herb_S[herb_S$Year <= 1997, ])
summary(gamma_model)
# Create a new data frame for predictions
prediction_data_S <- data.frame(
  Year = seq(min(herb_S$Year, na.rm = TRUE), max(herb_S$Year, na.rm = TRUE), length.out = 100)
)
# Generate predictions
prediction_data_S$Predicted_Heterozygosity <- predict(gamma_model, newdata = prediction_data_S, type = "response")

herb <- read.csv("Data/herbarium/Input/herbarium_structure_dataframe_1192026.csv",header=TRUE)

herb_M <- herb[herb$N_S=="M",]
gamma_model <- lm(Heterozygosity ~ Year, data = herb_M[herb_M$Year <= 1997, ])
summary(gamma_model)
simulation_output <- simulateResiduals(fittedModel = gamma_model, plot = TRUE)
# Optional: Perform additional tests
testDispersion(simulation_output)
testUniformity(simulation_output)
testOutliers(simulation_output)
# Create a new data frame for predictions
prediction_data_M <- data.frame(
  Year = seq(min(herb_M$Year, na.rm = TRUE), max(herb_M$Year, na.rm = TRUE), length.out = 100)
)

# Generate predictions
prediction_data_M$Predicted_Heterozygosity <- predict(gamma_model, newdata = prediction_data_M, type = "response")

# Extract stats for Panel A (Year models)
f_all_yr  <- summary(lm(Heterozygosity ~ Year * lat, data = herb[herb$Year <= 1997, ]))$fstatistic
f_N_yr    <- summary(lm(Heterozygosity ~ Year, data = herb_N[herb_N$Year <= 1997, ]))$fstatistic
f_S_yr    <- summary(lm(Heterozygosity ~ Year, data = herb_S[herb_S$Year <= 1997, ]))$fstatistic
f_M_yr    <- summary(lm(Heterozygosity ~ Year, data = herb_M[herb_M$Year <= 1997, ]))$fstatistic

p_all_yr  <- pf(f_all_yr[1], f_all_yr[2], f_all_yr[3], lower.tail = FALSE)
p_N_yr    <- pf(f_N_yr[1],   f_N_yr[2],   f_N_yr[3],   lower.tail = FALSE)
p_S_yr    <- pf(f_S_yr[1],   f_S_yr[2],   f_S_yr[3],   lower.tail = FALSE)
p_M_yr    <- pf(f_M_yr[1],   f_M_yr[2],   f_M_yr[3],   lower.tail = FALSE)

m_all_yr <- glm(Heterozygosity ~ Year * lat, 
                data = herb[herb$Year <= 1997, ], 
                family = gaussian)

coefs_yr  <- summary(m_all_yr)$coefficients
f_all_yr  <- summary(lm(Heterozygosity ~ Year * lat, data = herb[herb$Year <= 1997, ]))$fstatistic
p_all_yr  <- pf(f_all_yr[1], f_all_yr[2], f_all_yr[3], lower.tail = FALSE)

ann_yr <- paste0(
  "F(", round(f_all_yr[2],0), ",", round(f_all_yr[3],0), ")=", round(f_all_yr[1],2), ", p=", format.pval(p_all_yr, digits=3), "\n",
  "Year:       B=", round(coefs_yr["Year",        "Estimate"], 6), ", p=", format.pval(coefs_yr["Year",        "Pr(>|t|)"], digits=3), "\n",
  "Lat:        B=", round(coefs_yr["lat",          "Estimate"], 6), ", p=", format.pval(coefs_yr["lat",          "Pr(>|t|)"], digits=3), "\n",
  "Year x Lat: B=", round(coefs_yr["Year:lat",     "Estimate"], 6), ", p=", format.pval(coefs_yr["Year:lat",     "Pr(>|t|)"], digits=3)
)

plot_NS <- ggplot(data = herb[herb$Year <= 1997, ], aes(x = Year, y = Heterozygosity)) +
  geom_point(data = herb, color = "darkgrey", alpha = 0.4) +
  geom_line(data = prediction_data,   aes(x = Year, y = Predicted_Heterozygosity), color = "black",    size = 2, linetype = "dashed") +
  geom_line(data = prediction_data_N, aes(x = Year, y = Predicted_Heterozygosity), color = "blue",     size = 2) +
  geom_line(data = prediction_data_S, aes(x = Year, y = Predicted_Heterozygosity), color = "red",      size = 2) +
  geom_line(data = prediction_data_M, aes(x = Year, y = Predicted_Heterozygosity), color = "lightblue",size = 2) +
  annotate("text", x = -Inf, y = Inf, label = ann_yr,
           hjust = -0.05, vjust = 1.1, size = 3, color = "black", family = "mono") +
  xlab("Year") +
  theme_light() + theme(
    plot.title   = element_text(hjust = 0.5),
    axis.text    = element_text(size = 12, color = "black"),
    axis.title   = element_text(size = 12, color = "black")
  )


############################
#Figure 3################

#Heterozygosity of All 
#####################
#Heterozygosity Graphs#
#####################
library(ggplot2)
library(ggpubr)
library(Rmisc)
library(emmeans)
library(multcomp)
herb <- read.csv("Data/herbarium/Input/herbarium_structure_dataframe_1192026.csv",header=TRUE)

herb <- herb[!is.na(herb$N_S),]

# Fit the Gamma GLM
gamma_model <- glm(Heterozygosity ~ MaxK3_LD * lat, data = herb[herb$MaxK3_LD <= 1997, ], family = gaussian)

summary(gamma_model)

# Load the package
library(DHARMa)

# Simulate residuals from your gamma model
simulation_output <- simulateResiduals(fittedModel = gamma_model, plot = TRUE)

# Optional: Perform additional tests
testDispersion(simulation_output)
testUniformity(simulation_output)
testOutliers(simulation_output)

# Create a new data frame for predictions
prediction_data <- data.frame(
  MaxK3_LD = seq(min(herb$MaxK3_LD, na.rm = TRUE), max(herb$MaxK3_LD, na.rm = TRUE), length.out = 100),
  lat = mean(herb$lat, na.rm = TRUE)  # Use the mean latitude or adjust as needed
)

# Generate predictions
prediction_data$Predicted_Heterozygosity <- predict(gamma_model, newdata = prediction_data, type = "response")
# Plot one for N and one for S

herb_N <- herb[herb$N_S=="N",]
gamma_model <- glm(Heterozygosity ~ MaxK3_LD, data = herb_N[herb_N$MaxK3_LD <= 1997, ], family = gaussian)
summary(gamma_model)
simulation_output <- simulateResiduals(fittedModel = gamma_model, plot = TRUE)
# Optional: Perform additional tests
testDispersion(simulation_output)
testUniformity(simulation_output)
testOutliers(simulation_output)
# Create a new data frame for predictions
prediction_data_N <- data.frame(
  MaxK3_LD = seq(min(herb_N$MaxK3_LD, na.rm = TRUE), max(herb_N$MaxK3_LD, na.rm = TRUE), length.out = 100)
)

# Generate predictions
prediction_data_N$Predicted_Heterozygosity <- predict(gamma_model, newdata = prediction_data_N, type = "response")

herb_S <- herb[herb$N_S=="S",]
gamma_model <- glm(Heterozygosity ~ MaxK3_LD,data = herb_S[herb_S$MaxK3_LD <= 1997, ], family = gaussian)
summary(gamma_model)
# Create a new data frame for predictions
prediction_data_S <- data.frame(
  MaxK3_LD = seq(min(herb_S$MaxK3_LD, na.rm = TRUE), max(herb_S$MaxK3_LD, na.rm = TRUE), length.out = 100)
)
# Generate predictions
prediction_data_S$Predicted_Heterozygosity <- predict(gamma_model, newdata = prediction_data_S, type = "response")

herb <- read.csv("Data/herbarium/Input/herbarium_structure_dataframe_1192026.csv",header=TRUE)

herb_M <- herb[herb$N_S=="M",]
gamma_model <- glm(Heterozygosity ~ MaxK3_LD, data = herb_M[herb_M$MaxK3_LD <= 1997, ], family = gaussian)
summary(gamma_model)
simulation_output <- simulateResiduals(fittedModel = gamma_model, plot = TRUE)
# Optional: Perform additional tests
testDispersion(simulation_output)
testUniformity(simulation_output)
testOutliers(simulation_output)
# Create a new data frame for predictions
prediction_data_M <- data.frame(
  MaxK3_LD = seq(min(herb_M$MaxK3_LD, na.rm = TRUE), max(herb_M$MaxK3_LD, na.rm = TRUE), length.out = 100)
)

# Generate predictions
prediction_data_M$Predicted_Heterozygosity <- predict(gamma_model, newdata = prediction_data_M, type = "response")
f_all_q <- ((m_all_q$null.deviance - m_all_q$deviance) / (m_all_q$df.null - m_all_q$df.residual)) /
             (m_all_q$deviance / m_all_q$df.residual)
f_N_q   <- ((m_N_q$null.deviance - m_N_q$deviance) / (m_N_q$df.null - m_N_q$df.residual)) /
             (m_N_q$deviance / m_N_q$df.residual)
f_S_q   <- ((m_S_q$null.deviance - m_S_q$deviance) / (m_S_q$df.null - m_S_q$df.residual)) /
             (m_S_q$deviance / m_S_q$df.residual)
f_M_q   <- ((m_M_q$null.deviance - m_M_q$deviance) / (m_M_q$df.null - m_M_q$df.residual)) /
             (m_M_q$deviance / m_M_q$df.residual)

p_all_q <- pf(f_all_q, m_all_q$df.null - m_all_q$df.residual, m_all_q$df.residual, lower.tail = FALSE)
p_N_q   <- pf(f_N_q,   m_N_q$df.null   - m_N_q$df.residual,   m_N_q$df.residual,   lower.tail = FALSE)
p_S_q   <- pf(f_S_q,   m_S_q$df.null   - m_S_q$df.residual,   m_S_q$df.residual,   lower.tail = FALSE)
p_M_q   <- pf(f_M_q,   m_M_q$df.null   - m_M_q$df.residual,   m_M_q$df.residual,   lower.tail = FALSE)

m_all_q <- glm(Heterozygosity ~ MaxK3_LD * lat,
               data = herb[!is.na(herb$MaxK3_LD), ],
               family = gaussian)

coefs_q  <- summary(m_all_q)$coefficients
f_all_q  <- ((m_all_q$null.deviance - m_all_q$deviance) / (m_all_q$df.null - m_all_q$df.residual)) /
              (m_all_q$deviance / m_all_q$df.residual)
p_all_q  <- pf(f_all_q, m_all_q$df.null - m_all_q$df.residual, m_all_q$df.residual, lower.tail = FALSE)

ann_q <- paste0(
  "F(", m_all_q$df.null - m_all_q$df.residual, ",", m_all_q$df.residual, ")=", round(f_all_q,2), ", p=", format.pval(p_all_q, digits=3), "\n",
  "MaxQ:       B=", round(coefs_q["MaxK3_LD",        "Estimate"], 6), ", p=", format.pval(coefs_q["MaxK3_LD",        "Pr(>|t|)"], digits=3), "\n",
  "Lat:        B=", round(coefs_q["lat",          "Estimate"], 6), ", p=", format.pval(coefs_q["lat",          "Pr(>|t|)"], digits=3), "\n",
  "MaxQ x Lat: B=", round(coefs_q["MaxK3_LD:lat",     "Estimate"], 6), ", p=", format.pval(coefs_q["MaxK3_LD:lat",     "Pr(>|t|)"], digits=3)
)


plot_NSQ <- ggplot(data = herb[herb$MaxK3_LD <= 1997, ], aes(x = MaxK3_LD, y = Heterozygosity)) +
  geom_point(data = herb, color = "darkgrey", alpha = 0.4) +
  geom_line(data = prediction_data,   aes(x = MaxK3_LD, y = Predicted_Heterozygosity), color = "black",    size = 2, linetype = "dashed") +
  geom_line(data = prediction_data_N, aes(x = MaxK3_LD, y = Predicted_Heterozygosity), color = "blue",     size = 2) +
  geom_line(data = prediction_data_S, aes(x = MaxK3_LD, y = Predicted_Heterozygosity), color = "red",      size = 2) +
  geom_line(data = prediction_data_M, aes(x = MaxK3_LD, y = Predicted_Heterozygosity), color = "lightblue",size = 2) +
  annotate("text", x = -Inf, y = Inf, label = ann_q,
           hjust = -0.05, vjust = 1.1, size = 3, color = "black", family = "mono") +
  xlab("Max Q") +
  theme_light() + theme(
    plot.title = element_text(hjust = 0.5),
    axis.text  = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 12, color = "black")
  )
# Plot out for Average by Year
het_avg <- summarySE(herb[herb$Year<=1997,], measurevar="Heterozygosity", groupvars=c("Year"))
plot_avg <- ggplot(het_avg, aes(x=Year, y=Heterozygosity)) +
  geom_point() +
  geom_line() +
  theme_light() + theme(axis.tick.x=element_text(size=12), axis.text.x=element_text(size=12), axis.title.x = element_blank(), axis.text = element_text(size=12,color="black"),axis.title = element_text(size=12,color="black")) + scale_x_continuous(breaks = seq(1850, 2000, by = 50))+ylab("Average Heterozygosity per Year")

# Combine the plots
combined_panelA <- ggarrange(plot_NS, plot_NSQ, ncol = 2, nrow = 1,
                            labels = c("A","B"))

ggsave("Figure5.pdf", plot = combined_panelA, width = 8, height = 4, units = "in")

# Save Hetonly graphic data 
het_df <- herb[,c("Year","Heterozygosity","N_S","Samp")]
het_df <- het_df[!is.na(het_df$Heterozygosity),]

# #TES Native Range

# library(dplyr)
# library(ggplot2)
# TE_analysis_ready <- read.csv("Data/herbarium/Input/TE_analysis_ready.csv",header=TRUE)

# op <- TE_analysis_ready
# op_standardized <- op
# op_standardized[, 15:9453] <- scale(op[, 15:9453])

# # Reshape the data to a long format for faster grouping and summarization
# long_data <- op_standardized %>%
#   dplyr::select(Samp, 15:9452) %>%
#   tidyr::pivot_longer(cols = -Samp, names_to = "Column", values_to = "Value")

# # Summarize the data grouped by CT and Column
# summary_df <- long_data %>%
#   dplyr::group_by(Samp) %>%
#   dplyr::summarize(Mean = mean(Value, na.rm = TRUE), .groups = "drop")

# # Merge with the original dataframe to get CT information
# summary_df <- merge(summary_df, op_standardized[, c("Samp", "CT","H_C")], by = "Samp")

# # View the summarized dataframe
# print(summary_df)

# summR <- summarySE(summary_df[!is.na(summary_df$Mean),],measurevar = "Mean",groupvars = "CT")

# # Extract the mean and SD for the "Herbarium" entry
# one_mean <- as.numeric(summR[summR$CT == "1838-1877", "Mean"])
# one_sd <- as.numeric(summR[summR$CT == "1838-1877", "sd"])
# two_mean <- as.numeric(summR[summR$CT == "1878-1917", "Mean"])
# two_sd <- as.numeric(summR[summR$CT == "1878-1917", "sd"])
# three_sd <- as.numeric(summR[summR$CT == "1918-1957", "sd"])
# three_mean <- as.numeric(summR[summR$CT == "1918-1957", "Mean"])
# four_sd <- as.numeric(summR[summR$CT == "1958-1997", "sd"])
# four_mean <- as.numeric(summR[summR$CT == "1958-1997", "Mean"])
# five_sd <- as.numeric(summR[summR$CT == "1998-Present", "sd"])
# five_mean <- as.numeric(summR[summR$CT == "1998-Present", "Mean"])

# summR <- summR[!(summR$CT=="1838-1877"),]
# summR <- summR[!(summR$CT=="1878-1917"),]
# summR <- summR[!(summR$CT=="1918-1957"),]
# summR <- summR[!(summR$CT=="1958-1997"),]
# summR <- summR[!(summR$CT=="1998-Present"),]
# summR <- summR[!is.na(summR$CT),]


# rect_data <- data.frame(
#   xmin = -Inf, # Start before the first x-axis value
#   xmax = Inf,  # End after the last x-axis value
#   ymin = 0.3692823 - 0.2126707*2,
#   ymax = 0.3692823 + 0.2126707*2
# )
# # Set the desired order for CT
# summR$CT <- factor(summR$CT, levels = c("Belgium", "Germany", "Greece", "Poland","Sweden","UK","France","Spain"))  # Replace with your actual order

# # Now plot as before
# country <- ggplot(summR, aes(x = CT, y = Mean, color = CT)) +
#     geom_point(size = 3) + 
#     geom_errorbar(aes(ymin = Mean - sd, ymax = Mean + sd), width = 0.2) +
#     geom_rect(data = rect_data, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
#               inherit.aes = FALSE, fill = "red", alpha = 0.1) +
#     theme_light() +
#     theme(
#         legend.position = "null",
#         axis.text = element_text(size = 10, color = "black"),
#         axis.title = element_text(size = 14, color = "black"),
#         axis.title.x = element_blank(),
#         axis.text.x = element_text(angle = 45, hjust = 1)
#     ) +
#     labs(y = "Mean TE Copy Number")+ scale_color_manual(values = c("black","black","black","black","black","darkblue","#E69F00","#56B4E9"))
    


# # ...existing code...
# sumik <- summary_df[summary_df$H_C=="C",]
# # Use the summarized means per CT (from summary_df or summR)
# mod2 <- lm(Mean ~ CT, data = sumik)

# emm2 <- emmeans(mod2, ~ CT)
# pairs_tukey2 <- contrast(emm2, method = "pairwise", adjust = "tukey")
# cld_groups2 <- cld(emm2, Letters = letters, adjust = "tukey")

# print(emm2)
# print(pairs_tukey2)
# print(cld_groups2)
# # ...existing code...

# # Standardize each column (columns 12 to 9450)
# op <- TE_analysis_ready
# op_sub <- op[op$H_C=="H",]
# op_sub <- op_sub[!is.na(op_sub$Year),]
# op_standardized <- op_sub
# op_standardized[, 15:9453] <- scale(op_sub[, 15:9453])

# # Reshape the data to a long format for faster grouping and summarization
# long_data <- op_standardized %>%
#   dplyr::select(Samp, 15:9453) %>%
#   tidyr::pivot_longer(cols = -Samp, names_to = "Column", values_to = "Value")

# # Summarize the data grouped by CT and Column
# summary_df <- long_data %>%
#   dplyr::group_by(Samp) %>%
#   dplyr::summarize(Mean = mean(Value, na.rm = TRUE), .groups = "drop")
  
# summary_df <- merge(summary_df, op_standardized[, c("Samp", "Year","Region")], by = "Samp")

# summR <- summary_df
# summR <- summR[!is.na(summR$Region),]
# summR <- summR[summR$Year <= 1997,]
# # Plot Range on the X-axis and Mean on the Y-axis
# regress <- ggplot(data=summR,aes(x = as.numeric(Year))) +
#     geom_point(size = 3,color="black",alpha=0.4,aes(y=Mean)) +
#     geom_smooth(method="lm",data=summR[summR$Region=="North",],aes(y=Mean),color="blue",se=FALSE,size=2)+
#     geom_smooth(method="lm",data=summR[summR$Region=="South",],aes(y=Mean),color="red",se=FALSE,size=2)+
#     geom_smooth(method="lm",data=summR,aes(y=Mean),color="black",se=FALSE,linetype="dashed",size=2)+
#     theme_light() +
#     theme(legend.position="null", axis.text = element_text(size=12, color="black"), axis.title = element_text(size=12, color="black"), axis.title.x=element_blank()) +
#     labs(y="Mean TE Copy Number")

# summary(lm(Mean ~ Year, data = summR[summR$Region=="North",]))
# summary(lm(Mean ~ Year, data = summR[summR$Region=="South",]))
# summary(lm(Mean ~ Year, data = summR))
# # Combine the plots into a single figure
# library(ggpubr)

# final_figure <- ggarrange(plot_avg,plot_NS,country,regress,ncol=2,nrow=2,labels=c("A","B","C","D"))

library(dplyr)
library(broom)

# Read data
herb <- read.csv("Data/herbarium/Input/herbarium_structure_dataframe_1192026.csv", header = TRUE)
herb <- herb[!is.na(herb$N_S), ]

# Initialize list to store model results
model_results <- list()

# ========== Model 1: Year * lat (All data) ==========
gamma_model_all <- glm(Heterozygosity ~ Year * lat, 
                       data = herb[herb$Year <= 1997, ], 
                       family = gaussian)

model_results[["All_Year_lat"]] <- tidy(gamma_model_all) %>%
  mutate(Model = "All", 
         Predictors = "Year * lat",
         Region = "All")

# ========== Model 2: Year (North only) ==========
herb_N <- herb[herb$N_S == "N", ]
gamma_model_N <- glm(Heterozygosity ~ Year, 
                     data = herb_N[herb_N$Year <= 1997, ], 
                     family = gaussian)

model_results[["North_Year"]] <- tidy(gamma_model_N) %>%
  mutate(Model = "North", 
         Predictors = "Year",
         Region = "North")

# ========== Model 3: Year (South only) ==========
herb_S <- herb[herb$N_S == "S", ]
gamma_model_S <- glm(Heterozygosity ~ Year, 
                     data = herb_S[herb_S$Year <= 1997, ], 
                     family = gaussian)

model_results[["South_Year"]] <- tidy(gamma_model_S) %>%
  mutate(Model = "South", 
         Predictors = "Year",
         Region = "South")

# ========== Combine all model results ==========
all_model_results <- bind_rows(model_results)

# Reorder columns for clarity
all_model_results <- all_model_results %>%
  dplyr::select(Model, Region, Predictors, term, estimate, std.error, statistic, p.value)

# ========== Extract model fit statistics ==========
model_fit <- data.frame(
  Model = c("All_Year_lat", "North_Year", "South_Year"),
  Region = c("All", "North", "South"),
  Predictors = c("Year * lat", "Year", "Year"),
  AIC = c(AIC(gamma_model_all), AIC(gamma_model_N), AIC(gamma_model_S)),
  Deviance = c(deviance(gamma_model_all), deviance(gamma_model_N), deviance(gamma_model_S)),
  Null_Deviance = c(gamma_model_all$null.deviance, gamma_model_N$null.deviance, gamma_model_S$null.deviance),
  DF_Residual = c(gamma_model_all$df.residual, gamma_model_N$df.residual, gamma_model_S$df.residual),
  N_obs = c(nobs(gamma_model_all), nobs(gamma_model_N), nobs(gamma_model_S))
)

# ========== View results ==========
print("Model Coefficients:")
print(all_model_results)

print("\nModel Fit Statistics:")
print(model_fit)

# ========== Save to CSV ==========
write.csv(all_model_results, "gamma_model_coefficients.csv", row.names = FALSE)
write.csv(model_fit, "gamma_model_fit_statistics.csv", row.names = FALSE)

cat("\n========================================\n")
cat("Model summaries extracted and saved!\n")
cat("========================================\n")