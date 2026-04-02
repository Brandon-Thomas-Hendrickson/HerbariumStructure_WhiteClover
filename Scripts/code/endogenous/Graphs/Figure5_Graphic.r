library(ggplot2)
library(ggpubr)
library(Rmisc)
library(emmeans)
library(multcomp)
herb <- read.csv("Data/herbarium/Input/herbarium_structure_dataframe_1192026.csv",header=TRUE)

herb <- herb[!is.na(herb$N_S),]

gamma_model <- glm(Heterozygosity ~ Year * lat, data = herb[herb$Year <= 1997, ], family = gaussian)

summary(gamma_model)

library(DHARMa)

simulation_output <- simulateResiduals(fittedModel = gamma_model, plot = TRUE)

testDispersion(simulation_output)
testUniformity(simulation_output)
testOutliers(simulation_output)


prediction_data <- data.frame(
  Year = seq(min(herb$Year, na.rm = TRUE), max(herb$Year, na.rm = TRUE), length.out = 100),
  lat = mean(herb$lat, na.rm = TRUE) 
)


prediction_data$Predicted_Heterozygosity <- predict(gamma_model, newdata = prediction_data, type = "response")


herb_N <- herb[herb$N_S=="N",]
gamma_model <- lm(Heterozygosity ~ Year, data = herb_N[herb_N$Year <= 1997, ])
summary(gamma_model)
simulation_output <- simulateResiduals(fittedModel = gamma_model, plot = TRUE)
testDispersion(simulation_output)
testUniformity(simulation_output)
testOutliers(simulation_output)

prediction_data_N <- data.frame(
  Year = seq(min(herb_N$Year, na.rm = TRUE), max(herb_N$Year, na.rm = TRUE), length.out = 100)
)


prediction_data_N$Predicted_Heterozygosity <- predict(gamma_model, newdata = prediction_data_N, type = "response")

herb_S <- herb[herb$N_S=="S",]
gamma_model <- lm(Heterozygosity ~ Year,data = herb_S[herb_S$Year <= 1997, ])
summary(gamma_model)

prediction_data_S <- data.frame(
  Year = seq(min(herb_S$Year, na.rm = TRUE), max(herb_S$Year, na.rm = TRUE), length.out = 100)
)

prediction_data_S$Predicted_Heterozygosity <- predict(gamma_model, newdata = prediction_data_S, type = "response")

herb <- read.csv("Data/herbarium/Input/herbarium_structure_dataframe_1192026.csv",header=TRUE)

herb_M <- herb[herb$N_S=="M",]
gamma_model <- lm(Heterozygosity ~ Year, data = herb_M[herb_M$Year <= 1997, ])
summary(gamma_model)
simulation_output <- simulateResiduals(fittedModel = gamma_model, plot = TRUE)
testDispersion(simulation_output)
testUniformity(simulation_output)
testOutliers(simulation_output)

prediction_data_M <- data.frame(
  Year = seq(min(herb_M$Year, na.rm = TRUE), max(herb_M$Year, na.rm = TRUE), length.out = 100)
)


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

herb <- read.csv("Data/herbarium/Input/herbarium_structure_dataframe_1192026.csv",header=TRUE)

herb <- herb[!is.na(herb$N_S),]

gamma_model <- glm(Heterozygosity ~ MaxK3_LD * lat, data = herb[herb$MaxK3_LD <= 1997, ], family = gaussian)

summary(gamma_model)

library(DHARMa)

simulation_output <- simulateResiduals(fittedModel = gamma_model, plot = TRUE)

testDispersion(simulation_output)
testUniformity(simulation_output)
testOutliers(simulation_output)


prediction_data <- data.frame(
  MaxK3_LD = seq(min(herb$MaxK3_LD, na.rm = TRUE), max(herb$MaxK3_LD, na.rm = TRUE), length.out = 100),
  lat = mean(herb$lat, na.rm = TRUE)
)


prediction_data$Predicted_Heterozygosity <- predict(gamma_model, newdata = prediction_data, type = "response")


herb_N <- herb[herb$N_S=="N",]
gamma_model <- glm(Heterozygosity ~ MaxK3_LD, data = herb_N[herb_N$MaxK3_LD <= 1997, ], family = gaussian)
summary(gamma_model)
simulation_output <- simulateResiduals(fittedModel = gamma_model, plot = TRUE)
testDispersion(simulation_output)
testUniformity(simulation_output)
testOutliers(simulation_output)

prediction_data_N <- data.frame(
  MaxK3_LD = seq(min(herb_N$MaxK3_LD, na.rm = TRUE), max(herb_N$MaxK3_LD, na.rm = TRUE), length.out = 100)
)


prediction_data_N$Predicted_Heterozygosity <- predict(gamma_model, newdata = prediction_data_N, type = "response")

herb_S <- herb[herb$N_S=="S",]
gamma_model <- glm(Heterozygosity ~ MaxK3_LD,data = herb_S[herb_S$MaxK3_LD <= 1997, ], family = gaussian)
summary(gamma_model)

prediction_data_S <- data.frame(
  MaxK3_LD = seq(min(herb_S$MaxK3_LD, na.rm = TRUE), max(herb_S$MaxK3_LD, na.rm = TRUE), length.out = 100)
)

prediction_data_S$Predicted_Heterozygosity <- predict(gamma_model, newdata = prediction_data_S, type = "response")

herb <- read.csv("Data/herbarium/Input/herbarium_structure_dataframe_1192026.csv",header=TRUE)

herb_M <- herb[herb$N_S=="M",]
gamma_model <- glm(Heterozygosity ~ MaxK3_LD, data = herb_M[herb_M$MaxK3_LD <= 1997, ], family = gaussian)
summary(gamma_model)
simulation_output <- simulateResiduals(fittedModel = gamma_model, plot = TRUE)
testDispersion(simulation_output)
testUniformity(simulation_output)
testOutliers(simulation_output)

prediction_data_M <- data.frame(
  MaxK3_LD = seq(min(herb_M$MaxK3_LD, na.rm = TRUE), max(herb_M$MaxK3_LD, na.rm = TRUE), length.out = 100)
)


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

het_avg <- summarySE(herb[herb$Year<=1997,], measurevar="Heterozygosity", groupvars=c("Year"))
plot_avg <- ggplot(het_avg, aes(x=Year, y=Heterozygosity)) +
  geom_point() +
  geom_line() +
  theme_light() + theme(axis.tick.x=element_text(size=12), axis.text.x=element_text(size=12), axis.title.x = element_blank(), axis.text = element_text(size=12,color="black"),axis.title = element_text(size=12,color="black")) + scale_x_continuous(breaks = seq(1850, 2000, by = 50))+ylab("Average Heterozygosity per Year")

combined_panelA <- ggarrange(plot_NS, plot_NSQ, ncol = 2, nrow = 1,
                            labels = c("A","B"))

ggsave("Figure5.pdf", plot = combined_panelA, width = 8, height = 4, units = "in")

het_df <- herb[,c("Year","Heterozygosity","N_S","Samp")]
het_df <- het_df[!is.na(het_df$Heterozygosity),]

library(dplyr)
library(broom)

herb <- read.csv("Data/herbarium/Input/herbarium_structure_dataframe_1192026.csv", header = TRUE)
herb <- herb[!is.na(herb$N_S), ]

model_results <- list()

gamma_model_all <- glm(Heterozygosity ~ Year * lat, 
                       data = herb[herb$Year <= 1997, ], 
                       family = gaussian)

model_results[["All_Year_lat"]] <- tidy(gamma_model_all) %>%
  mutate(Model = "All", 
         Predictors = "Year * lat",
         Region = "All")

herb_N <- herb[herb$N_S == "N", ]
gamma_model_N <- glm(Heterozygosity ~ Year, 
                     data = herb_N[herb_N$Year <= 1997, ], 
                     family = gaussian)

model_results[["North_Year"]] <- tidy(gamma_model_N) %>%
  mutate(Model = "North", 
         Predictors = "Year",
         Region = "North")

herb_S <- herb[herb$N_S == "S", ]
gamma_model_S <- glm(Heterozygosity ~ Year, 
                     data = herb_S[herb_S$Year <= 1997, ], 
                     family = gaussian)

model_results[["South_Year"]] <- tidy(gamma_model_S) %>%
  mutate(Model = "South", 
         Predictors = "Year",
         Region = "South")

all_model_results <- bind_rows(model_results)

all_model_results <- all_model_results %>%
  dplyr::select(Model, Region, Predictors, term, estimate, std.error, statistic, p.value)

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
