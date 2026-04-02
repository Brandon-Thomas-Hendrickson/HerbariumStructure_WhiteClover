library(ggplot2)

op <- read.csv("Data/herbarium/Input/herbarium_structure_dataframe_1192026.csv",header=TRUE)
op <- op[op$Year <= 1997, ]

op_N <- op[op$Region=="North",]

ac_model <- lm(stdAC ~ Year, data = op_N)
summary(ac_model)
prediction_data_N <- data.frame(
  Year = seq(min(op_N$Year, na.rm = TRUE), max(op_N$Year, na.rm = TRUE), length.out = 100)
)

prediction_data_N$Predicted_stdAC <- predict(ac_model, newdata = prediction_data_N)

op_S <- op[op$Region=="South",]

ac_model <- lm(stdAC ~ Year, data = op_S)
summary(ac_model)
prediction_data_S <- data.frame(
  Year = seq(min(op_S$Year, na.rm = TRUE), max(op_S$Year, na.rm = TRUE), length.out = 100)
)
prediction_data_S$Predicted_stdAC <- predict(ac_model, newdata = prediction_data_S)

op_M <- op[op$Region=="Middle",]

ac_model <- lm(stdAC ~ Year, data = op_M)
summary(ac_model)
prediction_data_M <- data.frame(
  Year = seq(min(op_M$Year, na.rm = TRUE), max(op_M$Year, na.rm = TRUE), length.out = 100)
)
prediction_data_M$Predicted_stdAC <- predict(ac_model, newdata = prediction_data_M)

ac_model <- lm(stdAC ~ Year * lat, data = op)
summary(ac_model)
prediction_data <- data.frame(
  Year = seq(min(op$Year, na.rm = TRUE), max(op$Year, na.rm = TRUE), length.out = 100),
  lat = mean(op$lat, na.rm = TRUE) 
)

prediction_data$Predicted_stdAC <- predict(ac_model, newdata = prediction_data)

ac_model_all <- lm(stdAC ~ Year * lat, data = op)
coefs_ac     <- summary(ac_model_all)$coefficients
f_ac         <- summary(ac_model_all)$fstatistic
p_ac         <- pf(f_ac[1], f_ac[2], f_ac[3], lower.tail = FALSE)

ann_ac <- paste0(
  "F(", round(f_ac[2],0), ",", round(f_ac[3],0), ")=", round(f_ac[1],2), ", p=", format.pval(p_ac, digits=3), "\n",
  "Year:       B=", round(coefs_ac["Year",        "Estimate"], 6), ", p=", format.pval(coefs_ac["Year",        "Pr(>|t|)"], digits=3), "\n",
  "Lat:        B=", round(coefs_ac["lat",          "Estimate"], 6), ", p=", format.pval(coefs_ac["lat",          "Pr(>|t|)"], digits=3), "\n",
  "Year x Lat: B=", round(coefs_ac["Year:lat",     "Estimate"], 6), ", p=", format.pval(coefs_ac["Year:lat",     "Pr(>|t|)"], digits=3)
)

plot_NS_AC <- ggplot(data = op, aes(x = Year, y = stdAC)) +
  geom_point(alpha = 0.6, color = "darkgrey") +
  geom_line(data = prediction_data_N, aes(x = Year, y = Predicted_stdAC), color = "blue",     size = 2) +
  geom_line(data = prediction_data_S, aes(x = Year, y = Predicted_stdAC), color = "red",      size = 2) +
  geom_line(data = prediction_data_M, aes(x = Year, y = Predicted_stdAC), color = "lightblue",size = 2) +
  geom_line(data = prediction_data,   aes(x = Year, y = Predicted_stdAC), color = "black",    size = 2, linetype = "dashed") +
  annotate("text", x = -Inf, y = Inf, label = ann_ac,
           hjust = -0.05, vjust = 1.1, size = 3, color = "black", family = "mono") +
  ylab("Standardized AC") +
  theme_light() + theme(
    plot.title   = element_text(hjust = 0.5),
    axis.title.x = element_blank(),
    axis.text    = element_text(size = 12, color = "black"),
    axis.title.y = element_text(size = 12, color = "black")
  )

li_model <- lm(stdLI ~ Year * lat, data = op)

summary(li_model)

prediction_data <- data.frame(
    Year = seq(min(op$Year, na.rm = TRUE), max(op$Year, na.rm = TRUE), length.out = 100),
    lat = mean(op$lat, na.rm = TRUE) 
)

prediction_data$Predicted_stdLI <- predict(li_model, newdata = prediction_data)


op_N <- op[op$Region=="North",]

li_model <- lm(stdLI ~ Year, data = op_N)
summary(li_model)
prediction_data_N <- data.frame(
  Year = seq(min(op_N$Year, na.rm = TRUE), max(op_N$Year, na.rm = TRUE), length.out = 100)
)

prediction_data_N$Predicted_stdLI <- predict(li_model, newdata = prediction_data_N)

op_S <- op[op$Region=="South",]

li_model <- lm(stdLI ~ Year, data = op_S)
summary(li_model)
prediction_data_S <- data.frame(
  Year = seq(min(op_S$Year, na.rm = TRUE), max(op_S$Year, na.rm = TRUE), length.out = 100)
)
prediction_data_S$Predicted_stdLI <- predict(li_model, newdata = prediction_data_S)

op_M <- op[op$Region=="Middle",]

li_model <- lm(stdLI ~ Year, data = op_M)
summary(li_model)
prediction_data_M <- data.frame(
  Year = seq(min(op_M$Year, na.rm = TRUE), max(op_M$Year, na.rm = TRUE), length.out = 100)
)

prediction_data_M$Predicted_stdLI <- predict(li_model, newdata = prediction_data_M)

li_model_all <- lm(stdLI ~ Year * lat, data = op)
coefs_li     <- summary(li_model_all)$coefficients
f_li         <- summary(li_model_all)$fstatistic
p_li         <- pf(f_li[1], f_li[2], f_li[3], lower.tail = FALSE)

ann_li <- paste0(
  "F(", round(f_li[2],0), ",", round(f_li[3],0), ")=", round(f_li[1],2), ", p=", format.pval(p_li, digits=3), "\n",
  "Year:       B=", round(coefs_li["Year",        "Estimate"], 6), ", p=", format.pval(coefs_li["Year",        "Pr(>|t|)"], digits=3), "\n",
  "Lat:        B=", round(coefs_li["lat",          "Estimate"], 6), ", p=", format.pval(coefs_li["lat",          "Pr(>|t|)"], digits=3), "\n",
  "Year x Lat: B=", round(coefs_li["Year:lat",     "Estimate"], 6), ", p=", format.pval(coefs_li["Year:lat",     "Pr(>|t|)"], digits=3)
)

plot_NS_LI <- ggplot(data = op, aes(x = Year, y = stdLI)) +
  geom_point(alpha = 0.6, color = "darkgrey") +
  geom_line(data = prediction_data_N, aes(x = Year, y = Predicted_stdLI), color = "blue",     size = 2) +
  geom_line(data = prediction_data_S, aes(x = Year, y = Predicted_stdLI), color = "red",      size = 2) +
  geom_line(data = prediction_data_M, aes(x = Year, y = Predicted_stdLI), color = "lightblue",size = 2) +
  geom_line(data = prediction_data,   aes(x = Year, y = Predicted_stdLI), color = "black",    size = 2, linetype = "dashed") +
  annotate("text", x = -Inf, y = Inf, label = ann_li,
           hjust = -0.05, vjust = 1.1, size = 3, color = "black", family = "mono") +
  ylab("Standardized LI") +
  theme_light() + theme(
    plot.title   = element_text(hjust = 0.5),
    axis.title.x = element_blank(),
    axis.text    = element_text(size = 12, color = "black"),
    axis.title.y = element_text(size = 12, color = "black")
  )

final_figure <- ggarrange(plot_NS_AC,plot_NS_LI,ncol=2,nrow=1,labels=c("A","B"))

ggsave("Figure5.pdf", plot = final_figure, width = 8, height = 4, units = "in")

op <- op[,c("stdAC","stdLI","Year","Region","Samp","lat","lon")]

library(dplyr)
library(broom)

model_results <- list()

op_N <- op[op$Region == "North", ]
ac_model_N <- lm(stdAC ~ Year, data = op_N)

model_results[["AC_North_Year"]] <- tidy(ac_model_N) %>%
  mutate(Model = "AC_North", 
         Response = "stdAC",
         Predictors = "Year",
         Region = "North")

op_S <- op[op$Region == "South", ]
ac_model_S <- lm(stdAC ~ Year, data = op_S)

model_results[["AC_South_Year"]] <- tidy(ac_model_S) %>%
  mutate(Model = "AC_South", 
         Response = "stdAC",
         Predictors = "Year",
         Region = "South")

ac_model_all <- lm(stdAC ~ Year * lat, data = op)

model_results[["AC_All_Year_lat"]] <- tidy(ac_model_all) %>%
  mutate(Model = "AC_All", 
         Response = "stdAC",
         Predictors = "Year * lat",
         Region = "All")

li_model_all <- lm(stdLI ~ Year * lat, data = op)

model_results[["LI_All_Year_lat"]] <- tidy(li_model_all) %>%
  mutate(Model = "LI_All", 
         Response = "stdLI",
         Predictors = "Year * lat",
         Region = "All")

li_model_N <- lm(stdLI ~ Year, data = op_N)

model_results[["LI_North_Year"]] <- tidy(li_model_N) %>%
  mutate(Model = "LI_North", 
         Response = "stdLI",
         Predictors = "Year",
         Region = "North")

li_model_S <- lm(stdLI ~ Year, data = op_S)

model_results[["LI_South_Year"]] <- tidy(li_model_S) %>%
  mutate(Model = "LI_South", 
         Response = "stdLI",
         Predictors = "Year",
         Region = "South")

all_model_results <- bind_rows(model_results)

# Reorder columns for clarity
all_model_results <- all_model_results %>%
  dplyr::select(Model, Response, Region, Predictors, term, estimate, std.error, statistic, p.value)

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

print(all_model_results)
