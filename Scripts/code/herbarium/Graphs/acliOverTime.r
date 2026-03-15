####################
#AC LI Plotting ####
####################

library(ggplot2)

op <- op[op$Year <= 1997, ]

# Fit the linear regression model
ac_model <- lm(stdAC ~ Year * lat, data = op)

# Summary of the model
summary(ac_model)

# Create a new data frame for predictions
prediction_data <- data.frame(
    Year = seq(min(op$Year, na.rm = TRUE), max(op$Year, na.rm = TRUE), length.out = 100),
    lat = mean(op$lat, na.rm = TRUE)  # Use the mean latitude or adjust as needed
)

# Generate predicted values
prediction_data$Predicted_stdAC <- predict(ac_model, newdata = prediction_data)

# Extract p-values for annotation
p_year <- summary(ac_model)$coefficients["Year", "Pr(>|t|)"]
p_lat <- summary(ac_model)$coefficients["lat", "Pr(>|t|)"]
p_interaction <- summary(ac_model)$coefficients["Year:lat", "Pr(>|t|)"]

# Plot the data with predictions
ac_plot <- ggplot(op, aes(x = Year, y = stdAC)) +
    geom_point(data=op,aes(color = K3Q1S1964)) + scale_color_gradient(low = "grey", high = "#8DD3C7") +
    geom_line(data = prediction_data, aes(x = Year, y = Predicted_stdAC), color = "black", size = 1) +  # Predicted line
    labs(
        y = "stdAC") +
    theme_light() + theme(legend.position="null",axis.text.x=element_blank(),axis.title.x=element_blank(), axis.ticks.x=element_blank())

# Display the plot
print(ac_plot)

op_N <- op[op$Region=="North",]
# Create a new data frame for predictions
prediction_data_N <- data.frame(
  Year = seq(min(op_N$Year, na.rm = TRUE), max(op_N$Year, na.rm = TRUE), length.out = 100),
  lat = mean(op_N$lat, na.rm = TRUE)  # Use the mean latitude or adjust as needed
)

prediction_data_N$Predicted_stdAC <- predict(ac_model, newdata = prediction_data_N)

op_S <- op[op$Region=="South",]

# Create a new data frame for predictions
prediction_data_S <- data.frame(
  Year = seq(min(op_S$Year, na.rm = TRUE), max(op_S$Year, na.rm = TRUE), length.out = 100),
  lat = mean(op_S$lat, na.rm = TRUE)  # Use the mean latitude or adjust as needed
)
# Generate predictions
prediction_data_S$Predicted_stdAC <- predict(ac_model, newdata = prediction_data_S)

plot_NS_AC <- ggplot(data = op[op$Year <= 1997, ], aes(x = Year, y = stdAC)) +
    geom_point(data = op_N, aes(x = Year, y = stdAC), alpha = 0.4, color = "blue") +  
    geom_point(data = op_S, aes(x = Year, y = stdAC), alpha = 0.4, color = "red") + # Original data points
    geom_line(data = prediction_data_N, aes(x = Year, y = Predicted_stdAC), color = "blue", size = 1) +
    geom_line(data = prediction_data_S, aes(x = Year, y = Predicted_stdAC), color = "red", size = 1) + # Predicted lines
    theme_light() + theme(plot.title = element_text(hjust = 0.5),axis.title.x=element_blank(), axis.ticks.x=element_blank(), axis.text.x=element_blank()) + 
        annotate("text", x = 1850, y = max(op$stdAC, na.rm = TRUE) * 0.98, 
             label = expression(p[Year] == 0.005 ~ "," ~ p[Latitude] == 0.007 ~ "," ~ p[Interaction] == 0.006), 
             size = 4, hjust = 0)

# Fit the linear regression model
li_model <- lm(stdLI ~ Year * lat, data = op)

# Summary of the model
summary(li_model)

# Create a new data frame for predictions
prediction_data <- data.frame(
    Year = seq(min(op$Year, na.rm = TRUE), max(op$Year, na.rm = TRUE), length.out = 100),
    lat = mean(op$lat, na.rm = TRUE)  # Use the mean latitude or adjust as needed
)

# Generate predicted values
prediction_data$Predicted_stdLI <- predict(li_model, newdata = prediction_data)

# Extrlit p-values for annotation
p_year <- summary(li_model)$coefficients["Year", "Pr(>|t|)"]
p_lat <- summary(li_model)$coefficients["lat", "Pr(>|t|)"]
p_interlition <- summary(li_model)$coefficients["Year:lat", "Pr(>|t|)"]

# Plot the data with predictions
li_plot <- ggplot(op, aes(x = Year, y = stdLI)) +
    geom_point(data=op,aes(color = K3Q1S1964)) + scale_color_gradient(low = "grey", high = "#8DD3C7") +
    geom_line(data = prediction_data, aes(x = Year, y = Predicted_stdLI), color = "black", size = 1) +  # Predicted line
    labs(
        y = "stdLI") +
    theme_light() + theme(legend.position="null",axis.text.x=element_blank(),axis.title.x=element_blank(), axis.ticks.x=element_blank())

# Display the plot
print(li_plot)

op_N <- op[op$Region=="North",]
# Create a new data frame for predictions
prediction_data_N <- data.frame(
  Year = seq(min(op_N$Year, na.rm = TRUE), max(op_N$Year, na.rm = TRUE), length.out = 100),
  lat = mean(op_N$lat, na.rm = TRUE)  # Use the mean latitude or adjust as needed
)

prediction_data_N$Predicted_stdLI <- predict(li_model, newdata = prediction_data_N)

op_S <- op[op$Region=="South",]

# Create a new data frame for predictions
prediction_data_S <- data.frame(
  Year = seq(min(op_S$Year, na.rm = TRUE), max(op_S$Year, na.rm = TRUE), length.out = 100),
  lat = mean(op_S$lat, na.rm = TRUE)  # Use the mean latitude or adjust as needed
)
# Generate predictions
prediction_data_S$Predicted_stdLI <- predict(li_model, newdata = prediction_data_S)

plot_NS_LI <- ggplot(data = op[op$Year <= 1997, ], aes(x = Year, y = stdLI)) +
    geom_point(data = op_N, aes(x = Year, y = stdLI), alpha = 0.4, color = "blue") +  
    geom_point(data = op_S, aes(x = Year, y = stdLI), alpha = 0.4, color = "red") + # Original data points
    geom_line(data = prediction_data_N, aes(x = Year, y = Predicted_stdLI), color = "blue", size = 1) +
    geom_line(data = prediction_data_S, aes(x = Year, y = Predicted_stdLI), color = "red", size = 1) + # Predicted lines
    theme_light() + theme(plot.title = element_text(hjust = 0.5),axis.title.x=element_blank(), axis.ticks.x=element_blank(), axis.text.x=element_blank()) + 
        annotate("text", x = 1850, y = max(op$stdLI, na.rm = TRUE) * 0.98, 
             label = expression(p[Year] == 0.0007 ~ "," ~ p[Latitude] == 0.003 ~ "," ~ p[Interaction] == 0.002), 
             size = 4, hjust = 0)

# Plot out for Average by Year
het_avg_ac <- summarySE(op[!is.na(op$stdAC),], measurevar="stdAC", groupvars=c("Year"))
plot_avg_ac <- ggplot(het_avg_ac, aes(x=Year, y=stdAC)) +
  geom_point() +
  geom_line() +
  theme_light() + theme(axis.tick.x=element_text(size=12), axis.text.x=element_text(size=12), axis.title.x = element_blank()) + scale_x_continuous(breaks = seq(1850, 2000, by = 25))

# Plot out for Average by Year
het_avg_li <- summarySE(op[!is.na(op$stdLI),], measurevar="stdLI", groupvars=c("Year"))
plot_avg_li <- ggplot(het_avg_li, aes(x=Year, y=stdLI)) +
  geom_point() +
  geom_line() +
  theme_light() + theme(axis.tick.x=element_text(size=12), axis.text.x=element_text(size=12), axis.title.x = element_blank()) + scale_x_continuous(breaks = seq(1850, 2000, by = 25))

combined_plot_ac <- ggarrange(plot_NS_AC, plot_NS_LI, ac_plot,li_plot, plot_avg_ac, plot_avg_li, ncol = 2, nrow = 3,
                            labels = c("A", "B", "C", "D", "E", "F"))
