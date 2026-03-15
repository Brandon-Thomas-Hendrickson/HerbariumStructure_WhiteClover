#####################
#Heterozygosity Graphs#
#####################
library(ggplot2)
library(ggpubr)
library(Rmisc)
herb <- read.csv("Github_Projects/HerbariumStructure_WhiteClover/Data/herbarium/Input/herbarium_structure_dataframe_542025.csv",header=TRUE)

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

# Plot the data
plot_ALL <- ggplot(data = herb[herb$Year <= 1997, ], aes(x = Year, y = Heterozygosity)) +
  geom_point(alpha = 0.4,color="black") +  # Original data points
  geom_line(data = prediction_data, aes(x = Year, y = Predicted_Heterozygosity), color = "black", size = 1) +  # Predicted line
  theme_light() + theme(plot.title = element_text(hjust = 0.5),axis.title.x=element_blank(), axis.ticks.x=element_blank(), axis.text.x=element_blank(), axis.text = element_text(size=12,color="black"),axis.title = element_text(size=12,color="black"))

# Plot one for N and one for S

herb_N <- herb[herb$N_S=="N",]
# Create a new data frame for predictions
prediction_data_N <- data.frame(
  Year = seq(min(herb_N$Year, na.rm = TRUE), max(herb_N$Year, na.rm = TRUE), length.out = 100),
  lat = mean(herb_N$lat, na.rm = TRUE)  # Use the mean latitude or adjust as needed
)

# Generate predictions
prediction_data_N$Predicted_Heterozygosity <- predict(gamma_model, newdata = prediction_data_N, type = "response")

herb_S <- herb[herb$N_S=="S",]

# Create a new data frame for predictions
prediction_data_S <- data.frame(
  Year = seq(min(herb_S$Year, na.rm = TRUE), max(herb_S$Year, na.rm = TRUE), length.out = 100),
  lat = mean(herb_S$lat, na.rm = TRUE)  # Use the mean latitude or adjust as needed
)
# Generate predictions
prediction_data_S$Predicted_Heterozygosity <- predict(gamma_model, newdata = prediction_data_S, type = "response")

plot_NS <- ggplot(data = herb[herb$Year <= 1997, ], aes(x = Year, y = Heterozygosity)) +
    geom_point(data = herb_N, aes(x = Year, y = Heterozygosity), alpha = 0.4, color = "blue") +  
    geom_point(data = herb_S, aes(x = Year, y = Heterozygosity), alpha = 0.4, color = "red") + # Original data points
    geom_line(data = prediction_data_N, aes(x = Year, y = Predicted_Heterozygosity), color = "blue", size = 1) +
    geom_line(data = prediction_data_S, aes(x = Year, y = Predicted_Heterozygosity), color = "red", size = 1) + # Predicted lines
    theme_light() + theme(plot.title = element_text(hjust = 0.5),axis.title.x=element_blank(), axis.ticks.x=element_blank(), axis.text.x=element_blank(), axis.text = element_text(size=12,color="black"),axis.title = element_text(size=12,color="black")) + 
    annotate("text", x = 1860, y = max(herb$Heterozygosity, na.rm = TRUE) * 0.98, 
             label = expression(p[Year] == 0.002 ~ "," ~ p[Latitude] == 0.006 ~ "," ~ p[Interaction] == 0.007), 
             size = 4, hjust = 0)

# Plot out for Average by Year
het_avg <- summarySE(herb[herb$Year<=1997,], measurevar="Heterozygosity", groupvars=c("Year"))
plot_avg <- ggplot(het_avg, aes(x=Year, y=Heterozygosity)) +
  geom_point() +
  geom_line() +
  theme_light() + theme(axis.tick.x=element_text(size=12), axis.text.x=element_text(size=12), axis.title.x = element_blank(), axis.text = element_text(size=12,color="black"),axis.title = element_text(size=12,color="black")) + scale_x_continuous(breaks = seq(1850, 2000, by = 25))

# Combine the plots
combined_plot <- ggarrange(plot_NS, plot_ALL, plot_avg, ncol = 1, nrow = 3,
                            labels = c("A", "B", "C"))

combined_plot
