################################
##PC with Year and Lat Plots######################
################################


hei <- herbarium_structure_dataframe[herbarium_structure_dataframe$H_C == "H" & herbarium_structure_dataframe$Year <= 1997, ]

lm_model <- lm(PC2 ~ Year + lat + Year:lat, 
               data = hei[hei$H_C == "H" & hei$Year <= 1997, ])

# Summary of the model
summary(lm_model)

# Create prediction data for Year
year_pred_data <- data.frame(
  Year = seq(min(hei$Year, na.rm = TRUE), 
             max(hei$Year, na.rm = TRUE), length.out = 100),
  lat = mean(hei$lat, na.rm = TRUE)  # Use mean latitude
)

# Predict PC2 for Year
year_pred_data$PC2 <- predict(lm_model, newdata = year_pred_data)

# Create prediction data for lat
lat_pred_data <- data.frame(
  Year = mean(hei$Year, na.rm = TRUE),  # Use mean year
  lat = seq(min(hei$lat, na.rm = TRUE), 
            max(hei$lat, na.rm = TRUE), length.out = 100)
)

# Predict PC2 for lat
lat_pred_data$PC2 <- predict(lm_model, newdata = lat_pred_data)
# Predict PC2 for lat
lat_pred_data$PC2 <- predict(lm_model, newdata = lat_pred_data)
# Plot PC2 vs Year
plot_year_k3q1 <- ggplot(year_pred_data, aes(x = Year, y = PC2)) +
  geom_line(color = "black", size = 1) +
  geom_point(data = hei[hei$H_C == "H", ], 
             aes(color = K3Q1S1964)) +
  labs(x = "Year", y = "PC2", color = "Q1")  +  scale_color_gradient(low = "grey", high = "#8DD3C7")  + theme_light() +theme(axis.text.x=element_blank(),axis.title.x=element_blank())

# Plot PC2 vs lat
plot_lat_k3q1 <- ggplot(lat_pred_data, aes(x = lat, y = PC2)) +
  geom_line(color = "black", size = 1) +
  geom_point(data = hei[hei$H_C == "H", ], 
             aes(color = K3Q1S1964)) +
  labs(x = "Latitude", y = "PC2", color = "Q1")  +  scale_color_gradient(low = "grey", high = "#8DD3C7")  + theme_light() +theme(axis.text.x=element_blank(),axis.title.x=element_blank())

plot_year_k3q2 <- ggplot(year_pred_data, aes(x = Year, y = PC2)) +
  geom_line(color = "black", size = 1) +
  geom_point(data = hei[hei$H_C == "H", ], 
             aes(color=K3Q2S1964)) +
  labs(x = "Year", y = "PC2", color = "Q2") +  scale_color_gradient(low = "grey", high = "#FFFFB3")  + theme_light() +theme(axis.text.x=element_blank(),axis.title.x=element_blank())

# Plot PC2 vs lat
plot_lat_k3q2 <- ggplot(lat_pred_data, aes(x = lat, y = PC2)) +
  geom_line(color = "black", size = 1) +
  geom_point(data = hei[hei$H_C == "H", ], 
             aes(color=K3Q2S1964)) +
  labs(x = "Latitude", y = "PC2", color = "Q2") +  scale_color_gradient(low = "grey", high = "#FFFFB3")  + theme_light() +theme(axis.text.x=element_blank(),axis.title.x=element_blank())

plot_year_k3q3 <- ggplot(year_pred_data, aes(x = Year, y = PC2)) +
  geom_line(color = "black", size = 1) +
  geom_point(data = hei[hei$H_C == "H", ], 
             aes(color=K3Q3S1964)) +
  labs(x = "Year", y = "PC2", color = "Q3") +  scale_color_gradient(low = "grey", high = "#BEBADA")  + theme_light()

# Plot PC2 vs lat
plot_lat_k3q3 <- ggplot(lat_pred_data, aes(x = lat, y = PC2)) +
  geom_line(color = "black", size = 1) +
  geom_point(data = hei[hei$H_C == "H", ], 
             aes(color=K3Q3S1964)) +
  labs(x = "Latitude", y = "PC2", color = "Q3") +  scale_color_gradient(low = "grey", high = "#BEBADA")  + theme_light()


# Arrange the plots
library(ggpubr)
ggarrange(plot_year_k3q1, plot_lat_k3q1, plot_year_k3q2, plot_lat_k3q2, plot_year_k3q3, plot_lat_k3q3, ncol = 2, nrow = 3,
          labels = c("A", "B", "C", "D", "E", "F"))

