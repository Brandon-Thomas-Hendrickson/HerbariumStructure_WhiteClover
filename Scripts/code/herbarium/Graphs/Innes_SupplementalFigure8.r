

library(geodata)
library(terra)
df <- read.csv("Data/herbarium/Input/herbarium_structure_dataframe_1192026.csv",header=TRUE)
# Get WorldClim temperature data (mean annual temperature)
# This downloads global temperature data at ~1km resolution
temp_data <- worldclim_global(var = "tmin", res = 10, path = tempdir())

# Convert your lat/long to a spatial points object
coords <- data.frame(lon = df$lon, lat = df$lat)  # Adjust column names as needed
points <- vect(coords, geom = c("lon", "lat"), crs = "EPSG:4326")

# Extract temperature values for your coordinates
temperatures <- extract(temp_data, points)

# Add temperatures back to your dataframe
df$mean_temp <- rowMeans(temperatures[, -1], na.rm = TRUE)
# Calculate percentiles for stdAC and stdLI
percentiles_AC <- quantile(df$stdAC, c(0.1, 0.5, 0.9), na.rm = TRUE)
percentiles_LI <- quantile(df$stdLI, c(0.1, 0.5, 0.9), na.rm = TRUE)

# Create the HCN variable
df$HCN <- ifelse(
    df$stdAC >= percentiles_AC[3] & df$stdLI >= percentiles_LI[3], 
    "Cyanogenic",
    ifelse(
        df$stdAC <= percentiles_AC[1] & df$stdLI <= percentiles_LI[1], 
        "Acyanogenic",
        ifelse(
            df$stdAC >= percentiles_AC[2] & df$stdLI >= percentiles_LI[2], 
            "Possibly Cyanogenic",
            "Possibly Acyanogenic"
        )
    )
)

# Convert to factor with desired order
df$HCN <- factor(df$HCN, levels = c("Cyanogenic", "Possibly Cyanogenic", 
                                    "Possibly Acyanogenic", "Acyanogenic"))
t1 <- df[df$TimeBin=="1918-1957",]
t2 <- df[df$TimeBin=="1958-1997",]
# Create binary HCN grouping
t1$HCN_binary <- ifelse(t1$HCN %in% c("Cyanogenic", "Possibly Cyanogenic"), 
                        1, 0)

t2$HCN_binary <- ifelse(t2$HCN %in% c("Cyanogenic", "Possibly Cyanogenic"), 
                        1, 0)

# Method 1: Logistic regression
model <- glm(stdAC ~ mean_temp, data = t1, family = gaussian())
summary(model)
model <- glm(stdLI ~ mean_temp, data = t1, family = gaussian())
summary(model)
model <- glm(stdAC ~ mean_temp, data = t2, family = gaussian())
summary(model)
model <- glm(stdLI ~ mean_temp, data = t2, family = gaussian())
summary(model)



# Fit logistic regression models for each time period
model_t1 <- glm(HCN_binary ~ mean_temp, data = t1, family = binomial())
model_t2 <- glm(HCN_binary ~ mean_temp, data = t2, family = binomial())

# Check model summaries
summary(model_t1)
summary(model_t2)

# Extract coefficients for each model
coef_t1_ac <- round(coef(lm(stdAC ~ log(mean_temp), data = t1))[2], 4)
coef_t2_ac <- round(coef(lm(stdAC ~ log(mean_temp), data = t2))[2], 4)

coef_t1_li <- round(coef(lm(stdLI ~ log(mean_temp), data = t1))[2], 4)
coef_t2_li <- round(coef(lm(stdLI ~ log(mean_temp), data = t2))[2], 4)

coef_t1_cyano <- round(coef(model_t1)[2], 4)
coef_t2_cyano <- round(coef(model_t2)[2], 4)

# Updated plots with beta estimates
ac <- ggplot(df, aes(x = mean_temp)) +
    geom_smooth(method = "lm", formula = y ~ log(x), data = t2, aes(y = stdAC), color = "purple") + 
    geom_smooth(method = "lm", formula = y ~ log(x), data = t1, aes(y = stdAC), color = "orange") +
    annotate("text", x = max(df$mean_temp, na.rm = TRUE) * 0.8, y = max(df$stdAC, na.rm = TRUE) * 0.9, 
             label = paste("1918-1957: β =", coef_t1_ac), color = "orange", size = 3) +
    annotate("text", x = max(df$mean_temp, na.rm = TRUE) * 0.8, y = max(df$stdAC, na.rm = TRUE) * 0.8, 
             label = paste("1958-1997: β =", coef_t2_ac), color = "purple", size = 3) +
    labs(x = "Mean Temperature (°C)", y = "stdAC") +
    theme_light()

li <- ggplot(df, aes(x = mean_temp)) +
    geom_smooth(method = "lm", formula = y ~ log(x), data = t2, aes(y = stdLI), color = "purple") + 
    geom_smooth(method = "lm", formula = y ~ log(x), data = t1, aes(y = stdLI), color = "orange") +
    annotate("text", x = max(df$mean_temp, na.rm = TRUE) * 0.8, y = max(df$stdLI, na.rm = TRUE) * 0.9, 
             label = paste("1918-1957: β =", coef_t1_li), color = "orange", size = 3) +
    annotate("text", x = max(df$mean_temp, na.rm = TRUE) * 0.8, y = max(df$stdLI, na.rm = TRUE) * 0.8, 
             label = paste("1958-1997: β =", coef_t2_li), color = "purple", size = 3) +
    labs(x = "Mean Temperature (°C)", y = "stdLI") +
    theme_light()

cyano <- ggplot(df, aes(x = mean_temp)) +
    geom_smooth(method = "glm", method.args = list(family = "binomial"),
                data = t1, aes(y = HCN_binary), color = "orange", se = TRUE) +
    geom_smooth(method = "glm", method.args = list(family = "binomial"),
                data = t2, aes(y = HCN_binary), color = "purple", se = TRUE) +
    geom_point(data = t1, aes(y = HCN_binary), color = "orange", alpha = 0.3) +
    geom_point(data = t2, aes(y = HCN_binary), color = "purple", alpha = 0.3) +
    annotate("text", x = max(df$mean_temp, na.rm = TRUE) * 0.8, y = 0.9, 
             label = paste("1918-1957: β =", coef_t1_cyano), color = "orange", size = 3) +
    annotate("text", x = max(df$mean_temp, na.rm = TRUE) * 0.8, y = 0.8, 
             label = paste("1958-1997: β =", coef_t2_cyano), color = "purple", size = 3) +
    labs(x = "Mean Temperature (°C)", y = "Frequency of Cyanogenic Forms") +
    theme_light()


library(ggpubr)

ggarrange(ac,li,cyano,nrow=3,ncol=1)