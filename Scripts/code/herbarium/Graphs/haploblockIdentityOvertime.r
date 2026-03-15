#############################
#PC of local PCA haploblock ID#
#############################

# Load necessary libraries
library(data.table)

# Load the covariance matrix from a .cov file
# Replace 'your_file.cov' with the path to your .cov file
cov_matrix <- fread("hb7a1_cov.cov", header = FALSE)
cov_matrix <- as.matrix(cov_matrix)

# Perform eigen decomposition
eigen_decomp <- eigen(cov_matrix)

# Extract eigenvalues and eigenvectors
eigenvalues <- eigen_decomp$values
eigenvectors <- eigen_decomp$vectors

# Select the top 10 PC axes
top_10_eigenvectors <- eigenvectors[, 1:10]

# Create a data frame for the top 10 PC axes
df_hb7a1 <- as.data.frame(top_10_eigenvectors)
colnames(df_hb7a1) <- paste0("PC_", 1:10)

names <- read.csv("hb7a1_location_bam_list.csv")

df_hb7a1$sample <- names$Samp

confirm <- read.csv("Chr04_Occ_50000_1350000.hbdat.geno.csv")

merge <- merge(df_hb7a1, confirm, by = "sample", all = TRUE)

merge <- merge[!is.na(merge$PC_1),]

df_hb7a1 <- merge[, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11,18)]

write.csv(df_hb7a1, "hb7a1_pca.csv", row.names = FALSE)

# Load necessary libraries
library(data.table)

# Load the covariance matrix from a .cov file
# Replace 'your_file.cov' with the path to your .cov file
cov_matrix <- fread("hb7a2_cov.cov", header = FALSE)
cov_matrix <- as.matrix(cov_matrix)

# Perform eigen decomposition
eigen_decomp <- eigen(cov_matrix)

# Extract eigenvalues and eigenvectors
eigenvalues <- eigen_decomp$values
eigenvectors <- eigen_decomp$vectors

# Select the top 10 PC axes
top_10_eigenvectors <- eigenvectors[, 1:10]

# Create a data frame for the top 10 PC axes
df_hb7a2 <- as.data.frame(top_10_eigenvectors)
colnames(df_hb7a2) <- paste0("PC_", 1:10)

theta <- 10 * pi / 180

# Create the rotation matrix
rotation_matrix <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), nrow = 2)

# Extract PC_1 and PC_2
pc1_pc2 <- as.matrix(df_hb7a2[, c("PC_1", "PC_2")])

# Apply the rotation
rotated_pc1_pc2 <- pc1_pc2 %*% rotation_matrix

# Update the data frame with the rotated values
df_hb7a2$PC_1_rot <- rotated_pc1_pc2[, 1]
df_hb7a2$PC_2_rot <- rotated_pc1_pc2[, 2]

names <- read.csv("hb7a2_location_bam_list.csv")

df_hb7a2$sample <- names$Samp

confirm <- read.csv("Chr04_Occ_3850000_8450000.hbdat.geno.csv")

merge <- merge(df_hb7a2, confirm, by = "sample", all = TRUE)

merge <- merge[!is.na(merge$PC_1),]

df_hb7a2 <- merge[, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 22)]

write.csv(df_hb7a2, "hb7a2_pca.csv", row.names = FALSE)

# Load necessary libraries
library(data.table)

# Load the covariance matrix from a .cov file
# Replace 'your_file.cov' with the path to your .cov file
cov_matrix <- fread("hb7b_cov.cov", header = FALSE)
cov_matrix <- as.matrix(cov_matrix)

# Perform eigen decomposition
eigen_decomp <- eigen(cov_matrix)

# Extract eigenvalues and eigenvectors
eigenvalues <- eigen_decomp$values
eigenvectors <- eigen_decomp$vectors

# Select the top 10 PC axes
top_10_eigenvectors <- eigenvectors[, 1:10]

# Create a data frame for the top 10 PC axes
df_hb7b <- as.data.frame(top_10_eigenvectors)
colnames(df_hb7b) <- paste0("PC_", 1:10)

theta <- -2.5 * pi / 180
# Create the rotation matrix
rotation_matrix <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), nrow = 2)

# Extract PC_1 and PC_2
pc1_pc2 <- as.matrix(df_hb7b[, c("PC_1", "PC_2")])

# Apply the rotation
rotated_pc1_pc2 <- pc1_pc2 %*% rotation_matrix

# Update the data frame with the rotated values
df_hb7b$PC_1_rot <- rotated_pc1_pc2[, 1]
df_hb7b$PC_2_rot <- rotated_pc1_pc2[, 2]

names <- read.csv("hb7b_location_bam_list.csv")

df_hb7b$sample <- names$Samp

confirm <- read.csv("Chr04_Occ_50850000_54450000.hbdat.geno.csv")

merge <- merge(df_hb7b, confirm, by = "sample", all = TRUE)

merge <- merge[!is.na(merge$PC_1),]

df_hb7b <- merge[, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 22)]

write.csv(df_hb7b, "hb7b_pca.csv", row.names = FALSE)

# Load necessary libraries
library(data.table)

# Load the covariance matrix from a .cov file
# Replace 'your_file.cov' with the path to your .cov file
cov_matrix <- fread("hb9_cov.cov", header = FALSE)
cov_matrix <- as.matrix(cov_matrix)

# Perform eigen decomposition
eigen_decomp <- eigen(cov_matrix)

# Extract eigenvalues and eigenvectors
eigenvalues <- eigen_decomp$values
eigenvectors <- eigen_decomp$vectors

# Select the top 10 PC axes
top_10_eigenvectors <- eigenvectors[, 1:10]

# Create a data frame for the top 10 PC axes
df_hb9 <- as.data.frame(top_10_eigenvectors)
colnames(df_hb9) <- paste0("PC_", 1:10)

theta <- -9 * pi / 180
# Create the rotation matrix
rotation_matrix <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), nrow = 2)

# Extract PC_1 and PC_2
pc1_pc2 <- as.matrix(df_hb9[, c("PC_1", "PC_2")])

# Apply the rotation
rotated_pc1_pc2 <- pc1_pc2 %*% rotation_matrix

# Update the data frame with the rotated values
df_hb9$PC_1_rot <- rotated_pc1_pc2[, 1]
df_hb9$PC_2_rot <- rotated_pc1_pc2[, 2]

names <- read.csv("hb9_location_bam_list.csv")

df_hb9$sample <- names$Samp

confirm <- read.csv("Chr05_Occ_3950000_5050000.hbdat.geno.csv")

merge <- merge(df_hb9, confirm, by = "sample", all = TRUE)

merge <- merge[!is.na(merge$PC_1),]

df_hb9 <- merge[, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 22)]

write.csv(df_hb9, "hb9_pca.csv", row.names = FALSE)

# Load necessary libraries
library(data.table)

# Load the covariance matrix from a .cov file
# Replace 'your_file.cov' with the path to your .cov file
cov_matrix <- fread("hb13_cov.cov", header = FALSE)
cov_matrix <- as.matrix(cov_matrix)

# Perform eigen decomposition
eigen_decomp <- eigen(cov_matrix)

# Extract eigenvalues and eigenvectors
eigenvalues <- eigen_decomp$values
eigenvectors <- eigen_decomp$vectors

# Select the top 10 PC axes
top_10_eigenvectors <- eigenvectors[, 1:10]

# Create a data frame for the top 10 PC axes
df_hb13 <- as.data.frame(top_10_eigenvectors)
colnames(df_hb13) <- paste0("PC_", 1:10)

names <- read.csv("hb13_location_bam_list.csv")

df_hb13$sample <- names$Samp

confirm <- read.csv("Chr07_Occ_50000_1750000.hbdat.geno.csv")

merge <- merge(df_hb13, confirm, by = "sample", all = TRUE)

merge <- merge[!is.na(merge$PC_1),]

df_hb13 <- merge[, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11,20)]

write.csv(df_hb13, "hb13_pca.csv", row.names = FALSE)

# Load necessary libraries
library(ggplot2)
library(ggpubr)
library(Rmisc)

op <- read.csv("Github_Projects/HerbariumStructure_WhiteClover/Data/herbarium/Input/herbarium_structure_dataframe_542025.csv",header=TRUE)

op <- op[op$H_C=="H",]

op <- op[op$Year <= 1997, ]

# Fit the linear regression model
hb7a1_model <- lm(geno_Hb7a1 ~ Year * lat, data = op)

# Summary of the model
summary(hb7a1_model)

# Create a new data frame for predictions
prediction_data <- data.frame(
    Year = seq(min(op$Year, na.rm = TRUE), max(op$Year, na.rm = TRUE), length.out = 100),
    lat = mean(op$lat, na.rm = TRUE)  # Use the mean latitude or adjust as needed
)

# Generate predicted values
prediction_data$Predicted_geno_Hb7a1 <- predict(hb7a1_model, newdata = prediction_data)

# Extract p-values for annotation
p_year <- summary(hb7a1_model)$coefficients["Year", "Pr(>|t|)"]
p_lat <- summary(hb7a1_model)$coefficients["lat", "Pr(>|t|)"]
p_interaction <- summary(hb7a1_model)$coefficients["Year:lat", "Pr(>|t|)"]

# Plot the data with predictions
hb7a1_plot <- ggplot(op, aes(x = Year, y = geno_Hb7a1)) +
    geom_point(alpha = 0.4, color = "darkgrey") +  # Original data points
    geom_line(data = prediction_data, aes(x = Year, y = Predicted_geno_Hb7a1), color = "black", size = 1) +  # Predicted line
    labs(
        y = "geno_Hb7a1") +
    theme_light() + theme(axis.text.x=element_blank(),axis.title.x=element_blank(), axis.ticks.x=element_blank(),axis.title.y=element_blank())

# Display the plot
print(hb7a1_plot)

op_N <- op[op$Region=="North",]
# Create a new data frame for predictions
prediction_data_N <- data.frame(
  Year = seq(min(op_N$Year, na.rm = TRUE), max(op_N$Year, na.rm = TRUE), length.out = 100),
  lat = mean(op_N$lat, na.rm = TRUE)  # Use the mean latitude or adjust as needed
)

prediction_data_N$Predicted_geno_Hb7a1 <- predict(hb7a1_model, newdata = prediction_data_N)

op_S <- op[op$Region=="South",]

# Create a new data frame for predictions
prediction_data_S <- data.frame(
  Year = seq(min(op_S$Year, na.rm = TRUE), max(op_S$Year, na.rm = TRUE), length.out = 100),
  lat = mean(op_S$lat, na.rm = TRUE)  # Use the mean latitude or adjust as needed
)
# Generate predictions
prediction_data_S$Predicted_geno_Hb7a1 <- predict(hb7a1_model, newdata = prediction_data_S)

plot_NS_hb7a1 <- ggplot(data = op[op$Year <= 1997, ], aes(x = Year, y = geno_Hb7a1)) +
    geom_point(data = op_N, aes(x = Year, y = geno_Hb7a1), alpha = 0.4, color = "blue") +  
    geom_point(data = op_S, aes(x = Year, y = geno_Hb7a1), alpha = 0.4, color = "red") + ylab("HB7a1") +
    geom_line(data = prediction_data_N, aes(x = Year, y = Predicted_geno_Hb7a1), color = "blue", size = 1) +
    geom_line(data = prediction_data_S, aes(x = Year, y = Predicted_geno_Hb7a1), color = "red", size = 1) + # Predicted lines
    theme_light() + theme(plot.title = element_text(hjust = 0.5),axis.title.x=element_blank(), axis.ticks.x=element_blank(), axis.text.x=element_blank()) + 
        annotate("text", x = 1850, y = max(op$geno_Hb7a1, na.rm = TRUE) * 0.85, 
             label = expression(p[Year] == 0.133 ~ "," ~ p[Latitude] == 0.103 ~ "," ~ p[Interaction] == 0.102), 
             size = 4, hjust = 0)

# Plot out for Average by Year
het_Hb7a1 <- summarySE(op[!is.na(op$geno_Hb7a1),], measurevar="geno_Hb7a1", groupvars=c("Year"))
plot_Hb7a1 <- ggplot(het_Hb7a1, aes(x=Year, y=geno_Hb7a1)) +
  geom_point() +
  geom_line() +
  theme_light() + theme(axis.tick.x=element_text(size=12), axis.text.x=element_text(size=12), axis.title.x = element_blank()) + scale_x_continuous(breaks = seq(1850, 2000, by = 25))

op <- op[op$H_C=="H",]

op <- op[op$Year <= 1997, ]

# Fit the linear regression model
Hb7a2_model <- lm(geno_Hb7a2 ~ Year * lat, data = op)

# Summary of the model
summary(Hb7a2_model)

# Create a new data frame for predictions
prediction_data <- data.frame(
    Year = seq(min(op$Year, na.rm = TRUE), max(op$Year, na.rm = TRUE), length.out = 100),
    lat = mean(op$lat, na.rm = TRUE)  # Use the mean latitude or adjust as needed
)

# Generate predicted values
prediction_data$Predicted_geno_Hb7a2 <- predict(Hb7a2_model, newdata = prediction_data)

# Extract p-values for annotation
p_year <- summary(Hb7a2_model)$coefficients["Year", "Pr(>|t|)"]
p_lat <- summary(Hb7a2_model)$coefficients["lat", "Pr(>|t|)"]
p_interaction <- summary(Hb7a2_model)$coefficients["Year:lat", "Pr(>|t|)"]

# Plot the data with predictions
hb7a2_plot <- ggplot(op, aes(x = Year, y = geno_Hb7a2)) +
    geom_point(alpha = 0.4, color = "darkgrey") +  # Original data points
    geom_line(data = prediction_data, aes(x = Year, y = Predicted_geno_Hb7a2), color = "black", size = 1) +  # Predicted line
    labs(
        y = "geno_Hb7a2") +
    theme_light() + theme(axis.text.x=element_blank(),axis.title.x=element_blank(), axis.ticks.x=element_blank(),axis.title.y=element_blank())

# Display the plot
print(hb7a1_plot)

op_N <- op[op$Region=="North",]
# Create a new data frame for predictions
prediction_data_N <- data.frame(
  Year = seq(min(op_N$Year, na.rm = TRUE), max(op_N$Year, na.rm = TRUE), length.out = 100),
  lat = mean(op_N$lat, na.rm = TRUE)  # Use the mean latitude or adjust as needed
)

prediction_data_N$Predicted_geno_Hb7a2 <- predict(Hb7a2_model, newdata = prediction_data_N)

op_S <- op[op$Region=="South",]

# Create a new data frame for predictions
prediction_data_S <- data.frame(
  Year = seq(min(op_S$Year, na.rm = TRUE), max(op_S$Year, na.rm = TRUE), length.out = 100),
  lat = mean(op_S$lat, na.rm = TRUE)  # Use the mean latitude or adjust as needed
)
# Generate predictions
prediction_data_S$Predicted_geno_Hb7a2 <- predict(Hb7a2_model, newdata = prediction_data_S)

plot_NS_hb7a2 <- ggplot(data = op[op$Year <= 1997, ], aes(x = Year, y = geno_Hb7a2)) +
    geom_point(data = op_N, aes(x = Year, y = geno_Hb7a2), alpha = 0.4, color = "blue") +  
    geom_point(data = op_S, aes(x = Year, y = geno_Hb7a2), alpha = 0.4, color = "red") + ylab("HB7a2") +
    geom_line(data = prediction_data_N, aes(x = Year, y = Predicted_geno_Hb7a2), color = "blue", size = 1) +
    geom_line(data = prediction_data_S, aes(x = Year, y = Predicted_geno_Hb7a2), color = "red", size = 1) + # Predicted lines
    theme_light() + theme(plot.title = element_text(hjust = 0.5),axis.title.x=element_blank(), axis.ticks.x=element_blank(), axis.text.x=element_blank()) + 
        annotate("text", x = 1850, y = max(op$geno_Hb7a2, na.rm = TRUE) * 0.85, 
             label = expression(p[Year] == 0.214 ~ "," ~ p[Latitude] == 0.277 ~ "," ~ p[Interaction] == 0.280), 
             size = 4, hjust = 0)

# Plot out for Average by Year
het_Hb7a2 <- summarySE(op[!is.na(op$geno_Hb7a2),], measurevar="geno_Hb7a2", groupvars=c("Year"))
plot_Hb7a2 <- ggplot(het_Hb7a2, aes(x=Year, y=geno_Hb7a2)) +
  geom_point() +
  geom_line() +
  theme_light() + theme(axis.tick.x=element_text(size=12), axis.text.x=element_text(size=12), axis.title.x = element_blank()) + scale_x_continuous(breaks = seq(1850, 2000, by = 25))

op <- op[op$H_C=="H",]

op <- op[op$Year <= 1997, ]

# Fit the linear regression model
Hb7b_model <- lm(geno_Hb7b ~ Year * lat, data = op)

# Summary of the model
summary(Hb7b_model)

# Create a new data frame for predictions
prediction_data <- data.frame(
    Year = seq(min(op$Year, na.rm = TRUE), max(op$Year, na.rm = TRUE), length.out = 100),
    lat = mean(op$lat, na.rm = TRUE)  # Use the mean latitude or adjust as needed
)

# Generate predicted values
prediction_data$Predicted_geno_Hb7b <- predict(Hb7b_model, newdata = prediction_data)

# Extract p-values for annotation
p_year <- summary(Hb7b_model)$coefficients["Year", "Pr(>|t|)"]
p_lat <- summary(Hb7b_model)$coefficients["lat", "Pr(>|t|)"]
p_interaction <- summary(Hb7b_model)$coefficients["Year:lat", "Pr(>|t|)"]

# Plot the data with predictions
hb7b_plot <- ggplot(op, aes(x = Year, y = geno_Hb7b)) +
    geom_point(alpha = 0.4, color = "darkgrey") +  # Original data points
    geom_line(data = prediction_data, aes(x = Year, y = Predicted_geno_Hb7b), color = "black", size = 1) +  # Predicted line
    labs(
        y = "geno_Hb7b") +
    theme_light() + theme(axis.text.x=element_blank(),axis.title.x=element_blank(), axis.ticks.x=element_blank(),axis.title.y=element_blank())

# Display the plot
print(hb7b_plot)

op_N <- op[op$Region=="North",]
# Create a new data frame for predictions
prediction_data_N <- data.frame(
  Year = seq(min(op_N$Year, na.rm = TRUE), max(op_N$Year, na.rm = TRUE), length.out = 100),
  lat = mean(op_N$lat, na.rm = TRUE)  # Use the mean latitude or adjust as needed
)

prediction_data_N$Predicted_geno_Hb7b <- predict(Hb7b_model, newdata = prediction_data_N)

op_S <- op[op$Region=="South",]

# Create a new data frame for predictions
prediction_data_S <- data.frame(
  Year = seq(min(op_S$Year, na.rm = TRUE), max(op_S$Year, na.rm = TRUE), length.out = 100),
  lat = mean(op_S$lat, na.rm = TRUE)  # Use the mean latitude or adjust as needed
)
# Generate predictions
prediction_data_S$Predicted_geno_Hb7b <- predict(Hb7b_model, newdata = prediction_data_S)

plot_NS_Hb7b <- ggplot(data = op[op$Year <= 1997, ], aes(x = Year, y = geno_Hb7b)) +
    geom_point(data = op_N, aes(x = Year, y = geno_Hb7b), alpha = 0.4, color = "blue") +  
    geom_point(data = op_S, aes(x = Year, y = geno_Hb7b), alpha = 0.4, color = "red") + ylab("HB7b") +
    geom_line(data = prediction_data_N, aes(x = Year, y = Predicted_geno_Hb7b), color = "blue", size = 1) +
    geom_line(data = prediction_data_S, aes(x = Year, y = Predicted_geno_Hb7b), color = "red", size = 1) + # Predicted lines
    theme_light() + theme(plot.title = element_text(hjust = 0.5),axis.title.x=element_blank(), axis.ticks.x=element_blank(), axis.text.x=element_blank()) + 
        annotate("text", x = 1850, y = max(op$geno_Hb7b, na.rm = TRUE) * 0.85, 
             label = expression(p[Year] == 0.505 ~ "," ~ p[Latitude] == 0.271 ~ "," ~ p[Interaction] == 0.281), 
             size = 4, hjust = 0)

# Plot out for Average by Year
het_Hb7b <- summarySE(op[!is.na(op$geno_Hb7b),], measurevar="geno_Hb7b", groupvars=c("Year"))
plot_Hb7b <- ggplot(het_Hb7b, aes(x=Year, y=geno_Hb7b)) +
  geom_point() +
  geom_line() +
  theme_light() + theme(axis.tick.x=element_text(size=12), axis.text.x=element_text(size=12), axis.title.x = element_blank()) + scale_x_continuous(breaks = seq(1850, 2000, by = 25))

op <- op[op$H_C=="H",]

op <- op[op$Year <= 1997, ]

# Fit the linear regression model
Hb9_model <- lm(geno_Hb9 ~ Year * lat, data = op)

# Summary of the model
summary(Hb9_model)

# Create a new data frame for predictions
prediction_data <- data.frame(
    Year = seq(min(op$Year, na.rm = TRUE), max(op$Year, na.rm = TRUE), length.out = 100),
    lat = mean(op$lat, na.rm = TRUE)  # Use the mean latitude or adjust as needed
)

# Generate predicted values
prediction_data$Predicted_geno_Hb9 <- predict(Hb9_model, newdata = prediction_data)

# Extract p-values for annotation
p_year <- summary(Hb9_model)$coefficients["Year", "Pr(>|t|)"]
p_lat <- summary(Hb9_model)$coefficients["lat", "Pr(>|t|)"]
p_interaction <- summary(Hb9_model)$coefficients["Year:lat", "Pr(>|t|)"]

# Plot the data with predictions
Hb9_plot <- ggplot(op, aes(x = Year, y = geno_Hb9)) +
    geom_point(alpha = 0.4, color = "darkgrey") +  # Original data points
    geom_line(data = prediction_data, aes(x = Year, y = Predicted_geno_Hb9), color = "black", size = 1) +  # Predicted line
    labs(
        y = "geno_Hb9") +
    theme_light() + theme(axis.text.x=element_blank(),axis.title.x=element_blank(), axis.ticks.x=element_blank(),axis.title.y=element_blank())

# Display the plot
print(Hb9_plot)

op_N <- op[op$Region=="North",]
# Create a new data frame for predictions
prediction_data_N <- data.frame(
  Year = seq(min(op_N$Year, na.rm = TRUE), max(op_N$Year, na.rm = TRUE), length.out = 100),
  lat = mean(op_N$lat, na.rm = TRUE)  # Use the mean latitude or adjust as needed
)

prediction_data_N$Predicted_geno_Hb9 <- predict(Hb9_model, newdata = prediction_data_N)

op_S <- op[op$Region=="South",]

# Create a new data frame for predictions
prediction_data_S <- data.frame(
  Year = seq(min(op_S$Year, na.rm = TRUE), max(op_S$Year, na.rm = TRUE), length.out = 100),
  lat = mean(op_S$lat, na.rm = TRUE)  # Use the mean latitude or adjust as needed
)
# Generate predictions
prediction_data_S$Predicted_geno_Hb9 <- predict(Hb9_model, newdata = prediction_data_S)

plot_NS_Hb9 <- ggplot(data = op[op$Year <= 1997, ], aes(x = Year, y = geno_Hb9)) +
    geom_point(data = op_N, aes(x = Year, y = geno_Hb9), alpha = 0.4, color = "blue") +  
    geom_point(data = op_S, aes(x = Year, y = geno_Hb9), alpha = 0.4, color = "red") + ylab("HB9") +
    geom_line(data = prediction_data_N, aes(x = Year, y = Predicted_geno_Hb9), color = "blue", size = 1) +
    geom_line(data = prediction_data_S, aes(x = Year, y = Predicted_geno_Hb9), color = "red", size = 1) + # Predicted lines
    theme_light() + theme(plot.title = element_text(hjust = 0.5),axis.title.x=element_blank(), axis.ticks.x=element_blank(), axis.text.x=element_blank()) + 
        annotate("text", x = 1850, y = max(op$geno_Hb9, na.rm = TRUE) * 0.85, 
             label = expression(p[Year] == 0.193 ~ "," ~ p[Latitude] == 0.377 ~ "," ~ p[Interaction] == 0.377), 
             size = 4, hjust = 0)

# Plot out for Average by Year
het_Hb9 <- summarySE(op[!is.na(op$geno_Hb9),], measurevar="geno_Hb9", groupvars=c("Year"))
plot_Hb9 <- ggplot(het_Hb9, aes(x=Year, y=geno_Hb9)) +
  geom_point() +
  geom_line() +
  theme_light() + theme(axis.tick.x=element_text(size=12), axis.text.x=element_text(size=12), axis.title.x = element_blank()) + scale_x_continuous(breaks = seq(1850, 2000, by = 25))

op <- op[op$H_C=="H",]

op <- op[op$Year <= 1997, ]

# Fit the linear regression model
Hb13_model <- lm(geno_Hb13 ~ Year * lat, data = op)

# Summary of the model
summary(Hb13_model)

# Create a new data frame for predictions
prediction_data <- data.frame(
    Year = seq(min(op$Year, na.rm = TRUE), max(op$Year, na.rm = TRUE), length.out = 100),
    lat = mean(op$lat, na.rm = TRUE)  # Use the mean latitude or adjust as needed
)

# Generate predicted values
prediction_data$Predicted_geno_Hb13 <- predict(Hb13_model, newdata = prediction_data)

# Extract p-values for annotation
p_year <- summary(Hb13_model)$coefficients["Year", "Pr(>|t|)"]
p_lat <- summary(Hb13_model)$coefficients["lat", "Pr(>|t|)"]
p_interaction <- summary(Hb13_model)$coefficients["Year:lat", "Pr(>|t|)"]

# Plot the data with predictions
Hb13_plot <- ggplot(op, aes(x = Year, y = geno_Hb13)) +
    geom_point(alpha = 0.4, color = "darkgrey") +  # Original data points
    geom_line(data = prediction_data, aes(x = Year, y = Predicted_geno_Hb13), color = "black", size = 1) +  # Predicted line
    labs(
        y = "geno_Hb13") +
    theme_light() + theme(axis.text.x=element_text(size=12,color="black"),axis.title.x=element_blank(),axis.title.y=element_blank())

# Display the plot
print(Hb13_plot)

op_N <- op[op$Region=="North",]
# Create a new data frame for predictions
prediction_data_N <- data.frame(
  Year = seq(min(op_N$Year, na.rm = TRUE), max(op_N$Year, na.rm = TRUE), length.out = 100),
  lat = mean(op_N$lat, na.rm = TRUE)  # Use the mean latitude or adjust as needed
)

prediction_data_N$Predicted_geno_Hb13 <- predict(Hb13_model, newdata = prediction_data_N)

op_S <- op[op$Region=="South",]

# Create a new data frame for predictions
prediction_data_S <- data.frame(
  Year = seq(min(op_S$Year, na.rm = TRUE), max(op_S$Year, na.rm = TRUE), length.out = 100),
  lat = mean(op_S$lat, na.rm = TRUE)  # Use the mean latitude or adjust as needed
)
# Generate predictions
prediction_data_S$Predicted_geno_Hb13 <- predict(Hb13_model, newdata = prediction_data_S)

plot_NS_Hb13 <- ggplot(data = op[op$Year <= 1997, ], aes(x = Year, y = geno_Hb13)) +
    geom_point(data = op_N, aes(x = Year, y = geno_Hb13), alpha = 0.4, color = "blue") +  
    geom_point(data = op_S, aes(x = Year, y = geno_Hb13), alpha = 0.4, color = "red") + ylab("HB13") +
    geom_line(data = prediction_data_N, aes(x = Year, y = Predicted_geno_Hb13), color = "blue", size = 1) +
    geom_line(data = prediction_data_S, aes(x = Year, y = Predicted_geno_Hb13), color = "red", size = 1) + # Predicted lines
    theme_light() + theme(axis.text.x=element_text(size=12,color="black"),axis.title.x=element_blank()) + 
        annotate("text", x = 1850, y = max(op$geno_Hb13, na.rm = TRUE) * 0.85, 
             label = expression(p[Year] == 0.529 ~ "," ~ p[Latitude] == 0.506 ~ "," ~ p[Interaction] == 0.494), 
             size = 4, hjust = 0) 

# Plot out for Average by Year
het_Hb13 <- summarySE(op[!is.na(op$geno_Hb13),], measurevar="geno_Hb13", groupvars=c("Year"))
plot_Hb13 <- ggplot(het_Hb13, aes(x=Year, y=geno_Hb13)) +
  geom_point() +
  geom_line() +
  theme_light() + theme(axis.tick.x=element_text(size=12), axis.text.x=element_text(size=12), axis.title.x = element_blank()) + scale_x_continuous(breaks = seq(1850, 2000, by = 25))

# Combine all plots into a single figure
combined_plot <- ggarrange(plot_NS_hb7a1, hb7a1_plot,plot_NS_hb7a2, hb7a2_plot,plot_NS_Hb7b, hb7b_plot,plot_NS_Hb9, Hb9_plot, plot_NS_Hb13, Hb13_plot,
                            ncol = 2, nrow = 5,align="hv")