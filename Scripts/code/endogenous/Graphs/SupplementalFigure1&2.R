# Load in the herbarium data
df <- read.csv("Data/herbarium/Input/herbarium_structure_dataframe_1192026.csv",header=TRUE)
colnames(df)[33]<-"Latitude"
df$Latitude <- as.numeric(df$Latitude)
df1 <- df
df <- df[df$H_C=="H",]
# First, let's examine the data structure and distribution
print("Data overview:")
str(df[c("Latitude", "PC_LD2")])
summary(df[c("Latitude", "PC_LD2")])

# Check for missing values
print("\nMissing values:")
colSums(is.na(df[c("Latitude", "PC_LD2")]))

# Remove any rows with missing values in key variables
df_clean <- df[!is.na(df$Latitude) & !is.na(df$PC_LD2), ]
print(paste("Observations after removing missing values:", nrow(df_clean)))

# Perform logarithmic regression on Latitude and PC_LD2
model <- lm(PC_LD2 ~ log(abs(Latitude)), data = df_clean)
print("\nRegression model summary:")
summary(model)

# Create visualizations to understand the relationship
library(ggplot2)

# Plot 1: Scatter plot with regression line
p1 <- ggplot(df_clean, aes(x = Latitude, y = PC_LD2)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_smooth(method = "lm", formula = y ~ log(abs(x)), color = "red") +
  labs(title = "PC_LD2 vs Latitude with Log Regression Line",
       x = "Latitude", y = "PC_LD2 (Ancestry)") +
  theme_minimal()

print(p1)

# Calculate fitted values and residuals
df_clean$fitted <- predict(model)
df_clean$residuals <- residuals(model)

# Method 1: Find the midpoint of the regression line
lat_range <- range(df_clean$Latitude)
lat_midpoint <- mean(lat_range)
print(paste("\nSimple midpoint of latitude range:", round(lat_midpoint, 2)))

# Method 2: Find the latitude where the regression prediction equals the mean of PC_LD2
mean_PC_LD2 <- mean(df_clean$PC_LD2)
# For log regression: PC_LD2 = intercept + slope * log(abs(Latitude))
# Solve for Latitude when PC_LD2 = mean_PC_LD2
intercept <- coef(model)[1]
slope <- coef(model)[2]
optimal_lat <- exp((mean_PC_LD2 - intercept) / slope)
print(paste("Latitude where regression equals mean PC_LD2:", round(optimal_lat, 2)))

# Method 3: Use changepoint detection to find natural breakpoints
library(changepoint)
# Sort data by latitude for changepoint analysis
df_sorted <- df_clean[order(df_clean$Latitude), ]
cpt_analysis <- cpt.mean(df_sorted$PC_LD2, method = "PELT")
changepoints <- cpts(cpt_analysis)

if(length(changepoints) > 0) {
  # Get the latitude values at changepoints
  changepoint_lats <- df_sorted$Latitude[changepoints]
  print(paste("\nChangepoints detected at latitudes:", paste(round(changepoint_lats, 2), collapse = ", ")))
  
  # If multiple changepoints, choose the one closest to the middle
  if(length(changepoint_lats) > 1) {
    middle_idx <- which.min(abs(changepoint_lats - lat_midpoint))
    optimal_changepoint <- changepoint_lats[middle_idx]
  } else {
    optimal_changepoint <- changepoint_lats[1]
  }
  print(paste("Selected changepoint for north/south division:", round(optimal_changepoint, 2)))
} else {
  print("\nNo significant changepoints detected")
  optimal_changepoint <- optimal_lat
}

# Method 4: K-means clustering to identify natural groupings
set.seed(123)
kmeans_result <- kmeans(df_clean[c("Latitude", "PC_LD2")], centers = 2)
df_clean$cluster <- kmeans_result$cluster

# Find the latitude that best separates the clusters
cluster_centers <- aggregate(Latitude ~ cluster, df_clean, mean)
cluster_boundary <- mean(cluster_centers$Latitude)
print(paste("\nK-means cluster boundary at latitude:", round(cluster_boundary, 2)))

# Create the final north/south classification
# Use the changepoint if available, otherwise use the regression-based method
final_breakpoint <- if(exists("optimal_changepoint")) optimal_changepoint else optimal_lat

df_clean$Region <- ifelse(df_clean$Latitude >= final_breakpoint, "North", "South")

# Summary of the classification
print(paste("\nFinal breakpoint chosen:", round(final_breakpoint, 2)))
print("\nRegion classification summary:")
table(df_clean$Region)

# Validate the separation quality
print("\nMean PC_LD2 by Region:")
aggregate(PC_LD2 ~ Region, df_clean, function(x) c(mean = mean(x), sd = sd(x)))

# Plot the final classification
# Plot the final classification
p2 <- ggplot(df_clean, aes(x = Latitude, y = PC_LD2, color = Region)) +
geom_point(data = df_clean[df_clean$Latitude>40,], alpha = 0.7, color="blue") +
geom_point(data = df_clean[df_clean$Latitude<35,], alpha = 0.7, color="red") +
geom_point(data = df_clean[df_clean$Latitude>=35 & df_clean$Latitude<=40,], alpha = 0.7, color="lightblue") +
  geom_vline(xintercept = final_breakpoint, linetype = "dashed", color = "black", size = 1) +
  geom_smooth(method = "lm", formula = y ~ log(abs(x)), se = FALSE) +
  labs( x = "Latitude", y = "PC_LD2", color = "Region") +
  theme_light() + scale_color_manual(values=c("North"="blue","South"="red","Transition"="lightblue")) + theme(text=element_text(size=12),legend.position="none",axis.text=element_text(size=12,color="black"),axis.title=element_text(size=12,color="black"))+xlim(c(27,50))

print(p2)

df_clean <- df_clean[,c("Latitude","PC_LD2","Region","Samp")]
# Statistical test to validate the separation
t_test_result <- t.test(PC_LD2 ~ Region, data = df_clean)
print("\nT-test comparing PC_LD2 between North and South Regions:")
print(t_test_result)

# Additional analysis: Effect size (Cohen's d)
library(effsize)
cohen_d <- cohen.d(df_clean$PC_LD2[df_clean$Region == "North"], 
                   df_clean$PC_LD2[df_clean$Region == "South"])
print("\nEffect size (Cohen's d):")
print(cohen_d)

# Save the results
df_clean_with_Regions <- df_clean[c("Latitude", "PC_LD2", "Region")]

# Load in the herbarium data
df <- read.csv("Data/herbarium/Input/herbarium_structure_dataframe_1192026.csv",header=TRUE)
colnames(df)[33]<-"Latitude"
df <- df[df$H_C=="H",]
# First, let's examine the data structure and distribution
print("Data overview:")
str(df[c("Latitude", "K3Q1S1964")])
summary(df[c("Latitude", "K3Q1S1964")])

# Check for missing values
print("\nMissing values:")
colSums(is.na(df[c("Latitude", "K3Q1S1964")]))

# Remove any rows with missing values in key variables
df_clean <- df[!is.na(df$Latitude) & !is.na(df$K3Q1S1964), ]
print(paste("Observations after removing missing values:", nrow(df_clean)))

# Perform logarithmic regression on Latitude and K3Q1S1964
model <- lm(K3Q1S1964 ~ log(abs(Latitude)), data = df_clean)
print("\nRegression model summary:")
summary(model)

# Create visualizations to understand the relationship
library(ggplot2)

# Plot 1: Scatter plot with regression line
p1 <- ggplot(df_clean, aes(x = Latitude, y = K3Q1S1964)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_smooth(method = "lm", formula = y ~ log(abs(x)), color = "red") +
  labs(title = "K3Q1S1964 vs Latitude with Log Regression Line",
       x = "Latitude", y = "K3Q1S1964 (Ancestry)") +
  theme_minimal()

print(p1)

# Calculate fitted values and residuals
df_clean$fitted <- predict(model)
df_clean$residuals <- residuals(model)

# Method 1: Find the midpoint of the regression line
lat_range <- range(df_clean$Latitude)
lat_midpoint <- mean(lat_range)
print(paste("\nSimple midpoint of latitude range:", round(lat_midpoint, 2)))

# Method 2: Find the latitude where the regression prediction equals the mean of K3Q1S1964
mean_K3Q1S1964 <- mean(df_clean$K3Q1S1964)
# For log regression: K3Q1S1964 = intercept + slope * log(abs(Latitude))
# Solve for Latitude when K3Q1S1964 = mean_K3Q1S1964
intercept <- coef(model)[1]
slope <- coef(model)[2]
optimal_lat <- exp((mean_K3Q1S1964 - intercept) / slope)
print(paste("Latitude where regression equals mean K3Q1S1964:", round(optimal_lat, 2)))

# Method 3: Use changepoint detection to find natural breakpoints
library(changepoint)
# Sort data by latitude for changepoint analysis
df_sorted <- df_clean[order(df_clean$Latitude), ]
cpt_analysis <- cpt.mean(df_sorted$K3Q1S1964, method = "PELT")
changepoints <- cpts(cpt_analysis)

if(length(changepoints) > 0) {
  # Get the latitude values at changepoints
  changepoint_lats <- df_sorted$Latitude[changepoints]
  print(paste("\nChangepoints detected at latitudes:", paste(round(changepoint_lats, 2), collapse = ", ")))
  
  # If multiple changepoints, choose the one closest to the middle
  if(length(changepoint_lats) > 1) {
    middle_idx <- which.min(abs(changepoint_lats - lat_midpoint))
    optimal_changepoint <- changepoint_lats[middle_idx]
  } else {
    optimal_changepoint <- changepoint_lats[1]
  }
  print(paste("Selected changepoint for north/south division:", round(optimal_changepoint, 2)))
} else {
  print("\nNo significant changepoints detected")
  optimal_changepoint <- optimal_lat
}

# Method 4: K-means clustering to identify natural groupings
set.seed(123)
kmeans_result <- kmeans(df_clean[c("Latitude", "K3Q1S1964")], centers = 2)
df_clean$cluster <- kmeans_result$cluster

# Find the latitude that best separates the clusters
cluster_centers <- aggregate(Latitude ~ cluster, df_clean, mean)
cluster_boundary <- mean(cluster_centers$Latitude)
print(paste("\nK-means cluster boundary at latitude:", round(cluster_boundary, 2)))

# Create the final north/south classification
# Use the changepoint if available, otherwise use the regression-based method
final_breakpoint <- if(exists("optimal_changepoint")) optimal_changepoint else optimal_lat

df_clean$Region <- ifelse(df_clean$Latitude >= final_breakpoint, "North", "South")

# Summary of the classification
print(paste("\nFinal breakpoint chosen:", round(final_breakpoint, 2)))
print("\nRegion classification summary:")
table(df_clean$Region)

# Validate the separation quality
print("\nMean K3Q1S1964 by Region:")
aggregate(K3Q1S1964 ~ Region, df_clean, function(x) c(mean = mean(x), sd = sd(x)))

# Plot the final classification
kl <- ggplot(df_clean, aes(x = Latitude, y = K3Q1S1964, color = Region)) +
geom_point(data = df_clean[df_clean$Latitude>40,], alpha = 0.7, color="blue") +
geom_point(data = df_clean[df_clean$Latitude<35,], alpha = 0.7, color="red") +
geom_point(data = df_clean[df_clean$Latitude>=35 & df_clean$Latitude<=40,], alpha = 0.7, color="lightblue") +
  geom_vline(xintercept = final_breakpoint, linetype = "dashed", color = "black", size = 1) +
  geom_smooth(method = "lm", formula = y ~ log(abs(x)), se = FALSE) +
  labs( x = "Latitude", y = "Ancestral Proportion\nCluster 1", color = "Region") +
  theme_light() + scale_color_manual(values=c("North"="blue","South"="red","Transition"="lightblue")) + theme(text=element_text(size=12),legend.position="none")

print(kl)

# Statistical test to validate the separation
t_test_result <- t.test(K3Q1S1964 ~ Region, data = df_clean)
print("\nT-test comparing K3Q1S1964 between North and South Regions:")
print(t_test_result)

# Additional analysis: Effect size (Cohen's d)
library(effsize)
cohen_d <- cohen.d(df_clean$K3Q1S1964[df_clean$Region == "North"], 
                   df_clean$K3Q1S1964[df_clean$Region == "South"])
print("\nEffect size (Cohen's d):")
print(cohen_d)

# Save the results
df_clean_with_Regions <- df_clean[c("Latitude", "K3Q1S1964", "Region")]

# Calculate sliding window variance of PC_LD2 by latitude

window_size <- 5
lat_min <- min(df$Latitude, na.rm = TRUE)
lat_max <- max(df$Latitude, na.rm = TRUE)
window_starts <- seq(lat_min, lat_max - window_size, by = 1)

results <- data.frame(
  window_start = numeric(),
  window_end = numeric(),
  pc_LD2_variance = numeric()
)

for (start in window_starts) {
  end <- start + window_size
  subset <- df[df$Latitude >= start & df$Latitude < end, ]
  if (nrow(subset) > 1) { # Need at least 2 points for variance
    var_pc_LD2 <- var(subset$PC_LD2, na.rm = TRUE)
    results <- rbind(results, data.frame(window_start = start, window_end = end, pc_LD2_variance = var_pc_LD2))
  }
}


library(ggplot2)
ggplot(results) +
  geom_line(aes(x = window_start, y = pc_LD2_variance),color="black") +
  labs(x = "Latitude (window center)", y = "PC_LD2 Variance", title = "Sliding Window PC_LD2 Variance by Latitude")+scale_x_continuous(breaks=seq(30, 50, by=5)) +
  theme_light()
# Count number of samples with K3Q1S1964 between 0.05 and 0.95 in each window

window_size <- 5
lat_min <- min(df$Latitude, na.rm = TRUE)
lat_max <- max(df$Latitude, na.rm = TRUE)
window_starts <- seq(lat_min, lat_max - window_size, by = 1)

count_results <- data.frame(
  window_start = numeric(),
  window_end = numeric(),
  count_in_range = numeric()
)

for (start in window_starts) {
  end <- start + window_size
  subset <- df[df$Latitude >= start & df$Latitude < end, ]
  count <- sum(subset$K3Q1S1964 > 0.1 & subset$K3Q1S1964 < 0.90, na.rm = TRUE)
  count_results <- rbind(count_results, data.frame(window_start = start, window_end = end, count_in_range = count))
}

# Calculate scaling parameters
count_range <- range(count_results$count_in_range, na.rm = TRUE)
var_range <- range(results$pc_LD2_variance, na.rm = TRUE)
scale_factor <- diff(count_range) / diff(var_range)
offset <- count_range[1] - var_range[1] * scale_factor

# Add scaled variance to count_results for plotting
count_results$pc_LD2_variance_scaled <- results$pc_LD2_variance * scale_factor + offset

library(ggplot2)
range_plot <- ggplot(count_results, aes(x = window_start)) +
  geom_line(aes(y = count_in_range), color = "red",size=1.5) +
  geom_line(aes(y = pc_LD2_variance_scaled), color = "black",size=1.5) +
  scale_y_continuous(
    name = "Count",
    sec.axis = sec_axis(~ (. - offset) / scale_factor, name = "PC_LD2 Variance")
  ) +
  labs(x = "Latitude (window start)") +
  scale_x_continuous(breaks = seq(25, 50, by = 5)) +
  theme_light()+theme(
    axis.title.y = element_text(color = "red",size=12),
    axis.title.y.right = element_text(color = "black",size=12),
    axis.text.y.right=element_text(size=12,color="black"),
    axis.text.y=element_text(size=12,color="red"),
    axis.text.x=element_text(size=12,color="black"),
    axis.title.x=element_text(size=12,color="black")
  )+xlim(c(27,50))

ik <- ggplot(df1,aes(x=RANGE,y=PC_LD1,color=RANGE))+geom_boxplot()+theme_light()+theme(legend.position="none",axis.title.x=element_blank(),axis.title.y=element_text(size=12,color="black"),axis.text=element_text(size=12,color="black"))+scale_color_manual(values=c("black","darkgrey"))+scale_x_discrete(labels=c("Europe\n(Native)", "North America\n(Introduced)"))
io <- ggplot(df1,aes(x=RANGE,y=PC_LD2,color=RANGE))+geom_boxplot()+theme_light()+theme(legend.position="none",axis.title.x=element_blank(),axis.title.y=element_text(size=12,color="black"),axis.text=element_text(size=12,color="black"))+scale_color_manual(values=c("black","darkgrey"))+scale_x_discrete(labels=c("Europe\n(Native)", "North America\n(Introduced)"))
ki <- ggplot(df1,aes(x=K3Q1S1964,y=PC_LD1))+geom_point()+geom_smooth(method="lm",color="red")+theme_light()+xlab("Ancestral Proportion (Cluster 1)")+ylab("PC1")
df1$opaq <- (df1$Year/max(df1$Year))
oi <- ggplot(df1,aes(x=K3Q1S1964,y=PC_LD2))+geom_point(aes(alpha=opaq))+geom_smooth(method="lm",color="red")+theme_light()+xlab("Ancestral Proportion (Cluster 1)")+ylab("PC_LD2")
library(ggpubr)
SupplementalFigure1 <- ggarrange(ik,io,ki,oi,ncol=2,nrow=2)
ggsave("SupFig1.pdf", plot = SupplementalFigure1, width = 8, height = 6, units = "in")
SupplementalFigure2<- ggarrange(range_plot,p2,ncol=1,nrow=2,align="hv")
ggsave("SupFig2.pdf", plot = SupplementalFigure2, width = 8, height = 6, units = "in")
