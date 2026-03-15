################################
#Transposon Clusters############
################################

library(car)
library(dplyr)  # For data manipulation
library(ggplot2)
library(ggforce)  # For confidence ellipses

pca_data <- read.csv("herbarium_structure_dataframe_542025.csv")
countries <- pca_data$Country

# Assign colors to each country
country_colors <- as.factor(countries)  # Convert countries to a factor
palette <- rainbow(length(levels(country_colors)))  # Generate a color palette
point_colors <- palette[country_colors]  # Map countries to colors

colorBlindBlack8 <- c("#F0E442", "#E69F00", "#D55E00", "#009E73", 
                      "#000000", "#0072B2", "#56B4E9", "#CC79A7", "darkblue")

# Plot using ggplot for Ttotal
Ttotal <- ggplot(pca_data, aes(x = PC1_TE, y = PC2_TE, color = Country)) +
  geom_point(size = 3, alpha = 0.8) +  # Scatter plot
  geom_mark_ellipse(aes(fill = Country), alpha = 0.2, show.legend = FALSE) +  # Confidence ellipses
  scale_color_manual(values = colorBlindBlack8) +  # Apply dynamically generated palette
  scale_fill_manual(values = colorBlindBlack8) +  # Match fill colors for ellipses
  labs(title = "PC1 vs PC2:\n1838 - 1997", x = "PC1", y = "PC2") +  # Labels
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 24),  # Center, bold, and increase size
    legend.text = element_text(size = 12)  # Increase legend text size
  ) 

min_year <- min(pca_data[!is.na(pca_data$Year), "Year"])  # Get the minimum year from the data

# Filter the data
filtered_data_T1 <- pca_data %>%
    filter(
        (H_C == "C") | (H_C == "H" & Year >= min_year & Year <= min_year + 39)
    )

# Filter the data
filtered_data_T2 <- pca_data %>%
    filter(
        (H_C == "C") | (H_C == "H" & Year >= min_year + 40 & Year <= min_year + 79)
    )

# Filter the data
filtered_data_T3 <- pca_data %>%
    filter(
        (H_C == "C") | (H_C == "H" & Year >= min_year + 80 & Year <= min_year + 119)
    )

# Filter the data
filtered_data_T4 <- pca_data %>%
    filter(
        (H_C == "C") | (H_C == "H" & Year >= min_year + 120 & Year <= min_year + 159)
    )


# Plot using ggplot for filtered_data_T1
T1 <- ggplot(filtered_data_T1, aes(x = PC1_TE, y = PC2_TE, color = Country)) +
  geom_point(aes(alpha = 0.2), size = 3) +  # Adjust alpha for "C" rows
  scale_color_manual(values = colorBlindBlack8) +  # Apply dynamically generated palette
  scale_fill_manual(values = colorBlindBlack8) +  # Match fill colors for ellipses
  scale_alpha_identity() +  # Use the alpha values directly
  theme_light() + theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

# Repeat for filtered_data_T2
T2 <- ggplot(filtered_data_T2, aes(x = PC1_TE, y = PC2_TE, color = Country)) +
  geom_point(aes(alpha = 0.2), size = 3) +  # Adjust alpha for "C" rows
  scale_color_manual(values = colorBlindBlack8) +  # Apply dynamically generated palette
  scale_fill_manual(values = colorBlindBlack8) +  # Match fill colors for ellipses
  scale_alpha_identity() +  # Use the alpha values directly
  theme_light() + theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

# Repeat for filtered_data_T3
T3 <- ggplot(filtered_data_T3, aes(x = PC1_TE, y = PC2_TE, color = Country)) +
  geom_point(aes(alpha = 0.2), size = 3) +  # Adjust alpha for "C" rows
  scale_color_manual(values = colorBlindBlack8) +  # Apply dynamically generated palette
  scale_fill_manual(values = colorBlindBlack8) +  # Match fill colors for ellipses
  scale_alpha_identity() +  # Use the alpha values directly
  theme_light() + theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

# Repeat for filtered_data_T4
T4 <- ggplot(filtered_data_T4, aes(x = PC1_TE, y = PC2_TE, color = Country)) +
  geom_point(aes(alpha = 0.2), size = 3) +  # Adjust alpha for "C" rows
  scale_color_manual(values = colorBlindBlack8) +  # Apply dynamically generated palette
  scale_fill_manual(values = colorBlindBlack8) +  # Match fill colors for ellipses
  scale_alpha_identity() +  # Use the alpha values directly
  theme_light() + theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

# Arrange the plots
combined_plot <- ggarrange(
    Ttotal,  # The total plot
    ggarrange(T1, T2, T3, T4, ncol = 4, labels = c("1838 - 1877", "1878 - 1917", "1918 - 1957", "1958 - 1997")),  # The four smaller plots
    nrow = 2,  # Two rows: one for Ttotal, one for T1-T4
    heights = c(2, 1)  # Make Ttotal twice the height of the row with T1-T4
)

# Display the combined plot
print(combined_plot)

colorBlindBlack8 <- c("#000000", "#E69F00", 
                      "#56B4E9", "darkblue")

pca_data_UFS <- pca_data[pca_data$Country == "UK" | pca_data$Country == "France" | pca_data$Country == "Spain" | pca_data$Country == "USA",]


Ttotal_UFS <- ggplot(pca_data_UFS, aes(x = PC1_TE, y = PC2_TE, color = Country)) +
  geom_point(size = 3, alpha = 0.5) +  # Scatter plot
  geom_mark_ellipse(aes(fill = Country), alpha = 0.2, show.legend = FALSE) + ggtitle("Herbarium Samples from 1838-1997\n& Contemporary European Native Range Samples") +  # Confidence ellipses
  scale_color_manual(
    values = colorBlindBlack8,
    breaks = c("USA", "France", "Spain", "UK"),  # Change order
    labels = c("Herbarium", "France", "Spain", "UK")  # Update labels
  ) +
  scale_fill_manual(
    values = colorBlindBlack8,
    breaks = c("USA", "France", "Spain", "UK"),  # Change order
    labels = c("Herbarium", "France", "Spain", "UK")  # Update labels
  ) +
  labs(x = "PC1", y = "PC2") +  # Labels
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5,face="bold",size = 16),  # Center, bold, and increase size
    legend.title = element_blank(),  # Remove legend title
    legend.text = element_text(size = 12)  # Increase legend text size
  ) 

# Filter the data
filtered_data_T1_UFS <- pca_data_UFS %>%
    filter(
        (H_C == "C") | (H_C == "H" & Year >= min_year & Year <= min_year + 39)
    )

# Filter the data
filtered_data_T2_UFS <- pca_data_UFS %>%
    filter(
        (H_C == "C") | (H_C == "H" & Year >= min_year + 40 & Year <= min_year + 79)
    )

# Filter the data
filtered_data_T3_UFS <- pca_data_UFS %>%
    filter(
        (H_C == "C") | (H_C == "H" & Year >= min_year + 80 & Year <= min_year + 119)
    )

# Filter the data
filtered_data_T4_UFS <- pca_data_UFS %>%
    filter(
        (H_C == "C") | (H_C == "H" & Year >= min_year + 120 & Year <= min_year + 159)
    )

colorBlindBlack8 <- c("#E69F00", 
                      "#56B4E9","darkblue","#000000")

# Plot using ggplot for filtered_data_T1_UFS
T1_UFS <- ggplot(filtered_data_T1_UFS, aes(x = PC1_TE, y = PC2_TE, color = Country)) +
  geom_point(aes(alpha = 0.2), size = 3) + ggtitle("1838 - 1877") +  # Adjust alpha for "C" rows
  scale_color_manual(values = colorBlindBlack8) +  # Apply dynamically generated palette
  scale_fill_manual(values = colorBlindBlack8) +  # Match fill colors for ellipses
  scale_alpha_identity() +  # Use the alpha values directly
  theme_light() + theme(axis.text = element_blank(),legend.position = "none", plot.title = element_text(hjust = 0.5,face="bold"))

# Repeat for filtered_data_T2_UFS
T2_UFS <- ggplot(filtered_data_T2_UFS, aes(x = PC1_TE, y = PC2_TE, color = Country)) +
  geom_point(aes(alpha = 0.2), size = 3) + ggtitle("1878 - 1917") +  # Adjust alpha for "C" rows
  scale_color_manual(values = colorBlindBlack8) +  # Apply dynamically generated palette
  scale_fill_manual(values = colorBlindBlack8) +  # Match fill colors for ellipses
  scale_alpha_identity() +  # Use the alpha values directly
  theme_light() + theme(axis.text = element_blank(),legend.position = "none", plot.title = element_text(hjust = 0.5,face="bold"))

# Repeat for filtered_data_T3_UFS
T3_UFS <- ggplot(filtered_data_T3_UFS, aes(x = PC1_TE, y = PC2_TE, color = Country)) +
  geom_point(aes(alpha = 0.2), size = 3) +  ggtitle("1918 - 1957") + # Adjust alpha for "C" rows
  scale_color_manual(values = colorBlindBlack8) +  # Apply dynamically generated palette
  scale_fill_manual(values = colorBlindBlack8) +  # Match fill colors for ellipses
  scale_alpha_identity() +  # Use the alpha values directly
  theme_light() + theme(axis.text = element_blank(),legend.position = "none", plot.title = element_text(hjust = 0.5,face="bold"))

# Repeat for filtered_data_T4_UFS
T4_UFS <- ggplot(filtered_data_T4_UFS, aes(x = PC1_TE, y = PC2_TE, color = Country)) +
  geom_point(aes(alpha = 0.2), size = 3) +  ggtitle("1958 - 1997") + # Adjust alpha for "C" rows
  scale_color_manual(values = colorBlindBlack8) +  # Apply dynamically generated palette
  scale_fill_manual(values = colorBlindBlack8) +  # Match fill colors for ellipses
  scale_alpha_identity() +  # Use the alpha values directly
  theme_light() + theme(axis.text = element_blank(),legend.position = "none", plot.title = element_text(hjust = 0.5,face="bold"))

# Arrange the plots
combined_plot_UFS <- ggarrange(
    Ttotal_UFS,  # The total plot
    ggarrange(T1_UFS, T2_UFS, T3_UFS, T4_UFS, ncol = 4),  # The four smaller plots
    nrow = 2,  # Two rows: one for Ttotal, one for T1-T4
    heights = c(2, 1)  # Make Ttotal twice the height of the row with T1-T4
)

# Display the combined plot
print(combined_plot_UFS)