################################
#Transposon Clusters############
################################

library(car)
library(dplyr)  # For data manipulation
library(ggplot2)
library(ggforce)  # For confidence ellipses

pca_data <- read.csv("Data/herbarium/Input/herbarium_structure_dataframe_1192026.csv",header=TRUE)
countries <- pca_data$Country

# Assign colors to each country
country_colors <- as.factor(countries)  # Convert countries to a factor
palette <- rainbow(length(levels(country_colors)))  # Generate a color palette
point_colors <- palette[country_colors]  # Map countries to colors
pca_data$Country[pca_data$Country=="USA"]<-"Herbarium"
colorBlindBlack8 <- c("#F0E442", "#E69F00", "#D55E00", "#009E73", 
                      "#000000", "#0072B2", "#56B4E9", "#CC79A7", "darkblue")

# Plot using ggplot for Ttotal
Ttotal <- ggplot(pca_data, aes(x = PC1_TE, y = PC2_TE, color = Country)) +
  geom_point(size = 3, alpha = 0.8) +  # Scatter plot
  geom_mark_ellipse(aes(fill = Country), alpha = 0.2, show.legend = FALSE) +  # Confidence ellipses
  scale_color_manual(values = colorBlindBlack8) +  # Apply dynamically generated palette
  scale_fill_manual(values = colorBlindBlack8) +  # Match fill colors for ellipses
  labs(x = "PC1", y = "PC2") +  # Labels
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
  theme_light() + theme(legend.position = "none", axis.text=element_blank(), axis.title=element_blank(),axis.ticks=element_blank())

# Repeat for filtered_data_T2
T2 <- ggplot(filtered_data_T2, aes(x = PC1_TE, y = PC2_TE, color = Country)) +
  geom_point(aes(alpha = 0.2), size = 3) +  # Adjust alpha for "C" rows
  scale_color_manual(values = colorBlindBlack8) +  # Apply dynamically generated palette
  scale_fill_manual(values = colorBlindBlack8) +  # Match fill colors for ellipses
  scale_alpha_identity() +  # Use the alpha values directly
  theme_light() + theme(legend.position = "none", axis.text=element_blank(), axis.title=element_blank(),axis.ticks=element_blank())

# Repeat for filtered_data_T3
T3 <- ggplot(filtered_data_T3, aes(x = PC1_TE, y = PC2_TE, color = Country)) +
  geom_point(aes(alpha = 0.2), size = 3) +  # Adjust alpha for "C" rows
  scale_color_manual(values = colorBlindBlack8) +  # Apply dynamically generated palette
  scale_fill_manual(values = colorBlindBlack8) +  # Match fill colors for ellipses
  scale_alpha_identity() +  # Use the alpha values directly
  theme_light() + theme(legend.position = "none", axis.text=element_blank(), axis.title=element_blank(),axis.ticks=element_blank())

# Repeat for filtered_data_T4
T4 <- ggplot(filtered_data_T4, aes(x = PC1_TE, y = PC2_TE, color = Country)) +
  geom_point(aes(alpha = 0.2), size = 3) +  # Adjust alpha for "C" rows
  scale_color_manual(values = colorBlindBlack8) +  # Apply dynamically generated palette
  scale_fill_manual(values = colorBlindBlack8) +  # Match fill colors for ellipses
  scale_alpha_identity() +  # Use the alpha values directly
  theme_light() + theme(legend.position = "none", axis.text=element_blank(), axis.title=element_blank(),axis.ticks=element_blank())

# Arrange the plots
combined_plot <- ggarrange(
    Ttotal,  # The total plot
    ggarrange(T1, T2, T3, T4, ncol = 2,nrow=2, labels = c("1838 - 1877", "1878 - 1917", "1918 - 1957", "1958 - 1997")),  # The four smaller plots
    nrow = 1,ncol=2,  # Two rows: one for Ttotal, one for T1-T4
    heights = c(2, 1),legend="left"  # Make Ttotal twice the height of the row with T1-T4
)

# Display the combined plot
print(combined_plot)

pca_data <- pca_data[,c("PC1_TE","PC2_TE","Country","Samp","Year","H_C")]
#######################
#Transposon Associations#
#######################


library(dplyr)
library(ggplot2)
TE_analysis_ready <- read.csv("Github_Projects/HerbariumStructure_WhiteClover/Data/herbarium/Input/TE_analysis_ready.csv",header=TRUE)
op <- TE_analysis_ready
# Standardize each column (columns 12 to 9450)
op_standardized <- op
op_standardized[, 15:9453] <- scale(op[, 15:9453])

# Reshape the data to a long format for faster grouping and summarization
long_data <- op_standardized %>%
  dplyr::select(Range, 15:9453) %>%
  tidyr::pivot_longer(cols = -Range, names_to = "Column", values_to = "Value")

# Summarize the data grouped by Range and Column
summary_df <- long_data %>%
  dplyr::group_by(Range, Column) %>%
  dplyr::summarize(Mean = mean(Value, na.rm = TRUE), .groups = "drop")

# View the summarized dataframe
print(summary_df)

# Update the x-axis text labels for Range
summary_df$Range <- recode(summary_df$Range, "EUR" = "Europe\n(Contemporary)", "NAM" = "North America\n(Herbarium)")

# Plot Range on the X-axis as violin plot
rangeTE <- ggplot(summary_df[!is.na(summary_df$Mean),], aes(x = Range, y = Mean)) +
    geom_violin(fill = "lightgray", alpha = 0.7) +
    geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
    stat_summary(fun = mean, geom = "point", size = 3, color = "red") +
    theme_light() +
    theme(legend.position="null", axis.text = element_text(size=12, color="black"), axis.title = element_text(size=14, color="black"), axis.title.x=element_blank()) +
    labs(y="Mean per TE Family Copy Number")

# Standardize each column (columns 12 to 9450)
op <- TE_analysis_ready
op_sub <- op[op$H_C=="H",]
op_sub <- op_sub[!is.na(op_sub$Time_bin),]
op_standardized <- op_sub
op_standardized[, 15:9453] <- scale(op_sub[, 15:9453])

# Reshape the data to a long format for faster grouping and summarization
long_data <- op_standardized %>%
  dplyr::select(Time_bin, 15:9453) %>%
  tidyr::pivot_longer(cols = -Time_bin, names_to = "Column", values_to = "Value")

# Summarize the data grouped by Range and Column
summary_df <- long_data %>%
  dplyr::group_by(Time_bin, Column) %>%
  dplyr::summarize(Mean = mean(Value, na.rm = TRUE), .groups = "drop")

# View the summarized dataframe
print(summary_df)

summR <- summarySE(summary_df[!is.na(summary_df$Mean),], measurevar = "Mean", groupvars = "Time_bin")
# Plot Range on the X-axis and Mean on the Y-axis
overtime <- ggplot(summR, aes(x = Time_bin, y = Mean)) +
    geom_point(size = 3,color="black") +
    geom_errorbar(aes(ymin = Mean - sd, ymax = Mean + sd), width = 0.2) +
    theme_light() +
    theme(legend.position="null", axis.text = element_text(size=12, color="black"), axis.title = element_text(size=14, color="black"), axis.title.x=element_blank(),axis.text.x=element_text(angle=45,hjust=1)) +
    labs(y="Mean per TE Family Copy Number")

panelB <- ggarrange(rangeTE,overtime,ncol=2,nrow=1)

SupplementalFigure6 <- ggarrange(combined_plot,panelB,ncol=1,nrow=2)