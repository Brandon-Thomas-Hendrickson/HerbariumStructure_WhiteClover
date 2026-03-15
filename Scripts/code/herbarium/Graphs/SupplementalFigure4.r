
library(car)
library(dplyr)  # For data manipulation
library(ggplot2)
library(ggforce)  # For confidence ellipses

# Load the covariance matrix
cov_file <- "Data/herbarium/Input/herb_GLUE_SPAIN_filtered_LD_MAF05.cov"  # Replace with your .cov file path
pca_data <- read.csv("Data/herbarium/Input/herbarium_structure_dataframe_1192026.csv",header=TRUE)
herb_EUR_list <- pca_data

pca_data$Year <- herb_EUR_list$Year
pca_data$Lat <- herb_EUR_list$Lat
pca_data$Lon <- herb_EUR_list$Lon
pca_data$H_C <- herb_EUR_list$H_C
pca_data$File <- herb_EUR_list$File
pca_data$State <- herb_EUR_list$State
pca_data$Sample <- herb_EUR_list$Sample

colorBlindBlack8 <- c("#F0E442", "#E69F00", "#D55E00", "#009E73", "#0072B2", "#56B4E9", "#CC79A7", "darkblue", "#000000")

# Plot using ggplot for Ttotal
Ttotal <- ggplot(pca_data, aes(x = PC_LD1, y = PC_LD2, color = Country)) +
  geom_point(size = 3, alpha = 0.8) +  # Scatter plot
  geom_mark_ellipse(aes(fill = Country), alpha = 0.2, show.legend = FALSE) +  # Confidence ellipses
  scale_color_manual(values = colorBlindBlack8) +  # Apply dynamically generated palette
  scale_fill_manual(values = colorBlindBlack8) +  # Match fill colors for ellipses
  labs(x = "PC_LD1", y = "PC_LD2") +  # Labels
  theme_light() +
  theme(
    legend.text = element_text(size = 12),legend.title = element_blank()  # Increase legend text size
  ) +
  ylim(c(-0.08, 0.08)) +
  xlim(c(-0.08, 0.07))

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
T1 <- ggplot(filtered_data_T1, aes(x = PC_LD1, y = PC_LD2, color = Country)) +
  geom_point(aes(alpha = 0.2), size = 3) +  # Adjust alpha for "C" rows
  scale_color_manual(values = colorBlindBlack8) +  # Apply dynamically generated palette
  scale_fill_manual(values = colorBlindBlack8) +  # Match fill colors for ellipses
  scale_alpha_identity() +  # Use the alpha values directly
  theme_light() + ggtitle("1838-1877")+theme(legend.position = "none",axis.text=element_blank(),axis.title=element_blank()) + ylim(c(-0.05, 0.08)) + xlim(c(-0.04,0.06))

# Repeat for filtered_data_T2
T2 <- ggplot(filtered_data_T2, aes(x = PC_LD1, y = PC_LD2, color = Country)) +
  geom_point(aes(alpha = 0.2), size = 3) +  # Adjust alpha for "C" rows
  scale_color_manual(values = colorBlindBlack8) +  # Apply dynamically generated palette
  scale_fill_manual(values = colorBlindBlack8) +  # Match fill colors for ellipses
  scale_alpha_identity() +  # Use the alpha values directly
  theme_light() + ggtitle("1878-1917")+theme(legend.position = "none", plot.title = element_text(hjust = 0.5),axis.text=element_blank(),axis.title=element_blank()) + ylim(c(-0.05, 0.08)) + xlim(c(-0.04,0.06))

# Repeat for filtered_data_T3
T3 <- ggplot(filtered_data_T3, aes(x = PC_LD1, y = PC_LD2, color = Country)) +
  geom_point(aes(alpha = 0.2), size = 3) +  # Adjust alpha for "C" rows
  scale_color_manual(values = colorBlindBlack8) +  # Apply dynamically generated palette
  scale_fill_manual(values = colorBlindBlack8) +  # Match fill colors for ellipses
  scale_alpha_identity() +  # Use the alpha values directly
  theme_light() + ggtitle("1918-1957")+ theme(legend.position = "none", plot.title = element_text(hjust = 0.5),axis.text=element_blank(),axis.title=element_blank()) + ylim(c(-0.05, 0.08)) + xlim(c(-0.04,0.06))

# Repeat for filtered_data_T4
T4 <- ggplot(filtered_data_T4, aes(x = PC_LD1, y = PC_LD2, color = Country)) +
  geom_point(aes(alpha = 0.2), size = 3) +  # Adjust alpha for "C" rows
  scale_color_manual(values = colorBlindBlack8) +  # Apply dynamically generated palette
  scale_fill_manual(values = colorBlindBlack8) +  # Match fill colors for ellipses
  scale_alpha_identity() +  # Use the alpha values directly
  theme_light() + ggtitle("1958-1997")+ theme(legend.position = "none", plot.title = element_text(hjust = 0.5),axis.text=element_blank(),axis.title=element_blank()) + ylim(c(-0.05, 0.08)) + xlim(c(-0.04,0.06))

panelB <- ggarrange(T1,T2,T3,T4,ncol=2,nrow=2)
# Arrange the plots
combined_plot <- ggarrange(
    Ttotal, 
    panelB, # The total plot
    ncol=2,
nrow = 1,  # Two rows: one for Ttotal, one for T1-T4
    heights = c(1, 1),legend="top"  # Make Ttotal twice the height of the row with T1-T4
)

# Display the combined plot
print(combined_plot)




herb_df <- read.csv("Data/herbarium/Input/herbarium_structure_dataframe_1192026.csv",header=TRUE)
pc_cols <- c("PC_LD1","PC_LD2")
# Assume your data frame is called df and has columns: Country, PC_LD1, PC_LD2, PC3, ...
df <- herb_df[herb_df$Region=="North" | herb_df$H_C=="C",]
df <- df[df$TimeBin=="1838-1877" | df$H_C=="C",]
library(dplyr)

# Calculate centroids for each country except USA
centroids <- df %>%
    filter(Country != "USA") %>%
    group_by(Country) %>%
    dplyr::summarise(across(all_of(pc_cols), mean), .groups = "drop")

# Get USA samples
usa_samples <- df %>%
    filter(Country == "USA") %>%
    dplyr::select(all_of(pc_cols))

# Function to calculate Euclidean distance
euclidean <- function(a, b) sqrt(sum((a - b)^2))

# For each centroid, calculate distances to all USA samples
results_N_1 <- centroids %>%
    rowwise() %>%
    dplyr::mutate(
        distances = list(
            apply(usa_samples, 1, function(x) euclidean(x, c_across(all_of(pc_cols))))
        ),
        avg_distance = mean(unlist(distances)),
        sd_distance = sd(unlist(distances))
    ) %>%
    dplyr::select(Country, avg_distance, sd_distance)

pc_cols <- c("PC_LD1","PC_LD2")
# Assume your data frame is called df and has columns: Country, PC_LD1, PC_LD2, PC3, ...
df <- herb_df[herb_df$Region=="North" | herb_df$H_C=="C",]
df <- df[df$TimeBin=="1878-1917" | df$H_C=="C",]
library(dplyr)

# Calculate centroids for each country except USA
centroids <- df %>%
    filter(Country != "USA") %>%
    group_by(Country) %>%
    dplyr::summarise(across(all_of(pc_cols), mean), .groups = "drop")

# Get USA samples
usa_samples <- df %>%
    filter(Country == "USA") %>%
    dplyr::select(all_of(pc_cols))

# Function to calculate Euclidean distance
euclidean <- function(a, b) sqrt(sum((a - b)^2))

# For each centroid, calculate distances to all USA samples
results_N_2<- centroids %>%
    rowwise() %>%
    dplyr::mutate(
        distances = list(
            apply(usa_samples, 1, function(x) euclidean(x, c_across(all_of(pc_cols))))
        ),
        avg_distance = mean(unlist(distances)),
        sd_distance = sd(unlist(distances))
    ) %>%
    dplyr::select(Country, avg_distance, sd_distance)

pc_cols <- c("PC_LD1","PC_LD2")
# Assume your data frame is called df and has columns: Country, PC_LD1, PC_LD2, PC3, ...
df <- herb_df[herb_df$Region=="North" | herb_df$H_C=="C",]
df <- df[df$TimeBin=="1918-1957" | df$H_C=="C",]
library(dplyr)

# Calculate centroids for each country except USA
centroids <- df %>%
    filter(Country != "USA") %>%
    group_by(Country) %>%
    dplyr::summarise(across(all_of(pc_cols), mean), .groups = "drop")

# Get USA samples
usa_samples <- df %>%
    filter(Country == "USA") %>%
    dplyr::select(all_of(pc_cols))

# Function to calculate Euclidean distance
euclidean <- function(a, b) sqrt(sum((a - b)^2))

# For each centroid, calculate distances to all USA samples
results_N_3<- centroids %>%
    rowwise() %>%
    dplyr::mutate(
        distances = list(
            apply(usa_samples, 1, function(x) euclidean(x, c_across(all_of(pc_cols))))
        ),
        avg_distance = mean(unlist(distances)),
        sd_distance = sd(unlist(distances))
    ) %>%
    dplyr::select(Country, avg_distance, sd_distance)

pc_cols <- c("PC_LD1","PC_LD2")
# Assume your data frame is called df and has columns: Country, PC_LD1, PC_LD2, PC3, ...
df <- herb_df[herb_df$Region=="North" | herb_df$H_C=="C",]
df <- df[df$TimeBin=="1958-1997" | df$H_C=="C",]
library(dplyr)

# Calculate centroids for each country except USA
centroids <- df %>%
    filter(Country != "USA") %>%
    group_by(Country) %>%
    dplyr::summarise(across(all_of(pc_cols), mean), .groups = "drop")

# Get USA samples
usa_samples <- df %>%
    filter(Country == "USA") %>%
    dplyr::select(all_of(pc_cols))

# Function to calculate Euclidean distance
euclidean <- function(a, b) sqrt(sum((a - b)^2))

# For each centroid, calculate distances to all USA samples
results_N_4<- centroids %>%
    rowwise() %>%
    dplyr::mutate(
        distances = list(
            apply(usa_samples, 1, function(x) euclidean(x, c_across(all_of(pc_cols))))
        ),
        avg_distance = mean(unlist(distances)),
        sd_distance = sd(unlist(distances))
    ) %>%
    dplyr::select(Country, avg_distance, sd_distance)

pc_cols <- c("PC_LD1","PC_LD2")
# Assume your data frame is called df and has columns: Country, PC_LD1, PC_LD2, PC3, ...
df <- herb_df[herb_df$Region=="South" | herb_df$H_C=="C",]
df <- df[df$TimeBin=="1838-1877" | df$H_C=="C",]
library(dplyr)

# Calculate centroids for each country except USA
centroids <- df %>%
    filter(Country != "USA") %>%
    group_by(Country) %>%
    dplyr::summarise(across(all_of(pc_cols), mean), .groups = "drop")

# Get USA samples
usa_samples <- df %>%
    filter(Country == "USA") %>%
    dplyr::select(all_of(pc_cols))

# Function to calculate Euclidean distance
euclidean <- function(a, b) sqrt(sum((a - b)^2))

# For each centroid, calculate distances to all USA samples
results_S_1 <- centroids %>%
    rowwise() %>%
    dplyr::mutate(
        distances = list(
            apply(usa_samples, 1, function(x) euclidean(x, c_across(all_of(pc_cols))))
        ),
        avg_distance = mean(unlist(distances)),
        sd_distance = sd(unlist(distances))
    ) %>%
    dplyr::select(Country, avg_distance, sd_distance)

pc_cols <- c("PC_LD1","PC_LD2")
# Assume your data frame is called df and has columns: Country, PC_LD1, PC_LD2, PC3, ...
df <- herb_df[herb_df$Region=="South" | herb_df$H_C=="C",]
df <- df[df$TimeBin=="1878-1917" | df$H_C=="C",]
library(dplyr)

# Calculate centroids for each country except USA
centroids <- df %>%
    filter(Country != "USA") %>%
    group_by(Country) %>%
    dplyr::summarise(across(all_of(pc_cols), mean), .groups = "drop")

# Get USA samples
usa_samples <- df %>%
    filter(Country == "USA") %>%
    dplyr::select(all_of(pc_cols))

# Function to calculate Euclidean distance
euclidean <- function(a, b) sqrt(sum((a - b)^2))

# For each centroid, calculate distances to all USA samples
results_S_2<- centroids %>%
    rowwise() %>%
    dplyr::mutate(
        distances = list(
            apply(usa_samples, 1, function(x) euclidean(x, c_across(all_of(pc_cols))))
        ),
        avg_distance = mean(unlist(distances)),
        sd_distance = sd(unlist(distances))
    ) %>%
    dplyr::select(Country, avg_distance, sd_distance)

pc_cols <- c("PC_LD1","PC_LD2")
# Assume your data frame is called df and has columns: Country, PC_LD1, PC_LD2, PC3, ...
df <- herb_df[herb_df$Region=="South" | herb_df$H_C=="C",]
df <- df[df$TimeBin=="1918-1957" | df$H_C=="C",]
library(dplyr)

# Calculate centroids for each country except USA
centroids <- df %>%
    filter(Country != "USA") %>%
    group_by(Country) %>%
    dplyr::summarise(across(all_of(pc_cols), mean), .groups = "drop")

# Get USA samples
usa_samples <- df %>%
    filter(Country == "USA") %>%
    dplyr::select(all_of(pc_cols))

# Function to calculate Euclidean distance
euclidean <- function(a, b) sqrt(sum((a - b)^2))

# For each centroid, calculate distances to all USA samples
results_S_3<- centroids %>%
    rowwise() %>%
    dplyr::mutate(
        distances = list(
            apply(usa_samples, 1, function(x) euclidean(x, c_across(all_of(pc_cols))))
        ),
        avg_distance = mean(unlist(distances)),
        sd_distance = sd(unlist(distances))
    ) %>%
    dplyr::select(Country, avg_distance, sd_distance)

pc_cols <- c("PC_LD1","PC_LD2")
# Assume your data frame is called df and has columns: Country, PC_LD1, PC_LD2, PC3, ...
df <- herb_df[herb_df$Region=="South" | herb_df$H_C=="C",]
df <- df[df$TimeBin=="1958-1997" | df$H_C=="C",]
library(dplyr)

# Calculate centroids for each country except USA
centroids <- df %>%
    filter(Country != "USA") %>%
    group_by(Country) %>%
    dplyr::summarise(across(all_of(pc_cols), mean), .groups = "drop")

# Get USA samples
usa_samples <- df %>%
    filter(Country == "USA") %>%
    dplyr::select(all_of(pc_cols))

# Function to calculate Euclidean distance
euclidean <- function(a, b) sqrt(sum((a - b)^2))

# For each centroid, calculate distances to all USA samples
results_S_4<- centroids %>%
    rowwise() %>%
    dplyr::mutate(
        distances = list(
            apply(usa_samples, 1, function(x) euclidean(x, c_across(all_of(pc_cols))))
        ),
        avg_distance = mean(unlist(distances)),
        sd_distance = sd(unlist(distances))
    ) %>%
    dplyr::select(Country, avg_distance, sd_distance)

pc_cols <- c("PC_LD1","PC_LD2")
# Assume your data frame is called df and has columns: Country, PC_LD1, PC_LD2, PC3, ...
df <- herb_df[herb_df$Region=="Middle" | herb_df$H_C=="C",]
df <- df[df$TimeBin=="1838-1877" | df$H_C=="C",]
library(dplyr)

# Calculate centroids for each country except USA
centroids <- df %>%
    filter(Country != "USA") %>%
    group_by(Country) %>%
    dplyr::summarise(across(all_of(pc_cols), mean), .groups = "drop")

# Get USA samples
usa_samples <- df %>%
    filter(Country == "USA") %>%
    dplyr::select(all_of(pc_cols))

# Function to calculate Euclidean distance
euclidean <- function(a, b) sqrt(sum((a - b)^2))

# For each centroid, calculate distances to all USA samples
results_M_1 <- centroids %>%
    rowwise() %>%
    dplyr::mutate(
        distances = list(
            apply(usa_samples, 1, function(x) euclidean(x, c_across(all_of(pc_cols))))
        ),
        avg_distance = mean(unlist(distances)),
        sd_distance = sd(unlist(distances))
    ) %>%
    dplyr::select(Country, avg_distance, sd_distance)

pc_cols <- c("PC_LD1","PC_LD2")
# Assume your data frame is called df and has columns: Country, PC_LD1, PC_LD2, PC3, ...
df <- herb_df[herb_df$Region=="Middle" | herb_df$H_C=="C",]
df <- df[df$TimeBin=="1878-1917" | df$H_C=="C",]
library(dplyr)

# Calculate centroids for each country except USA
centroids <- df %>%
    filter(Country != "USA") %>%
    group_by(Country) %>%
    dplyr::summarise(across(all_of(pc_cols), mean), .groups = "drop")

# Get USA samples
usa_samples <- df %>%
    filter(Country == "USA") %>%
    dplyr::select(all_of(pc_cols))

# Function to calculate Euclidean distance
euclidean <- function(a, b) sqrt(sum((a - b)^2))

# For each centroid, calculate distances to all USA samples
results_M_2<- centroids %>%
    rowwise() %>%
    dplyr::mutate(
        distances = list(
            apply(usa_samples, 1, function(x) euclidean(x, c_across(all_of(pc_cols))))
        ),
        avg_distance = mean(unlist(distances)),
        sd_distance = sd(unlist(distances))
    ) %>%
    dplyr::select(Country, avg_distance, sd_distance)

pc_cols <- c("PC_LD1","PC_LD2")
# Assume your data frame is called df and has columns: Country, PC_LD1, PC_LD2, PC3, ...
df <- herb_df[herb_df$Region=="Middle" | herb_df$H_C=="C",]
df <- df[df$TimeBin=="1918-1957" | df$H_C=="C",]
library(dplyr)

# Calculate centroids for each country except USA
centroids <- df %>%
    filter(Country != "USA") %>%
    group_by(Country) %>%
    dplyr::summarise(across(all_of(pc_cols), mean), .groups = "drop")

# Get USA samples
usa_samples <- df %>%
    filter(Country == "USA") %>%
    dplyr::select(all_of(pc_cols))

# Function to calculate Euclidean distance
euclidean <- function(a, b) sqrt(sum((a - b)^2))

# For each centroid, calculate distances to all USA samples
results_M_3<- centroids %>%
    rowwise() %>%
    dplyr::mutate(
        distances = list(
            apply(usa_samples, 1, function(x) euclidean(x, c_across(all_of(pc_cols))))
        ),
        avg_distance = mean(unlist(distances)),
        sd_distance = sd(unlist(distances))
    ) %>%
    dplyr::select(Country, avg_distance, sd_distance)

pc_cols <- c("PC_LD1","PC_LD2")
# Assume your data frame is called df and has columns: Country, PC_LD1, PC_LD2, PC3, ...
df <- herb_df[herb_df$Region=="Middle" | herb_df$H_C=="C",]
df <- df[df$TimeBin=="1958-1997" | df$H_C=="C",]
library(dplyr)

# Calculate centroids for each country except USA
centroids <- df %>%
    filter(Country != "USA") %>%
    group_by(Country) %>%
    dplyr::summarise(across(all_of(pc_cols), mean), .groups = "drop")

# Get USA samples
usa_samples <- df %>%
    filter(Country == "USA") %>%
    dplyr::select(all_of(pc_cols))

# Function to calculate Euclidean distance
euclidean <- function(a, b) sqrt(sum((a - b)^2))

# For each centroid, calculate distances to all USA samples
results_M_4<- centroids %>%
    rowwise() %>%
    dplyr::mutate(
        distances = list(
            apply(usa_samples, 1, function(x) euclidean(x, c_across(all_of(pc_cols))))
        ),
        avg_distance = mean(unlist(distances)),
        sd_distance = sd(unlist(distances))
    ) %>%
    dplyr::select(Country, avg_distance, sd_distance)

results_N_1$TimeBin <- "1838-1877"
results_N_2$TimeBin <- "1878-1917"
results_N_3$TimeBin <- "1918-1957"
results_N_4$TimeBin <- "1958-1997"
results_M_1$TimeBin <- "1838-1877"
results_M_2$TimeBin <- "1878-1917"
results_M_3$TimeBin <- "1918-1957"
results_M_4$TimeBin <- "1958-1997"
results_S_1$TimeBin <- "1838-1877"
results_S_2$TimeBin <- "1878-1917"
results_S_3$TimeBin <- "1918-1957"
results_S_4$TimeBin <- "1958-1997"
results_N_1$Region <- "North"
results_N_2$Region <- "North"
results_N_3$Region <- "North"
results_N_4$Region <- "North"
results_M_1$Region <- "Middle"
results_M_2$Region <- "Middle"
results_M_3$Region <- "Middle"
results_M_4$Region <- "Middle"
results_S_1$Region <- "South"
results_S_2$Region <- "South"
results_S_3$Region <- "South"
results_S_4$Region <- "South"

results <- rbind(results_N_1, results_N_2, results_N_3, results_N_4, results_M_1, results_M_2, results_M_3, results_M_4, results_S_1, results_S_2, results_S_3, results_S_4)

# Set factor levels to control the order of facets
results$Region <- factor(results$Region, levels = c("South", "Middle", "North"))

results_UFS <- results[results$Country=="UK" | results$Country=="France" | results$Country=="Spain",]
library(ggplot2)

colorBlindBlack8 <- c("#E69F00", "#56B4E9", "darkblue")

UFS_distance <- ggplot(results_UFS, aes(x = TimeBin, y = avg_distance, color = Country, group = Country)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_line(position = position_dodge(width = 0.5), size = 1) +
  scale_color_manual(values = colorBlindBlack8) +
  scale_fill_manual(values = colorBlindBlack8) +
  geom_errorbar(aes(ymin = avg_distance - sd_distance, ymax = avg_distance + sd_distance),
                width = 0.2, position = position_dodge(width = 0.5)) +
  facet_wrap(~Region, nrow = 1) +
  theme_light() +
  theme(
    strip.text = element_text(color = "black", size = 12),
    strip.background = element_rect(fill = "white"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12)
  ) +
  labs(
    x = "Time Bin",
    y = "Average Distance"
  )

colorBlindBlack8 <- c("#F0E442", "#E69F00", "#D55E00", "#009E73", "#0072B2", "#56B4E9", "#CC79A7", "darkblue")

all_countries <- ggplot(results, aes(x = TimeBin, y = avg_distance, color = Country, group = Country)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_line(position = position_dodge(width = 0.5), size = 1) +
  scale_color_manual(values = colorBlindBlack8) +
  scale_fill_manual(values = colorBlindBlack8) +
  facet_wrap(~Region, nrow = 1) +
  theme_light() +
  theme(
    strip.text = element_text(color = "black", size = 12),
    strip.background = element_rect(fill = "white"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.position="none"
  ) +
  labs(
    x = "Time Bin",
    y = "Average Distance"
  )

SupplementalFigure4 <- ggarrange(combined_plot,all_countries,ncol=1,nrow=2,labels=c("A","B"))
ggsave("SupFig4.pdf", plot = SupplementalFigure4, width = 10, height = 8, units = "in")
