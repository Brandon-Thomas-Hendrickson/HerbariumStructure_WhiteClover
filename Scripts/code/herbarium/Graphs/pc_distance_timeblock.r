herb_df <- read.csv("Data/herbarium/Input/herbarium_structure_dataframe_542025.csv",header=TRUE)
pc_cols <- c("PC1","PC2")
# Assume your data frame is called df and has columns: Country, PC1, PC2, PC3, ...
df <- herb_df[herb_df$Region=="North" | herb_df$H_C=="C",]
df <- df[df$TimeBin=="1838-1877" | df$H_C=="C",]
library(dplyr)

# Calculate centroids for each country except USA
centroids <- df %>%
    filter(Country != "USA") %>%
    group_by(Country) %>%
    summarise(across(all_of(pc_cols), mean), .groups = "drop")

# Get USA samples
usa_samples <- df %>%
    filter(Country == "USA") %>%
    dplyr::select(all_of(pc_cols))

# Function to calculate Euclidean distance
euclidean <- function(a, b) sqrt(sum((a - b)^2))

# For each centroid, calculate distances to all USA samples
results_N_1 <- centroids %>%
    rowwise() %>%
    mutate(
        distances = list(
            apply(usa_samples, 1, function(x) euclidean(x, c_across(all_of(pc_cols))))
        ),
        avg_distance = mean(unlist(distances)),
        sd_distance = sd(unlist(distances))
    ) %>%
    dplyr::select(Country, avg_distance, sd_distance)

pc_cols <- c("PC1","PC2")
# Assume your data frame is called df and has columns: Country, PC1, PC2, PC3, ...
df <- herb_df[herb_df$Region=="North" | herb_df$H_C=="C",]
df <- df[df$TimeBin=="1878-1917" | df$H_C=="C",]
library(dplyr)

# Calculate centroids for each country except USA
centroids <- df %>%
    filter(Country != "USA") %>%
    group_by(Country) %>%
    summarise(across(all_of(pc_cols), mean), .groups = "drop")

# Get USA samples
usa_samples <- df %>%
    filter(Country == "USA") %>%
    dplyr::select(all_of(pc_cols))

# Function to calculate Euclidean distance
euclidean <- function(a, b) sqrt(sum((a - b)^2))

# For each centroid, calculate distances to all USA samples
results_N_2<- centroids %>%
    rowwise() %>%
    mutate(
        distances = list(
            apply(usa_samples, 1, function(x) euclidean(x, c_across(all_of(pc_cols))))
        ),
        avg_distance = mean(unlist(distances)),
        sd_distance = sd(unlist(distances))
    ) %>%
    dplyr::select(Country, avg_distance, sd_distance)

pc_cols <- c("PC1","PC2")
# Assume your data frame is called df and has columns: Country, PC1, PC2, PC3, ...
df <- herb_df[herb_df$Region=="North" | herb_df$H_C=="C",]
df <- df[df$TimeBin=="1918-1957" | df$H_C=="C",]
library(dplyr)

# Calculate centroids for each country except USA
centroids <- df %>%
    filter(Country != "USA") %>%
    group_by(Country) %>%
    summarise(across(all_of(pc_cols), mean), .groups = "drop")

# Get USA samples
usa_samples <- df %>%
    filter(Country == "USA") %>%
    dplyr::select(all_of(pc_cols))

# Function to calculate Euclidean distance
euclidean <- function(a, b) sqrt(sum((a - b)^2))

# For each centroid, calculate distances to all USA samples
results_N_3<- centroids %>%
    rowwise() %>%
    mutate(
        distances = list(
            apply(usa_samples, 1, function(x) euclidean(x, c_across(all_of(pc_cols))))
        ),
        avg_distance = mean(unlist(distances)),
        sd_distance = sd(unlist(distances))
    ) %>%
    dplyr::select(Country, avg_distance, sd_distance)

pc_cols <- c("PC1","PC2")
# Assume your data frame is called df and has columns: Country, PC1, PC2, PC3, ...
df <- herb_df[herb_df$Region=="North" | herb_df$H_C=="C",]
df <- df[df$TimeBin=="1958-1997" | df$H_C=="C",]
library(dplyr)

# Calculate centroids for each country except USA
centroids <- df %>%
    filter(Country != "USA") %>%
    group_by(Country) %>%
    summarise(across(all_of(pc_cols), mean), .groups = "drop")

# Get USA samples
usa_samples <- df %>%
    filter(Country == "USA") %>%
    dplyr::select(all_of(pc_cols))

# Function to calculate Euclidean distance
euclidean <- function(a, b) sqrt(sum((a - b)^2))

# For each centroid, calculate distances to all USA samples
results_N_4<- centroids %>%
    rowwise() %>%
    mutate(
        distances = list(
            apply(usa_samples, 1, function(x) euclidean(x, c_across(all_of(pc_cols))))
        ),
        avg_distance = mean(unlist(distances)),
        sd_distance = sd(unlist(distances))
    ) %>%
    dplyr::select(Country, avg_distance, sd_distance)

pc_cols <- c("PC1","PC2")
# Assume your data frame is called df and has columns: Country, PC1, PC2, PC3, ...
df <- herb_df[herb_df$Region=="South" | herb_df$H_C=="C",]
df <- df[df$TimeBin=="1838-1877" | df$H_C=="C",]
library(dplyr)

# Calculate centroids for each country except USA
centroids <- df %>%
    filter(Country != "USA") %>%
    group_by(Country) %>%
    summarise(across(all_of(pc_cols), mean), .groups = "drop")

# Get USA samples
usa_samples <- df %>%
    filter(Country == "USA") %>%
    dplyr::select(all_of(pc_cols))

# Function to calculate Euclidean distance
euclidean <- function(a, b) sqrt(sum((a - b)^2))

# For each centroid, calculate distances to all USA samples
results_S_1 <- centroids %>%
    rowwise() %>%
    mutate(
        distances = list(
            apply(usa_samples, 1, function(x) euclidean(x, c_across(all_of(pc_cols))))
        ),
        avg_distance = mean(unlist(distances)),
        sd_distance = sd(unlist(distances))
    ) %>%
    dplyr::select(Country, avg_distance, sd_distance)

pc_cols <- c("PC1","PC2")
# Assume your data frame is called df and has columns: Country, PC1, PC2, PC3, ...
df <- herb_df[herb_df$Region=="South" | herb_df$H_C=="C",]
df <- df[df$TimeBin=="1878-1917" | df$H_C=="C",]
library(dplyr)

# Calculate centroids for each country except USA
centroids <- df %>%
    filter(Country != "USA") %>%
    group_by(Country) %>%
    summarise(across(all_of(pc_cols), mean), .groups = "drop")

# Get USA samples
usa_samples <- df %>%
    filter(Country == "USA") %>%
    dplyr::select(all_of(pc_cols))

# Function to calculate Euclidean distance
euclidean <- function(a, b) sqrt(sum((a - b)^2))

# For each centroid, calculate distances to all USA samples
results_S_2<- centroids %>%
    rowwise() %>%
    mutate(
        distances = list(
            apply(usa_samples, 1, function(x) euclidean(x, c_across(all_of(pc_cols))))
        ),
        avg_distance = mean(unlist(distances)),
        sd_distance = sd(unlist(distances))
    ) %>%
    dplyr::select(Country, avg_distance, sd_distance)

pc_cols <- c("PC1","PC2")
# Assume your data frame is called df and has columns: Country, PC1, PC2, PC3, ...
df <- herb_df[herb_df$Region=="South" | herb_df$H_C=="C",]
df <- df[df$TimeBin=="1918-1957" | df$H_C=="C",]
library(dplyr)

# Calculate centroids for each country except USA
centroids <- df %>%
    filter(Country != "USA") %>%
    group_by(Country) %>%
    summarise(across(all_of(pc_cols), mean), .groups = "drop")

# Get USA samples
usa_samples <- df %>%
    filter(Country == "USA") %>%
    dplyr::select(all_of(pc_cols))

# Function to calculate Euclidean distance
euclidean <- function(a, b) sqrt(sum((a - b)^2))

# For each centroid, calculate distances to all USA samples
results_S_3<- centroids %>%
    rowwise() %>%
    mutate(
        distances = list(
            apply(usa_samples, 1, function(x) euclidean(x, c_across(all_of(pc_cols))))
        ),
        avg_distance = mean(unlist(distances)),
        sd_distance = sd(unlist(distances))
    ) %>%
    dplyr::select(Country, avg_distance, sd_distance)

pc_cols <- c("PC1","PC2")
# Assume your data frame is called df and has columns: Country, PC1, PC2, PC3, ...
df <- herb_df[herb_df$Region=="South" | herb_df$H_C=="C",]
df <- df[df$TimeBin=="1958-1997" | df$H_C=="C",]
library(dplyr)

# Calculate centroids for each country except USA
centroids <- df %>%
    filter(Country != "USA") %>%
    group_by(Country) %>%
    summarise(across(all_of(pc_cols), mean), .groups = "drop")

# Get USA samples
usa_samples <- df %>%
    filter(Country == "USA") %>%
    dplyr::select(all_of(pc_cols))

# Function to calculate Euclidean distance
euclidean <- function(a, b) sqrt(sum((a - b)^2))

# For each centroid, calculate distances to all USA samples
results_S_4<- centroids %>%
    rowwise() %>%
    mutate(
        distances = list(
            apply(usa_samples, 1, function(x) euclidean(x, c_across(all_of(pc_cols))))
        ),
        avg_distance = mean(unlist(distances)),
        sd_distance = sd(unlist(distances))
    ) %>%
    dplyr::select(Country, avg_distance, sd_distance)

pc_cols <- c("PC1","PC2")
# Assume your data frame is called df and has columns: Country, PC1, PC2, PC3, ...
df <- herb_df[herb_df$Region=="Middle" | herb_df$H_C=="C",]
df <- df[df$TimeBin=="1838-1877" | df$H_C=="C",]
library(dplyr)

# Calculate centroids for each country except USA
centroids <- df %>%
    filter(Country != "USA") %>%
    group_by(Country) %>%
    summarise(across(all_of(pc_cols), mean), .groups = "drop")

# Get USA samples
usa_samples <- df %>%
    filter(Country == "USA") %>%
    dplyr::select(all_of(pc_cols))

# Function to calculate Euclidean distance
euclidean <- function(a, b) sqrt(sum((a - b)^2))

# For each centroid, calculate distances to all USA samples
results_M_1 <- centroids %>%
    rowwise() %>%
    mutate(
        distances = list(
            apply(usa_samples, 1, function(x) euclidean(x, c_across(all_of(pc_cols))))
        ),
        avg_distance = mean(unlist(distances)),
        sd_distance = sd(unlist(distances))
    ) %>%
    dplyr::select(Country, avg_distance, sd_distance)

pc_cols <- c("PC1","PC2")
# Assume your data frame is called df and has columns: Country, PC1, PC2, PC3, ...
df <- herb_df[herb_df$Region=="Middle" | herb_df$H_C=="C",]
df <- df[df$TimeBin=="1878-1917" | df$H_C=="C",]
library(dplyr)

# Calculate centroids for each country except USA
centroids <- df %>%
    filter(Country != "USA") %>%
    group_by(Country) %>%
    summarise(across(all_of(pc_cols), mean), .groups = "drop")

# Get USA samples
usa_samples <- df %>%
    filter(Country == "USA") %>%
    dplyr::select(all_of(pc_cols))

# Function to calculate Euclidean distance
euclidean <- function(a, b) sqrt(sum((a - b)^2))

# For each centroid, calculate distances to all USA samples
results_M_2<- centroids %>%
    rowwise() %>%
    mutate(
        distances = list(
            apply(usa_samples, 1, function(x) euclidean(x, c_across(all_of(pc_cols))))
        ),
        avg_distance = mean(unlist(distances)),
        sd_distance = sd(unlist(distances))
    ) %>%
    dplyr::select(Country, avg_distance, sd_distance)

pc_cols <- c("PC1","PC2")
# Assume your data frame is called df and has columns: Country, PC1, PC2, PC3, ...
df <- herb_df[herb_df$Region=="Middle" | herb_df$H_C=="C",]
df <- df[df$TimeBin=="1918-1957" | df$H_C=="C",]
library(dplyr)

# Calculate centroids for each country except USA
centroids <- df %>%
    filter(Country != "USA") %>%
    group_by(Country) %>%
    summarise(across(all_of(pc_cols), mean), .groups = "drop")

# Get USA samples
usa_samples <- df %>%
    filter(Country == "USA") %>%
    dplyr::select(all_of(pc_cols))

# Function to calculate Euclidean distance
euclidean <- function(a, b) sqrt(sum((a - b)^2))

# For each centroid, calculate distances to all USA samples
results_M_3<- centroids %>%
    rowwise() %>%
    mutate(
        distances = list(
            apply(usa_samples, 1, function(x) euclidean(x, c_across(all_of(pc_cols))))
        ),
        avg_distance = mean(unlist(distances)),
        sd_distance = sd(unlist(distances))
    ) %>%
    dplyr::select(Country, avg_distance, sd_distance)

pc_cols <- c("PC1","PC2")
# Assume your data frame is called df and has columns: Country, PC1, PC2, PC3, ...
df <- herb_df[herb_df$Region=="Middle" | herb_df$H_C=="C",]
df <- df[df$TimeBin=="1958-1997" | df$H_C=="C",]
library(dplyr)

# Calculate centroids for each country except USA
centroids <- df %>%
    filter(Country != "USA") %>%
    group_by(Country) %>%
    summarise(across(all_of(pc_cols), mean), .groups = "drop")

# Get USA samples
usa_samples <- df %>%
    filter(Country == "USA") %>%
    dplyr::select(all_of(pc_cols))

# Function to calculate Euclidean distance
euclidean <- function(a, b) sqrt(sum((a - b)^2))

# For each centroid, calculate distances to all USA samples
results_M_4<- centroids %>%
    rowwise() %>%
    mutate(
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
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12)
  ) +
  labs(
    x = "Time Bin",
    y = "Average Distance"
  )