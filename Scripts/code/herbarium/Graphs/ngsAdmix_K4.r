####################
#Supplemental K Figures#
######################
library(dplyr)
library(ggplot2)
library(tidyr)

# Load the data
herb_data <- read.csv("herbarium_structure_dataframe.csv")

# Filter and sort the data
filtered_data <- herb_data %>%
  filter(H_C == "C") %>%  # Filter rows where H_C is "H"
  arrange(Country)        # Sort by Country

# Reshape the data for ggplot (long format)
long_data <- filtered_data %>%
  select(Samp, Country, K4Q1S1964, K4Q2S1964, K4Q3S1964, K4Q4S1964) %>%
  pivot_longer(cols = starts_with("K4"), names_to = "Cluster", values_to = "Proportion") %>%
  mutate(Cluster = case_when(
    Cluster == "K4Q1S1964" ~ "1",
    Cluster == "K4Q2S1964" ~ "2",
    Cluster == "K4Q3S1964" ~ "3",
    Cluster == "K4Q4S1964" ~ "4"
  ))

# Create a custom order for Samp based on Cluster = 1 and then Cluster = 2
custom_order <- long_data %>%
  filter(Cluster %in% c("1", "4")) %>%  # Focus on Cluster 1 and 2
  pivot_wider(names_from = Cluster, values_from = Proportion, values_fill = 0) %>%  # Reshape to wide format
  arrange(Country, desc(`4`), desc(`1`)) %>%  # Sort by Country, Cluster 1, then Cluster 2
  pull(Samp)  # Extract the ordered Samp values

# Apply the custom order to Samp
long_data <- long_data %>%
  mutate(Samp = factor(Samp, levels = custom_order))  # Convert Samp to a factor with the custom order

# Create the NGSadmix plot with separate panels for each Country
ngsadmix_plot <- ggplot(long_data, aes(x = Samp, y = Proportion, fill = Cluster)) +
  geom_bar(stat = "identity", position = "stack", width = 1) +  # Ensure no space between bars
  labs(title = "NGSadmix Plot for K = 3", y = "Proportion") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_blank(),  # Hide x-axis text for clarity
    axis.ticks.x = element_blank(), # Hide x-axis ticks
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    strip.text = element_text(size = 12, face = "bold")  # Style for facet labels
  ) +
  scale_fill_brewer(palette = "Set3") +
  scale_y_continuous(limits = c(0, 1.01), expand = c(0, 0)) +  # Disable axis expansion
  facet_wrap(~ Country, nrow = 2, ncol = 4, scales = "free_x")  # Specify 2 rows and 4 columns

# Display the plot
print(ngsadmix_plot)

min_year <- min(herb_data[!is.na(herb_data$Year), "Year"])  # Get the minimum year from the data
# Filter the data
filtered_data_T1_UFS <- herb_data %>%
    filter(
 (H_C == "H" & Year >= min_year & Year <= min_year + 39)
    )

# Reshape the data for ggplot (long format)
long_data_T1 <- filtered_data_T1_UFS %>%
  select(Samp, Country, K4Q1S1964, K4Q2S1964, K4Q3S1964, K4Q4S1964) %>%
  pivot_longer(cols = starts_with("K4"), names_to = "Cluster", values_to = "Proportion") %>%
  mutate(Cluster = case_when(
    Cluster == "K4Q1S1964" ~ "1",
    Cluster == "K4Q2S1964" ~ "2",
    Cluster == "K4Q3S1964" ~ "3",
    Cluster == "K4Q4S1964" ~ "4"
  ))

# Create a custom order for Samp based on Lat
custom_order_T1 <- filtered_data_T1_UFS %>%
  arrange(desc(lat)) %>%  # Sort by Lat in descending order (use `asc(lat)` for ascending order)
  pull(Samp)  # Extract the ordered Samp values

# Apply the custom order to Samp
long_data_T1 <- long_data_T1 %>%
  mutate(Samp = factor(Samp, levels = custom_order_T1)) 


# Filter the data
filtered_data_T2_UFS <- herb_data %>%
    filter(
 (H_C == "H" & Year >= min_year + 40 & Year <= min_year + 79)
    )

# Reshape the data for ggplot (long format)
long_data_T2 <- filtered_data_T2_UFS %>%
  select(Samp, Country, K4Q1S1964, K4Q2S1964, K4Q3S1964, K4Q4S1964) %>%
  pivot_longer(cols = starts_with("K4"), names_to = "Cluster", values_to = "Proportion") %>%
  mutate(Cluster = case_when(
    Cluster == "K4Q1S1964" ~ "1",
    Cluster == "K4Q2S1964" ~ "2",
    Cluster == "K4Q3S1964" ~ "3",
    Cluster == "K4Q4S1964" ~ "4"
  ))

# Create a custom order for Samp based on Lat
custom_order_T2 <- filtered_data_T2_UFS %>%
  arrange(desc(lat)) %>%  # Sort by Lat in descending order (use `asc(lat)` for ascending order)
  pull(Samp)  # Extract the ordered Samp values

# Apply the custom order to Samp
long_data_T2 <- long_data_T2 %>%
  mutate(Samp = factor(Samp, levels = custom_order_T2)) 


# Filter the data
filtered_data_T3_UFS <- herb_data %>%
    filter(
 (H_C == "H" & Year >= min_year + 80 & Year <= min_year + 119)
    )

# Reshape the data for ggplot (long format)
long_data_T3 <- filtered_data_T3_UFS %>%
  select(Samp, Country, K4Q1S1964, K4Q2S1964, K4Q3S1964, K4Q4S1964) %>%
  pivot_longer(cols = starts_with("K4"), names_to = "Cluster", values_to = "Proportion") %>%
  mutate(Cluster = case_when(
    Cluster == "K4Q1S1964" ~ "1",
    Cluster == "K4Q2S1964" ~ "2",
    Cluster == "K4Q3S1964" ~ "3",
    Cluster == "K4Q4S1964" ~ "4"
  ))

# Create a custom order for Samp based on Lat
custom_order_T3 <- filtered_data_T3_UFS %>%
  arrange(desc(lat)) %>%  # Sort by Lat in descending order (use `asc(lat)` for ascending order)
  pull(Samp)  # Extract the ordered Samp values

# Apply the custom order to Samp
long_data_T3 <- long_data_T3 %>%
  mutate(Samp = factor(Samp, levels = custom_order_T3)) 


# Filter the data
filtered_data_T4_UFS <- herb_data %>%
    filter(
 (H_C == "H" & Year >= min_year + 120 & Year <= min_year + 169)
    )

# Reshape the data for ggplot (long format)
long_data_T4 <- filtered_data_T4_UFS %>%
  select(Samp, Country, K4Q1S1964, K4Q2S1964, K4Q3S1964, K4Q4S1964) %>%
  pivot_longer(cols = starts_with("K4"), names_to = "Cluster", values_to = "Proportion") %>%
  mutate(Cluster = case_when(
    Cluster == "K4Q1S1964" ~ "1",
    Cluster == "K4Q2S1964" ~ "2",
    Cluster == "K4Q3S1964" ~ "3",
    Cluster == "K4Q4S1964" ~ "4"
  ))

# Create a custom order for Samp based on Lat
custom_order_T4 <- filtered_data_T4_UFS %>%
  arrange(desc(lat)) %>%  # Sort by Lat in descending order (use `asc(lat)` for ascending order)
  pull(Samp)  # Extract the ordered Samp values

# Apply the custom order to Samp
long_data_T4 <- long_data_T4 %>%
  mutate(Samp = factor(Samp, levels = custom_order_T4)) 

# Create the NGSadmix plot with separate panels for each Country
ngsadmix_plot_T1 <- ggplot(long_data_T1, aes(x = Samp, y = Proportion, fill = Cluster)) +
  geom_bar(stat = "identity", position = "stack", width = 1) +  # Ensure no space between bars
  labs(title = "1838 - 1877", y = "Proportion") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),  # Hide x-axis text for clarity
    axis.ticks.x = element_blank(), # Hide x-axis ticks
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16)
  ) +
  scale_fill_brewer(palette = "Set3") +
  scale_y_continuous(limits = c(0, 1.01), expand = c(0, 0)) # Disable axis expansion

# Create the NGSadmix plot with separate panels for each Country
ngsadmix_plot_T2 <- ggplot(long_data_T2, aes(x = Samp, y = Proportion, fill = Cluster)) +
  geom_bar(stat = "identity", position = "stack", width = 1) +  # Ensure no space between bars
  labs(title = "1878 - 1917", y = "Proportion") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),  # Hide x-axis text for clarity
    axis.ticks.x = element_blank(), # Hide x-axis ticks
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16)
  ) +
  scale_fill_brewer(palette = "Set3") +
  scale_y_continuous(limits = c(0, 1.01), expand = c(0, 0)) 

ngsadmix_plot_T3 <- ggplot(long_data_T3, aes(x = Samp, y = Proportion, fill = Cluster)) +
  geom_bar(stat = "identity", position = "stack", width = 1) +  # Ensure no space between bars
  labs(title = "1918 - 1957", y = "Proportion") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),  # Hide x-axis text for clarity
    axis.ticks.x = element_blank(), # Hide x-axis ticks
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16)
  ) +
  scale_fill_brewer(palette = "Set3") +
  scale_y_continuous(limits = c(0, 1.01), expand = c(0, 0)) 

ngsadmix_plot_T4 <- ggplot(long_data_T4, aes(x = Samp, y = Proportion, fill = Cluster)) +
  geom_bar(stat = "identity", position = "stack", width = 1) +  # Ensure no space between bars
  labs(title = "1958 - 1997", y = "Proportion") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),  # Hide x-axis text for clarity
    axis.ticks.x = element_blank(), # Hide x-axis ticks
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16)
  ) +
  scale_fill_brewer(palette = "Set3") +
  scale_y_continuous(limits = c(0, 1.01), expand = c(0, 0)) 


part2 <- ggarrange(
  ngsadmix_plot_T1,
  ngsadmix_plot_T2,
  ngsadmix_plot_T3,
  ngsadmix_plot_T4,
  ncol = 1, nrow = 4
)

filtered_data_All <- herb_data %>%
    filter(
 (H_C == "H")
    )

# Reshape the data for ggplot (long format)
long_data_All <- filtered_data_All %>%
  select(Samp, Country, K4Q1S1964, K4Q2S1964, K4Q3S1964, K4Q4S1964) %>%
  pivot_longer(cols = starts_with("K4"), names_to = "Cluster", values_to = "Proportion") %>%
  mutate(Cluster = case_when(
    Cluster == "K4Q1S1964" ~ "1",
    Cluster == "K4Q2S1964" ~ "2",
    Cluster == "K4Q3S1964" ~ "3",
    Cluster == "K4Q4S1964" ~ "4"
  ))

# Create a custom order for Samp based on Lat
custom_order_All <- filtered_data_All %>%
  arrange(desc(lat)) %>%  # Sort by Lat in descending order (use `asc(lat)` for ascending order)
  pull(Samp)  # Extract the ordered Samp values

# Apply the custom order to Samp
long_data_All <- long_data_All %>%
  mutate(Samp = factor(Samp, levels = custom_order_All))  # Convert Samp to a factor with the custom order

ngsadmix_plot_All <- ggplot(long_data_All, aes(x = Samp, y = Proportion, fill = Cluster)) +
  geom_bar(stat = "identity", position = "stack", width = 1) +  # Ensure no space between bars
  labs(title = "Herbarium Samples", y = "Proportion") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),  # Hide x-axis text for clarity
    axis.ticks.x = element_blank(), # Hide x-axis ticks
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16)
  ) +
  scale_fill_brewer(palette = "Set3") +
  scale_y_continuous(limits = c(0, 1.01), expand = c(0, 0)) 

part1 <- ggarrange(
  ngsadmix_plot_All, ngsadmix_plot,
  ncol = 1, nrow = 2
)

ggarrange(part1, part2,
          ncol = 2, nrow = 1,
          labels = c("A", "B"))