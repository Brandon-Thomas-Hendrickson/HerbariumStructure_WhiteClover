library(dplyr)
library(purrr)
library(ggplot2)
library(ggpubr)
library(adegenet)

herb <- read.csv("Data/herbarium/Input/herbarium_structure_dataframe_1192026.csv",header=TRUE)
order <- read.csv("Data/herbarium/Input/order_files.csv")
herb <- herb[match(order$Samp, herb$Samp), ]
herb_df_only <- herb[herb$H_C == "H",]
herb_df_north <- herb_df_only[herb_df_only$Region == "North",]
herb_df_south <- herb_df_only[herb_df_only$Region == "South" & herb_df_only$Year > 1900,]
herb_df_middle <- herb_df_only[herb_df_only$Region == "Middle",]

herb_df_south <- herb_df_south[herb_df_south$Year < 1998,]
herb_df_north <- herb_df_north[herb_df_north$Year < 1998,]
herb_df_middle <- herb_df_middle[herb_df_middle$Year < 1998,]

detect_anomalies_30yr <- function(df, year_col = "Year", MaxK3_LD_col = "MaxK3_LD",
                                  window_years = 30, z_threshold = 3) {
  df2 <- df %>%
    dplyr::rename(Year = all_of(year_col),
                  MaxK3_LD = all_of(MaxK3_LD_col)) %>%
    arrange(Year) %>%
    mutate(
      baseline_mu = purrr::map_dbl(seq_along(MaxK3_LD), function(i) {
        yrs <- Year[i] - window_years
        base_vals <- MaxK3_LD[Year < Year[i] & Year >= yrs]
        if(length(base_vals) >= 3) mean(base_vals, na.rm = TRUE) else NA_real_
      }),
      baseline_sd = purrr::map_dbl(seq_along(MaxK3_LD), function(i) {
        yrs <- Year[i] - window_years
        base_vals <- MaxK3_LD[Year < Year[i] & Year >= yrs]
        if(length(base_vals) >= 3) sd(base_vals, na.rm = TRUE) else NA_real_
      }),
      z = ifelse(!is.na(baseline_sd) & baseline_sd > 0,
                 (MaxK3_LD - baseline_mu) / baseline_sd, NA_real_),
      flag = !is.na(z) & abs(z) >= z_threshold
    )
  
  return(df2)
}

herb_df_North <- detect_anomalies_30yr(herb_df_north, year_col="Year", MaxK3_LD_col="MaxK3_LD")
herb_df_South <- detect_anomalies_30yr(herb_df_south, year_col="Year", MaxK3_LD_col="MaxK3_LD")
herb_df_Middle <- detect_anomalies_30yr(herb_df_middle, year_col="Year", MaxK3_LD_col="MaxK3_LD")

herb_df_North$color <- ifelse(herb_df_North$flag, "red", "black")
herb_df_South$color <- ifelse(herb_df_South$flag, "red", "black")
herb_df_Middle$color <- ifelse(herb_df_Middle$flag, "red", "black")


herb_save <- rbind(herb_df_North,herb_df_South,herb_df_Middle)
herb_save <- herb_save[,c("Year","MaxK3_LD","K3Q1S1964","K3Q2S1964","Region","color","Samp")]
library(ggplot2)

North <- ggplot(herb_df_North, aes(x = Year)) +
    geom_point(aes(y = MaxK3_LD, color = herb_df_North$color)) +
    geom_smooth(aes(y = K3Q1S1964, color = "#8DD3C7"), method="lm", size = 1.5) +
    geom_smooth(aes(y = K3Q2S1964, color = "#FFFFB3"), method="lm", size = 1.5,formula = y ~ poly(x, 2)) +
    scale_color_identity() +
    theme_light() +
    theme(
        axis.text = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        plot.title = element_text(hjust = 0.5)
    ) +
    labs(
        x = "Year",
        y = "Q Proportion"
    )+ggtitle("North: > 40°N")

South <- ggplot(herb_df_South, aes(x = Year)) +
    geom_point(aes(y = MaxK3_LD, color = herb_df_South$color)) +
    geom_smooth(aes(y = K3Q1S1964, color = "#8DD3C7"), method="lm", size = 1.5) +
    geom_smooth(aes(y = K3Q2S1964, color = "#FFFFB3"), method="lm", size = 1.5) +
    scale_color_identity() +
    theme_light() +
    theme(
        axis.text = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        plot.title = element_text(hjust = 0.5)
    ) +
    labs(
        x = "Year",
        y = "Q Proportion"
    )+ggtitle("South: < 35°N")

Middle <- ggplot(herb_df_Middle, aes(x = Year)) +
    geom_point(aes(y = MaxK3_LD,color = herb_df_Middle$color)) +
    geom_smooth(aes(y = K3Q1S1964, color = "#8DD3C7"),method="lm", size = 1.5) +
    geom_smooth(aes(y = K3Q2S1964, color = "#FFFFB3"),method="lm", size = 1.5) +
    scale_color_identity() +
    theme_light() +
    theme(
        axis.text = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        plot.title = element_text(hjust = 0.5)
    ) +
    labs(
        x = "Year",
        y = "Q Proportion"
    )+ggtitle("Mid-Atlantic: 40°N - 35°N")

library(ggpubr)



North <- North + theme(plot.margin = margin(10, 10, 10, 10))
Middle <- Middle + theme(plot.margin = margin(10, 10, 10, 10))
South <- South + theme(plot.margin = margin(10, 10, 10, 10))

combined_panelA <- ggarrange(North, Middle, South, ncol = 1, nrow = 3,
                      common.legend = TRUE, legend = "bottom")

responses <- c("MaxK3_LD", "K3Q1S1964", "K3Q2S1964")
regions <- list(North = herb_df_North, South = herb_df_South, Middle = herb_df_Middle)

model_results <- data.frame(
  country = character(),
  region = character(),
  best_model = character(),
  linear_AIC = numeric(),
  quadratic_AIC = numeric(),
  linear_p_value = numeric(),
  linear_B = numeric(),
  linear_F = numeric(),
  linear_num_df = numeric(),
  linear_den_df = numeric(),
  quad_p_value = numeric(),
  quad_F = numeric(),
  quad_num_df = numeric(),
  quad_den_df = numeric(),
  stringsAsFactors = FALSE
)

for (resp in responses) {
  for (reg in names(regions)) {
    df <- regions[[reg]]
    # Remove rows with NA in response or Year
    df <- df[!is.na(df[[resp]]) & !is.na(df$Year), ]
    if (nrow(df) > 2) {
      lm_linear <- lm(as.formula(paste(resp, "~ Year")), data = df)
      lm_quad <- lm(as.formula(paste(resp, "~ Year + I(Year^2)")), data = df)
      linear_AIC <- AIC(lm_linear)
      quadratic_AIC <- AIC(lm_quad)
      best_model <- ifelse(quadratic_AIC < linear_AIC, "quadratic", "linear")
      linear_p_value <- summary(lm_linear)$coefficients["Year", "Pr(>|t|)"]
      linear_B <- summary(lm_linear)$coefficients["Year", "Estimate"]
      linear_F = summary(lm_linear)$fstatistic[1]
      quadratic_p_value <- summary(lm_quad)$coefficients["Year", "Pr(>|t|)"]
      quadratic_F = summary(lm_quad)$fstatistic[1]
      linear_num_df <- summary(lm_linear)$fstatistic[2]
linear_den_df <- summary(lm_linear)$fstatistic[3]
quad_num_df   <- summary(lm_quad)$fstatistic[2]
quad_den_df   <- summary(lm_quad)$fstatistic[3]
      model_results <- rbind(model_results, data.frame(
        response = resp,
        region = reg,
        best_model = best_model,
        linear_AIC = linear_AIC,
        quadratic_AIC = quadratic_AIC,
        linear_p_value = linear_p_value,
        linear_B = linear_B,
        linear_F = linear_F,
        linear_num_df = linear_num_df,
        linear_den_df = linear_den_df,
        quadratic_p_value = quadratic_p_value,
        quadratic_F = quadratic_F,
        quad_num_df = quad_num_df,
        quad_den_df = quad_den_df,
        stringsAsFactors = FALSE
      ))
    }
  }
}

print(model_results)
###########################################
#########DAPC Analysis######################
###########################################

# Install and load the adegenet package
if (!requireNamespace("adegenet", quietly = TRUE)) {
  install.packages("adegenet")
}
library(adegenet)


# Load your .cov file
# Replace 'herb_GLUE_SPAIN_filtered_MAF05.cov' with the path to your file
cov_data <- read.table("Data/herbarium/Input/herb_GLUE_SPAIN_filtered_LD_MAF05.cov", header = FALSE)

group_data <- read.csv("Data/herbarium/Input/herbarium_structure_dataframe_1192026.csv")
order <- read.csv("Data/herbarium/Input/order_files.csv")
group_data <- group_data[match(order$Samp, group_data$Samp), ]
grp <- factor(group_data$PcoaYR) 

# Define the range of years
years <- 1900:1997

# Initialize an empty list to store results
results_list <- list()

# Loop through each year
for (year in years) {
  # Define the herbarium block for the current year
herbarium_block <- paste0(year, "_South")  # Adjust this if your group labels differ
selected_groups <- c("Spain", "UK", "France", "Poland","Belgium","Germany","Sweden", "Greece",herbarium_block)
filtered_indices <- grp %in% selected_groups
cov_data_filtered <- cov_data[filtered_indices, filtered_indices]
grp_filtered <- droplevels(grp[filtered_indices])

# Perform DAPC analysis
dapc_result <- dapc(cov_data_filtered, grp = grp_filtered, n.pca = 2, n.da = 2)
  
  # Check if the current year exists in the filtered groups
  if (herbarium_block %in% levels(grp_filtered)) {
    # Extract indices for the current herbarium block
    herbarium_indices <- which(grp_filtered == herbarium_block)
    herbarium_contributions <- dapc_result$posterior[herbarium_indices, , drop = FALSE]
    
    # Subset to only Spain, UK, and France
    herbarium_contributions_subset <- herbarium_contributions[, c("Spain", "UK", "France", "Poland","Belgium","Germany","Sweden", "Greece"), drop = FALSE]
    
    # Normalize the contributions so they sum to 1
    herbarium_contributions_normalized <- sweep(herbarium_contributions_subset, 1, rowSums(herbarium_contributions_subset), "/")
    
    # Calculate mean contribution of Spain, UK, and France
    mean_contributions <- colMeans(herbarium_contributions_normalized)
    
    # Add the results to the list
    results_list[[as.character(year)]] <- c(Year = year, mean_contributions)
  }
}

# Combine the results into a dataframe
results_df_south <- do.call(rbind, results_list)

# Convert to a dataframe and set column names
results_df_south <- as.data.frame(results_df_south)
colnames(results_df_south) <- c("Year", "Spain", "UK", "France", "Poland","Belgium","Germany","Sweden", "Greece")

# Print the dataframe
print(results_df_south)

colorBlindBlack8 <- c("#F0E442", "#E69F00", "#D55E00", "#009E73", "#0072B2", "#56B4E9", "#CC79A7", "darkblue", "#000000")

# Reshape results_df to long format
library(tidyr)
results_df_S_long <- results_df_south %>%
  pivot_longer(cols = c("Belgium", "Spain", "UK", "France", "Poland", "Germany", "Sweden", "Greece"),
               names_to = "Country", values_to = "Proportion")

south <- ggplot(results_df_S_long, aes(x = as.numeric(Year), y = Proportion, color = Country)) +
geom_point(size = 1.5)+
  geom_smooth(se = FALSE, method = "lm", size = 1.5) +
  scale_color_manual(values = colorBlindBlack8[1:8]) +
  theme_light() +
  labs(y = "Mean Ancestry Proportion", color = "Country") +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 12, color = "black"),
    axis.text = element_text(size = 12, color = "black"),
    axis.ticks.x = element_blank(),
      legend.text = element_text(size = 16),   # Make legend text larger
  legend.title = element_blank(),          # Remove legend title
  legend.position = "null"
  )


group_data <- read.csv("Data/herbarium/Input/herbarium_structure_dataframe_1192026.csv")
order <- read.csv("Data/herbarium/Input/order_files.csv")
group_data <- group_data[match(order$Samp, group_data$Samp), ]
grp <- factor(group_data$PcoaYR) 


# Define the range of years
years <- 1838:1997

# Initialize an empty list to store results
results_list <- list()

# Loop through each year
for (year in years) {
  # Define the herbarium block for the current year
herbarium_block <- paste0(year, "_North")  # Adjust this if your group labels differ
selected_groups <- c("Spain", "UK", "France", "Poland","Belgium","Germany","Sweden", "Greece",herbarium_block)
filtered_indices <- grp %in% selected_groups
cov_data_filtered <- cov_data[filtered_indices, filtered_indices]
grp_filtered <- droplevels(grp[filtered_indices])

# Perform DAPC analysis
dapc_result <- dapc(cov_data_filtered, grp = grp_filtered, n.pca = 2, n.da = 2)
  
  # Check if the current year exists in the filtered groups
  if (herbarium_block %in% levels(grp_filtered)) {
    # Extract indices for the current herbarium block
    herbarium_indices <- which(grp_filtered == herbarium_block)
    herbarium_contributions <- dapc_result$posterior[herbarium_indices, , drop = FALSE]
    
    # Subset to only Spain, UK, and France
    herbarium_contributions_subset <- herbarium_contributions[, c("Spain", "UK", "France", "Poland","Belgium","Germany","Sweden", "Greece"), drop = FALSE]
    
    # Normalize the contributions so they sum to 1
    herbarium_contributions_normalized <- sweep(herbarium_contributions_subset, 1, rowSums(herbarium_contributions_subset), "/")
    
    # Calculate mean contribution of Spain, UK, and France
    mean_contributions <- colMeans(herbarium_contributions_normalized)
    
    # Add the results to the list
    results_list[[as.character(year)]] <- c(Year = year, mean_contributions)
  }
}

# Combine the results into a dataframe
results_df_N <- do.call(rbind, results_list)

# Convert to a dataframe and set column names
results_df_N <- as.data.frame(results_df_N)
colnames(results_df_N) <- c("Year", "Spain", "UK", "France", "Poland","Belgium","Germany","Sweden", "Greece")

colorBlindBlack8 <- c("#F0E442", "#E69F00", "#D55E00", "#009E73", "#0072B2", "#56B4E9", "#CC79A7", "darkblue", "#000000")

# Reshape results_df to long format
library(tidyr)
results_df_N_long <- results_df_N %>%
  pivot_longer(cols = c("Belgium", "Spain", "UK", "France", "Poland", "Germany", "Sweden", "Greece"),
               names_to = "Country", values_to = "Proportion")

north <- ggplot(results_df_N_long, aes(x = as.numeric(Year), y = Proportion, color = Country)) +
  geom_point(size = 1.5) + # Ensures legend is correct
  # Belgium (quadratic)
  geom_smooth(
    data = subset(results_df_N_long, Country == "Belgium"),
    aes(color = Country),
    se = FALSE, method = "lm", formula = y ~ poly(x, 2), size = 1.5
  ) +
  # UK (quadratic)
  geom_smooth(
    data = subset(results_df_N_long, Country == "UK"),
    aes(color = Country),
    se = FALSE, method = "lm", formula = y ~ poly(x, 2), size = 1.5
  ) +
  # All others (linear)
  geom_smooth(
    data = subset(results_df_N_long, !(Country %in% c("Belgium", "UK"))),
    aes(color = Country),
    se = FALSE, method = "lm", size = 1.5
  ) +
  scale_color_manual(values = colorBlindBlack8[1:8]) +
  theme_light() +
  labs(y = "Mean Ancestry Proportion", color = "Country") +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 12, color = "black"),
    axis.text = element_text(size = 12, color = "black"),
    axis.ticks.x = element_blank(),
    legend.text = element_text(size = 16),
    legend.title = element_blank(),
    legend.position = "null"
  )

library(ggpubr)

# Mid Atlantic
group_data <- read.csv("Data/herbarium/Input/herbarium_structure_dataframe_1192026.csv")
order <- read.csv("Data/herbarium/Input/order_files.csv")
group_data <- group_data[match(order$Samp, group_data$Samp), ]
grp <- factor(group_data$PcoaYR) 

# Define the range of years
years <- 1838:1997

# Initialize an empty list to store results
results_list <- list()

# Loop through each year
for (year in years) {
  # Define the herbarium block for the current year
herbarium_block <- paste0(year, "_Middle")  # Adjust this if your group labels differ
selected_groups <- c("Spain", "UK", "France", "Poland","Belgium","Germany","Sweden", "Greece",herbarium_block)
filtered_indices <- grp %in% selected_groups
cov_data_filtered <- cov_data[filtered_indices, filtered_indices]
grp_filtered <- droplevels(grp[filtered_indices])

# Perform DAPC analysis
dapc_result <- dapc(cov_data_filtered, grp = grp_filtered, n.pca = 2, n.da = 2)
  
  # Check if the current year exists in the filtered groups
  if (herbarium_block %in% levels(grp_filtered)) {
    # Extract indices for the current herbarium block
    herbarium_indices <- which(grp_filtered == herbarium_block)
    herbarium_contributions <- dapc_result$posterior[herbarium_indices, , drop = FALSE]
    
    # Subset to only Spain, UK, and France
    herbarium_contributions_subset <- herbarium_contributions[, c("Spain", "UK", "France", "Poland","Belgium","Germany","Sweden", "Greece"), drop = FALSE]
    
    # Normalize the contributions so they sum to 1
    herbarium_contributions_normalized <- sweep(herbarium_contributions_subset, 1, rowSums(herbarium_contributions_subset), "/")
    
    # Calculate mean contribution of Spain, UK, and France
    mean_contributions <- colMeans(herbarium_contributions_normalized)
    
    # Add the results to the list
    results_list[[as.character(year)]] <- c(Year = year, mean_contributions)
  }
}

# Combine the results into a dataframe
results_df <- do.call(rbind, results_list)

# Convert to a dataframe and set column names
results_df <- as.data.frame(results_df)
colnames(results_df) <- c("Year", "Spain", "UK", "France", "Poland","Belgium","Germany","Sweden", "Greece")

colorBlindBlack8 <- c("#F0E442", "#E69F00", "#D55E00", "#009E73", "#0072B2", "#56B4E9", "#CC79A7", "darkblue", "#000000")

# Reshape results_df to long format
library(tidyr)
results_df_long <- results_df %>%
  pivot_longer(cols = c("Belgium", "Spain", "UK", "France", "Poland", "Germany", "Sweden", "Greece"),
               names_to = "Country", values_to = "Proportion")

middle <- ggplot(results_df_long, aes(x = as.numeric(Year), y = Proportion, color = Country)) +
  geom_point(size = 1.5) + # Ensures legend is correct
  geom_smooth(
    data = subset(results_df_long, Country == "UK"),
    aes(color = Country),
    se = FALSE, method = "lm", formula = y ~ poly(x, 2), size = 1.5
  ) +
  # All others (linear)
  geom_smooth(
    data = subset(results_df_long, !(Country %in% c("UK"))),
    aes(color = Country),
    se = FALSE, method = "lm", size = 1.5
  ) +
  scale_color_manual(values = colorBlindBlack8[1:8]) +
  theme_light() +
  labs(y = "Mean Ancestry Proportion", color = "Country") +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 12, color = "black"),
    axis.text = element_text(size = 12, color = "black"),
    axis.ticks.x = element_blank(),
    legend.text = element_text(size = 16),
    legend.title = element_blank(),
    legend.position = "null"
  )

final_figure <- ggarrange(South, Middle, North, south, middle, north, ncol = 3, nrow = 2, labels = c("A", "B","C","D","E","F"),
                          common.legend = TRUE, legend = "bottom")

ggsave("Figure3.pdf", plot = final_figure, width = 9, height = 6, units = "in")

# Save big dataframe 
results_df$Region <- "Middle"
results_df_N$Region <- "North"
results_df_south$Region <- "South"
herb_save <- rbind(results_df_N,results_df_south,results_df)
###########################################
#########Combine Panels######################

#Model Summaries for each Country
model_south_spain <- lm(Spain ~ Year, data = results_df_south)
summary(model_south_spain)
model_south_uk <- lm(UK ~ Year, data = results_df_south)
summary(model_south_uk)
model_south_france <- lm(France ~ Year, data = results_df_south)
summary(model_south_france)
model_south_belgium <- lm(Belgium ~ Year, data = results_df_south)
summary(model_south_belgium)

model_N_spain <- lm(Spain ~ Year, data = results_df_N)
summary(model_N_spain)
model_N_uk <- lm(UK ~ Year, data = results_df_N)
summary(model_N_uk)
model_N_france <- lm(France ~ Year, data = results_df_N)
summary(model_N_france)
model_N_belgium <- lm(Belgium ~ Year, data = results_df_N)
summary(model_N_belgium)

model_spain <- lm(Spain ~ Year, data = results_df)
summary(model_spain)
model_uk <- lm(UK ~ Year, data = results_df)
summary(model_uk)
model_france <- lm(France ~ Year, data = results_df)
summary(model_france)
model_belgium <- lm(Belgium ~ Year, data = results_df)
summary(model_belgium)


countries <- c("Spain", "UK", "France", "Belgium")
regions <- list(
  South = results_df_south,
  North = results_df_N,
  Middle = results_df
)

model_results <- data.frame(
  country = character(),
  region = character(),
  best_model = character(),
  linear_AIC = numeric(),
  quadratic_AIC = numeric(),
  linear_p_value = numeric(),
  linear_B = numeric(),
  linear_F = numeric(),
  linear_num_df = numeric(),
  linear_den_df = numeric(),
  quad_p_value = numeric(),
  quad_F = numeric(),
  quad_num_df = numeric(),
  quad_den_df = numeric(),
  stringsAsFactors = FALSE
)

for (country in countries) {
  for (reg in names(regions)) {
    df <- regions[[reg]]
    # Remove rows with NA in country or Year
    df <- df[!is.na(df[[country]]) & !is.na(df$Year), ]
    if (nrow(df) > 2) {
      lm_linear <- lm(as.formula(paste(country, "~ Year")), data = df)
      lm_quad <- lm(as.formula(paste(country, "~ Year + I(Year^2)")), data = df)
      linear_AIC <- AIC(lm_linear)
      quadratic_AIC <- AIC(lm_quad)
      # Extract p-values for linear and quadratic terms
      linear_p <- summary(lm_linear)$coefficients["Year", "Pr(>|t|)"]
      linear_B <- summary(lm_linear)$coefficients["Year", "Estimate"]
      linear_F <- summary(lm_linear)$fstatistic[1]
       linear_num_df <- summary(lm_linear)$fstatistic[2]
      linear_den_df <- summary(lm_linear)$fstatistic[3]
      quad_p <- summary(lm_quad)$coefficients["I(Year^2)", "Pr(>|t|)"]
      quad_F <- summary(lm_quad)$fstatistic[1]
      quad_num_df <- summary(lm_quad)$fstatistic[2]
      quad_den_df <- summary(lm_quad)$fstatistic[3]
      best_model <- ifelse(quadratic_AIC < linear_AIC, "quadratic", "linear")
      model_results <- rbind(model_results, data.frame(
        country = country,
        region = reg,
        best_model = best_model,
        linear_AIC = linear_AIC,
        quadratic_AIC = quadratic_AIC,
        linear_p_value = linear_p,
        linear_B = linear_B,
        linear_F = linear_F,
        linear_num_df = linear_num_df,
        linear_den_df = linear_den_df,
        quad_p_value = quad_p,
        quad_F = quad_F,
        quad_num_df = quad_num_df,
        quad_den_df = quad_den_df,
        stringsAsFactors = FALSE
      ))
    }
  }
}

print(model_results)