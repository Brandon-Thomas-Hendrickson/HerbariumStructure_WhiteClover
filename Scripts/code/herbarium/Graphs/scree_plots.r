library(car)
library(dplyr)  # For data manipulation
library(ggplot2)
library(ggforce)  # For confidence ellipses

# Load the covariance matrix
cov_file <- "herb_GLUE_SPAIN_filtered_MAF05.cov"  # Replace with your .cov file path
cov_matrix <- as.matrix(read.table(cov_file))
# Perform eigen decomposition
eigen_decomp <- eigen(cov_matrix)

# Create a data frame for ggplot
# Extract PC1 through PC10
pc_axes <- eigen_decomp$vectors[, 1:10]  # Extract the first 10 principal components

# Create a data frame for ggplot, including PC1 through PC10
pca_data <- data.frame(
  PC1 = pc_axes[, 1],
  PC2 = pc_axes[, 2],
  PC3 = pc_axes[, 3],
  PC4 = pc_axes[, 4],
  PC5 = pc_axes[, 5],
  PC6 = pc_axes[, 6],
  PC7 = pc_axes[, 7],
  PC8 = pc_axes[, 8],
  PC9 = pc_axes[, 9],
  PC10 = pc_axes[, 10]
)

# Scree plot with PC contributions
explained_var <- eigen_decomp$values / sum(eigen_decomp$values)
scree_df <- data.frame(
  PC = factor(paste0("PC", 1:10), levels = paste0("PC", 1:10)),
  Variance = explained_var[1:10]
)

scree_plot <- ggplot(scree_df, aes(x = PC, y = Variance)) +
  geom_bar(stat = "identity", fill = "#56B4E9") +
  geom_text(aes(label = scales::percent(Variance, accuracy = 0.1)),
            vjust = -0.5, size = 4) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Principal Component",
    y = "Variance Explained (%)"
  ) +
  theme_light() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold")
  )

print(scree_plot)