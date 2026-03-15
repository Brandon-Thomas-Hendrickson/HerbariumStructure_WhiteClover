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

# Define group assignments
# Replace 'group_assignments.csv' with a file containing sample names and their groups
group_data <- read.csv("Data/herbarium/Input/herbarium_structure_dataframe_1192026.csv")
grp <- factor(group_data$PcoaGroups)  # Ensure this column contains the group labels (e.g., Spain, UK, France, 1838-1877, etc.)

# Filter to include only Spain, UK, France, and the 1838-1877 time block
selected_groups <- c("Spain", "UK", "France", "Poland","Belgium","Germany","Sweden", "Greece","1838-1877")
filtered_indices <- grp %in% selected_groups
cov_data_filtered <- cov_data[filtered_indices, filtered_indices]
grp_filtered <- droplevels(grp[filtered_indices])

# Perform DAPC analysis
dapc_result <- dapc(cov_data_filtered, grp = grp_filtered, n.pca = 10, n.da = 2)

# Extract proportions for the 1838-1877 herbarium time block
herbarium_block <- "1838-1877"  # Replace with the exact label used in your group assignments
herbarium_indices <- which(grp_filtered == herbarium_block)
herbarium_contributions <- dapc_result$posterior[herbarium_indices, ]

# Subset to only Spain, UK, and France
herbarium_contributions_subset <- herbarium_contributions[, c("Spain", "UK", "France", "Poland","Belgium","Germany","Sweden", "Greece")]

# Normalize the contributions so they sum to 1
herbarium_contributions_normalized <- sweep(herbarium_contributions_subset, 1, rowSums(herbarium_contributions_subset), "/")

# Calculate mean contribution of Spain, UK, and France to the herbarium block
mean_contributions1 <- colMeans(herbarium_contributions_normalized)

# Filter to include only Spain, UK, France, and the 1838-1877 time block
selected_groups <- c("Spain", "UK", "France", "Poland","Belgium","Germany","Sweden", "Greece","1878-1917")
filtered_indices <- grp %in% selected_groups
cov_data_filtered <- cov_data[filtered_indices, filtered_indices]
grp_filtered <- droplevels(grp[filtered_indices])

# Perform DAPC analysis
dapc_result <- dapc(cov_data_filtered, grp = grp_filtered, n.pca = 10, n.da = 2)

# Extract proportions for the 1838-1877 herbarium time block
herbarium_block <- "1878-1917"  # Replace with the exact label used in your group assignments
herbarium_indices <- which(grp_filtered == herbarium_block)
herbarium_contributions <- dapc_result$posterior[herbarium_indices, ]

# Subset to only Spain, UK, and France
herbarium_contributions_subset <- herbarium_contributions[, c("Spain", "UK", "France", "Poland","Belgium","Germany","Sweden", "Greece")]

# Normalize the contributions so they sum to 1
herbarium_contributions_normalized <- sweep(herbarium_contributions_subset, 1, rowSums(herbarium_contributions_subset), "/")

# Calculate mean contribution of Spain, UK, and France to the herbarium block
mean_contributions2 <- colMeans(herbarium_contributions_normalized)

# Filter to include only Spain, UK, France, and the 1838-1877 time block
selected_groups <- c("Spain", "UK", "France", "Poland","Belgium","Germany","Sweden", "Greece","1918-1957")
filtered_indices <- grp %in% selected_groups
cov_data_filtered <- cov_data[filtered_indices, filtered_indices]
grp_filtered <- droplevels(grp[filtered_indices])

# Perform DAPC analysis
dapc_result <- dapc(cov_data_filtered, grp = grp_filtered, n.pca = 10, n.da = 2)

# Extract proportions for the 1838-1877 herbarium time block
herbarium_block <- "1918-1957"  # Replace with the exact label used in your group assignments
herbarium_indices <- which(grp_filtered == herbarium_block)
herbarium_contributions <- dapc_result$posterior[herbarium_indices, ]

# Subset to only Spain, UK, and France
herbarium_contributions_subset <- herbarium_contributions[, c("Spain", "UK", "France", "Poland","Belgium","Germany","Sweden", "Greece")]

# Normalize the contributions so they sum to 1
herbarium_contributions_normalized <- sweep(herbarium_contributions_subset, 1, rowSums(herbarium_contributions_subset), "/")

# Calculate mean contribution of Spain, UK, and France to the herbarium block
mean_contributions3 <- colMeans(herbarium_contributions_normalized)

# Filter to include only Spain, UK, France, and the 1838-1877 time block
selected_groups <- c("Spain", "UK", "France", "Poland","Belgium","Germany","Sweden", "Greece","1958-1997")
filtered_indices <- grp %in% selected_groups
cov_data_filtered <- cov_data[filtered_indices, filtered_indices]
grp_filtered <- droplevels(grp[filtered_indices])

# Perform DAPC analysis
dapc_result <- dapc(cov_data_filtered, grp = grp_filtered, n.pca = 10, n.da = 2)

# Extract proportions for the 1838-1877 herbarium time block
herbarium_block <- "1958-1997"  # Replace with the exact label used in your group assignments
herbarium_indices <- which(grp_filtered == herbarium_block)
herbarium_contributions <- dapc_result$posterior[herbarium_indices, ]

# Subset to only Spain, UK, and France
herbarium_contributions_subset <- herbarium_contributions[, c("Spain", "UK", "France", "Poland","Belgium","Germany","Sweden", "Greece")]

# Normalize the contributions so they sum to 1
herbarium_contributions_normalized <- sweep(herbarium_contributions_subset, 1, rowSums(herbarium_contributions_subset), "/")

# Calculate mean contribution of Spain, UK, and France to the herbarium block
mean_contributions4 <- colMeans(herbarium_contributions_normalized)

# Filter to include only Spain, UK, France, and the 1838-1877 time block
selected_groups <- c("Spain", "UK", "France", "Poland","Belgium","Germany","Sweden", "Greece","1998-Present")
filtered_indices <- grp %in% selected_groups
cov_data_filtered <- cov_data[filtered_indices, filtered_indices]
grp_filtered <- droplevels(grp[filtered_indices])

# Perform DAPC analysis
dapc_result <- dapc(cov_data_filtered, grp = grp_filtered, n.pca = 10, n.da = 2)

# Extract proportions for the 1838-1877 herbarium time block
herbarium_block <- "1998-Present"  # Replace with the exact label used in your group assignments
herbarium_indices <- which(grp_filtered == herbarium_block)
herbarium_contributions <- dapc_result$posterior[herbarium_indices, ]

# Subset to only Spain, UK, and France
herbarium_contributions_subset <- herbarium_contributions[, c("Spain", "UK", "France", "Poland","Belgium","Germany","Sweden", "Greece")]

# Normalize the contributions so they sum to 1
herbarium_contributions_normalized <- sweep(herbarium_contributions_subset, 1, rowSums(herbarium_contributions_subset), "/")

# Calculate mean contribution of Spain, UK, and France to the herbarium block
mean_contributions5 <- colMeans(herbarium_contributions_normalized)

pol <- rbind(mean_contributions1, mean_contributions2, mean_contributions3, mean_contributions4, mean_contributions5)
rownames(pol) <- c("1838-1877", "1878-1917", "1918-1957", "1958-1997", "1998-Present")
pol <- as.data.frame(pol)
# Load your .cov file
# Replace 'herb_GLUE_SPAIN_filtered_MAF05.cov' with the path to your file
cov_data <- read.table("herb_GLUE_SPAIN_filtered_MAF05.cov", header = FALSE)

# Define group assignments
# Replace 'group_assignments.csv' with a file containing sample names and their groups
group_data <- read.csv("Data/herbarium/Input/herbarium_structure_dataframe_1192026.csv")
grp <- factor(group_data$PcoaGroupRegion)  # Ensure this column contains the group labels (e.g., Spain, UK, France, 1838-1877, etc.)

# Filter to include only Spain, UK, France, and the 1838-1877 time block
selected_groups <- c("Spain", "UK", "France", "Poland","Belgium","Germany","Sweden", "Greece","1838-1877_North")
filtered_indices <- grp %in% selected_groups
cov_data_filtered <- cov_data[filtered_indices, filtered_indices]
grp_filtered <- droplevels(grp[filtered_indices])

# Perform DAPC analysis
dapc_result <- dapc(cov_data_filtered, grp = grp_filtered, n.pca = 10, n.da = 2)

# Extract proportions for the 1838-1877 herbarium time block
herbarium_block <- "1838-1877_North"  # Replace with the exact label used in your group assignments
herbarium_indices <- which(grp_filtered == herbarium_block)
herbarium_contributions <- dapc_result$posterior[herbarium_indices, ]

# Subset to only Spain, UK, and France
herbarium_contributions_subset <- herbarium_contributions[, c("Spain", "UK", "France", "Poland","Belgium","Germany","Sweden", "Greece")]

# Normalize the contributions so they sum to 1
herbarium_contributions_normalized <- sweep(herbarium_contributions_subset, 1, rowSums(herbarium_contributions_subset), "/")

# Calculate mean contribution of Spain, UK, and France to the herbarium block
mean_contributions1 <- colMeans(herbarium_contributions_normalized)

# Filter to include only Spain, UK, France, and the 1838-1877 time block
selected_groups <- c("Spain", "UK", "France", "Poland","Belgium","Germany","Sweden", "Greece","1878-1917_North")
filtered_indices <- grp %in% selected_groups
cov_data_filtered <- cov_data[filtered_indices, filtered_indices]
grp_filtered <- droplevels(grp[filtered_indices])

# Perform DAPC analysis
dapc_result <- dapc(cov_data_filtered, grp = grp_filtered, n.pca = 10, n.da = 2)

# Extract proportions for the 1838-1877 herbarium time block
herbarium_block <- "1878-1917_North"  # Replace with the exact label used in your group assignments
herbarium_indices <- which(grp_filtered == herbarium_block)
herbarium_contributions <- dapc_result$posterior[herbarium_indices, ]

# Subset to only Spain, UK, and France
herbarium_contributions_subset <- herbarium_contributions[, c("Spain", "UK", "France", "Poland","Belgium","Germany","Sweden", "Greece")]

# Normalize the contributions so they sum to 1
herbarium_contributions_normalized <- sweep(herbarium_contributions_subset, 1, rowSums(herbarium_contributions_subset), "/")

# Calculate mean contribution of Spain, UK, and France to the herbarium block
mean_contributions2 <- colMeans(herbarium_contributions_normalized)

# Filter to include only Spain, UK, France, and the 1838-1877 time block
selected_groups <- c("Spain", "UK", "France", "Poland","Belgium","Germany","Sweden", "Greece","1918-1957_North")
filtered_indices <- grp %in% selected_groups
cov_data_filtered <- cov_data[filtered_indices, filtered_indices]
grp_filtered <- droplevels(grp[filtered_indices])

# Perform DAPC analysis
dapc_result <- dapc(cov_data_filtered, grp = grp_filtered, n.pca = 10, n.da = 2)

# Extract proportions for the 1838-1877 herbarium time block
herbarium_block <- "1918-1957_North"  # Replace with the exact label used in your group assignments
herbarium_indices <- which(grp_filtered == herbarium_block)
herbarium_contributions <- dapc_result$posterior[herbarium_indices, ]

# Subset to only Spain, UK, and France
herbarium_contributions_subset <- herbarium_contributions[, c("Spain", "UK", "France", "Poland","Belgium","Germany","Sweden", "Greece")]

# Normalize the contributions so they sum to 1
herbarium_contributions_normalized <- sweep(herbarium_contributions_subset, 1, rowSums(herbarium_contributions_subset), "/")

# Calculate mean contribution of Spain, UK, and France to the herbarium block
mean_contributions3 <- colMeans(herbarium_contributions_normalized)

# Filter to include only Spain, UK, France, and the 1838-1877 time block
selected_groups <- c("Spain", "UK", "France", "Poland","Belgium","Germany","Sweden", "Greece","1958-1997_North")
filtered_indices <- grp %in% selected_groups
cov_data_filtered <- cov_data[filtered_indices, filtered_indices]
grp_filtered <- droplevels(grp[filtered_indices])

# Perform DAPC analysis
dapc_result <- dapc(cov_data_filtered, grp = grp_filtered, n.pca = 10, n.da = 2)

# Extract proportions for the 1838-1877 herbarium time block
herbarium_block <- "1958-1997_North"  # Replace with the exact label used in your group assignments
herbarium_indices <- which(grp_filtered == herbarium_block)
herbarium_contributions <- dapc_result$posterior[herbarium_indices, ]

# Subset to only Spain, UK, and France
herbarium_contributions_subset <- herbarium_contributions[, c("Spain", "UK", "France", "Poland","Belgium","Germany","Sweden", "Greece")]

# Normalize the contributions so they sum to 1
herbarium_contributions_normalized <- sweep(herbarium_contributions_subset, 1, rowSums(herbarium_contributions_subset), "/")

# Calculate mean contribution of Spain, UK, and France to the herbarium block
mean_contributions4 <- colMeans(herbarium_contributions_normalized)

# Filter to include only Spain, UK, France, and the 1838-1877 time block
selected_groups <- c("Spain", "UK", "France", "Poland","Belgium","Germany","Sweden", "Greece","1998-Present_North")
filtered_indices <- grp %in% selected_groups
cov_data_filtered <- cov_data[filtered_indices, filtered_indices]
grp_filtered <- droplevels(grp[filtered_indices])

# Perform DAPC analysis
dapc_result <- dapc(cov_data_filtered, grp = grp_filtered, n.pca = 10, n.da = 2)

# Extract proportions for the 1838-1877 herbarium time block
herbarium_block <- "1998-Present_North"  # Replace with the exact label used in your group assignments
herbarium_indices <- which(grp_filtered == herbarium_block)
herbarium_contributions <- dapc_result$posterior[herbarium_indices, ]

# Subset to only Spain, UK, and France
herbarium_contributions_subset <- herbarium_contributions[, c("Spain", "UK", "France", "Poland","Belgium","Germany","Sweden", "Greece")]

# Normalize the contributions so they sum to 1
herbarium_contributions_normalized <- sweep(herbarium_contributions_subset, 1, rowSums(herbarium_contributions_subset), "/")

# Calculate mean contribution of Spain, UK, and France to the herbarium block
mean_contributions5 <- colMeans(herbarium_contributions_normalized)

pol_N <- rbind(mean_contributions1, mean_contributions2, mean_contributions3, mean_contributions4, mean_contributions5)
rownames(pol_N) <- c("1838-1877", "1878-1917", "1918-1957", "1958-1997", "1998-Present")
pol_N <- as.data.frame(pol_N)

# Load your .cov file
# Replace 'herb_GLUE_SPAIN_filtered_MAF05.cov' with the path to your file
cov_data <- read.table("herb_GLUE_SPAIN_filtered_MAF05.cov", header = FALSE)

# Define group assignments
# Replace 'group_assignments.csv' with a file containing sample names and their groups
group_data <- read.csv("Data/herbarium/Input/herbarium_structure_dataframe_1192026.csv")
grp <- factor(group_data$PcoaGroupRegion)  # Ensure this column contains the group labels (e.g., Spain, UK, France, 1838-1877, etc.)

# Filter to include only Spain, UK, France, and the 1838-1877 time block
selected_groups <- c("Spain", "UK", "France", "Poland","Belgium","Germany","Sweden", "Greece","1838-1877_South")
filtered_indices <- grp %in% selected_groups
cov_data_filtered <- cov_data[filtered_indices, filtered_indices]
grp_filtered <- droplevels(grp[filtered_indices])

# Perform DAPC analysis
dapc_result <- dapc(cov_data_filtered, grp = grp_filtered, n.pca = 10, n.da = 2)

# Extract proportions for the 1838-1877 herbarium time block
herbarium_block <- "1838-1877_South"  # Replace with the exact label used in your group assignments
herbarium_indices <- which(grp_filtered == herbarium_block)
herbarium_contributions <- dapc_result$posterior[herbarium_indices, ]

# Subset to only Spain, UK, and France
herbarium_contributions_subset <- herbarium_contributions[, c("Spain", "UK", "France", "Poland","Belgium","Germany","Sweden", "Greece")]

# Normalize the contributions so they sum to 1
herbarium_contributions_normalized <- sweep(herbarium_contributions_subset, 1, rowSums(herbarium_contributions_subset), "/")

# Calculate mean contribution of Spain, UK, and France to the herbarium block
mean_contributions1 <- colMeans(herbarium_contributions_normalized)

# Filter to include only Spain, UK, France, and the 1838-1877 time block
selected_groups <- c("Spain", "UK", "France", "Poland","Belgium","Germany","Sweden", "Greece","1878-1917_South")
filtered_indices <- grp %in% selected_groups
cov_data_filtered <- cov_data[filtered_indices, filtered_indices]
grp_filtered <- droplevels(grp[filtered_indices])

# Perform DAPC analysis
dapc_result <- dapc(cov_data_filtered, grp = grp_filtered, n.pca = 10, n.da = 2)

# Extract proportions for the 1838-1877 herbarium time block
herbarium_block <- "1878-1917_South"  # Replace with the exact label used in your group assignments
herbarium_indices <- which(grp_filtered == herbarium_block)
herbarium_contributions <- dapc_result$posterior[herbarium_indices, ]

# Subset to only Spain, UK, and France
herbarium_contributions_subset <- herbarium_contributions[, c("Spain", "UK", "France", "Poland","Belgium","Germany","Sweden", "Greece")]

# Normalize the contributions so they sum to 1
herbarium_contributions_normalized <- sweep(herbarium_contributions_subset, 1, rowSums(herbarium_contributions_subset), "/")

# Calculate mean contribution of Spain, UK, and France to the herbarium block
mean_contributions2 <- colMeans(herbarium_contributions_normalized)

# Filter to include only Spain, UK, France, and the 1838-1877 time block
selected_groups <- c("Spain", "UK", "France", "Poland","Belgium","Germany","Sweden", "Greece","1918-1957_South")
filtered_indices <- grp %in% selected_groups
cov_data_filtered <- cov_data[filtered_indices, filtered_indices]
grp_filtered <- droplevels(grp[filtered_indices])

# Perform DAPC analysis
dapc_result <- dapc(cov_data_filtered, grp = grp_filtered, n.pca = 10, n.da = 2)

# Extract proportions for the 1838-1877 herbarium time block
herbarium_block <- "1918-1957_South"  # Replace with the exact label used in your group assignments
herbarium_indices <- which(grp_filtered == herbarium_block)
herbarium_contributions <- dapc_result$posterior[herbarium_indices, ]

# Subset to only Spain, UK, and France
herbarium_contributions_subset <- herbarium_contributions[, c("Spain", "UK", "France", "Poland","Belgium","Germany","Sweden", "Greece")]

# Normalize the contributions so they sum to 1
herbarium_contributions_normalized <- sweep(herbarium_contributions_subset, 1, rowSums(herbarium_contributions_subset), "/")

# Calculate mean contribution of Spain, UK, and France to the herbarium block
mean_contributions3 <- colMeans(herbarium_contributions_normalized)

# Filter to include only Spain, UK, France, and the 1838-1877 time block
selected_groups <- c("Spain", "UK", "France", "Poland","Belgium","Germany","Sweden", "Greece","1958-1997_South")
filtered_indices <- grp %in% selected_groups
cov_data_filtered <- cov_data[filtered_indices, filtered_indices]
grp_filtered <- droplevels(grp[filtered_indices])

# Perform DAPC analysis
dapc_result <- dapc(cov_data_filtered, grp = grp_filtered, n.pca = 10, n.da = 2)

# Extract proportions for the 1838-1877 herbarium time block
herbarium_block <- "1958-1997_South"  # Replace with the exact label used in your group assignments
herbarium_indices <- which(grp_filtered == herbarium_block)
herbarium_contributions <- dapc_result$posterior[herbarium_indices, ]

# Subset to only Spain, UK, and France
herbarium_contributions_subset <- herbarium_contributions[, c("Spain", "UK", "France", "Poland","Belgium","Germany","Sweden", "Greece")]

# Normalize the contributions so they sum to 1
herbarium_contributions_normalized <- sweep(herbarium_contributions_subset, 1, rowSums(herbarium_contributions_subset), "/")

# Calculate mean contribution of Spain, UK, and France to the herbarium block
mean_contributions4 <- colMeans(herbarium_contributions_normalized)

# Filter to include only Spain, UK, France, and the 1838-1877 time block
selected_groups <- c("Spain", "UK", "France", "Poland","Belgium","Germany","Sweden", "Greece","1998-Present_South")
filtered_indices <- grp %in% selected_groups
cov_data_filtered <- cov_data[filtered_indices, filtered_indices]
grp_filtered <- droplevels(grp[filtered_indices])

# Perform DAPC analysis
dapc_result <- dapc(cov_data_filtered, grp = grp_filtered, n.pca = 10, n.da = 2)

# Extract proportions for the 1838-1877 herbarium time block
herbarium_block <- "1998-Present_South"  # Replace with the exact label used in your group assignments
herbarium_indices <- which(grp_filtered == herbarium_block)
herbarium_contributions <- dapc_result$posterior[herbarium_indices, ]

# Subset to only Spain, UK, and France
herbarium_contributions_subset <- herbarium_contributions[, c("Spain", "UK", "France", "Poland","Belgium","Germany","Sweden", "Greece")]

# Normalize the contributions so they sum to 1
herbarium_contributions_normalized <- sweep(herbarium_contributions_subset, 1, rowSums(herbarium_contributions_subset), "/")

# Calculate mean contribution of Spain, UK, and France to the herbarium block
mean_contributions5 <- colMeans(herbarium_contributions_normalized)

pol_S <- rbind(mean_contributions1, mean_contributions2, mean_contributions3, mean_contributions4, mean_contributions5)
rownames(pol_S) <- c("1838-1877", "1878-1917", "1918-1957", "1958-1997", "1998-Present")
pol_S <- as.data.frame(pol_S)

pol_ALL <- rbind(pol, pol_N, pol_S)

# Load your .cov file
# Replace 'herb_GLUE_SPAIN_filtered_MAF05.cov' with the path to your file
cov_data <- read.table("herb_GLUE_SPAIN_filtered_MAF05.cov", header = FALSE)

# Define group assignments
# Replace 'group_assignments.csv' with a file containing sample names and their groups
group_data <- read.csv("Data/herbarium/Input/herbarium_structure_dataframe_1192026.csv")
group_data$YR <- paste(group_data$Year,group_dat$Region,sep="_")
grp <- factor(group_data$YR)  # Ensure this column contains the group labels (e.g., Spain, UK, France, 1838-1877, etc.)

# Filter to include only Spain, UK, France, and the 1838_South group
selected_groups <- c("Spain", "UK", "France", "Poland","Belgium","Germany","Sweden", "Greece","1838_South")
filtered_indices <- grp %in% selected_groups
cov_data_filtered <- cov_data[filtered_indices, filtered_indices]
grp_filtered <- droplevels(grp[filtered_indices])

# Perform DAPC analysis
dapc_result <- dapc(cov_data_filtered, grp = grp_filtered, n.pca = 10, n.da = 2)

# Extract proportions for the 1838_South
herbarium_block <- "1838_South"  # Replace with the exact label used in your group assignments
herbarium_indices <- which(grp_filtered == herbarium_block)
herbarium_contributions <- dapc_result$posterior[herbarium_indices, , drop = FALSE] 

# Subset to only Spain, UK, and France
herbarium_contributions_subset <- herbarium_contributions[, c("Spain", "UK", "France", "Poland", "Belgium", "Germany", "Sweden", "Greece"), drop = FALSE]
# Normalize the contributions so they sum to 1
herbarium_contributions_normalized <- sweep(herbarium_contributions_subset, 1, rowSums(herbarium_contributions_subset), "/")

# Calculate mean contribution of Spain, UK, and France to the herbarium block
mean_contributions1 <- colMeans(herbarium_contributions_normalized)

group_data <- read.csv("Data/herbarium/Input/herbarium_structure_dataframe_1192026.csv")
grp <- factor(group_data$PcoaYR) 

# Define the range of years
years <- 1838:1997

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
dapc_result <- dapc(cov_data_filtered, grp = grp_filtered, n.pca = 10, n.da = 2)
  
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

south <- ggplot(results_df,aes(x=as.numeric(Year)))+geom_smooth(se=FALSE,aes(y=Belgium),color="blue")+geom_smooth(se=FALSE,aes(y=Spain),color="purple")+geom_smooth(se=FALSE,aes(y=UK),color="red")+geom_smooth(se=FALSE,aes(y=France),color="cyan")+geom_smooth(se=FALSE,aes(y=Poland),color="green")+geom_smooth(se=FALSE,aes(y=Germany),color="yellow")+geom_smooth(se=FALSE,aes(y=Sweden),color="grey")+geom_smooth(se=FALSE,aes(y=Greece),color="black")


group_data <- read.csv("Data/herbarium/Input/herbarium_structure_dataframe_1192026.csv")
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
dapc_result <- dapc(cov_data_filtered, grp = grp_filtered, n.pca = 10, n.da = 2)
  
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

north <- ggplot(results_df,aes(x=as.numeric(Year)))+geom_smooth(se=FALSE,aes(y=Belgium),color="blue")+geom_smooth(se=FALSE,aes(y=Spain),color="purple")+geom_smooth(se=FALSE,aes(y=UK),color="red")+geom_smooth(se=FALSE,aes(y=France),color="cyan")+geom_smooth(se=FALSE,aes(y=Poland),color="green")+geom_smooth(se=FALSE,aes(y=Germany),color="yellow")+geom_smooth(se=FALSE,aes(y=Sweden),color="grey")+geom_smooth(se=FALSE,aes(y=Greece),color="black")

library(ggpubr)

ggarrange(north,south,ncol=1,nrow=2)