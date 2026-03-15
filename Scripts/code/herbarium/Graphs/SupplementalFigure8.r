library(car)
library(dplyr)
library(ggplot2)
library(ggforce)
library(randomForest)

input.full.data <- read.csv("Data/herbarium/Input/herbarium_structure_dataframe_1192026.csv",header=TRUE)
input.full.data$Country[input.full.data$Country=="NA"]<-ifelse(input.full.data$H_C=="H","USA","NA")
herb_EUR_list <- input.full.data
n_iter <- 1000
all_summaries <- list()
all_models <- list()
all_errors <- numeric(n_iter)
all_test_sets <- list()  # Store test sets with predictions for each iteration
all_final_rows <- list()  # Store final_rows dataframes for each iteration
cov_file <- "Data/herbarium/Input/herb_GLUE_SPAIN_filtered_LD_MAF05.cov"  # Replace with your .cov file path
cov_matrix <- as.matrix(read.table(cov_file))
set.seed(123)
for (i in 1:n_iter) {
    # --- Sampling ---
    herb_EUR_list$row_id <- seq_len(nrow(herb_EUR_list))
    usa_rows <- herb_EUR_list %>% filter(Country == "USA")
    non_usa_rows <- herb_EUR_list %>% filter(Country != "USA")
    non_usa_sampled <- non_usa_rows %>%
        group_by(Country) %>%
        sample_n(size = min(6, n()), replace = FALSE) %>%
        ungroup()
    final_rows <- bind_rows(usa_rows, non_usa_sampled) %>%
        arrange(row_id)
    
    # Store the final_rows dataframe for this iteration
    all_final_rows[[i]] <- final_rows
    
    # --- Subset covariance matrix ---
    cov_matrix_downsampled <- cov_matrix[final_rows$row_id, final_rows$row_id]
    
    # --- PCA ---
    eigen_decomp_downsampled <- eigen(cov_matrix_downsampled)
    pc_axes <- eigen_decomp_downsampled$vectors[, 1:10]
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
        PC10 = pc_axes[, 10],
        Country = as.factor(final_rows$Country),
        RANGE = final_rows$RANGE
    )
    
    # --- Split data ---
    input.full.data <- pca_data
    training.set.Brandon <- input.full.data[input.full.data$RANGE == "EUR", ]
    test.set.Brandon <- input.full.data[!(input.full.data$RANGE == "EUR"), ]
    training.set.Brandon$Country <- droplevels(as.factor(training.set.Brandon$Country))
    
    # --- Random forest ---
    Model.10PCs <- randomForest(
        formula = Country ~ PC1 + PC2,
        data = training.set.Brandon,
        ntree = 5000
    )
    
    # --- Prediction and summary ---
    Predictions <- predict(Model.10PCs, test.set.Brandon[, 1:2])
    test.set.Brandon$prediction <- Predictions
    test.set.Brandon$Country <- as.character(test.set.Brandon$Country)
    test.set.Brandon$Count <- 1
    
    # Store test set with predictions and original row IDs for this iteration
    test_set_with_assignments <- test.set.Brandon
    test_set_with_assignments$row_id <- final_rows$row_id[!(final_rows$RANGE == "EUR")]
    test_set_with_assignments$iteration <- i
    all_test_sets[[i]] <- test_set_with_assignments
    
    summary_df <- summarySE(test.set.Brandon, measurevar = "Count", groupvars = "prediction")
    summary_df$iteration <- i
    all_summaries[[i]] <- summary_df

    # --- Store model and error rate ---
    all_models[[i]] <- Model.10PCs
    # OOB error rate
    all_errors[i] <- Model.10PCs$err.rate[Model.10PCs$ntree, "OOB"]
}

# Combine all iterations
all_summaries_df <- bind_rows(all_summaries)

# Calculate mean and sd of N (Count) for each prediction group
final_summary <- all_summaries_df %>%
    dplyr::group_by(prediction) %>%
    dplyr::summarise(
        mean_N = mean(N, na.rm = TRUE),
        sd_N = sd(N, na.rm = TRUE),
        .groups = "drop"
    )



# Find the best model (lowest error rate)
best_iter <- which.min(all_errors)
best_model <- all_models[[best_iter]]
cat("Best model is from iteration:", best_iter, "\n")
print(best_model)

# Get the summarySE result for the best model
best_summarySE <- all_summaries[[best_iter]]
cat("summarySE for best model:\n")
print(best_summarySE)

# Extract country assignments from the best model
best_test_set <- all_test_sets[[best_iter]]
best_final_rows <- all_final_rows[[best_iter]]

# Create comprehensive assignment dataframe with original sample information
best_assignments <- best_test_set %>%
  select(row_id, prediction, Country, RANGE, PC1, PC2) %>%
  rename(
    sample_row_id = row_id,
    predicted_country = prediction,
    actual_country = Country,
    range = RANGE
  )

# Use the final_rows from the best iteration to get the correct sample mapping
# The final_rows contains the actual sample information with the correct row_id mapping
test_samples_info <- best_final_rows[!(best_final_rows$RANGE == "EUR"), ]

# Merge to get full sample information with assignments
final_assignments <- best_assignments %>%
  left_join(test_samples_info, by = c("sample_row_id" = "row_id"))

# Display summary of assignments
cat("\nCountry assignments from best model (iteration", best_iter, "):\n")
assignment_summary <- final_assignments %>%
  count(predicted_country, name = "n_samples") %>%
  arrange(desc(n_samples))
print(assignment_summary)

# Save assignments to CSV for downstream analyses
write.csv(final_assignments, "best_model_country_assignments.csv", row.names = FALSE)
cat("\nFull assignment data saved to 'best_model_country_assignments.csv'\n")

# Display first few rows of the assignment data
cat("\nFirst 10 rows of assignment data:\n")
print(head(final_assignments, 10))

print(final_summary)





# Extract country assignments from the best model
best_test_set <- all_test_sets[[best_iter]]
best_final_rows <- all_final_rows[[best_iter]]

ok <- merge(best_test_set, best_final_rows, by = "row_id")

ok <- ok[!is.na(ok$prediction),]
ok <- ok[!is.na(ok$Region),]
ok <- ok[!is.na(ok$TimeBin),]


library(emmeans)

# Fit the model with interaction
lm_fit <- lm(stdAC ~ prediction * Region, data = ok)

# Get estimated marginal means for countries within each region
emm <- emmeans(lm_fit, ~ prediction | Region)

# Tukey pairwise comparisons for countries within each region
tukey_results <- pairs(emm, adjust = "tukey")

print(tukey_results)

library(multcompView)
library(multcomp)
# Get compact letter display for each region
cld_letters_AC <- cld(emm, adjust = "tukey")

# Fit the model with interaction
lm_fit <- lm(stdLI ~ prediction * Region, data = ok)

# Get estimated marginal means for countries within each region
emm <- emmeans(lm_fit, ~ prediction | Region)

# Tukey pairwise comparisons for countries within each region
tukey_results <- pairs(emm, adjust = "tukey")

print(tukey_results)

library(multcompView)
library(multcomp)
# Get compact letter display for each region
cld_letters_LI <- cld(emm, adjust = "tukey")

cld_letters_AC$Gene <- "Ac"
cld_letters_LI$Gene <- "Li"
cld_AC <- as.data.frame(cld_letters_AC)
cld_LI <- as.data.frame(cld_letters_LI)
merged_cld <- rbind(cld_AC, cld_LI)

merged_cld$Region <- factor(merged_cld$Region, levels = c("South", "Middle", "North"))
merged_cld$prediction <- factor(merged_cld$prediction, levels = c("Belgium","UK","France","Spain"))
# Create Figure
p1 <- ggplot(merged_cld[merged_cld$Gene=="Ac",],aes(x=prediction,y=emmean,color=prediction))+
    geom_point(size=3)+scale_color_manual(values=c("#F0E442", "darkblue","#E69F00", 
                      "#56B4E9"))+
    geom_errorbar(aes(ymin=emmean-SE,ymax=emmean+SE),width=0.2)+
    geom_text(aes(label=.group),hjust=1.5,size=5)+
facet_wrap(~Region) +
theme_light() +
theme(
  strip.background = element_rect(fill = "white"),
  strip.text = element_text(color = "black", size = 12),
  axis.text = element_text(size = 12, color = "black"),
  axis.title.x = element_blank(),axis.title.y = element_text(size = 12),legend.position="none"
) +
labs(y="Estimated marginal mean of Ac",x="Predicted country of origin")


p2 <- ggplot(merged_cld[merged_cld$Gene=="Li",],aes(x=prediction,y=emmean,color=prediction))+
    geom_point(size=3)+scale_color_manual(values=c("#F0E442", "darkblue", "#E69F00", 
                      "#56B4E9"))+
    geom_errorbar(aes(ymin=emmean-SE,ymax=emmean+SE),width=0.2)+
    geom_text(aes(label=.group),hjust=1.5,size=5)+
facet_wrap(~Region) +
theme_light() +
theme(
  strip.background = element_rect(fill = "white"),
  strip.text = element_text(color = "black", size = 12),
  axis.text = element_text(size = 12, color = "black"),
  axis.title.x = element_blank(),axis.title.y = element_text(size = 12),legend.position="none"
) +
labs(y="Estimated marginal mean of Li",x="Predicted country of origin")


Supplementalfigure8 <- ggarrange(p1, p2, ncol = 1, nrow = 2,labels=c("A","B"))