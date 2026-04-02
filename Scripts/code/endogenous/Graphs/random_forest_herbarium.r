library(car)
library(dplyr)
library(ggplot2)
library(ggforce)
library(randomForest)

input.full.data <- read.csv("Data/herbarium/Input/herbarium_structure_dataframe_1192026.csv",header=TRUE)
order <- read.csv("Data/herbarium/Input/order_files.csv")
input.full.data <- input.full.data[match(order$Samp, input.full.data$Samp), ]
input.full.data$Country[input.full.data$Country=="HERB"]<-"USA"
herb_EUR_list <- input.full.data
n_iter <- 1000
all_summaries <- list()
all_models <- list()
all_errors <- numeric(n_iter)
all_test_sets <- list() 
all_final_rows <- list() 
cov_file <- "Data/herbarium/Input/herb_GLUE_SPAIN_filtered_LD_MAF05.cov" 
cov_matrix <- as.matrix(read.table(cov_file))
set.seed(123)
for (i in 1:n_iter) {
    herb_EUR_list$row_id <- seq_len(nrow(herb_EUR_list))
    usa_rows <- herb_EUR_list %>% filter(Country == "USA")
    non_usa_rows <- herb_EUR_list %>% filter(Country != "USA")
    non_usa_sampled <- non_usa_rows %>%
        group_by(Country) %>%
        sample_n(size = min(6, n()), replace = FALSE) %>%
        ungroup()
    final_rows <- bind_rows(usa_rows, non_usa_sampled) %>%
        arrange(row_id)

    all_final_rows[[i]] <- final_rows

    cov_matrix_downsampled <- cov_matrix[final_rows$row_id, final_rows$row_id]

    eigen_decomp_downsampled <- eigen(cov_matrix_downsampled)
    pc_axes <- eigen_decomp_downsampled$vectors[, 1:10]
    pca_data <- data.frame(
        PC_LD1 = pc_axes[, 1],
        PC_LD2 = pc_axes[, 2],
        PC_LD3 = pc_axes[, 3],
        PC_LD4 = pc_axes[, 4],
        PC_LD5 = pc_axes[, 5],
        PC_LD6 = pc_axes[, 6],
        PC_LD7 = pc_axes[, 7],
        PC_LD8 = pc_axes[, 8],
        PC_LD9 = pc_axes[, 9],
        PC_LD10 = pc_axes[, 10],
        Country = as.factor(final_rows$Country),
        RANGE = final_rows$RANGE
    )

    input.full.data <- pca_data
    training.set.Brandon <- input.full.data[input.full.data$RANGE == "EUR", ]
    test.set.Brandon <- input.full.data[!(input.full.data$RANGE == "EUR"), ]
    training.set.Brandon$Country <- droplevels(as.factor(training.set.Brandon$Country))

    Model.10PCs <- randomForest(
        formula = Country ~ PC_LD1 + PC_LD2,
        data = training.set.Brandon,
        ntree = 5000
    )

    Predictions <- predict(Model.10PCs, test.set.Brandon[, 1:2])
    test.set.Brandon$prediction <- Predictions
    test.set.Brandon$Country <- as.character(test.set.Brandon$Country)
    test.set.Brandon$Count <- 1

    test_set_with_assignments <- test.set.Brandon
    test_set_with_assignments$row_id <- final_rows$row_id[!(final_rows$RANGE == "EUR")]
    test_set_with_assignments$iteration <- i
    all_test_sets[[i]] <- test_set_with_assignments
    
    summary_df <- summarySE(test.set.Brandon, measurevar = "Count", groupvars = "prediction")
    summary_df$iteration <- i
    all_summaries[[i]] <- summary_df

    all_models[[i]] <- Model.10PCs

    all_errors[i] <- Model.10PCs$err.rate[Model.10PCs$ntree, "OOB"]
}

all_summaries_df <- bind_rows(all_summaries)

final_summary <- all_summaries_df %>%
    dplyr::group_by(prediction) %>%
    dplyr::summarise(
        mean_N = mean(N, na.rm = TRUE),
        sd_N = sd(N, na.rm = TRUE),
        .groups = "drop"
    )

best_iter <- which.min(all_errors)
best_model <- all_models[[best_iter]]
print(best_model)

best_summarySE <- all_summaries[[best_iter]]
print(best_summarySE)

best_test_set <- all_test_sets[[best_iter]]
best_final_rows <- all_final_rows[[best_iter]]

best_assignments <- best_test_set %>%
  dplyr::select(row_id, prediction, Country, RANGE, PC_LD1, PC_LD2) %>%
  dplyr::rename(
    sample_row_id = row_id,
    predicted_country = prediction,
    actual_country = Country,
    range = RANGE
  )

test_samples_info <- best_final_rows[!(best_final_rows$RANGE == "EUR"), ]

final_assignments <- best_assignments %>%
  left_join(test_samples_info, by = c("sample_row_id" = "row_id"))

assignment_summary <- final_assignments %>%
  dplyr::count(predicted_country, name = "n_samples") %>%
  dplyr::arrange(desc(n_samples))
print(assignment_summary)

write.csv(final_assignments, "Data/herbarium/Output/best_model_country_assignments.csv", row.names = FALSE)

print(head(final_assignments, 10))

print(final_summary)

best_test_set <- all_test_sets[[best_iter]]
best_final_rows <- all_final_rows[[best_iter]]

ok <- merge(best_test_set, best_final_rows, by = "row_id")

ok <- ok[!is.na(ok$prediction),]
ok <- ok[!is.na(ok$Region),]
ok <- ok[!is.na(ok$TimeBin),]