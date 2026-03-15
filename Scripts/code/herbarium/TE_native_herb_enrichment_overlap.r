df <- TE_analysis_ready
te_cols <- names(df)[15:9453]

# Store results
results <- data.frame(TE=character(), p_value=numeric(), stringsAsFactors=FALSE)

for (te in te_cols) {
  formula <- as.formula(paste(te, "~ K3Q1S1964 + Year"))
  fit <- lm(formula, data=df)
  coef_summary <- summary(fit)$coefficients
  p_K3Q1S1964 <- coef_summary["K3Q1S1964", "Pr(>|t|)"]
  p_Year <- coef_summary["Year", "Pr(>|t|)"]
  results <- rbind(results, data.frame(TE=te, p_K3Q1S1964=p_K3Q1S1964, p_Year=p_Year))
}

# FDR correction
results$FDR_p_K3Q1S1964 <- p.adjust(results$p_K3Q1S1964, method="fdr")
results$FDR_p_Year <- p.adjust(results$p_Year, method="fdr")

# View results
head(results)
# ...existing code...

df_EUR <- df[df$Range=="EUR",]
# Assume you have two vectors:
# spain_enriched: TE families enriched in Spain
# Get TE columns
te_cols <- names(df_EUR)[15:9453]

# Store results
spain_enriched <- c()

for (te in te_cols) {
  te_spain <- as.numeric(df_EUR[df_EUR$Country == "Spain", ][[te]])
  te_other <- as.numeric(df_EUR[df_EUR$Country != "Spain", ][[te]])
  # Check for enough non-NA values and more than one unique value in each group
  if (
    sum(!is.na(te_spain)) > 1 && sum(!is.na(te_other)) > 1 &&
    length(unique(na.omit(te_spain))) > 1 && length(unique(na.omit(te_other))) > 1
  ) {
    test <- t.test(te_spain, te_other)
    mean_spain <- mean(te_spain, na.rm=TRUE)
    mean_other <- mean(te_other, na.rm=TRUE)
    if (!exists("spain_results")) spain_results <- data.frame(TE=character(), p_value=numeric(), mean_spain=numeric(), mean_other=numeric(), stringsAsFactors=FALSE)
    spain_results <- rbind(spain_results, data.frame(TE=te, p_value=test$p.value, mean_spain=mean_spain, mean_other=mean_other))
  }
}
spain_results$FDR_p <- p.adjust(spain_results$p_value, method="fdr")
spain_enriched <- spain_results$TE[
  !is.na(spain_results$FDR_p) &
  spain_results$FDR_p < 0.05 &
  spain_results$mean_spain > spain_results$mean_other
]

# Store results
Belgium_enriched <- c()

for (te in te_cols) {
  te_Belgium <- as.numeric(df_EUR[df_EUR$Country == "Belgium", ][[te]])
  te_other <- as.numeric(df_EUR[df_EUR$Country != "Belgium", ][[te]])
  # Check for enough non-NA values and more than one unique value in each group
  if (
    sum(!is.na(te_Belgium)) > 1 && sum(!is.na(te_other)) > 1 &&
    length(unique(na.omit(te_Belgium))) > 1 && length(unique(na.omit(te_other))) > 1
  ) {
    test <- t.test(te_Belgium, te_other)
    mean_Belgium <- mean(te_Belgium, na.rm=TRUE)
    mean_other <- mean(te_other, na.rm=TRUE)
    if (!exists("Belgium_results")) Belgium_results <- data.frame(TE=character(), p_value=numeric(), mean_Belgium=numeric(), mean_other=numeric(), stringsAsFactors=FALSE)
    Belgium_results <- rbind(Belgium_results, data.frame(TE=te, p_value=test$p.value, mean_Belgium=mean_Belgium, mean_other=mean_other))
  }
}
Belgium_results$FDR_p <- p.adjust(Belgium_results$p_value, method="fdr")
Belgium_enriched <- Belgium_results$TE[
  !is.na(Belgium_results$FDR_p) &
  Belgium_results$FDR_p < 0.05 &
  Belgium_results$mean_Belgium > Belgium_results$mean_other
]

# Store results
UK_enriched <- c()

for (te in te_cols) {
  te_UK <- as.numeric(df_EUR[df_EUR$Country == "UK", ][[te]])
  te_other <- as.numeric(df_EUR[df_EUR$Country != "UK", ][[te]])
  # Check for enough non-NA values and more than one unique value in each group
  if (
    sum(!is.na(te_UK)) > 1 && sum(!is.na(te_other)) > 1 &&
    length(unique(na.omit(te_UK))) > 1 && length(unique(na.omit(te_other))) > 1
  ) {
    test <- t.test(te_UK, te_other)
    mean_UK <- mean(te_UK, na.rm=TRUE)
    mean_other <- mean(te_other, na.rm=TRUE)
    if (!exists("UK_results")) UK_results <- data.frame(TE=character(), p_value=numeric(), mean_UK=numeric(), mean_other=numeric(), stringsAsFactors=FALSE)
    UK_results <- rbind(UK_results, data.frame(TE=te, p_value=test$p.value, mean_UK=mean_UK, mean_other=mean_other))
  }
}
UK_results$FDR_p <- p.adjust(UK_results$p_value, method="fdr")
UK_enriched <- UK_results$TE[
  !is.na(UK_results$FDR_p) &
  UK_results$FDR_p < 0.05 &
  UK_results$mean_UK > UK_results$mean_other
]

# Store results
France_enriched <- c()

for (te in te_cols) {
  te_France <- as.numeric(df_EUR[df_EUR$Country == "France", ][[te]])
  te_other <- as.numeric(df_EUR[df_EUR$Country != "France", ][[te]])
  # Check for enough non-NA values and more than one unique value in each group
  if (
    sum(!is.na(te_France)) > 1 && sum(!is.na(te_other)) > 1 &&
    length(unique(na.omit(te_France))) > 1 && length(unique(na.omit(te_other))) > 1
  ) {
    test <- t.test(te_France, te_other)
    mean_France <- mean(te_France, na.rm=TRUE)
    mean_other <- mean(te_other, na.rm=TRUE)
    if (!exists("France_results")) France_results <- data.frame(TE=character(), p_value=numeric(), mean_France=numeric(), mean_other=numeric(), stringsAsFactors=FALSE)
    France_results <- rbind(France_results, data.frame(TE=te, p_value=test$p.value, mean_France=mean_France, mean_other=mean_other))
  }
}
France_results$FDR_p <- p.adjust(France_results$p_value, method="fdr")
France_enriched <- France_results$TE[
  !is.na(France_results$FDR_p) &
  France_results$FDR_p < 0.05 &
  France_results$mean_France > France_results$mean_other
]

df_USA <- df[df$Range=="NAM",]
# usa_increasing: TE families increasing in USA
df_USA[, 15:9453] <- scale(df_USA[, 15:9453])
te_cols <- names(df_USA)[15:9453]
usa_results <- data.frame(TE=character(), p_value=numeric(), mean_slope=numeric(), stringsAsFactors=FALSE)
for (te in te_cols) {
  te_data <- df_USA[[te]]
  year_data <- df_USA$Year
  lat_data <- df_USA$lat
  if (sum(!is.na(te_data)) > 1 && sum(!is.na(year_data)) > 1) {
    fit <- lm(te_data ~ year_data * lat_data)
    coef_summary <- summary(fit)$coefficients
    p_Year <- coef_summary["year_data", "Pr(>|t|)"]
    mean_slope <- coef_summary["year_data", "Estimate"]
    usa_results <- rbind(usa_results, data.frame(TE=te, p_value=p_Year, mean_slope=mean_slope))
  }
}
usa_results$FDR_p <- p.adjust(usa_results$p_value, method="fdr")
usa_increasing <- usa_results$TE[
  !is.na(usa_results$FDR_p) &
  usa_results$FDR_p < 0.05 &
  usa_results$mean_slope > 0
]

# Get all unique TE families
all_te <- unique(c(spain_enriched, usa_increasing))

# Calculate overlap categories
in_spain <- all_te %in% spain_enriched
in_usa <- all_te %in% usa_increasing

# Build contingency table
table_overlap <- table(in_spain, in_usa)
rownames(table_overlap) <- c("Not_Spain_enriched", "Spain_enriched")
colnames(table_overlap) <- c("Not_USA_increasing", "USA_increasing")

print(table_overlap)
# Fisher's exact test
fisher_result <- fisher.test(table_overlap)

# View results
print(fisher_result)

# Get all unique TE families
all_te <- unique(c(UK_enriched, usa_increasing))

# Calculate overlap categories
in_UK <- all_te %in% UK_enriched
in_usa <- all_te %in% usa_increasing

# Build contingency table
table_overlap <- table(in_UK, in_usa)
rownames(table_overlap) <- c("Not_UK_enriched", "UK_enriched")
colnames(table_overlap) <- c("Not_USA_increasing", "USA_increasing")
print(table_overlap)
# Fisher's exact test
fisher_result_UK <- fisher.test(table_overlap)

# View results
print(fisher_result_UK)

# Get all unique TE families
all_te <- unique(c(France_enriched, usa_increasing))

# Calculate overlap categories
in_France <- all_te %in% France_enriched
in_usa <- all_te %in% usa_increasing

# Build contingency table
table_overlap <- table(in_France, in_usa)
rownames(table_overlap) <- c("Not_France_enriched", "France_enriched")
colnames(table_overlap) <- c("Not_USA_increasing", "USA_increasing")
print(table_overlap)
# Fisher's exact test
fisher_result_France <- fisher.test(table_overlap)

# View results
print(fisher_result_France)

# Get all unique TE families
all_te <- unique(c(Belgium_enriched, usa_increasing))

# Calculate overlap categories
in_Belgium <- all_te %in% Belgium_enriched
in_usa <- all_te %in% usa_increasing

# Build contingency table
table_overlap <- table(in_Belgium, in_usa)
rownames(table_overlap) <- c("Not_Belgium_enriched", "Belgium_enriched")
colnames(table_overlap) <- c("Not_USA_increasing", "USA_increasing")
print(table_overlap)
# Fisher's exact test
fisher_result_Belgium <- fisher.test(table_overlap)

# View results
print(fisher_result_Belgium)

prop_spain <- sum(usa_increasing %in% spain_enriched) / length(usa_increasing)
prop_belgium <- sum(usa_increasing %in% Belgium_enriched) / length(usa_increasing)
prop_uk <- sum(usa_increasing %in% UK_enriched) / length(usa_increasing)
prop_france <- sum(usa_increasing %in% France_enriched) / length(usa_increasing)


x <- c(sum(usa_increasing %in% spain_enriched), sum(usa_increasing %in% Belgium_enriched))
n <- c(length(usa_increasing), length(usa_increasing))
prop.test(x, n)

x <- c(sum(usa_increasing %in% spain_enriched), sum(usa_increasing %in% UK_enriched))
n <- c(length(usa_increasing), length(usa_increasing))
prop.test(x, n)

x <- c(sum(usa_increasing %in% spain_enriched), sum(usa_increasing %in% France_enriched))
n <- c(length(usa_increasing), length(usa_increasing))
prop.test(x, n)

country_list <- list(
  Spain = spain_enriched,
  Belgium = Belgium_enriched,
  UK = UK_enriched,
  France = France_enriched
)
summary_df <- data.frame(
  Country = character(),
  Overlap = integer(),
  Proportion = numeric(),
  stringsAsFactors = FALSE
)
for (country in names(country_list)) {
  overlap <- sum(usa_increasing %in% country_list[[country]])
  prop <- overlap / length(usa_increasing)
  summary_df <- rbind(summary_df, data.frame(Country=country, Overlap=overlap, Proportion=prop))
}
print(summary_df)