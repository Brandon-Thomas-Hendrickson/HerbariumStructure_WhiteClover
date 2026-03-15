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

op <- TE_analysis_ready
op_standardized <- op
op_standardized[, 15:9453] <- scale(op[, 15:9453])

long_data <- op_standardized %>%
  dplyr::select(CT, 15:9453) %>%
  tidyr::pivot_longer(cols = -CT, names_to = "Column", values_to = "Value")


# Summarize the data grouped by CT and Column
summary_df <- long_data %>%
  dplyr::group_by(CT, Column) %>%
  dplyr::summarize(Mean = mean(Value, na.rm = TRUE), .groups = "drop")

# View the summarized dataframe
print(summary_df)

summR <- summarySE(summary_df[!is.na(summary_df$Mean),],measurevar = "Mean",groupvars = "CT")

# Extract the mean and SD for the "Herbarium" entry
one_mean <- as.numeric(summR[summR$CT == "1838-1877", "Mean"])
one_sd <- as.numeric(summR[summR$CT == "1838-1877", "sd"])
two_mean <- as.numeric(summR[summR$CT == "1878-1917", "Mean"])
two_sd <- as.numeric(summR[summR$CT == "1878-1917", "sd"])
three_sd <- as.numeric(summR[summR$CT == "1918-1957", "sd"])
three_mean <- as.numeric(summR[summR$CT == "1918-1957", "Mean"])
four_sd <- as.numeric(summR[summR$CT == "1958-1997", "sd"])
four_mean <- as.numeric(summR[summR$CT == "1958-1997", "Mean"])
five_sd <- as.numeric(summR[summR$CT == "1998-Present", "sd"])
five_mean <- as.numeric(summR[summR$CT == "1998-Present", "Mean"])

summR <- summR[!(summR$CT=="1838-1877"),]
summR <- summR[!(summR$CT=="1878-1917"),]
summR <- summR[!(summR$CT=="1918-1957"),]
summR <- summR[!(summR$CT=="1958-1997"),]
summR <- summR[!(summR$CT=="1998-Present"),]
summR <- summR[!is.na(summR$CT),]


# Plot CT on the X-axis and Mean on the Y-axis with SD as error bars
country <- ggplot(summR, aes(x = CT, y = Mean)) +
    geom_point(size = 3) + 
    geom_errorbar(aes(ymin = Mean - sd, ymax = Mean + sd), width = 0.2) + # Add error bars for SD
    geom_hline(yintercept = one_mean, color = "#009E73", linetype = "dashed", size = 1) + 
    geom_hline(yintercept = three_mean, color = "#CC79A7", linetype = "dashed", size = 1) +
    geom_hline(yintercept = four_mean, color = "#0072B2", linetype = "dashed", size = 1) +
    geom_hline(yintercept = two_mean, color = "#D55E00", linetype = "dashed", size = 1) +
    geom_hline(yintercept = five_mean, color = "#56B4E9", linetype = "dashed", size = 1) +
    theme_light() +
    theme(
        legend.position = "null",
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14, color = "black"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    labs(y = "Mean per TE Family Copy Number")

# Standardize each column (columns 12 to 9450)
op <- TE_analysis_ready
op_sub <- op[op$H_C=="H",]
op_sub <- op_sub[!is.na(op_sub$Year),]
op_standardized <- op_sub
op_standardized[, 15:9453] <- scale(op_sub[, 15:9453])

# Reshape the data to a long format for faster grouping and summarization
long_data <- op_standardized %>%
  dplyr::select(Year, 15:9453) %>%
  tidyr::pivot_longer(cols = -Year, names_to = "Column", values_to = "Value")

# Summarize the data grouped by Range and Column
summary_df <- long_data %>%
  dplyr::group_by(Year, Column) %>%
  dplyr::summarize(Mean = mean(Value, na.rm = TRUE), .groups = "drop")

summR <- summarySE(summary_df[!is.na(summary_df$Mean),], measurevar = "Mean", groupvars = "Year")

# Plot Range on the X-axis and Mean on the Y-axis
regress <- ggplot(summR, aes(x = as.numeric(Year), y = Mean)) +
    geom_point(size = 3) +
    geom_smooth(method="lm",color="black")+
    theme_light() +
    theme(legend.position="null", axis.text = element_text(size=12, color="black"), axis.title = element_text(size=14, color="black"), axis.title.x=element_blank()) +
    labs(y="Mean per TE Family Copy Number")

  
# Standardize each column (columns 12 to 9450)
op <- TE_analysis_ready
op_sub <- op[op$H_C=="H",]
op_sub <- op_sub[!is.na(op_sub$Year),]
op_standardized <- op_sub
op_standardized[, 15:9453] <- scale(op_sub[, 15:9453])

# Reshape the data to a long format for faster grouping and summarization
long_data <- op_standardized %>%
  dplyr::select(Year, Region, 15:9453) %>%
  tidyr::pivot_longer(cols = -c(Year, Region), names_to = "Column", values_to = "Value")

# Summarize the data grouped by Year, Region, and Column
summary_df <- long_data %>%
  dplyr::group_by(Year, Region, Column) %>%
  dplyr::summarize(Mean = mean(Value, na.rm = TRUE), .groups = "drop")

# Summarize the data further for plotting (grouped by Year and Region)
summR <- summarySE(summary_df[!is.na(summary_df$Mean),], measurevar = "Mean", groupvars = c("Year", "Region"))

summR <- summR[!is.na(summR$Region),]

# Plot Year on the X-axis, Mean on the Y-axis, and color by Region
regressN_S <- ggplot(summR, aes(x = as.numeric(Year), y = Mean, color = Region)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("blue", "red")) + # Customize colors
  geom_smooth(method = "lm", alpha = 0.2) +
  theme_light() +
  theme(
    legend.position = "null",
    axis.text = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14, color = "black"),
    axis.title.x = element_blank()
  ) +
  labs(y = "Mean per TE Family Copy Number", color = "Region", fill = "Region")

# Print the plot
print(regressN_S)

# Combine the plots into a single figure
library(ggpubr)

range_combin <- ggarrange(rangeTE,country,ncol=1,nrow=2,align="v",labels=c("A","B"))

time <- ggarrange(regress,overtime,ncol=1,nrow=2,align="v",labels=c("C","D"))

TE_combined <- ggarrange(range_combin,time,ncol=2,nrow=1,align="h")

