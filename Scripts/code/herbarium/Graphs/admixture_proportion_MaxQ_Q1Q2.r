herb <- read.csv("Github_Projects/HerbariumStructure_WhiteClover/Data/herbarium/Input/herbarium_structure_dataframe_542025.csv",header=TRUE)
herb_df_only <- herb[herb$H_C == "H",]
herb_df_north <- herb_df_only[herb_df_only$Region == "North",]
herb_df_south <- herb_df_only[herb_df_only$Region == "South" & herb_df_only$Year > 1900,]
herb_df_middle <- herb_df_only[is.na(herb_df_only$Region),]
North <- ggplot(herb_df_north, aes(x = Year)) +
    geom_point(aes(y = MaxK3), color = "black") +
    geom_smooth(aes(y = K3Q1S1964, color = "Cluster 1"), se = FALSE, size = 1.5) +
    geom_smooth(aes(y = K3Q2S1964, color = "Cluster 2"), se = FALSE, size = 1.5) +
    scale_color_manual(
        name = "",
        values = c("Cluster 1" = "#8DD3C7", "Cluster 2" = "#FFFFB3"),
        labels = c("Cluster 1", "Cluster 2")
    ) +
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
    )+ggtitle("North: > 45°N")

South <- ggplot(herb_df_south, aes(x = Year)) +
    geom_point(aes(y = MaxK3), color = "black") +
    geom_smooth(aes(y = K3Q1S1964, color = "Cluster 1"), se = FALSE, size = 1.5) +
    geom_smooth(aes(y = K3Q2S1964, color = "Cluster 2"), se = FALSE, size = 1.5) +
    scale_color_manual(
        name = "",
        values = c("Cluster 1" = "#8DD3C7", "Cluster 2" = "#FFFFB3"),
        labels = c("Cluster 1", "Cluster 2")
    ) +
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

Middle <- ggplot(herb_df_middle, aes(x = Year)) +
    geom_point(aes(y = MaxK3), color = "black") +
    geom_smooth(aes(y = K3Q1S1964, color = "Cluster 1"), se = FALSE, size = 1.5) +
    geom_smooth(aes(y = K3Q2S1964, color = "Cluster 2"), se = FALSE, size = 1.5) +
    scale_color_manual(
        name = "",
        values = c("Cluster 1" = "#8DD3C7", "Cluster 2" = "#FFFFB3"),
        labels = c("Cluster 1", "Cluster 2")
    ) +
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
    )+ggtitle("45°N - 35°N")

library(ggpubr)

regional <- ggarrange(North, Middle, South, ncol = 1, nrow = 3,
                      common.legend = TRUE, legend = "bottom")