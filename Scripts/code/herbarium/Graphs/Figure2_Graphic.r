################
#Figure 1#######

#PCA (Total)
library(car)
library(dplyr)  # For data manipulation
library(ggplot2)
library(ggforce)  # For confidence ellipses
library(ggpubr)  # For arranging plots
pca_data <- read.csv("Data/herbarium/Input/herbarium_structure_dataframe_1192026.csv",header=TRUE)


colorBlindBlack8 <- c("#000000", "#E69F00", 
                      "#56B4E9", "darkblue")

pca_data$Country[pca_data$Country == "USA"] <- "Herbarium"
pca_data_UFS <- pca_data[pca_data$Country == "UK" | pca_data$Country == "France" | pca_data$Country == "Spain" | pca_data$Country == "Herbarium",]


Ttotal_UFS <- ggplot(pca_data_UFS, aes(x = PC_LD1, y = PC_LD2, color = Country)) +
  geom_point(size = 3, alpha = 0.5) +  # Scatter plot
  geom_mark_ellipse(aes(fill = Country), alpha = 0.2, show.legend = FALSE) +  # Confidence ellipses
  scale_color_manual(
    values = colorBlindBlack8,
    breaks = c("Herbarium", "France", "Spain", "UK"),  # Change order
    labels = c("Herbarium", "France", "Spain", "UK")  # Update labels
  ) +
  scale_fill_manual(
    values = colorBlindBlack8,
    breaks = c("Herbarium", "France", "Spain", "UK"),  # Change order
    labels = c("Herbarium", "France", "Spain", "UK")  # Update labels
  ) +
  labs(x = "PC1", y = "PC2") +  # Labels
  theme_light() +
  theme(
    legend.position = "none",  # Hide legend from this plot
    axis.text = element_text(size = 12)  # Increase axis title size
  ) +
  ylim(c(-0.08, 0.08)) +
  xlim(c(-0.04, 0.07))

#PCA Time Panels
min_year <- min(pca_data[!is.na(pca_data$Year), "Year"]) 

# Filter the data
filtered_data_T1_UFS <- pca_data_UFS %>%
    filter(
        (H_C == "C") | (H_C == "H" & Year >= min_year & Year <= min_year + 39)
    )

# Filter the data
filtered_data_T2_UFS <- pca_data_UFS %>%
    filter(
        (H_C == "C") | (H_C == "H" & Year >= min_year + 40 & Year <= min_year + 79)
    )

# Filter the data
filtered_data_T3_UFS <- pca_data_UFS %>%
    filter(
        (H_C == "C") | (H_C == "H" & Year >= min_year + 80 & Year <= min_year + 119)
    )

# Filter the data
filtered_data_T4_UFS <- pca_data_UFS %>%
    filter(
        (H_C == "C") | (H_C == "H" & Year >= min_year + 120 & Year <= min_year + 159)
    )

colorBlindBlack8 <- c("#E69F00", 
                      "#000000", "#56B4E9", "darkblue")

# Plot using ggplot for filtered_data_T1_UFS
T1_UFS <- ggplot(filtered_data_T1_UFS, aes(x = PC_LD1, y = PC_LD2, color = Country)) +
  geom_point(aes(alpha = 0.2), size = 3) + ggtitle("1838 - 1877") +  # Adjust alpha for "C" rows
  scale_color_manual(values = colorBlindBlack8) +  # Apply dynamically generated palette
  scale_fill_manual(values = colorBlindBlack8) +  # Match fill colors for ellipses
  scale_alpha_identity() +  # Use the alpha values directly
  theme_light() + theme(axis.text = element_blank(), legend.text = element_text(size = 16),legend.position = "none", legend.title = element_blank(),plot.title = element_text(hjust = 0.5)) + ylim(c(-0.05, 0.08)) + xlim(c(-0.04,0.07))+xlab("PC1") + ylab("PC2")

# Repeat for filtered_data_T2_UFS
T2_UFS <- ggplot(filtered_data_T2_UFS, aes(x = PC_LD1, y = PC_LD2, color = Country)) +
  geom_point(aes(alpha = 0.2), size = 3) + ggtitle("1878 - 1917") +  # Adjust alpha for "C" rows
  scale_color_manual(values = colorBlindBlack8) +  # Apply dynamically generated palette
  scale_fill_manual(values = colorBlindBlack8) +  # Match fill colors for ellipses
  scale_alpha_identity() +  # Use the alpha values directly
  theme_light() + theme(axis.text = element_blank(), legend.text = element_text(size = 16),legend.position = "none", legend.title = element_blank(),plot.title = element_text(hjust = 0.5)) + ylim(c(-0.05, 0.08)) + xlim(c(-0.04,0.07))+xlab("PC1") + ylab("PC2")

# Repeat for filtered_data_T3_UFS
T3_UFS <- ggplot(filtered_data_T3_UFS, aes(x = PC_LD1, y = PC_LD2, color = Country)) +
  geom_point(aes(alpha = 0.2), size = 3) +  ggtitle("1918 - 1957") + # Adjust alpha for "C" rows
  scale_color_manual(values = colorBlindBlack8) +  # Apply dynamically generated palette
  scale_fill_manual(values = colorBlindBlack8) +  # Match fill colors for ellipses
  scale_alpha_identity() +  # Use the alpha values directly
  theme_light() + theme(axis.text = element_blank(), legend.text = element_text(size = 16),legend.position = "none", legend.title = element_blank(),plot.title = element_text(hjust = 0.5)) + ylim(c(-0.05, 0.08)) + xlim(c(-0.04,0.07))+xlab("PC1") + ylab("PC2")

# Repeat for filtered_data_T4_UFS
T4_UFS <- ggplot(filtered_data_T4_UFS, aes(x = PC_LD1, y = PC_LD2, color = Country)) +
  geom_point(aes(alpha = 0.2), size = 3) +  ggtitle("1958 - 1997") + # Adjust alpha for "C" rows
  scale_color_manual(values = colorBlindBlack8) +  # Apply dynamically generated palette
  scale_fill_manual(values = colorBlindBlack8) +  # Match fill colors for ellipses
  scale_alpha_identity() +  # Use the alpha values directly
  theme_light() + theme(legend.position="none",axis.text = element_blank(), plot.title = element_text(hjust = 0.5),
                        legend.title = element_blank(), legend.text = element_text(size = 16)) + 
  ylim(c(-0.05, 0.08)) + xlim(c(-0.04,0.07))+xlab("PC1") + ylab("PC2")


# PANEL A

# Arrange the plots
combined_PanelA <- ggarrange(
    Ttotal_UFS,  # The total plot
    ggarrange(T1_UFS, T2_UFS, T3_UFS, T4_UFS, ncol = 2, nrow = 2, common.legend = TRUE, legend = "top"),  # The four smaller plots with legend on right
    ncol = 2,  # Two columns: one for Ttotal, one for T1-T4
    heights = c(1, 1)  # Make Ttotal twice the height of the row with T1-T4
)

#CHANGE THE POSITION OF THE LEGEND TO BE TO THE RIGHT. REMOVE TITLE OF FIRST PLOT

#ADMIX Native Groups

library(dplyr)
library(ggplot2)
library(tidyr)
library(ggpubr)

# Load the data
herb_data <- read.csv("Data/herbarium/Input/herbarium_structure_dataframe_1192026.csv",header=TRUE)

# Only France, UK, Spain, and Belgium
herb_data <- herb_data %>% filter(Country %in% c("France", "UK", "Spain", "Belgium"))

# Filter and sort the data
filtered_data <- herb_data %>%
  filter(H_C == "C") %>%  # Filter rows where H_C is "H"
  arrange(Country)        # Sort by Country

# Reshape the data for ggplot (long format)
long_data <- filtered_data %>%
  dplyr::select(Samp, Country, K3Q1S1964_LD, K3Q2S1964_LD, K3Q3S1964_LD) %>%
  tidyr::pivot_longer(cols = starts_with("K3"), names_to = "Cluster", values_to = "Proportion") %>%
  dplyr::mutate(Cluster = case_when(
    Cluster == "K3Q1S1964_LD" ~ "1",
    Cluster == "K3Q2S1964_LD" ~ "2",
    Cluster == "K3Q3S1964_LD" ~ "3"
  ))

# Create a custom order for Samp based on Cluster = 1 and then Cluster = 2
custom_order <- long_data %>%
  filter(Cluster %in% c("1", "3")) %>%  # Focus on Cluster 1 and 2
  pivot_wider(names_from = Cluster, values_from = Proportion, values_fill = 0) %>%  # Reshape to wide format
  arrange(Country, desc(`3`), desc(`1`)) %>%  # Sort by Country, Cluster 1, then Cluster 2
  pull(Samp)  # Extract the ordered Samp values

# Set the desired country order
country_order <- c("UK", "Belgium", "Sweden", "Poland", "Germany", "Greece", "France", "Spain")

# Apply the order to the Country factor
long_data <- long_data %>%
  mutate(Country = factor(Country, levels = country_order))

# Apply the custom order to Samp
long_data <- long_data %>%
  mutate(Samp = factor(Samp, levels = custom_order))  # Convert Samp to a factor with the custom order

# Create the NGSadmix plot with separate panels for each Country
ngsadmix_plot <- ggplot(long_data, aes(x = Samp, y = Proportion, fill = Cluster)) +
  geom_bar(stat = "identity", position = "stack", width = 1) +  # Ensure no space between bars
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_blank(),  # Hide x-axis text for clarity
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),  # Hide x-axis title
    axis.text.y = element_blank(),
    axis.title.y = element_text(size=12,color="black"), # Hide x-axis ticks
    strip.text = element_text(size = 12, face = "bold")  # Style for facet labels
  ) +
  scale_fill_brewer(palette = "Set3") +
  scale_y_continuous(limits = c(0, 1.01), expand = c(0, 0)) +  # Disable axis expansion
  facet_wrap(~ Country, nrow = 1, ncol = 4, scales = "free_x")  # Specify 4 rows and 1 column

# Display the plot
print(ngsadmix_plot)

#ADMIX Map Pies

#########################
#Pie Chart##############
#########################

#Big Map figure
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
    rworldmap,
    raster,
    ggplot2,
    ggmap,
    mapdata,
    maps,
    RColorBrewer,
    mapplots,
    cowplot
)

install.packages("maptools", repos = "https://packagemanager.posit.co/cran/2023-10-13")

devtools::install_github('oswaldosantos/ggsn')
library(ggsn)

#separate out by timebins
#Contemp Post1990 Pre1900 Pre1930 Pre1960 Pre1990
herbgen <- read.csv("Data/herbarium/Input/herbarium_structure_dataframe_1192026.csv",header=TRUE)

pre1900<-herbgen[herbgen$TimeBin == "1838-1877",]
pre1930<-herbgen[herbgen$TimeBin == "1878-1917",]
pre1960<-herbgen[herbgen$TimeBin == "1918-1957",]
pre1990<-herbgen[herbgen$TimeBin == "1958-1997",]
contemp<-herbgen[herbgen$TimeBin == "1998-Present",]


sample1<-herbgen[herbgen$SampleN == "1",]
sample1 <- sample1[!is.na(sample1$K3Q1S1964_LD), ]
sample2<-herbgen[herbgen$SampleN == "2",]
sample2 <- sample2[!is.na(sample2$K3Q1S1964_LD), ]
sample3<-herbgen[herbgen$SampleN == "3",]
sample3 <- sample3[!is.na(sample3$K3Q1S1964_LD), ]
sample4<-herbgen[herbgen$SampleN == "4",]
sample4 <- sample4[!is.na(sample4$K3Q1S1964_LD), ]
sample5<-herbgen[herbgen$SampleN == "5",]
sample5 <- sample5[!is.na(sample5$K3Q1S1964_LD), ]
sample6<-herbgen[herbgen$SampleN == "6",]
sample6 <- sample6[!is.na(sample6$K3Q1S1964_LD), ]
sample7<-herbgen[herbgen$SampleN == "7",]
sample7 <- sample7[!is.na(sample7$K3Q1S1964_LD), ]
sample8<-herbgen[herbgen$SampleN == "8",]
sample8 <- sample8[!is.na(sample8$K3Q1S1964_LD), ]
sample9<-herbgen[herbgen$SampleN == "9",]
sample9 <- sample9[!is.na(sample9$K3Q1S1964_LD), ]
sample10<-herbgen[herbgen$SampleN == "10",]
sample10 <- sample10[!is.na(sample10$K3Q1S1964_LD), ]
sample11<-herbgen[herbgen$SampleN == "11",]
sample11 <- sample11[!is.na(sample11$K3Q1S1964_LD), ]
sample12<-herbgen[herbgen$SampleN == "12",]
sample12 <- sample12[!is.na(sample12$K3Q1S1964_LD), ]
sample13<-herbgen[herbgen$SampleN == "13",]
sample13 <- sample13[!is.na(sample13$K3Q1S1964_LD), ]
sample14<-herbgen[herbgen$SampleN == "14",]
sample14 <- sample14[!is.na(sample14$K3Q1S1964_LD), ]
sample15<-herbgen[herbgen$SampleN == "15",]
sample15 <- sample15[!is.na(sample15$K3Q1S1964_LD), ]
sample16<-herbgen[herbgen$SampleN == "16",]
sample16 <- sample16[!is.na(sample16$K3Q1S1964_LD), ]
sample17<-herbgen[herbgen$SampleN == "17",]
sample17 <- sample17[!is.na(sample17$K3Q1S1964_LD), ]
sample18<-herbgen[herbgen$SampleN == "18",]
sample18 <- sample18[!is.na(sample18$K3Q1S1964_LD), ]
sample19<-herbgen[herbgen$SampleN == "19",]
sample19 <- sample19[!is.na(sample19$K3Q1S1964_LD), ]
sample20<-herbgen[herbgen$SampleN == "20",]
sample20 <- sample20[!is.na(sample20$K3Q1S1964_LD), ]
sample21<-herbgen[herbgen$SampleN == "21",]
sample21 <- sample21[!is.na(sample21$K3Q1S1964_LD), ]
sample22<-herbgen[herbgen$SampleN == "22",]
sample22 <- sample22[!is.na(sample22$K3Q1S1964_LD), ]
sample23<-herbgen[herbgen$SampleN == "23",]
sample23 <- sample23[!is.na(sample23$K3Q1S1964_LD), ]
sample24<-herbgen[herbgen$SampleN == "24",]
sample24 <- sample24[!is.na(sample24$K3Q1S1964_LD), ]
sample25<-herbgen[herbgen$SampleN == "25",]
sample25 <- sample25[!is.na(sample25$K3Q1S1964_LD), ]
sample26<-herbgen[herbgen$SampleN == "26",]
sample26 <- sample26[!is.na(sample26$K3Q1S1964_LD), ]
sample27<-herbgen[herbgen$SampleN == "27",]
sample27 <- sample27[!is.na(sample27$K3Q1S1964_LD), ]
sample28<-herbgen[herbgen$SampleN == "28",]
sample28 <- sample28[!is.na(sample28$K3Q1S1964_LD), ]
sample29<-herbgen[herbgen$SampleN == "29",]
sample29 <- sample29[!is.na(sample29$K3Q1S1964_LD), ]
sample30<-herbgen[herbgen$SampleN == "30",]
sample30 <- sample30[!is.na(sample30$K3Q1S1964_LD), ]
sample31<-herbgen[herbgen$SampleN == "31",]
sample31 <- sample31[!is.na(sample31$K3Q1S1964_LD), ]
sample32<-herbgen[herbgen$SampleN == "32",]
sample32 <- sample32[!is.na(sample32$K3Q1S1964_LD), ]
sample33<-herbgen[herbgen$SampleN == "33",]
sample33 <- sample33[!is.na(sample33$K3Q1S1964_LD), ]
sample34<-herbgen[herbgen$SampleN == "34",]
sample34 <- sample34[!is.na(sample34$K3Q1S1964_LD), ]
sample35<-herbgen[herbgen$SampleN == "35",]
sample35 <- sample35[!is.na(sample35$K3Q1S1964_LD), ]
sample36<-herbgen[herbgen$SampleN == "36",]
sample36 <- sample36[!is.na(sample36$K3Q1S1964_LD), ]
sample37<-herbgen[herbgen$SampleN == "37",]
sample37 <- sample37[!is.na(sample37$K3Q1S1964_LD), ]
sample38<-herbgen[herbgen$SampleN == "38",]
sample38 <- sample38[!is.na(sample38$K3Q1S1964_LD), ]
sample39<-herbgen[herbgen$SampleN == "39",]
sample39 <- sample39[!is.na(sample39$K3Q1S1964_LD), ]
sample40<-herbgen[herbgen$SampleN == "40",]
sample40 <- sample40[!is.na(sample40$K3Q1S1964_LD), ]
sample41<-herbgen[herbgen$SampleN == "41",]
sample41 <- sample41[!is.na(sample41$K3Q1S1964_LD), ]
sample42<-herbgen[herbgen$SampleN == "42",]
sample42 <- sample42[!is.na(sample42$K3Q1S1964_LD), ]
sample43<-herbgen[herbgen$SampleN == "43",]
sample43 <- sample43[!is.na(sample43$K3Q1S1964_LD), ]
sample44<-herbgen[herbgen$SampleN == "44",]
sample44 <- sample44[!is.na(sample44$K3Q1S1964_LD), ]
sample45<-herbgen[herbgen$SampleN == "45",]
sample45 <- sample45[!is.na(sample45$K3Q1S1964_LD), ]
sample46<-herbgen[herbgen$SampleN == "46",]
sample46 <- sample46[!is.na(sample46$K3Q1S1964_LD), ]
sample47<-herbgen[herbgen$SampleN == "47",]
sample47 <- sample47[!is.na(sample47$K3Q1S1964_LD), ]
sample48<-herbgen[herbgen$SampleN == "48",]
sample48 <- sample48[!is.na(sample48$K3Q1S1964_LD), ]
sample49<-herbgen[herbgen$SampleN == "49",]
sample49 <- sample49[!is.na(sample49$K3Q1S1964_LD), ]
sample50<-herbgen[herbgen$SampleN == "50",]
sample50 <- sample50[!is.na(sample50$K3Q1S1964_LD), ]
sample51<-herbgen[herbgen$SampleN == "51",]
sample51 <- sample51[!is.na(sample51$K3Q1S1964_LD), ]
sample52<-herbgen[herbgen$SampleN == "52",]
sample52 <- sample52[!is.na(sample52$K3Q1S1964_LD), ]
sample53<-herbgen[herbgen$SampleN == "53",]
sample53 <- sample53[!is.na(sample53$K3Q1S1964_LD), ]
sample54<-herbgen[herbgen$SampleN == "54",]
sample54 <- sample54[!is.na(sample54$K3Q1S1964_LD), ]
sample55<-herbgen[herbgen$SampleN == "55",]
sample55 <- sample55[!is.na(sample55$K3Q1S1964_LD), ]
sample56<-herbgen[herbgen$SampleN == "56",]
sample56 <- sample56[!is.na(sample56$K3Q1S1964_LD), ]
sample57<-herbgen[herbgen$SampleN == "57",]
sample57 <- sample57[!is.na(sample57$K3Q1S1964_LD), ]
sample58<-herbgen[herbgen$SampleN == "58",]
sample58 <- sample58[!is.na(sample58$K3Q1S1964_LD), ]
sample59<-herbgen[herbgen$SampleN == "59",]
sample59 <- sample59[!is.na(sample59$K3Q1S1964_LD), ]
sample60<-herbgen[herbgen$SampleN == "60",]
sample60 <- sample60[!is.na(sample60$K3Q1S1964_LD), ]
sample61<-herbgen[herbgen$SampleN == "61",]
sample61 <- sample61[!is.na(sample61$K3Q1S1964_LD), ]
sample62<-herbgen[herbgen$SampleN == "62",]
sample62 <- sample62[!is.na(sample62$K3Q1S1964_LD), ]
sample63<-herbgen[herbgen$SampleN == "63",]
sample63 <- sample63[!is.na(sample63$K3Q1S1964_LD), ]
sample64<-herbgen[herbgen$SampleN == "64",]
sample64 <- sample64[!is.na(sample64$K3Q1S1964_LD), ]
sample65<-herbgen[herbgen$SampleN == "65",]
sample65 <- sample65[!is.na(sample65$K3Q1S1964_LD), ]
sample66<-herbgen[herbgen$SampleN == "66",]
sample66 <- sample66[!is.na(sample66$K3Q1S1964_LD), ]
sample67<-herbgen[herbgen$SampleN == "67",]
sample67 <- sample67[!is.na(sample67$K3Q1S1964_LD), ]
sample68<-herbgen[herbgen$SampleN == "68",]
sample68 <- sample68[!is.na(sample68$K3Q1S1964_LD), ]
sample69<-herbgen[herbgen$SampleN == "69",]
sample69 <- sample69[!is.na(sample69$K3Q1S1964_LD), ]
sample70<-herbgen[herbgen$SampleN == "70",]
sample70 <- sample70[!is.na(sample70$K3Q1S1964_LD), ]
sample71<-herbgen[herbgen$SampleN == "71",]
sample71 <- sample71[!is.na(sample71$K3Q1S1964_LD), ]
sample72<-herbgen[herbgen$SampleN == "72",]
sample72 <- sample72[!is.na(sample72$K3Q1S1964_LD), ]
sample73<-herbgen[herbgen$SampleN == "73",]
sample73 <- sample73[!is.na(sample73$K3Q1S1964_LD), ]
sample74<-herbgen[herbgen$SampleN == "74",]
sample74 <- sample74[!is.na(sample74$K3Q1S1964_LD), ]
sample75<-herbgen[herbgen$SampleN == "75",]
sample75 <- sample75[!is.na(sample75$K3Q1S1964_LD), ]
sample76<-herbgen[herbgen$SampleN == "76",]
sample76 <- sample76[!is.na(sample76$K3Q1S1964_LD), ]
sample77<-herbgen[herbgen$SampleN == "77",]
sample77 <- sample77[!is.na(sample77$K3Q1S1964_LD), ]
sample78<-herbgen[herbgen$SampleN == "78",]
sample78 <- sample78[!is.na(sample78$K3Q1S1964_LD), ]
sample79<-herbgen[herbgen$SampleN == "79",]
sample79 <- sample79[!is.na(sample79$K3Q1S1964_LD), ]
sample80<-herbgen[herbgen$SampleN == "80",]
sample80 <- sample80[!is.na(sample80$K3Q1S1964_LD), ]
sample81<-herbgen[herbgen$SampleN == "81",]
sample81 <- sample81[!is.na(sample81$K3Q1S1964_LD), ]
sample82<-herbgen[herbgen$SampleN == "82",]
sample82 <- sample82[!is.na(sample82$K3Q1S1964_LD), ]
sample83<-herbgen[herbgen$SampleN == "83",]
sample83 <- sample83[!is.na(sample83$K3Q1S1964_LD), ]
sample84<-herbgen[herbgen$SampleN == "84",]
sample84 <- sample84[!is.na(sample84$K3Q1S1964_LD), ]
sample85<-herbgen[herbgen$SampleN == "85",]
sample85 <- sample85[!is.na(sample85$K3Q1S1964_LD), ]
sample86<-herbgen[herbgen$SampleN == "86",]
sample86 <- sample86[!is.na(sample86$K3Q1S1964_LD), ]
sample87<-herbgen[herbgen$SampleN == "87",]
sample87 <- sample87[!is.na(sample87$K3Q1S1964_LD), ]
sample88<-herbgen[herbgen$SampleN == "88",]
sample88 <- sample88[!is.na(sample88$K3Q1S1964_LD), ]
sample89<-herbgen[herbgen$SampleN == "89",]
sample89 <- sample89[!is.na(sample89$K3Q1S1964_LD), ]
sample90<-herbgen[herbgen$SampleN == "90",]
sample90 <- sample90[!is.na(sample90$K3Q1S1964_LD), ]
sample91<-herbgen[herbgen$SampleN == "91",]
sample91 <- sample91[!is.na(sample91$K3Q1S1964_LD), ]
sample92<-herbgen[herbgen$SampleN == "92",]
sample92 <- sample92[!is.na(sample92$K3Q1S1964_LD), ]
sample93<-herbgen[herbgen$SampleN == "93",]
sample93 <- sample93[!is.na(sample93$K3Q1S1964_LD), ]
sample94<-herbgen[herbgen$SampleN == "94",]
sample94 <- sample94[!is.na(sample94$K3Q1S1964_LD), ]
sample95<-herbgen[herbgen$SampleN == "95",]
sample95 <- sample95[!is.na(sample95$K3Q1S1964_LD), ]
sample96<-herbgen[herbgen$SampleN == "96",]
sample96 <- sample96[!is.na(sample96$K3Q1S1964_LD), ]
sample97<-herbgen[herbgen$SampleN == "97",]
sample97 <- sample97[!is.na(sample97$K3Q1S1964_LD), ]
sample98<-herbgen[herbgen$SampleN == "98",]
sample98 <- sample98[!is.na(sample98$K3Q1S1964_LD), ]
sample99<-herbgen[herbgen$SampleN == "99",]
sample99 <- sample99[!is.na(sample99$K3Q1S1964_LD), ]
sample100<-herbgen[herbgen$SampleN == "100",]
sample100 <- sample100[!is.na(sample100$K3Q1S1964_LD), ]
sample101<-herbgen[herbgen$SampleN == "101",]
sample101 <- sample101[!is.na(sample101$K3Q1S1964_LD), ]
sample102<-herbgen[herbgen$SampleN == "102",]
sample102 <- sample102[!is.na(sample102$K3Q1S1964_LD), ]
sample103<-herbgen[herbgen$SampleN == "103",]
sample103 <- sample103[!is.na(sample103$K3Q1S1964_LD), ]
sample104<-herbgen[herbgen$SampleN == "104",]
sample104 <- sample104[!is.na(sample104$K3Q1S1964_LD), ]
sample105<-herbgen[herbgen$SampleN == "105",]
sample105 <- sample105[!is.na(sample105$K3Q1S1964_LD), ]
sample106<-herbgen[herbgen$SampleN == "106",]
sample106 <- sample106[!is.na(sample106$K3Q1S1964_LD), ]
sample107<-herbgen[herbgen$SampleN == "107",]
sample107 <- sample107[!is.na(sample107$K3Q1S1964_LD), ]
sample108<-herbgen[herbgen$SampleN == "108",]
sample108 <- sample108[!is.na(sample108$K3Q1S1964_LD), ]
sample109<-herbgen[herbgen$SampleN == "109",]
sample109 <- sample109[!is.na(sample109$K3Q1S1964_LD), ]
sample110<-herbgen[herbgen$SampleN == "110",]
sample110 <- sample110[!is.na(sample110$K3Q1S1964_LD), ]
sample111<-herbgen[herbgen$SampleN == "111",]
sample111 <- sample111[!is.na(sample111$K3Q1S1964_LD), ]
sample112<-herbgen[herbgen$SampleN == "112",]
sample112 <- sample112[!is.na(sample112$K3Q1S1964_LD), ]
sample113<-herbgen[herbgen$SampleN == "113",]
sample113 <- sample113[!is.na(sample113$K3Q1S1964_LD), ]
sample114<-herbgen[herbgen$SampleN == "114",]
sample114 <- sample114[!is.na(sample114$K3Q1S1964_LD), ]
sample115<-herbgen[herbgen$SampleN == "115",]
sample115 <- sample115[!is.na(sample115$K3Q1S1964_LD), ]
sample116<-herbgen[herbgen$SampleN == "116",]
sample116 <- sample116[!is.na(sample116$K3Q1S1964_LD), ]
sample117<-herbgen[herbgen$SampleN == "117",]
sample117 <- sample117[!is.na(sample117$K3Q1S1964_LD), ]
sample118<-herbgen[herbgen$SampleN == "118",]
sample118 <- sample118[!is.na(sample118$K3Q1S1964_LD), ]
sample119<-herbgen[herbgen$SampleN == "119",]
sample119 <- sample119[!is.na(sample119$K3Q1S1964_LD), ]
sample120<-herbgen[herbgen$SampleN == "120",]
sample120 <- sample120[!is.na(sample120$K3Q1S1964_LD), ]
sample121<-herbgen[herbgen$SampleN == "121",]
sample121 <- sample121[!is.na(sample121$K3Q1S1964_LD), ]
sample122<-herbgen[herbgen$SampleN == "122",]
sample122 <- sample122[!is.na(sample122$K3Q1S1964_LD), ]
sample123<-herbgen[herbgen$SampleN == "123",]
sample123 <- sample123[!is.na(sample123$K3Q1S1964_LD), ]
sample124<-herbgen[herbgen$SampleN == "124",]
sample124 <- sample124[!is.na(sample124$K3Q1S1964_LD), ]
sample125<-herbgen[herbgen$SampleN == "125",]
sample125 <- sample125[!is.na(sample125$K3Q1S1964_LD), ]
sample126<-herbgen[herbgen$SampleN == "126",]
sample126 <- sample126[!is.na(sample126$K3Q1S1964_LD), ]
sample127<-herbgen[herbgen$SampleN == "127",]
sample127 <- sample127[!is.na(sample127$K3Q1S1964_LD), ]
sample128<-herbgen[herbgen$SampleN == "128",]
sample128 <- sample128[!is.na(sample128$K3Q1S1964_LD), ]
sample129<-herbgen[herbgen$SampleN == "129",]
sample129 <- sample129[!is.na(sample129$K3Q1S1964_LD), ]
sample130<-herbgen[herbgen$SampleN == "130",]
sample130 <- sample130[!is.na(sample130$K3Q1S1964_LD), ]
sample131<-herbgen[herbgen$SampleN == "131",]
sample131 <- sample131[!is.na(sample131$K3Q1S1964_LD), ]
sample132<-herbgen[herbgen$SampleN == "132",]
sample132 <- sample132[!is.na(sample132$K3Q1S1964_LD), ]
sample133<-herbgen[herbgen$SampleN == "133",]
sample133 <- sample133[!is.na(sample133$K3Q1S1964_LD), ]
sample134<-herbgen[herbgen$SampleN == "134",]
sample134 <- sample134[!is.na(sample134$K3Q1S1964_LD), ]
sample135<-herbgen[herbgen$SampleN == "135",]
sample135 <- sample135[!is.na(sample135$K3Q1S1964_LD), ]
sample136<-herbgen[herbgen$SampleN == "136",]
sample136 <- sample136[!is.na(sample136$K3Q1S1964_LD), ]
sample137<-herbgen[herbgen$SampleN == "137",]
sample137 <- sample137[!is.na(sample137$K3Q1S1964_LD), ]
sample138<-herbgen[herbgen$SampleN == "138",]
sample138 <- sample138[!is.na(sample138$K3Q1S1964_LD), ]
sample139<-herbgen[herbgen$SampleN == "139",]
sample139 <- sample139[!is.na(sample139$K3Q1S1964_LD), ]
sample140<-herbgen[herbgen$SampleN == "140",]
sample140 <- sample140[!is.na(sample140$K3Q1S1964_LD), ]
sample141<-herbgen[herbgen$SampleN == "141",]
sample141 <- sample141[!is.na(sample141$K3Q1S1964_LD), ]
sample142<-herbgen[herbgen$SampleN == "142",]
sample142 <- sample142[!is.na(sample142$K3Q1S1964_LD), ]
sample143<-herbgen[herbgen$SampleN == "143",]
sample143 <- sample143[!is.na(sample143$K3Q1S1964_LD), ]
sample144<-herbgen[herbgen$SampleN == "144",]
sample144 <- sample144[!is.na(sample144$K3Q1S1964_LD), ]
sample145<-herbgen[herbgen$SampleN == "145",]
sample145 <- sample145[!is.na(sample145$K3Q1S1964_LD), ]
sample146<-herbgen[herbgen$SampleN == "146",]
sample146 <- sample146[!is.na(sample146$K3Q1S1964_LD), ]
sample147<-herbgen[herbgen$SampleN == "147",]
sample147 <- sample147[!is.na(sample147$K3Q1S1964_LD), ]
sample148<-herbgen[herbgen$SampleN == "148",]
sample148 <- sample148[!is.na(sample148$K3Q1S1964_LD), ]
sample149<-herbgen[herbgen$SampleN == "149",]
sample149 <- sample149[!is.na(sample149$K3Q1S1964_LD), ]
sample150<-herbgen[herbgen$SampleN == "150",]
sample150 <- sample150[!is.na(sample150$K3Q1S1964_LD), ]
sample151<-herbgen[herbgen$SampleN == "151",]
sample151 <- sample151[!is.na(sample151$K3Q1S1964_LD), ]
sample152<-herbgen[herbgen$SampleN == "152",]
sample152 <- sample152[!is.na(sample152$K3Q1S1964_LD), ]
sample153<-herbgen[herbgen$SampleN == "153",]
sample153 <- sample153[!is.na(sample153$K3Q1S1964_LD), ]
sample154<-herbgen[herbgen$SampleN == "154",]
sample154 <- sample154[!is.na(sample154$K3Q1S1964_LD), ]
sample155<-herbgen[herbgen$SampleN == "155",]
sample155 <- sample155[!is.na(sample155$K3Q1S1964_LD), ]
sample156<-herbgen[herbgen$SampleN == "156",]
sample156 <- sample156[!is.na(sample156$K3Q1S1964_LD), ]
sample157<-herbgen[herbgen$SampleN == "157",]
sample157 <- sample157[!is.na(sample157$K3Q1S1964_LD), ]
sample158<-herbgen[herbgen$SampleN == "158",]
sample158 <- sample158[!is.na(sample158$K3Q1S1964_LD), ]
sample159<-herbgen[herbgen$SampleN == "159",]
sample159 <- sample159[!is.na(sample159$K3Q1S1964_LD), ]
sample160<-herbgen[herbgen$SampleN == "160",]
sample160 <- sample160[!is.na(sample160$K3Q1S1964_LD), ]
sample161<-herbgen[herbgen$SampleN == "161",]
sample161 <- sample161[!is.na(sample161$K3Q1S1964_LD), ]
sample162<-herbgen[herbgen$SampleN == "162",]
sample162 <- sample162[!is.na(sample162$K3Q1S1964_LD), ]
sample163<-herbgen[herbgen$SampleN == "163",]
sample163 <- sample163[!is.na(sample163$K3Q1S1964_LD), ]
sample164<-herbgen[herbgen$SampleN == "164",]
sample164 <- sample164[!is.na(sample164$K3Q1S1964_LD), ]
sample165<-herbgen[herbgen$SampleN == "165",]
sample165 <- sample165[!is.na(sample165$K3Q1S1964_LD), ]
sample166<-herbgen[herbgen$SampleN == "166",]
sample166 <- sample166[!is.na(sample166$K3Q1S1964_LD), ]
sample167<-herbgen[herbgen$SampleN == "167",]
sample167 <- sample167[!is.na(sample167$K3Q1S1964_LD), ]
sample168<-herbgen[herbgen$SampleN == "168",]
sample168 <- sample168[!is.na(sample168$K3Q1S1964_LD), ]
sample169<-herbgen[herbgen$SampleN == "169",]
sample169 <- sample169[!is.na(sample169$K3Q1S1964_LD), ]
sample170<-herbgen[herbgen$SampleN == "170",]
sample170 <- sample170[!is.na(sample170$K3Q1S1964_LD), ]
sample171<-herbgen[herbgen$SampleN == "171",]
sample171 <- sample171[!is.na(sample171$K3Q1S1964_LD), ]
sample172<-herbgen[herbgen$SampleN == "172",]
sample172 <- sample172[!is.na(sample172$K3Q1S1964_LD), ]
sample173<-herbgen[herbgen$SampleN == "173",]
sample173 <- sample173[!is.na(sample173$K3Q1S1964_LD), ]
sample174<-herbgen[herbgen$SampleN == "174",]
sample174 <- sample174[!is.na(sample174$K3Q1S1964_LD), ]
sample175<-herbgen[herbgen$SampleN == "175",]
sample175 <- sample175[!is.na(sample175$K3Q1S1964_LD), ]
sample176<-herbgen[herbgen$SampleN == "176",]
sample176 <- sample176[!is.na(sample176$K3Q1S1964_LD), ]
sample177<-herbgen[herbgen$SampleN == "177",]
sample177 <- sample177[!is.na(sample177$K3Q1S1964_LD), ]
sample178<-herbgen[herbgen$SampleN == "178",]
sample178 <- sample178[!is.na(sample178$K3Q1S1964_LD), ]
sample179<-herbgen[herbgen$SampleN == "179",]
sample179 <- sample179[!is.na(sample179$K3Q1S1964_LD), ]
sample180<-herbgen[herbgen$SampleN == "180",]
sample180 <- sample180[!is.na(sample180$K3Q1S1964_LD), ]
sample181<-herbgen[herbgen$SampleN == "181",]
sample181 <- sample181[!is.na(sample181$K3Q1S1964_LD), ]
sample182<-herbgen[herbgen$SampleN == "182",]
sample182 <- sample182[!is.na(sample182$K3Q1S1964_LD), ]
sample183<-herbgen[herbgen$SampleN == "183",]
sample183 <- sample183[!is.na(sample183$K3Q1S1964_LD), ]
sample184<-herbgen[herbgen$SampleN == "184",]
sample184 <- sample184[!is.na(sample184$K3Q1S1964_LD), ]
sample185<-herbgen[herbgen$SampleN == "185",]
sample185 <- sample185[!is.na(sample185$K3Q1S1964_LD), ]
sample186<-herbgen[herbgen$SampleN == "186",]
sample186 <- sample186[!is.na(sample186$K3Q1S1964_LD), ]
sample187<-herbgen[herbgen$SampleN == "187",]
sample187 <- sample187[!is.na(sample187$K3Q1S1964_LD), ]
sample188<-herbgen[herbgen$SampleN == "188",]
sample188 <- sample188[!is.na(sample188$K3Q1S1964_LD), ]
sample189<-herbgen[herbgen$SampleN == "189",]
sample189 <- sample189[!is.na(sample189$K3Q1S1964_LD), ]
sample190<-herbgen[herbgen$SampleN == "190",]
sample190 <- sample190[!is.na(sample190$K3Q1S1964_LD), ]
sample191<-herbgen[herbgen$SampleN == "191",]
sample191 <- sample191[!is.na(sample191$K3Q1S1964_LD), ]
sample192<-herbgen[herbgen$SampleN == "192",]
sample192 <- sample192[!is.na(sample192$K3Q1S1964_LD), ]
sample193<-herbgen[herbgen$SampleN == "193",]
sample193 <- sample193[!is.na(sample193$K3Q1S1964_LD), ]
sample194<-herbgen[herbgen$SampleN == "194",]
sample194 <- sample194[!is.na(sample194$K3Q1S1964_LD), ]
sample195<-herbgen[herbgen$SampleN == "195",]
sample195 <- sample195[!is.na(sample195$K3Q1S1964_LD), ]
sample196<-herbgen[herbgen$SampleN == "196",]
sample196 <- sample196[!is.na(sample196$K3Q1S1964_LD), ]
sample197<-herbgen[herbgen$SampleN == "197",]
sample197 <- sample197[!is.na(sample197$K3Q1S1964_LD), ]
sample198<-herbgen[herbgen$SampleN == "198",]
sample198 <- sample198[!is.na(sample198$K3Q1S1964_LD), ]
sample199<-herbgen[herbgen$SampleN == "199",]
sample199 <- sample199[!is.na(sample199$K3Q1S1964_LD), ]
sample200<-herbgen[herbgen$SampleN == "200",]
sample200 <- sample200[!is.na(sample200$K3Q1S1964_LD), ]
sample201<-herbgen[herbgen$SampleN == "201",]
sample201 <- sample201[!is.na(sample201$K3Q1S1964_LD), ]
sample202<-herbgen[herbgen$SampleN == "202",]
sample202 <- sample202[!is.na(sample202$K3Q1S1964_LD), ]
sample203<-herbgen[herbgen$SampleN == "203",]
sample203 <- sample203[!is.na(sample203$K3Q1S1964_LD), ]
sample204<-herbgen[herbgen$SampleN == "204",]
sample204 <- sample204[!is.na(sample204$K3Q1S1964_LD), ]
sample205<-herbgen[herbgen$SampleN == "205",]
sample205 <- sample205[!is.na(sample205$K3Q1S1964_LD), ]
sample206<-herbgen[herbgen$SampleN == "206",]
sample206 <- sample206[!is.na(sample206$K3Q1S1964_LD), ]
sample207<-herbgen[herbgen$SampleN == "207",]
sample207 <- sample207[!is.na(sample207$K3Q1S1964_LD), ]
sample208<-herbgen[herbgen$SampleN == "208",]
sample208 <- sample208[!is.na(sample208$K3Q1S1964_LD), ]
sample209<-herbgen[herbgen$SampleN == "209",]
sample209 <- sample209[!is.na(sample209$K3Q1S1964_LD), ]
sample210<-herbgen[herbgen$SampleN == "210",]
sample210 <- sample210[!is.na(sample210$K3Q1S1964_LD), ]
sample211<-herbgen[herbgen$SampleN == "211",]
sample211 <- sample211[!is.na(sample211$K3Q1S1964_LD), ]
sample212<-herbgen[herbgen$SampleN == "212",]
sample212 <- sample212[!is.na(sample212$K3Q1S1964_LD), ]
sample213<-herbgen[herbgen$SampleN == "213",]
sample213 <- sample213[!is.na(sample213$K3Q1S1964_LD), ]
sample214<-herbgen[herbgen$SampleN == "214",]
sample214 <- sample214[!is.na(sample214$K3Q1S1964_LD), ]
sample215<-herbgen[herbgen$SampleN == "215",]
sample215 <- sample215[!is.na(sample215$K3Q1S1964_LD), ]
sample216<-herbgen[herbgen$SampleN == "216",]
sample216 <- sample216[!is.na(sample216$K3Q1S1964_LD), ]
sample217<-herbgen[herbgen$SampleN == "217",]
sample217 <- sample217[!is.na(sample217$K3Q1S1964_LD), ]
sample218<-herbgen[herbgen$SampleN == "218",]
sample218 <- sample218[!is.na(sample218$K3Q1S1964_LD), ]
sample219<-herbgen[herbgen$SampleN == "219",]
sample219 <- sample219[!is.na(sample219$K3Q1S1964_LD), ]
sample220<-herbgen[herbgen$SampleN == "220",]
sample220 <- sample220[!is.na(sample220$K3Q1S1964_LD), ]
sample221<-herbgen[herbgen$SampleN == "221",]
sample221 <- sample221[!is.na(sample221$K3Q1S1964_LD), ]
sample222<-herbgen[herbgen$SampleN == "222",]
sample222 <- sample222[!is.na(sample222$K3Q1S1964_LD), ]
sample223<-herbgen[herbgen$SampleN == "223",]
sample223 <- sample223[!is.na(sample223$K3Q1S1964_LD), ]
sample224<-herbgen[herbgen$SampleN == "224",]
sample224 <- sample224[!is.na(sample224$K3Q1S1964_LD), ]
sample225<-herbgen[herbgen$SampleN == "225",]
sample225 <- sample225[!is.na(sample225$K3Q1S1964_LD), ]
sample226<-herbgen[herbgen$SampleN == "226",]
sample226 <- sample226[!is.na(sample226$K3Q1S1964_LD), ]
sample227<-herbgen[herbgen$SampleN == "227",]
sample227 <- sample227[!is.na(sample227$K3Q1S1964_LD), ]
sample228<-herbgen[herbgen$SampleN == "228",]
sample228 <- sample228[!is.na(sample228$K3Q1S1964_LD), ]
sample229<-herbgen[herbgen$SampleN == "229",]
sample229 <- sample229[!is.na(sample229$K3Q1S1964_LD), ]
sample230<-herbgen[herbgen$SampleN == "230",]
sample230 <- sample230[!is.na(sample230$K3Q1S1964_LD), ]
sample231<-herbgen[herbgen$SampleN == "231",]
sample231 <- sample231[!is.na(sample231$K3Q1S1964_LD), ]
sample232<-herbgen[herbgen$SampleN == "232",]
sample232 <- sample232[!is.na(sample232$K3Q1S1964_LD), ]
sample233<-herbgen[herbgen$SampleN == "233",]
sample233 <- sample233[!is.na(sample233$K3Q1S1964_LD), ]
sample234<-herbgen[herbgen$SampleN == "234",]
sample234 <- sample234[!is.na(sample234$K3Q1S1964_LD), ]
sample235<-herbgen[herbgen$SampleN == "235",]
sample235 <- sample235[!is.na(sample235$K3Q1S1964_LD), ]
sample236<-herbgen[herbgen$SampleN == "236",]
sample236 <- sample236[!is.na(sample236$K3Q1S1964_LD), ]
sample237<-herbgen[herbgen$SampleN == "237",]
sample237 <- sample237[!is.na(sample237$K3Q1S1964_LD), ]
sample238<-herbgen[herbgen$SampleN == "238",]
sample238 <- sample238[!is.na(sample238$K3Q1S1964_LD), ]
sample239<-herbgen[herbgen$SampleN == "239",]
sample239 <- sample239[!is.na(sample239$K3Q1S1964_LD), ]
sample240<-herbgen[herbgen$SampleN == "240",]
sample240 <- sample240[!is.na(sample240$K3Q1S1964_LD), ]
sample241<-herbgen[herbgen$SampleN == "241",]
sample241 <- sample241[!is.na(sample241$K3Q1S1964_LD), ]
sample242<-herbgen[herbgen$SampleN == "242",]
sample242 <- sample242[!is.na(sample242$K3Q1S1964_LD), ]
sample243<-herbgen[herbgen$SampleN == "243",]
sample243 <- sample243[!is.na(sample243$K3Q1S1964_LD), ]
sample244<-herbgen[herbgen$SampleN == "244",]
sample244 <- sample244[!is.na(sample244$K3Q1S1964_LD), ]
sample245<-herbgen[herbgen$SampleN == "245",]
sample245 <- sample245[!is.na(sample245$K3Q1S1964_LD), ]
sample246<-herbgen[herbgen$SampleN == "246",]
sample246 <- sample246[!is.na(sample246$K3Q1S1964_LD), ]
sample247<-herbgen[herbgen$SampleN == "247",]
sample247 <- sample247[!is.na(sample247$K3Q1S1964_LD), ]
sample248<-herbgen[herbgen$SampleN == "248",]
sample248 <- sample248[!is.na(sample248$K3Q1S1964_LD), ]
sample249<-herbgen[herbgen$SampleN == "249",]
sample249 <- sample249[!is.na(sample249$K3Q1S1964_LD), ]
sample250<-herbgen[herbgen$SampleN == "250",]
sample250 <- sample250[!is.na(sample250$K3Q1S1964_LD), ]
sample251<-herbgen[herbgen$SampleN == "251",]
sample251 <- sample251[!is.na(sample251$K3Q1S1964_LD), ]
sample252<-herbgen[herbgen$SampleN == "252",]
sample252 <- sample252[!is.na(sample252$K3Q1S1964_LD), ]
sample253<-herbgen[herbgen$SampleN == "253",]
sample253 <- sample253[!is.na(sample253$K3Q1S1964_LD), ]
sample254<-herbgen[herbgen$SampleN == "254",]
sample254 <- sample254[!is.na(sample254$K3Q1S1964_LD), ]
sample255<-herbgen[herbgen$SampleN == "255",]
sample255 <- sample255[!is.na(sample255$K3Q1S1964_LD), ]
sample256<-herbgen[herbgen$SampleN == "256",]
sample256 <- sample256[!is.na(sample256$K3Q1S1964_LD), ]
sample257<-herbgen[herbgen$SampleN == "257",]
sample257 <- sample257[!is.na(sample257$K3Q1S1964_LD), ]
sample258<-herbgen[herbgen$SampleN == "258",]
sample258 <- sample258[!is.na(sample258$K3Q1S1964_LD), ]
sample259<-herbgen[herbgen$SampleN == "259",]
sample259 <- sample259[!is.na(sample259$K3Q1S1964_LD), ]
sample260<-herbgen[herbgen$SampleN == "260",]
sample260 <- sample260[!is.na(sample260$K3Q1S1964_LD), ]
sample261<-herbgen[herbgen$SampleN == "261",]
sample261 <- sample261[!is.na(sample261$K3Q1S1964_LD), ]
sample262<-herbgen[herbgen$SampleN == "262",]
sample262 <- sample262[!is.na(sample262$K3Q1S1964_LD), ]
sample263<-herbgen[herbgen$SampleN == "263",]
sample263 <- sample263[!is.na(sample263$K3Q1S1964_LD), ]
sample264<-herbgen[herbgen$SampleN == "264",]
sample264 <- sample264[!is.na(sample264$K3Q1S1964_LD), ]
sample265<-herbgen[herbgen$SampleN == "265",]
sample265 <- sample265[!is.na(sample265$K3Q1S1964_LD), ]
sample266<-herbgen[herbgen$SampleN == "266",]
sample266 <- sample266[!is.na(sample266$K3Q1S1964_LD), ]
sample267<-herbgen[herbgen$SampleN == "267",]
sample267 <- sample267[!is.na(sample267$K3Q1S1964_LD), ]
sample268<-herbgen[herbgen$SampleN == "268",]
sample268 <- sample268[!is.na(sample268$K3Q1S1964_LD), ]
sample269<-herbgen[herbgen$SampleN == "269",]
sample269 <- sample269[!is.na(sample269$K3Q1S1964_LD), ]
sample270<-herbgen[herbgen$SampleN == "270",]
sample270 <- sample270[!is.na(sample270$K3Q1S1964_LD), ]
sample271<-herbgen[herbgen$SampleN == "271",]
sample271 <- sample271[!is.na(sample271$K3Q1S1964_LD), ]
sample272<-herbgen[herbgen$SampleN == "272",]
sample272 <- sample272[!is.na(sample272$K3Q1S1964_LD), ]
sample273<-herbgen[herbgen$SampleN == "273",]
sample273 <- sample273[!is.na(sample273$K3Q1S1964_LD), ]
sample274<-herbgen[herbgen$SampleN == "274",]
sample274 <- sample274[!is.na(sample274$K3Q1S1964_LD), ]
sample275<-herbgen[herbgen$SampleN == "275",]
sample275 <- sample275[!is.na(sample275$K3Q1S1964_LD), ]
sample276<-herbgen[herbgen$SampleN == "276",]
sample276 <- sample276[!is.na(sample276$K3Q1S1964_LD), ]
sample277<-herbgen[herbgen$SampleN == "277",]
sample277 <- sample277[!is.na(sample277$K3Q1S1964_LD), ]
sample278<-herbgen[herbgen$SampleN == "278",]
sample278 <- sample278[!is.na(sample278$K3Q1S1964_LD), ]
sample279<-herbgen[herbgen$SampleN == "279",]
sample279 <- sample279[!is.na(sample279$K3Q1S1964_LD), ]
sample280<-herbgen[herbgen$SampleN == "280",]
sample280 <- sample280[!is.na(sample280$K3Q1S1964_LD), ]
sample281<-herbgen[herbgen$SampleN == "281",]
sample281 <- sample281[!is.na(sample281$K3Q1S1964_LD), ]
sample282<-herbgen[herbgen$SampleN == "282",]
sample282 <- sample282[!is.na(sample282$K3Q1S1964_LD), ]
sample283<-herbgen[herbgen$SampleN == "283",]
sample283 <- sample283[!is.na(sample283$K3Q1S1964_LD), ]
sample284<-herbgen[herbgen$SampleN == "284",]
sample284 <- sample284[!is.na(sample284$K3Q1S1964_LD), ]
sample285<-herbgen[herbgen$SampleN == "285",]
sample285 <- sample285[!is.na(sample285$K3Q1S1964_LD), ]
sample286<-herbgen[herbgen$SampleN == "286",]
sample286 <- sample286[!is.na(sample286$K3Q1S1964_LD), ]
sample287<-herbgen[herbgen$SampleN == "287",]
sample287 <- sample287[!is.na(sample287$K3Q1S1964_LD), ]
sample288<-herbgen[herbgen$SampleN == "288",]
sample288 <- sample288[!is.na(sample288$K3Q1S1964_LD), ]
sample289<-herbgen[herbgen$SampleN == "289",]
sample289 <- sample289[!is.na(sample289$K3Q1S1964_LD), ]
sample290<-herbgen[herbgen$SampleN == "290",]
sample290 <- sample290[!is.na(sample290$K3Q1S1964_LD), ]
sample291<-herbgen[herbgen$SampleN == "291",]
sample291 <- sample291[!is.na(sample291$K3Q1S1964_LD), ]
sample292<-herbgen[herbgen$SampleN == "292",]
sample292 <- sample292[!is.na(sample292$K3Q1S1964_LD), ]
sample293<-herbgen[herbgen$SampleN == "293",]
sample293 <- sample293[!is.na(sample293$K3Q1S1964_LD), ]
sample294<-herbgen[herbgen$SampleN == "294",]
sample294 <- sample294[!is.na(sample294$K3Q1S1964_LD), ]
sample295<-herbgen[herbgen$SampleN == "295",]
sample295 <- sample295[!is.na(sample295$K3Q1S1964_LD), ]
sample296<-herbgen[herbgen$SampleN == "296",]
sample296 <- sample296[!is.na(sample296$K3Q1S1964_LD), ]
sample297<-herbgen[herbgen$SampleN == "297",]
sample297 <- sample297[!is.na(sample297$K3Q1S1964_LD), ]
sample298<-herbgen[herbgen$SampleN == "298",]
sample298 <- sample298[!is.na(sample298$K3Q1S1964_LD), ]
sample299<-herbgen[herbgen$SampleN == "299",]
sample299 <- sample299[!is.na(sample299$K3Q1S1964_LD), ]
sample300<-herbgen[herbgen$SampleN == "300",]
sample300 <- sample300[!is.na(sample300$K3Q1S1964_LD), ]
sample301<-herbgen[herbgen$SampleN == "301",]
sample301 <- sample301[!is.na(sample301$K3Q1S1964_LD), ]
sample302<-herbgen[herbgen$SampleN == "302",]
sample302 <- sample302[!is.na(sample302$K3Q1S1964_LD), ]
sample303<-herbgen[herbgen$SampleN == "303",]
sample303 <- sample303[!is.na(sample303$K3Q1S1964_LD), ]
sample304<-herbgen[herbgen$SampleN == "304",]
sample304 <- sample304[!is.na(sample304$K3Q1S1964_LD), ]
sample305<-herbgen[herbgen$SampleN == "305",]
sample305 <- sample305[!is.na(sample305$K3Q1S1964_LD), ]
sample306<-herbgen[herbgen$SampleN == "306",]
sample306 <- sample306[!is.na(sample306$K3Q1S1964_LD), ]
sample307<-herbgen[herbgen$SampleN == "307",]
sample307 <- sample307[!is.na(sample307$K3Q1S1964_LD), ]
sample308<-herbgen[herbgen$SampleN == "308",]
sample308 <- sample308[!is.na(sample308$K3Q1S1964_LD), ]
sample309<-herbgen[herbgen$SampleN == "309",]
sample309 <- sample309[!is.na(sample309$K3Q1S1964_LD), ]
sample310<-herbgen[herbgen$SampleN == "310",]
sample310 <- sample310[!is.na(sample310$K3Q1S1964_LD), ]
sample311<-herbgen[herbgen$SampleN == "311",]
sample311 <- sample311[!is.na(sample311$K3Q1S1964_LD), ]
sample312<-herbgen[herbgen$SampleN == "312",]
sample312 <- sample312[!is.na(sample312$K3Q1S1964_LD), ]
sample313<-herbgen[herbgen$SampleN == "313",]
sample313 <- sample313[!is.na(sample313$K3Q1S1964_LD), ]
sample314<-herbgen[herbgen$SampleN == "314",]
sample314 <- sample314[!is.na(sample314$K3Q1S1964_LD), ]
sample315<-herbgen[herbgen$SampleN == "315",]
sample315 <- sample315[!is.na(sample315$K3Q1S1964_LD), ]
sample316<-herbgen[herbgen$SampleN == "316",]
sample316 <- sample316[!is.na(sample316$K3Q1S1964_LD), ]
sample317<-herbgen[herbgen$SampleN == "317",]
sample317 <- sample317[!is.na(sample317$K3Q1S1964_LD), ]
sample318<-herbgen[herbgen$SampleN == "318",]
sample318 <- sample318[!is.na(sample318$K3Q1S1964_LD), ]
sample319<-herbgen[herbgen$SampleN == "319",]
sample319 <- sample319[!is.na(sample319$K3Q1S1964_LD), ]
sample320<-herbgen[herbgen$SampleN == "320",]
sample320 <- sample320[!is.na(sample320$K3Q1S1964_LD), ]
sample321<-herbgen[herbgen$SampleN == "321",]
sample321 <- sample321[!is.na(sample321$K3Q1S1964_LD), ]
sample322<-herbgen[herbgen$SampleN == "322",]
sample322 <- sample322[!is.na(sample322$K3Q1S1964_LD), ]
sample323<-herbgen[herbgen$SampleN == "323",]
sample323 <- sample323[!is.na(sample323$K3Q1S1964_LD), ]
sample324<-herbgen[herbgen$SampleN == "324",]
sample324 <- sample324[!is.na(sample324$K3Q1S1964_LD), ]
sample325<-herbgen[herbgen$SampleN == "325",]
sample325 <- sample325[!is.na(sample325$K3Q1S1964_LD), ]
sample326<-herbgen[herbgen$SampleN == "326",]
sample326 <- sample326[!is.na(sample326$K3Q1S1964_LD), ]
sample327<-herbgen[herbgen$SampleN == "327",]
sample327 <- sample327[!is.na(sample327$K3Q1S1964_LD), ]
sample328<-herbgen[herbgen$SampleN == "328",]
sample328 <- sample328[!is.na(sample328$K3Q1S1964_LD), ]
sample329<-herbgen[herbgen$SampleN == "329",]
sample329 <- sample329[!is.na(sample329$K3Q1S1964_LD), ]
sample330<-herbgen[herbgen$SampleN == "330",]
sample330 <- sample330[!is.na(sample330$K3Q1S1964_LD), ]
sample331<-herbgen[herbgen$SampleN == "331",]
sample331 <- sample331[!is.na(sample331$K3Q1S1964_LD), ]
sample332<-herbgen[herbgen$SampleN == "332",]
sample332 <- sample332[!is.na(sample332$K3Q1S1964_LD), ]
sample333<-herbgen[herbgen$SampleN == "333",]
sample333 <- sample333[!is.na(sample333$K3Q1S1964_LD), ]
sample334<-herbgen[herbgen$SampleN == "334",]
sample334 <- sample334[!is.na(sample334$K3Q1S1964_LD), ]
sample335<-herbgen[herbgen$SampleN == "335",]
sample335 <- sample335[!is.na(sample335$K3Q1S1964_LD), ]
sample336<-herbgen[herbgen$SampleN == "336",]
sample336 <- sample336[!is.na(sample336$K3Q1S1964_LD), ]
sample337<-herbgen[herbgen$SampleN == "337",]
sample337 <- sample337[!is.na(sample337$K3Q1S1964_LD), ]
sample338<-herbgen[herbgen$SampleN == "338",]
sample338 <- sample338[!is.na(sample338$K3Q1S1964_LD), ]
sample339<-herbgen[herbgen$SampleN == "339",]
sample339 <- sample339[!is.na(sample339$K3Q1S1964_LD), ]
sample340<-herbgen[herbgen$SampleN == "340",]
sample340 <- sample340[!is.na(sample340$K3Q1S1964_LD), ]
sample341<-herbgen[herbgen$SampleN == "341",]
sample341 <- sample341[!is.na(sample341$K3Q1S1964_LD), ]
sample342<-herbgen[herbgen$SampleN == "342",]
sample342 <- sample342[!is.na(sample342$K3Q1S1964_LD), ]
sample343<-herbgen[herbgen$SampleN == "343",]
sample343 <- sample343[!is.na(sample343$K3Q1S1964_LD), ]
sample344<-herbgen[herbgen$SampleN == "344",]
sample344 <- sample344[!is.na(sample344$K3Q1S1964_LD), ]
sample345<-herbgen[herbgen$SampleN == "345",]
sample345 <- sample345[!is.na(sample345$K3Q1S1964_LD), ]
sample346<-herbgen[herbgen$SampleN == "346",]
sample346 <- sample346[!is.na(sample346$K3Q1S1964_LD), ]
sample347<-herbgen[herbgen$SampleN == "347",]
sample347 <- sample347[!is.na(sample347$K3Q1S1964_LD), ]
sample348<-herbgen[herbgen$SampleN == "348",]
sample348 <- sample348[!is.na(sample348$K3Q1S1964_LD), ]
sample349<-herbgen[herbgen$SampleN == "349",]
sample349 <- sample349[!is.na(sample349$K3Q1S1964_LD), ]
sample350<-herbgen[herbgen$SampleN == "350",]
sample350 <- sample350[!is.na(sample350$K3Q1S1964_LD), ]
sample351<-herbgen[herbgen$SampleN == "351",]
sample351 <- sample351[!is.na(sample351$K3Q1S1964_LD), ]
sample352<-herbgen[herbgen$SampleN == "352",]
sample352 <- sample352[!is.na(sample352$K3Q1S1964_LD), ]
sample353<-herbgen[herbgen$SampleN == "353",]
sample353 <- sample353[!is.na(sample353$K3Q1S1964_LD), ]
sample354<-herbgen[herbgen$SampleN == "354",]
sample354 <- sample354[!is.na(sample354$K3Q1S1964_LD), ]
sample355<-herbgen[herbgen$SampleN == "355",]
sample355 <- sample355[!is.na(sample355$K3Q1S1964_LD), ]
sample356<-herbgen[herbgen$SampleN == "356",]
sample356 <- sample356[!is.na(sample356$K3Q1S1964_LD), ]
sample357<-herbgen[herbgen$SampleN == "357",]
sample357 <- sample357[!is.na(sample357$K3Q1S1964_LD), ]
sample358<-herbgen[herbgen$SampleN == "358",]
sample358 <- sample358[!is.na(sample358$K3Q1S1964_LD), ]
sample359<-herbgen[herbgen$SampleN == "359",]
sample359 <- sample359[!is.na(sample359$K3Q1S1964_LD), ]
sample360<-herbgen[herbgen$SampleN == "360",]
sample360 <- sample360[!is.na(sample360$K3Q1S1964_LD), ]
sample361<-herbgen[herbgen$SampleN == "361",]
sample361 <- sample361[!is.na(sample361$K3Q1S1964_LD), ]
sample362<-herbgen[herbgen$SampleN == "362",]
sample362 <- sample362[!is.na(sample362$K3Q1S1964_LD), ]
sample363<-herbgen[herbgen$SampleN == "363",]
sample363 <- sample363[!is.na(sample363$K3Q1S1964_LD), ]
sample364<-herbgen[herbgen$SampleN == "364",]
sample364 <- sample364[!is.na(sample364$K3Q1S1964_LD), ]
sample365<-herbgen[herbgen$SampleN == "365",]
sample365 <- sample365[!is.na(sample365$K3Q1S1964_LD), ]
sample366<-herbgen[herbgen$SampleN == "366",]
sample366 <- sample366[!is.na(sample366$K3Q1S1964_LD), ]
sample367<-herbgen[herbgen$SampleN == "367",]
sample367 <- sample367[!is.na(sample367$K3Q1S1964_LD), ]
sample368<-herbgen[herbgen$SampleN == "368",]
sample368 <- sample368[!is.na(sample368$K3Q1S1964_LD), ]
sample369<-herbgen[herbgen$SampleN == "369",]
sample369 <- sample369[!is.na(sample369$K3Q1S1964_LD), ]
sample370<-herbgen[herbgen$SampleN == "370",]
sample370 <- sample370[!is.na(sample370$K3Q1S1964_LD), ]
sample371<-herbgen[herbgen$SampleN == "371",]
sample371 <- sample371[!is.na(sample371$K3Q1S1964_LD), ]
sample372<-herbgen[herbgen$SampleN == "372",]
sample372 <- sample372[!is.na(sample372$K3Q1S1964_LD), ]
sample373<-herbgen[herbgen$SampleN == "373",]
sample373 <- sample373[!is.na(sample373$K3Q1S1964_LD), ]
sample374<-herbgen[herbgen$SampleN == "374",]
sample374 <- sample374[!is.na(sample374$K3Q1S1964_LD), ]
sample375<-herbgen[herbgen$SampleN == "375",]
sample375 <- sample375[!is.na(sample375$K3Q1S1964_LD), ]
sample376<-herbgen[herbgen$SampleN == "376",]
sample376 <- sample376[!is.na(sample376$K3Q1S1964_LD), ]
sample377<-herbgen[herbgen$SampleN == "377",]
sample377 <- sample377[!is.na(sample377$K3Q1S1964_LD), ]
sample378<-herbgen[herbgen$SampleN == "378",]
sample378 <- sample378[!is.na(sample378$K3Q1S1964_LD), ]
sample379<-herbgen[herbgen$SampleN == "379",]
sample379 <- sample379[!is.na(sample379$K3Q1S1964_LD), ]
sample380<-herbgen[herbgen$SampleN == "380",]
sample380 <- sample380[!is.na(sample380$K3Q1S1964_LD), ]
sample381<-herbgen[herbgen$SampleN == "381",]
sample381 <- sample381[!is.na(sample381$K3Q1S1964_LD), ]
sample382<-herbgen[herbgen$SampleN == "382",]
sample382 <- sample382[!is.na(sample382$K3Q1S1964_LD), ]
sample383<-herbgen[herbgen$SampleN == "383",]
sample383 <- sample383[!is.na(sample383$K3Q1S1964_LD), ]
sample384<-herbgen[herbgen$SampleN == "384",]
sample384 <- sample384[!is.na(sample384$K3Q1S1964_LD), ]
sample385<-herbgen[herbgen$SampleN == "385",]
sample385 <- sample385[!is.na(sample385$K3Q1S1964_LD), ]
sample386<-herbgen[herbgen$SampleN == "386",]
sample386 <- sample386[!is.na(sample386$K3Q1S1964_LD), ]
sample387<-herbgen[herbgen$SampleN == "387",]
sample387 <- sample387[!is.na(sample387$K3Q1S1964_LD), ]
sample388<-herbgen[herbgen$SampleN == "388",]
sample388 <- sample388[!is.na(sample388$K3Q1S1964_LD), ]
sample389<-herbgen[herbgen$SampleN == "389",]
sample389 <- sample389[!is.na(sample389$K3Q1S1964_LD), ]
sample390<-herbgen[herbgen$SampleN == "390",]
sample390 <- sample390[!is.na(sample390$K3Q1S1964_LD), ]
sample391<-herbgen[herbgen$SampleN == "391",]
sample391 <- sample391[!is.na(sample391$K3Q1S1964_LD), ]
sample392<-herbgen[herbgen$SampleN == "392",]
sample392 <- sample392[!is.na(sample392$K3Q1S1964_LD), ]
sample393<-herbgen[herbgen$SampleN == "393",]
sample393 <- sample393[!is.na(sample393$K3Q1S1964_LD), ]
sample394<-herbgen[herbgen$SampleN == "394",]
sample394 <- sample394[!is.na(sample394$K3Q1S1964_LD), ]
sample395<-herbgen[herbgen$SampleN == "395",]
sample395 <- sample395[!is.na(sample395$K3Q1S1964_LD), ]
sample396<-herbgen[herbgen$SampleN == "396",]
sample396 <- sample396[!is.na(sample396$K3Q1S1964_LD), ]
sample397<-herbgen[herbgen$SampleN == "397",]
sample397 <- sample397[!is.na(sample397$K3Q1S1964_LD), ]
sample398<-herbgen[herbgen$SampleN == "398",]
sample398 <- sample398[!is.na(sample398$K3Q1S1964_LD), ]
sample399<-herbgen[herbgen$SampleN == "399",]
sample399 <- sample399[!is.na(sample399$K3Q1S1964_LD), ]
sample400<-herbgen[herbgen$SampleN == "400",]
sample400 <- sample400[!is.na(sample400$K3Q1S1964_LD), ]
sample401<-herbgen[herbgen$SampleN == "401",]
sample401 <- sample401[!is.na(sample401$K3Q1S1964_LD), ]
sample402<-herbgen[herbgen$SampleN == "402",]
sample402 <- sample402[!is.na(sample402$K3Q1S1964_LD), ]
sample403<-herbgen[herbgen$SampleN == "403",]
sample403 <- sample403[!is.na(sample403$K3Q1S1964_LD), ]
sample404<-herbgen[herbgen$SampleN == "404",]
sample404 <- sample404[!is.na(sample404$K3Q1S1964_LD), ]
sample405<-herbgen[herbgen$SampleN == "405",]
sample405 <- sample405[!is.na(sample405$K3Q1S1964_LD), ]
sample406<-herbgen[herbgen$SampleN == "406",]
sample406 <- sample406[!is.na(sample406$K3Q1S1964_LD), ]
sample407<-herbgen[herbgen$SampleN == "407",]
sample407 <- sample407[!is.na(sample407$K3Q1S1964_LD), ]
sample408<-herbgen[herbgen$SampleN == "408",]
sample408 <- sample408[!is.na(sample408$K3Q1S1964_LD), ]
sample409<-herbgen[herbgen$SampleN == "409",]
sample409 <- sample409[!is.na(sample409$K3Q1S1964_LD), ]
sample410<-herbgen[herbgen$SampleN == "410",]
sample410 <- sample410[!is.na(sample410$K3Q1S1964_LD), ]
sample411<-herbgen[herbgen$SampleN == "411",]
sample411 <- sample411[!is.na(sample411$K3Q1S1964_LD), ]
sample412<-herbgen[herbgen$SampleN == "412",]
sample412 <- sample412[!is.na(sample412$K3Q1S1964_LD), ]
sample413<-herbgen[herbgen$SampleN == "413",]
sample413 <- sample413[!is.na(sample413$K3Q1S1964_LD), ]
sample414<-herbgen[herbgen$SampleN == "414",]
sample414 <- sample414[!is.na(sample414$K3Q1S1964_LD), ]
library(ggplot2)
library(sf)
library(rnaturalearth)
library(scatterpie)

# Get world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Function using time bins
create_pie_map_by_timebin <- function(data, timebin_name, title,pie_scale) {
  timebin_data <- data[data$TimeBin == timebin_name, ]
  timebin_data <- timebin_data[!is.na(timebin_data$K3Q1S1964_LD), ]
  
  if(nrow(timebin_data) == 0) return(ggplot() + theme_void())
  
  pie_data <- data.frame(
    lon = timebin_data$lon,
    lat = timebin_data$lat,
    cluster1 = timebin_data$K3Q1S1964_LD,
    cluster2 = timebin_data$K3Q2S1964_LD,
    cluster3 = timebin_data$K3Q3S1964_LD
  )
  
  ggplot() +
    geom_sf(data = world, fill = "#e9e9e9ff", color = "#070707ff") +
    geom_scatterpie(data = pie_data, 
                    aes(x = lon, y = lat), 
                    cols = c("cluster1", "cluster2", "cluster3"),
                    pie_scale = pie_scale) +
    scale_fill_manual(values = c("#8DD3C7", "#ffffb3d7", "#BEBADA")) +
    coord_sf(xlim = c(-105, -60), ylim = c(25, 50)) +
    labs(title = title) +
    theme_minimal()+theme(
      plot.title = element_text(hjust = 0.5, size = 14),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),legend.position = "none"
    )
}

# Create all plots
plot1_gg <- create_pie_map_by_timebin(herbgen, "1838-1877", "1838-1877",pie_scale=3)
plot2_gg <- create_pie_map_by_timebin(herbgen, "1878-1917", "1878-1917",pie_scale=2)
plot3_gg <- create_pie_map_by_timebin(herbgen, "1918-1957", "1918-1957",pie_scale=1)
plot4_gg <- create_pie_map_by_timebin(herbgen, "1958-1997", "1958-1997",pie_scale=0.6)

# Combine plots into a single figure
mapseries_plot <- ggarrange(plot1_gg, plot2_gg, plot3_gg, plot4_gg,
          ncol = 2, nrow = 2)
#Panel B
combined_PanelB <- ggarrange(ngsadmix_plot,mapseries_plot,
          ncol = 2, nrow = 1, widths = c(1,3))


#Euclidean Distance

herb_df <- read.csv("Data/herbarium/Input/herbarium_structure_dataframe_1192026.csv",header=TRUE)
pc_cols <- c("PC_LD1","PC_LD2")
# Assume your data frame is called df and has columns: Country, PC_LD1, PC_LD2, PC3, ...
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

pc_cols <- c("PC_LD1","PC_LD2")
# Assume your data frame is called df and has columns: Country, PC_LD1, PC_LD2, PC3, ...
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

pc_cols <- c("PC_LD1","PC_LD2")
# Assume your data frame is called df and has columns: Country, PC_LD1, PC_LD2, PC3, ...
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

pc_cols <- c("PC_LD1","PC_LD2")
# Assume your data frame is called df and has columns: Country, PC_LD1, PC_LD2, PC3, ...
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

pc_cols <- c("PC_LD1","PC_LD2")
# Assume your data frame is called df and has columns: Country, PC_LD1, PC_LD2, PC3, ...
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

pc_cols <- c("PC_LD1","PC_LD2")
# Assume your data frame is called df and has columns: Country, PC_LD1, PC_LD2, PC3, ...
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

pc_cols <- c("PC_LD1","PC_LD2")
# Assume your data frame is called df and has columns: Country, PC_LD1, PC_LD2, PC3, ...
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

pc_cols <- c("PC_LD1","PC_LD2")
# Assume your data frame is called df and has columns: Country, PC_LD1, PC_LD2, PC3, ...
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

results_N_1$TimeBin <- "1838-1877"
results_N_2$TimeBin <- "1878-1917"
results_N_3$TimeBin <- "1918-1957"
results_N_4$TimeBin <- "1958-1997"
results_S_1$TimeBin <- "1838-1877"
results_S_2$TimeBin <- "1878-1917"
results_S_3$TimeBin <- "1918-1957"
results_S_4$TimeBin <- "1958-1997"
results_N_1$Region <- "North"
results_N_2$Region <- "North"
results_N_3$Region <- "North"
results_N_4$Region <- "North"
results_S_1$Region <- "South"
results_S_2$Region <- "South"
results_S_3$Region <- "South"
results_S_4$Region <- "South"

results <- rbind(results_N_1, results_N_2, results_N_3, results_N_4, results_S_1, results_S_2, results_S_3, results_S_4)

# Middle region analysis for each TimeBin
pc_cols <- c("PC_LD1","PC_LD2")

# 1838-1877
df <- herb_df[herb_df$Region=="Middle" | herb_df$H_C=="C",]
df <- df[df$TimeBin=="1838-1877" | df$H_C=="C",]
centroids <- df %>%
    filter(Country != "USA") %>%
    group_by(Country) %>%
    summarise(across(all_of(pc_cols), mean), .groups = "drop")
usa_samples <- df %>%
    filter(Country == "USA") %>%
    dplyr::select(all_of(pc_cols))
results_M_1 <- centroids %>%
    rowwise() %>%
    mutate(
        distances = list(apply(usa_samples, 1, function(x) euclidean(x, c_across(all_of(pc_cols))))),
        avg_distance = mean(unlist(distances)),
        sd_distance = sd(unlist(distances))
    ) %>%
    dplyr::select(Country, avg_distance, sd_distance)

# 1878-1917
df <- herb_df[herb_df$Region=="Middle" | herb_df$H_C=="C",]
df <- df[df$TimeBin=="1878-1917" | df$H_C=="C",]
centroids <- df %>%
    filter(Country != "USA") %>%
    group_by(Country) %>%
    summarise(across(all_of(pc_cols), mean), .groups = "drop")
usa_samples <- df %>%
    filter(Country == "USA") %>%
    dplyr::select(all_of(pc_cols))
results_M_2 <- centroids %>%
    rowwise() %>%
    mutate(
        distances = list(apply(usa_samples, 1, function(x) euclidean(x, c_across(all_of(pc_cols))))),
        avg_distance = mean(unlist(distances)),
        sd_distance = sd(unlist(distances))
    ) %>%
    dplyr::select(Country, avg_distance, sd_distance)

# 1918-1957
df <- herb_df[herb_df$Region=="Middle" | herb_df$H_C=="C",]
df <- df[df$TimeBin=="1918-1957" | df$H_C=="C",]
centroids <- df %>%
    filter(Country != "USA") %>%
    group_by(Country) %>%
    summarise(across(all_of(pc_cols), mean), .groups = "drop")
usa_samples <- df %>%
    filter(Country == "USA") %>%
    dplyr::select(all_of(pc_cols))
results_M_3 <- centroids %>%
    rowwise() %>%
    mutate(
        distances = list(apply(usa_samples, 1, function(x) euclidean(x, c_across(all_of(pc_cols))))),
        avg_distance = mean(unlist(distances)),
        sd_distance = sd(unlist(distances))
    ) %>%
    dplyr::select(Country, avg_distance, sd_distance)

# 1958-1997
df <- herb_df[herb_df$Region=="Middle" | herb_df$H_C=="C",]
df <- df[df$TimeBin=="1958-1997" | df$H_C=="C",]
centroids <- df %>%
    filter(Country != "USA") %>%
    group_by(Country) %>%
    summarise(across(all_of(pc_cols), mean), .groups = "drop")
usa_samples <- df %>%
    filter(Country == "USA") %>%
    dplyr::select(all_of(pc_cols))
results_M_4 <- centroids %>%
    rowwise() %>%
    mutate(
        distances = list(apply(usa_samples, 1, function(x) euclidean(x, c_across(all_of(pc_cols))))),
        avg_distance = mean(unlist(distances)),
        sd_distance = sd(unlist(distances))
    ) %>%
    dplyr::select(Country, avg_distance, sd_distance)

# Add TimeBin and Region columns
results_M_1$TimeBin <- "1838-1877"
results_M_2$TimeBin <- "1878-1917"
results_M_3$TimeBin <- "1918-1957"
results_M_4$TimeBin <- "1958-1997"
results_M_1$Region <- "Middle"
results_M_2$Region <- "Middle"
results_M_3$Region <- "Middle"
results_M_4$Region <- "Middle"

# Combine with previous results
results <- rbind(results, results_M_1, results_M_2, results_M_3, results_M_4)
results_UFS <- results[results$Country=="UK" | results$Country=="France" | results$Country=="Spain",]
library(ggplot2)

colorBlindBlack8 <- c("#E69F00", "#56B4E9", "darkblue")

# Custom labels for Region
region_labels <- c("North" = "North: > 40°N", "Middle" = "Middle: 35°N - 40°N", "South" = "South: < 35°N")
results_UFS$Region <- factor(results_UFS$Region, levels = c("South", "Middle", "North"))

combined_PanelC <- ggplot(results_UFS, aes(x = TimeBin, y = avg_distance, color = Country, group = Country)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_line(position = position_dodge(width = 0.5), size = 1) +
  scale_color_manual(values = colorBlindBlack8) +
  scale_fill_manual(values = colorBlindBlack8) +
  geom_errorbar(aes(ymin = avg_distance - sd_distance, ymax = avg_distance + sd_distance),
                width = 0.2, position = position_dodge(width = 0.5)) +
  facet_wrap(~Region, nrow = 1, labeller = labeller(Region = region_labels)) + 
  theme_light() +
  theme(
    strip.text = element_text(color = "black", size = 12),
    strip.background = element_rect(fill = "white"),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 8),
    axis.title.y = element_text(size = 12),
    axis.title.x=element_blank(),
    #legend.title = element_blank(),
    #legend.text = element_text(size = 12),
    legend.position="none",
    #legend.background = element_rect(fill = "white", color = "black", size = 0.5, linetype = "solid"), # Box for legend
    #legend.box.margin = margin(4, 4, 4, 4) # Optional: margin inside the box
  ) +
  labs(
    y = "Average Distance"
  )

final_figure <- ggarrange(combined_PanelA,combined_PanelB,combined_PanelC,
          ncol = 1, nrow = 3, heights = c(1.25,1.75,1),labels = c("A", "B", "C"),font.label = list(size = 20))

figure1 <- ggarrange(combined_PanelA,combined_PanelC,ncol=1,heights=c(1.5,1), labels = c("A","B"),font.label = list(size = 20))

figure2 <- ggarrange(ngsadmix_plot,mapseries_plot,
          ncol = 1, nrow = 2,heights= c(0.5,2),labels = c("A", "B"),font.label = list(size = 12))

ggsave("Figure1.pdf", plot = figure1, width = 9, height = 6, units = "in")
ggsave("Figure2_New.pdf", plot = figure2, width = 6, height = 6, units = "in")

# Figure C Analysis
library(dplyr)
library(emmeans)
library(multcomp)

herb_df <- read.csv("Data/herbarium/Input/herbarium_structure_dataframe_1192026.csv", header = TRUE)
pc_cols <- c("PC_LD1", "PC_LD2")

# Function to calculate Euclidean distance
euclidean <- function(a, b) sqrt(sum((a - b)^2))

# ========== CREATE RAW DISTANCE DATA ==========

regions <- c("North", "Middle", "South")
timebins <- c("1838-1877", "1878-1917", "1918-1957", "1958-1997")

# Initialize empty list to store all distance data
all_distances <- list()

for (region in regions) {
  for (timebin in timebins) {
    # Filter data for this region and timebin
    df <- herb_df[herb_df$Region == region | herb_df$H_C == "C", ]
    df <- df[df$TimeBin == timebin | df$H_C == "C", ]
    
    # Calculate centroids for each country except USA
    centroids <- df %>%
      filter(Country != "USA") %>%
      group_by(Country) %>%
      summarise(across(all_of(pc_cols), mean), .groups = "drop")
    
    # Get USA samples
    usa_samples <- df %>%
      filter(Country == "USA") %>%
      dplyr::select(all_of(pc_cols))
    
    # Skip if no USA samples
    if (nrow(usa_samples) == 0) next
    
    # For each centroid, calculate distances to ALL USA samples individually
    for (i in 1:nrow(centroids)) {
      country_name <- centroids$Country[i]
      centroid_coords <- as.numeric(centroids[i, pc_cols])
      
      # Calculate distance from this centroid to each USA sample
      for (j in 1:nrow(usa_samples)) {
        usa_coords <- as.numeric(usa_samples[j, pc_cols])
        dist <- euclidean(centroid_coords, usa_coords)
        
        # Store the individual distance
        all_distances[[length(all_distances) + 1]] <- data.frame(
          Country = country_name,
          Region = region,
          TimeBin = timebin,
          USA_Sample = j,
          distance = dist
        )
      }
    }
  }
}

# Combine all individual distances into one dataframe
results_UFS_raw <- bind_rows(all_distances)

# Filter to only UK, France, Spain
results_UFS_raw <- results_UFS_raw %>%
  filter(Country %in% c("UK", "France", "Spain"))

# ========== NOW RUN STATISTICAL MODELS ==========

# Prepare data
results_UFS_raw <- results_UFS_raw %>%
  mutate(
    TimeBin = factor(TimeBin, levels = c("1838-1877", "1958-1997")),
    Region = factor(Region, levels = c("North", "Middle", "South")),
    Country = factor(Country)
  )

# ========== ANALYZE EACH COUNTRY-REGION COMBINATION ==========

countries <- c("UK", "France", "Spain")
regions <- c("North", "Middle", "South")

all_models <- list()
all_emmeans <- list()
all_pairs <- list()
all_cld <- list()

for (country in countries) {
  for (region in regions) {
    cat("\n========================================\n")
    cat("ANALYZING:", country, "-", region, "\n")
    cat("========================================\n\n")
    
    # Subset data for this country-region combination
    subset_data <- results_UFS_raw %>% 
      filter(Country == country, Region == region)
    
    # Skip if no data
    if (nrow(subset_data) == 0) {
      cat("No data available. Skipping.\n\n")
      next
    }
    
    # Fit model: distance ~ TimeBin
    model <- lm(distance ~ TimeBin, data = subset_data)
    all_models[[paste0(country, "_", region)]] <- model
    
    # Model summary
    cat("Model Summary:\n")
    print(summary(model))
    cat("\n")
    
    # Emmeans by TimeBin
    emm_timebin <- emmeans(model, ~ TimeBin)
    all_emmeans[[paste0(country, "_", region, "_TimeBin")]] <- emm_timebin
    
    cat("Estimated Marginal Means by TimeBin:\n")
    print(emm_timebin)
    cat("\n")
    
    # Pairwise comparisons with Tukey adjustment
    pairs_timebin <- pairs(emm_timebin, adjust = "tukey")
    all_pairs[[paste0(country, "_", region, "_TimeBin")]] <- pairs_timebin
    
    cat("Pairwise Comparisons (TimeBin):\n")
    print(pairs_timebin)
    cat("\n")
    
    # Compact Letter Display
    cld_timebin <- cld(emm_timebin, Letters = letters, adjust = "tukey")
    all_cld[[paste0(country, "_", region, "_TimeBin")]] <- cld_timebin
    
    cat("Compact Letter Display (TimeBin):\n")
    print(cld_timebin)
    cat("\n")
  }
}

# ========== CREATE SUMMARY DATAFRAME OF ALL RESULTS ==========

# Initialize empty list to store results
results_list <- list()

for (country in countries) {
  for (region in regions) {
    key <- paste0(country, "_", region, "_TimeBin")
    
    # Skip if no results for this combination
    if (!key %in% names(all_emmeans)) next
    
    # Extract emmeans
    emm_df <- as.data.frame(all_emmeans[[key]])
    
    # Extract pairwise comparisons
    pairs_df <- as.data.frame(all_pairs[[key]])
    
    # Extract compact letter display
    cld_df <- as.data.frame(all_cld[[key]])
    
    # Add Country and Region columns to emmeans
    emm_df$Country <- country
    emm_df$Region <- region
    
    # Add Country and Region columns to pairwise comparisons
    pairs_df$Country <- country
    pairs_df$Region <- region
    
    # Add Country and Region columns to CLD
    cld_df$Country <- country
    cld_df$Region <- region
    
    # Store in list
    results_list[[paste0(country, "_", region)]] <- list(
      emmeans = emm_df,
      pairs = pairs_df,
      cld = cld_df
    )
  }
}

# Combine all emmeans into one dataframe
all_emmeans_df <- bind_rows(lapply(results_list, function(x) x$emmeans))

# Combine all pairwise comparisons into one dataframe
all_pairs_df <- bind_rows(lapply(results_list, function(x) x$pairs))

# Combine all CLDs into one dataframe
all_cld_df <- bind_rows(lapply(results_list, function(x) x$cld))

# Reorder columns for better readability
all_emmeans_df <- all_emmeans_df %>%
  dplyr::select(Country, Region, TimeBin, emmean, SE, df, lower.CL, upper.CL)

all_pairs_df <- all_pairs_df %>%
  dplyr::select(Country, Region, contrast, estimate, SE, df, t.ratio, p.value)

all_cld_df <- all_cld_df %>%
  dplyr::select(Country, Region, TimeBin, emmean, SE, df, lower.CL, upper.CL, .group)

# View the results
head(all_emmeans_df)
head(all_pairs_df)
head(all_cld_df)
