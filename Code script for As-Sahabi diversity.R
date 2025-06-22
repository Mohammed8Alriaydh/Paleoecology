# Used libraries in this study
library(tidyverse)      # Data manipulation and visualization
library(indicspecies)   # Indicator species analysis
library(iNEXT.beta3D)   # Diversity analysis
library(ggpubr)         # Plot arrangement
library(openxlsx)       # Excel file handling
library(FactoMineR)     # Multivariate analysis
library(factoextra)     # Visualization for multivariate results
library(vegan)          # Ecological diversity analysis
library(reshape2)       # Data reshaping
library(pvclust)        # Cluster analysis with p-values
library(dunn.test)      # Post-hoc Dunn's tests
library(ggrepel)        # Label positioning in plots
library(metan)          # Specialized plotting (ggpie)
library(officer)        # Word document generation
library(flextable)      # Tables for Word documents
library(sf)             # Spatial data handling
library(betapart)       # Beta diversity partitioning
###########################################################################
###########################################################################
#calculating the percentage of data sets (Order/Family; Family/Subfamily) of the whole data
#Table 
whole_data<-5762 # Total No. of specimens 
order_family<-5389 #No. of specimens
order_family_mammals<-3127 #No. of specimens with all localities
order_family_mammals_Marine<-2841 #No. of specimens from 45 samples
family_tribe_mammals_Marine<-1719 #No. of specimens 
genus_species_mammals_Marine<-1393 #No. of specimens 

percentage_order_family1<-order_family_mammals_Marine/whole_data*100 #51
percentage_order_family2<-order_family_mammals_Marine/order_family_mammals*100 #93

percentage_family_tribe1<-family_tribe_mammals_Marine/whole_data*100 #23
percentage_family_tribe2<-family_tribe_mammals_Marine/order_family_mammals*100 #43

##########################################################################
######## Taxonomic composition #####################################
######## Pie charts ########################################################
#################################################################
####### For Marine and Terrestrial Taxa #####################

# Create the data
data2 <- tibble(
  Locality = c(rep("T", 12), rep("U1", 12), rep("U2", 12), rep("V", 12)),
  Taxa = rep(c("Prim", "Carn", "Prob", "Equi", 
               "Rhin", "Suid", "Anth", "Hipp", "Gira", "Bovi", "Ceta", "Sire"), 4),
  Abundance = c(
    # T
    1, 9, 7, 4, 0, 3, 27, 0, 3, 9,  5, 95,   
    # U1
    27, 136, 207, 201, 17, 139, 498, 112, 102, 971, 3, 64,
    # U2
    3,  14, 5, 12, 0, 31, 12, 5, 2, 28, 9, 2,
    # V
    2,  1, 0, 0, 0, 4, 12, 36, 1, 22,0, 0
  )
)

# Group the data by unit and calculate relative abundance
data2 <- data2 %>%
  group_by(unit) %>%
  mutate(Relative_Abundance = Abundance / sum(Abundance) * 100)

# Add a label column with both Taxa and Relative Abundance
data2 <- data2 %>%
  mutate(label = ifelse(Relative_Abundance > 0, paste0(round(Relative_Abundance), "%"), ""))
# Define the color palette
color_palette <- c("#A6CEE3", "#1F78B4", "#B2DF8A", "#FFD700","#33A02C","#FB9A99", 
                   "#E31A1C", "#FDBF6F", "#FF7F00", "black", "#D3D3D3","#6A3D9A")


# Plot the pie chart for unit T with percentage labels
fig1<-ggpie(data2 %>% filter(unit == "T"), 
            x = "Relative_Abundance", 
            label = "Label",  # Using the Label column for both Taxa and percentage
            fill = "Taxa", 
            lab.pos = "out",         # Position the labels outside
            lab.adjust = 1.5,        # Adjust label position
            lab.font = c(4, "plain", "black"),
            palette=color_palette)+
  theme(plot.title = element_text(hjust = 0.5,size=16,face="bold",colour = "blue"))+
  ggtitle("T")

# Plot for unit U1 with percentage labels
fig2<-ggpie(data2 %>% filter(unit == "U1"), 
            x = "Relative_Abundance", 
            label = "Label",  
            fill = "Taxa", 
            lab.pos = "out",         
            lab.adjust = 1.5,        
            lab.font = c(4, "plain", "black"),
            palette=color_palette)+ 
  theme(plot.title = element_text(hjust = 0.5,size=16,face="bold",colour = "deepskyblue2"))+
  ggtitle("U1")

# Plot for unit U2 with percentage labels
fig3<-ggpie(data2 %>% filter(unit == "U2"), 
            x = "Relative_Abundance", 
            label = "Label",  
            fill = "Taxa", 
            lab.pos = "out",        
            lab.adjust = 1.5,        
            lab.font = c(4, "plain", "black"),
            palette=color_palette)+
  theme(plot.title = element_text(hjust = 0.5,size=16,face="bold",colour = "darkgreen"))+
  ggtitle("U2")

# Plot for unit V with percentage labels
fig4<-ggpie(data2 %>% filter(unit == "V"), 
            x = "Relative_Abundance", 
            label = "Label",  #
            fill = "Taxa", 
            lab.pos = "out",         
            lab.adjust = 1.5,        
            lab.font = c(4, "plain", "black"),
            palette=color_palette)+
  theme(plot.title = element_text(hjust = 0.5,size=16,face="bold", colour = "lightgreen"))+
  ggtitle("V")

figure_gamma_full2 <- ggarrange(fig1,fig2,fig3,fig4,
                                labels = c("A", "B", "C","D"),hjust = -1,vjust = 1.5,
                                ncol = 4, nrow = 1,common.legend = T, legend = "bottom")
fi2<-figure_gamma_full2+
  theme(panel.background = element_rect(fill = "white", color = NA),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        plot.background = element_rect(fill = "white", color = "black", size = 2))

fi2

#ggsave(file="Figs/Fig _2.svg", plot = fi2, width = 13, height = 9)
##################################################################################
######## Taxonomic Diversity ###################################################
##### DIVERSITY ###################################################################
#USING Chao et al. 2023 The iNEXT.beta3D standardization for the different time periods sahabi data.
# Load the data D1
D1<-list('T'   = read.xlsx('Data/D1.xlsx', rowNames = T, sheet = 1),
          'U1' = read.xlsx('Data/D1.xlsx', rowNames = T, sheet = 2),
          'U2' = read.xlsx('Data/D1.xlsx', rowNames = T, sheet = 3),
          'V' = read.xlsx('Data/D1.xlsx', rowNames = T, sheet = 4))

#BETA2<-iNEXTbeta3D(D1,base = "size")
# Run the beta diversity analysis
BETA21<-iNEXTbeta3D(D1,base = "coverage", nboot=20)

# Create a new Excel workbook
wb <- createWorkbook()

# Add sheets for each type of result

# 1. Gamma diversity
addWorksheet(wb, "Gamma_T")
writeData(wb, "Gamma_T", BETA21$T$gamma, rowNames = TRUE)
addWorksheet(wb, "Gamma_U1")
writeData(wb, "Gamma_U1", BETA21$U1$gamma, rowNames = TRUE)
addWorksheet(wb, "Gamma_U2")
writeData(wb, "Gamma_U2", BETA21$U2$gamma, rowNames = TRUE)
addWorksheet(wb, "Gamma_V")
writeData(wb, "Gamma_V", BETA21$V$gamma, rowNames = TRUE)
# 2. Alpha diversity
addWorksheet(wb, "Alpha_T")
writeData(wb, "Alpha_T", BETA21$T$alpha, rowNames = TRUE)
addWorksheet(wb, "Alpha_U1")
writeData(wb, "Alpha_U1", BETA21$U1$alpha, rowNames = TRUE)
addWorksheet(wb, "Alpha_U2")
writeData(wb, "Alpha_U2", BETA21$U1$alpha, rowNames = TRUE)
addWorksheet(wb, "Alpha_V")
writeData(wb, "Alpha_V", BETA21$V$alpha, rowNames = TRUE)

# 3. Beta diversity
addWorksheet(wb, "Beta_T")
writeData(wb, "Beta_T", BETA21$T$beta, rowNames = TRUE)
addWorksheet(wb, "Beta_U1")
writeData(wb, "Beta_U1", BETA21$U1$beta, rowNames = TRUE)
addWorksheet(wb, "Beta_U2")
writeData(wb, "Beta_U2", BETA21$U2$beta, rowNames = TRUE)
addWorksheet(wb, "Beta_V")
writeData(wb, "Beta_V", BETA21$V$beta, rowNames = TRUE)

# 4. Pairwise dissimilarity 1-C
addWorksheet(wb, "Pairwise_Dissimilarity1_T")
writeData(wb, "Pairwise_Dissimilarity1_T", BETA21$T$`1-C`, rowNames = TRUE)
addWorksheet(wb, "Pairwise_Dissimilarity1_U1")
writeData(wb, "Pairwise_Dissimilarity1_U1", BETA21$U1$`1-C`, rowNames = TRUE)
addWorksheet(wb, "Pairwise_Dissimilarity1_U2")
writeData(wb, "Pairwise_Dissimilarity1_U2", BETA21$U2$`1-C`, rowNames = TRUE)
addWorksheet(wb, "Pairwise_Dissimilarity1_V")
writeData(wb, "Pairwise_Dissimilarity1_V", BETA21$V$`1-C`, rowNames = TRUE)

# 5. Pairwise dissimilarity 1-U
addWorksheet(wb, "Pairwise_Dissimilarity2_T")
writeData(wb, "Pairwise_Dissimilarity2_T", BETA21$T$`1-U`, rowNames = TRUE)
addWorksheet(wb, "Pairwise_Dissimilarity2_U1")
writeData(wb, "Pairwise_Dissimilarity2_U1", BETA21$U1$`1-U`, rowNames = TRUE)
addWorksheet(wb, "Pairwise_Dissimilarity2_U2")
writeData(wb, "Pairwise_Dissimilarity2_U2", BETA21$U2$`1-U`, rowNames = TRUE)
addWorksheet(wb, "Pairwise_Dissimilarity2_V")
writeData(wb, "Pairwise_Dissimilarity2_V", BETA21$V$`1-U`, rowNames = TRUE)
# 6. Pairwise dissimilarity 1-V
addWorksheet(wb, "Pairwise_Dissimilarity3_T")
writeData(wb, "Pairwise_Dissimilarity3_T", BETA21$T$`1-V`, rowNames = TRUE)
addWorksheet(wb, "Pairwise_Dissimilarity3_U1")
writeData(wb, "Pairwise_Dissimilarity3_U1", BETA21$U1$`1-V`, rowNames = TRUE)
addWorksheet(wb, "Pairwise_Dissimilarity3_U2")
writeData(wb, "Pairwise_Dissimilarity3_U2", BETA21$U2$`1-V`, rowNames = TRUE)
addWorksheet(wb, "Pairwise_Dissimilarity3_V")
writeData(wb, "Pairwise_Dissimilarity3_V", BETA21$V$`1-V`, rowNames = TRUE)

# 7. Pairwise dissimilarity 1-S
addWorksheet(wb, "Pairwise_Dissimilarity4_T")
writeData(wb, "Pairwise_Dissimilarity4_T", BETA21$T$`1-S`, rowNames = TRUE)
addWorksheet(wb, "Pairwise_Dissimilarity4_U1")
writeData(wb, "Pairwise_Dissimilarity4_U1", BETA21$U1$`1-S`, rowNames = TRUE)
addWorksheet(wb, "Pairwise_Dissimilarity4_U2")
writeData(wb, "Pairwise_Dissimilarity4_U2", BETA21$U2$`1-S`, rowNames = TRUE)
addWorksheet(wb, "Pairwise_Dissimilarity4_V")
writeData(wb, "Pairwise_Dissimilarity4_V", BETA21$V$`1-S`, rowNames = TRUE)

# Save the workbook
saveWorkbook(wb, "S3.xlsx", overwrite = TRUE)

beta22<-betaf_chao(BETA21)  ### betaf_chao is a modification for the ggiNEXTbeta3D function
beta22
#ggsave(file="Figs/Fig_4.tiff", plot = beta22, width = 13, height = 9, dpi = 600, compression = "lzw")
####################################################
########################################################
### Taxonomic composition ###############################
#######################################################
# Read the data
data <- read.csv("Data/D1.csv", header = TRUE)

# Separate locality and rock unit
data <- data %>%
  separate(Locality, into = c("Locality", "RockUnit"), sep = " ", remove = FALSE)

# Create species matrix
species_matrix <- data %>% 
  dplyr::select(-Locality, -RockUnit, -`Locality`) %>%
  as.matrix()

# Adding row names as Rock Unit
rownames(species_matrix) <- data$RockUnit

# PERMANOVA using Bray-Curtis
dist_matrix <- vegdist(species_matrix, method = "bray")
set.seed(42)
permanova_result <- adonis2(dist_matrix ~ RockUnit, data = data, permutations = 999)

# Adjusted p-values
adjusted_p <- p.adjust(permanova_result$`Pr(>F)`, method = "bonferroni")

# SIMPER Analysis
set.seed(42)
simper_result <- simper(species_matrix, group = data$RockUnit)
simper_summary <- summary(simper_result)

# Indicator Species Analysis
set.seed(42)
indval_result <- multipatt(species_matrix, data$RockUnit, control = how(nperm = 999))
indval_summary <- summary(indval_result, indvalcomp = TRUE)

# Create Word Document
doc <- read_docx()

# Adding PERMANOVA results
doc <- doc %>%
  body_add_par("1. PERMANOVA Analysis", style = "heading 1") %>%
  body_add_par("Bray-Curtis dissimilarity was used to test differences in species composition among rock units.")

# PERMANOVA table
perm_table <- as.data.frame(permanova_result)
perm_table$`Adjusted Pr(>F)` <- adjusted_p
doc <- body_add_flextable(doc, flextable(perm_table))

# Adding SIMPER summary
doc <- doc %>%
  body_add_par("2. SIMPER Analysis", style = "heading 1") %>%
  body_add_par("Taxa contributing most to dissimilarities:")

# Adding SIMPER results for all comparisons
if (length(simper_summary) > 0) {
  doc <- doc %>%
    body_add_par("SIMPER results for all group comparisons:", style = "heading 2")
  
  # Loop through each comparison
  for (comparison_name in names(simper_summary)) {
    sim_df <- as.data.frame(simper_summary[[comparison_name]])
    
    # Order by highest average contribution and take top 3
    top_taxa <- head(sim_df[order(-sim_df$average), ], 3)
    
    # Adding comparison title and table
    doc <- doc %>%
      body_add_par(paste("Comparison:", comparison_name), style = "heading 3") %>%
      body_add_flextable(flextable(top_taxa))
  }
}


# Add Indicator Species Analysis
doc <- doc %>%
  body_add_par("3. Indicator Species Analysis", style = "heading 1") %>%
  body_add_par("Species significantly associated with specific rock units:")

# Extract and add indicator species table
ind_df <- as.data.frame(indval_result$sign)
doc <- body_add_flextable(doc, flextable(ind_df))

# Save Word Document
print(doc, target = "S4.docx")
##############################################################################
##UPGMAs FOR FAMILY/SUBFAMILY LEVEL FOR SAMPLES ##################################
###############################################################################
########## Faunal Similarity Analysis to differentiate between localities #########
# Load our dataset
#Marine family-tribe data (Binary)
set.seed(42)
D2 <- read.csv("Data/D2.csv",row.names = 1)
data_t <- t(D2)
df <- scale(D2)
# Sorensen (Bray-Curtis) dissimilarity index (presence-absence, binary)
sorensen_index <- vegdist(D2, method = "bray", binary = T)
#Sorensen Clustering:
hclust_sorensen <- hclust(sorensen_index, method = "average")
fviz_dend(hclust_sorensen, rect = TRUE, show_labels = TRUE)
hclust_sorensen$height
#1Calculate Cophenetic Correlation Coefficient
#Sorensen Cophenetic Correlation:
cophenetic_sorensen <- cophenetic(hclust_sorensen)
cor_sorensen <- cor(cophenetic_sorensen, sorensen_index)
cat("Cophenetic Correlation (Sorensen): ", cor_sorensen, "\n")
#2Bootstrap Resampling for Cluster Stability
bootstrap_jaccard <- pvclust(data_t, method.hclust = "average", method.dist = function(x)vegan::vegdist(t(x), "bray",binary = T), nboot = 1000, parallel = TRUE)
plot(bootstrap_jaccard, main = "",sub = "",
     ylab="Bray-Curtis dissimilarity",xlab="UPGMA Ccc=0.86",print.num=F, print.pv="bp")
pvrect(bootstrap_jaccard, alpha = 0.95)  # Highlight significant clusters

#Cut tree into 4 groups
grp <- cutree(hclust_sorensen, k = 1)
head(grp, n = 4)
# Get the names for the members of cluster 1
rownames(df)[grp == 1]
# Cut tree into groups
grp <- cutree(hclust_sorensen, k = 1)
head(grp, n = 4)

# Get the names for the members of cluster 1
rownames(df)[grp == 1]

# Generate the dendrogram 8
cluster_plot2 <- fviz_dend(
  hclust_sorensen, 
  k = 1, 
  cex = 0.8, 
  horiz = TRUE,
  k_colors = c("black", "red", "green", "orange", "blue", "brown"),
  rect = FALSE, 
  rect_border = "jco", 
  rect_fill = TRUE
)

# Customize the plot with titles and theme adjustments
cluster_plot2 <- cluster_plot2 + 
  ggtitle("Ccc = 0.86") +  # Removed duplicate ggtitle() call
  ylab("Bray-Curtis dissimilarity") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),  # Larger title
    plot.subtitle = element_text(hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(margin = margin(r = 10)))  # Adjust y-axis spacing if needed

# Display the plot
cluster_plot2
#ggsave("Figs/Fig_4.svg", plot = cluster_plot2, width = 9, height = 8, dpi = 600)
##############################################################
#UPGMAs FOR FAMILY/Subfamily LEVEL FOR rock units
########################################################################
##########Faunal Similarity Analysis to differentiate between ROCK UNITS
# Load our dataset
D3 <- read.csv("Data/D3.csv", row.names = 1)
data3_t <- t(D3)
df <- scale(D3)

# Sorensen (Bray-Curtis) dissimilarity index (presence-absence, binary)
sorensen_index <- vegdist(D3, method = "bray", binary = TRUE)

# Sorensen Clustering
hclust_sorensen <- hclust(sorensen_index, method = "average")
fviz_dend(hclust_sorensen, rect = TRUE, show_labels = TRUE)
cophenetic_sorensen <- cophenetic(hclust_sorensen)
cor_sorensen <- cor(cophenetic_sorensen, sorensen_index)
cat("Cophenetic Correlation (Sorensen): ", cor_sorensen, "\n")
hclust_sorensen$height
# Bootstrap clustering with Bray-Curtis dissimilarity
bootstrap_bray <- pvclust(data3_t, method.hclust = "average",  function(x)vegan::vegdist(t(x), "bray",binary = T), nboot = 1000)
plot(bootstrap_bray, main = "Dendrogram\nAll As-Sahabi mammals", sub = "",
     ylab = "Bray-Curtis dissimilarity ", xlab = "UPGMA Ccc=0.97", print.pv = "bp", print.num = FALSE, col.pv = c(bp = 2, edge = 0))
pvrect(bootstrap_bray, alpha = 0.95)  # Highlight significant clusters
#Cut tree into 4 groups
grp2 <- cutree(hclust_sorensen, k = 3)
head(grp2, n = 4)
# Get the names for the members of cluster 1
rownames(df)[grp2 == 3]
# Cut in 4 groups and color by groups
fviz_dend(hclust_sorensen, k = 3, # Cut in four groups
          cex = 1.3, # label size
          k_colors = c( "black","red","green","orange","blue","brown")
          ,         color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)
cluster_plot2<-fviz_dend(hclust_sorensen, k = 3, k_colors = c("lightgreen",'darkgreen','blue'), 
                         rect = F, rect_border = "gray", rect_fill = F, 
                         lwd = 1.2, label_cols = "black", repel = TRUE,
                         horiz = T,
                         cex = 1.3)
cluster_plot2<-cluster_plot2 + 
  ggtitle("Ccc = 0.97",subtitle = "") +  # Custom main title
  #xlab("Custom X-axis Title") +           # Custom X-axis title
  ylab("Bray-Curtis dissimilarity") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),  # Larger title
    plot.subtitle = element_text(hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(margin = margin(r = 10)))
cluster_plot2
#ggsave("Figs/Fig_5.svg", plot = cluster_plot2, width = 10, height = 5, dpi = 500)

#######################################################
## Family/subfamily turnover #######
# Create the data frame
data <- read.csv("Data/D3.csv")

# Convert to presence/absence (1 = present, 0 = absent)
data_pa <- data
data_pa[, -1] <- ifelse(data_pa[, -1] > 0, 1, 0)
rownames(data_pa) <- data_pa$Locality
data_pa <- data_pa[, -1]  

# Calculate Jaccard dissimilarity
dist_bray <- vegdist(data_pa, method = "bray", binary = TRUE)
cat("\nBray Dissimilarity Matrix:\n")
print(as.matrix(dist_bray))

# Visualize as heatmap
dist_matrix <- as.matrix(dist_bray)
dist_melt <- melt(dist_matrix)
colnames(dist_melt) <- c("Unit1", "Unit2", "Dissimilarity")

heatmap_plot <- ggplot(dist_melt, aes(Unit1, Unit2, fill = Dissimilarity)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Dissimilarity, 2)), color = "black", size = 4) +
  scale_fill_gradient(low = "white", high = "red", limits = c(0, 1)) +
  labs(title = "Taxa Turnover (Bray Dissimilarity)",
       x = "Rock Unit", y = "Rock Unit") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"))

# Cluster analysis
cluster_plot <- function() {
  plot(hclust(dist_bray, method = "average"),
       main = "Cluster Dendrogram of Rock Units",
       xlab = "Units", sub = "",
       hang = -1, cex = 1.2)
  rect.hclust(hclust(dist_bray), k = 3, border = 2:4)
}

# Richness analysis
richness <- data.frame(
  Unit = rownames(data_pa),
  Richness = rowSums(data_pa),
  Total_Individuals = rowSums(data[, -1])
)

# Output results
cat("\nTaxon Richness per Unit:\n")
print(richness)

# Generate plots
print(heatmap_plot)
cluster_plot()

# Optional: Turnover between consecutive units
cat("\nTurnover between consecutive units:\n")
units <- rownames(data_pa)
for(i in 1:(length(units)-1)) {
  turnover <- vegdist(data_pa[c(i, i+1), ], method = "bray")
  cat(paste0(units[i], " -> ", units[i+1], ": ", round(turnover, 3), "\n"))
}


# 1. Load and prepare data (corrected version)
data <- read.csv("Data/D3.csv", row.names = 1)
data <- as.matrix(data)  # Convert to numeric matrix

# 2. Convert to incidence (presence-absence) data
data_pa <- ifelse(data > 0, 1, 0)

# 3. Calculate Sorensen-based beta diversity partitioning
sorensen_part <- beta.pair(data_pa, index.family = "sorensen")

# 4. Extract components and create results table
results <- data.frame(
  Comparison = c("T-U1", "T-U2", "T-V", "U1-U2", "U1-V", "U2-V"),
  
  Turnover = c(as.matrix(sorensen_part$beta.sim)[1,2],
               as.matrix(sorensen_part$beta.sim)[1,3],
               as.matrix(sorensen_part$beta.sim)[1,4],
               as.matrix(sorensen_part$beta.sim)[2,3],
               as.matrix(sorensen_part$beta.sim)[2,4],
               as.matrix(sorensen_part$beta.sim)[3,4]),
  
  Nestedness = c(as.matrix(sorensen_part$beta.sne)[1,2],
                 as.matrix(sorensen_part$beta.sne)[1,3],
                 as.matrix(sorensen_part$beta.sne)[1,4],
                 as.matrix(sorensen_part$beta.sne)[2,3],
                 as.matrix(sorensen_part$beta.sne)[2,4],
                 as.matrix(sorensen_part$beta.sne)[3,4]),
  
  Total = c(as.matrix(sorensen_part$beta.sor)[1,2],
            as.matrix(sorensen_part$beta.sor)[1,3],
            as.matrix(sorensen_part$beta.sor)[1,4],
            as.matrix(sorensen_part$beta.sor)[2,3],
            as.matrix(sorensen_part$beta.sor)[2,4],
            as.matrix(sorensen_part$beta.sor)[3,4])
)

# 5. Plot selected comparisons (T-U1, U1-U2, U2-V)
#selected <- results[results$Comparison %in% c("T-U1", "U1-U2", "U2-V"), ]

# Convert data to long format for proper grouping
results_long <- results %>%
  pivot_longer(cols = c(Turnover, Nestedness, Total),
               names_to = "Component",
               values_to = "Value") %>%
  mutate(Component = factor(Component, levels = c("Total", "Nestedness", "Turnover")))

# Create clustered bar plot
turnover <- ggplot(results_long, 
                   aes(x = Comparison, y = Value, fill = Component)) +
  geom_col(position = "stack",          # Stack components instead of dodging
           width = 0.7,                # Bar width
           color = "white", size = 0.2) +  # Optional borders
  
  scale_fill_manual(values = c("Total" = "gray70", 
                               "Nestedness" = "#56B4E9",
                               "Turnover" = "#E69F00"),
                    name = "Component") +
  
  labs(x = "Transition",
       y = "Beta Diversity") +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12),
        axis.text.y = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 14),
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        panel.grid.major.x = element_blank())
# Display the plot
print(turnover)
#ggsave("Figs/Fig_6.tiff", plot = turnover, width = 9, height = 7, dpi = 600)
#####################################################################
######## Taxonomic Structure #####################################
#####################################################################
################
# 4. Visualization - NMDS plot
# Read and prepare data
data <- read.csv("Data/D1.csv")

# Convert to community matrix (samples x taxa)
comm_matrix <- data %>% 
  column_to_rownames("Locality")

# Extract rock units from locality names (last character(s) after space)
rock_units <- gsub(".*\\s([A-Za-z0-9]+)$", "\\1", rownames(comm_matrix))
locality_ids <- gsub("(P[0-9]+).*", "\\1", rownames(comm_matrix))

# Data transformation and distance calculation
comm_hell <- decostand(comm_matrix, "hellinger")  # Hellinger transformation
bray_dist <- vegdist(comm_hell, "bray")          # Bray-Curtis dissimilarity

# NMDS analysis
set.seed(123)
nmds <- metaMDS(bray_dist, 
                autotransform = FALSE,  
                k = 3,
                trymax = 100,
                wascores = TRUE) 


# Check stress value
cat("NMDS stress value:", nmds$stress, "\n")
if(nmds$stress > 0.2) {
  warning("Stress value > 0.2 - consider increasing k or checking data quality")
}

# Calculate species scores
species_scores <- wascores(nmds$points, comm_hell)
species_scores <- as.data.frame(species_scores)
colnames(species_scores) <- c("NMDS1", "NMDS2")
species_scores$Taxa <- rownames(species_scores)

# Filter important species (those with high loadings)
loading_threshold <- 0.1  # Adjust this threshold as needed
important_species <- species_scores[
  abs(species_scores$NMDS1) > loading_threshold | 
    abs(species_scores$NMDS2) > loading_threshold, 
]


nmds_plot <- data.frame(NMDS1 = nmds$points[,1], 
                        NMDS2 = nmds$points[,2],
                        NMDS3 = nmds$points[,3],
                        Unit = rock_units,
                        Locality = locality_ids)


# Create the plot
nmds_plot$Unit <- factor(nmds_plot$Unit, levels = c("T", "U1", "U2", "V"))

base_plot <- ggplot(nmds_plot, aes(x = NMDS1, y = NMDS2, color = Unit, fill = Unit, shape = Unit)) +
  geom_point(size = 3) +
  geom_text_repel(
    aes(label = Locality),
    size = 3,
    show.legend = FALSE,
    box.padding = 0.5,
    max.overlaps = Inf,
    min.segment.length = 0.1,
    seed = 42
  ) +
  
  # Add species loadings as arrows
  geom_segment(
    data = important_species,
    aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
    arrow = arrow(length = unit(0.2, "cm")),
    color = "red",
    inherit.aes = FALSE
  ) +
  
  # Add species names
  geom_text_repel(
    data = important_species,
    aes(x = NMDS1, y = NMDS2, label = Taxa),
    color = "red",
    size = 3,
    inherit.aes = FALSE,
    max.overlaps = 10
  ) +
  
  # Add ellipses or convex hulls based on group size
  {
    unit_counts <- table(nmds_plot$Unit)
    layers <- lapply(names(unit_counts), function(unit) {
      n <- unit_counts[unit]
      if(n >= 4) {
        stat_ellipse(
          data = subset(nmds_plot, Unit == unit),
          level = 0.95,
          geom = "polygon",
          alpha = 0.2,
          show.legend = FALSE
        )
      } else if(n == 3) {
        geom_polygon(
          data = subset(nmds_plot, Unit == unit) %>%
            slice(chull(NMDS1, NMDS2)),
          alpha = 0.2, 
          show.legend = FALSE
        )
      } else if(n == 2) {
        geom_line(
          data = subset(nmds_plot, Unit == unit),
          linetype = "dashed",
          show.legend = FALSE
        )
      }
    })
    layers
  } +
  
  # Aesthetic settings
  scale_shape_manual(values = c(24, 21, 22, 23)) +
  scale_fill_manual(values = c('blue', 'deepskyblue2', 'darkgreen', 'green')) +
  scale_color_manual(values = c('blue', 'deepskyblue2', 'darkgreen', 'green')) +
  theme_minimal() +
  labs(subtitle = paste("Stress =", round(nmds$stress, 3)),
       color="Rock unit",fill="Rock unit", shape="Rock unit") +
  theme(legend.position = "bottom")+
  theme(plot.subtitle = element_text(hjust = 0.5,size = 16,face = "bold"),
        axis.text.x = element_text(angle = 0, hjust = 1,size = 12),
        axis.title.x = element_text(size = 14,face = "bold"),
        axis.text.y = element_text(size = 12,face = "bold"),
        axis.title.y = element_text(size = 14,face = "bold"),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        legend.text = element_text(size = 12))

# Display the plot
print(base_plot)

# Save the plot
#ggsave("Figs/Fig_7.tiff", plot = base_plot, width = 10, height = 7, dpi = 500)
###############################
#### RANK ANALYSIS ##############
SM1_1<-read.csv("Data/SM1.csv")
SM1_1$taxa<-as.factor(SM1_1$taxa)
######
#separately 
ggplot(SM1_1, aes(x = Unit, y = abndance, fill = Unit)) +
  geom_boxplot(outlier.shape = NA) +
  facet_wrap(~taxa, scales = "free_y") +
  scale_fill_manual(values = c('blue',"deepskyblue2",'darkgreen',"lightgreen"))+
  theme_minimal() +
  labs(title = "Taxa Rank Abundance per Rock Unit",
       y = "Relative Abundance",
       x = "Rock unit")

marine_rank<-ggplot(SM1_1, aes(x = taxa, y = abndance,fill = Unit)) +
  geom_boxplot()+                # Draw lines
  scale_fill_manual(values = c('blue',"deepskyblue2",'darkgreen',"lightgreen"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5)) + # Rotate x-axis labels for better readability
  labs(title="Marine & terrestrial mammals",
       x = "Taxa", y = "Standarized abundance Ranks",
       fill="Rock Units")

#test for normal distribution
shapiro.test(SM1_1$abndance) #this result means that our data are not normally distributed
#test for homogeneity
bartlett.test(SM1_1$abndance~SM1_1$taxa) #this result means that our data are not homogeneous 

# Box plot with statistical significance annotation
marine_rank_stat <- ggplot(SM1_1, aes(x = taxa, y = abndance, fill = Unit)) +
  geom_boxplot(outlier.colour = "red") +
  scale_fill_manual(values = c('blue', "deepskyblue2", 'darkgreen', "lightgreen")) +
  theme_classic() +
  scale_y_continuous(breaks=seq(0,1,0.25))+
  theme(plot.title = element_text(hjust = 1,size = 16,face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1,size = 12),
        axis.title.x = element_text(size = 14,face = "bold"),
        axis.text.y = element_text(size = 12,face = "bold"),
        axis.title.y = element_text(size = 14,face = "bold"),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5)) +
  labs(title = "",
       x = "Taxa", y = "Standardized Abundance Ranks",
       fill = "Rock Units") +
  stat_compare_means(method = "kruskal.test", label = "p.signif", label.y = 1.1,size=5)
marine_rank_stat<-marine_rank_stat + 
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 1.07, ymax = 1.2, fill = "lightgray", alpha = 0.3)

print(marine_rank_stat)
#ggsave("Figs/Fig_8.tiff", plot = marine_rank_stat, width = 12, height = 7, dpi = 600, compression = "lzw")

# Apply Dunn's test for post-hoc pairwise comparisons
dunn_test_results1 <- SM1_1 %>%
  group_by(taxa) %>%
  summarise(dunn_test = list(dunn.test(abndance, Unit, method="bonferroni")))
dunn_test_results1$taxa
print(dunn_test_results1)
dunn_test_results1$dunn_test
marine_p_value<-dunn_test_results1$dunn_test

p_value1<-marine_p_value %>% 
  as.data.frame()
p_value1<-p_value1 %>% 
  rename(anth=P.adjusted,
         bovi=P.adjusted.1,
         carn=P.adjusted.2,
         ceta=P.adjusted.3,
         equi=P.adjusted.4,
         gira=P.adjusted.5,
         hipp=P.adjusted.6,
         pirm=P.adjusted.7,
         prob=P.adjusted.8,
         rhin=P.adjusted.9 ,
         sire=P.adjusted.10 ,
         suid=P.adjusted.11)

p_value1<-p_value1 %>%
  dplyr::select(-chi2,-Z,-P,-chi2.1,-Z.1,-P.1,-comparisons.1,-chi2.2,-Z.2,-P.2,-comparisons.2,-chi2.3,-Z.3,-P.3,-comparisons.3,
                -chi2.4,-Z.4,-P.4,-comparisons.4,-chi2.5,-Z.5,-P.5,-comparisons.5,-chi2.6,-Z.6,-P.6,-comparisons.6,-chi2.7,-Z.7,-P.7,-comparisons.7,
                -chi2.8,-Z.8,-P.8,-comparisons.8,-chi2.9,-Z.9,-P.9,-comparisons.9,-chi2.10,-Z.10,-P.10,-comparisons.10,-chi2.11,-Z.11,-P.11,-comparisons.11)
write.csv(p_value1,"Data/p_value1.csv")
# Create the data frame manually based on your table
# Reshape the data into long format
data_melt <- melt(p_value1, id.vars = "comparisons")
# Create the heatmap
M1<-ggplot(data_melt, aes(x = variable, y = comparisons, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "red", high = "green", mid = "green", 
                       midpoint = 0.5, limit = c(0, max(data_melt$value)), 
                       space = "Lab", name="p-value") +
  geom_text(aes(label = round(value, 3)), color = "black", size = 3) +
  labs(x = "Taxa", y = "Assemblages pairs",
       fill="p-value") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45,hjust = 1,size = 11),
        axis.text.y = element_text(angle = 90,size = 11,face = "bold",hjust = 0.5),
        axis.title.y = element_text(size = 14),
        plot.background = element_rect(colour = "black")) +
  geom_tile(data = subset(data_melt, value < 0.05), 
            aes(x = variable, y = comparisons), 
            color = "black", fill = "red", alpha = 0.5) +
  ggtitle("Dunn's test")

M1
#ggsave("Figs/S6.tiff", plot = M1, width = 9, height = 6, dpi = 600, compression = "lzw")
##################################################################
####### Spatial distribution map of Samples
# Load and modify data
fossils <- read.csv("Data/D1cor.csv") %>%
  mutate(Locality = str_extract(Locality, "^[^ ]+"))  # Keep only prefix (e.g., "P10")

#  Rotate coordinates (90° clockwise: swap X/Y and invert new X)
fossils_rotated <- fossils %>%
  mutate(
    Rotated_Lon = -Latitude,  # New X = -original Y
    Rotated_Lat = Longitude   # New Y = original X
  )

#  Convert to SF object (using rotated coordinates)
fossils_sf_rotated <- st_as_sf(
  fossils_rotated,
  coords = c("Rotated_Lon", "Rotated_Lat"),  # Use rotated coordinates
  crs = 4326
)

# Read basemap shapefile
basemap_shape <- st_read("Data/Study area.shp")

#  Rotate the basemap similarly
basemap_shape_rotated <- basemap_shape %>%
  st_coordinates() %>%
  as_tibble() %>%
  mutate(
    Rotated_X = -Y,
    Rotated_Y = X
  ) %>%
  st_as_sf(coords = c("Rotated_X", "Rotated_Y"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

# Plot (rotated 90°)
MAP<-ggplot() +
  geom_sf(data = basemap_shape_rotated, fill = "white", color = "red", size = 3) +
  geom_point(
    data = habitat_unique_rotated,
    aes(x = Rotated_Lon, y = Rotated_Lat, shape = Unit, fill = Unit),
    size = 3
  ) +
  scale_shape_manual(values = c(24, 21, 22, 23)) +
  scale_fill_manual(values = c('blue', "deepskyblue2", 'darkgreen', "lightgreen")) +
  geom_text_repel(
    data = fossils_rotated,
    aes(x = Rotated_Lon, y = Rotated_Lat, label = Locality),
    size = 3,
    min.segment.length = 0,
    max.overlaps = 100
  ) +
  labs(#title = "As-Sahabi Fossil Localities Map"
    ,color="Rock unit",shape="Rock unit",fill="Rock unit",
    x="Latitude",
    y="Longitude") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 0,hjust = 1,size = 10),
        axis.text.y = element_text(angle = 90,size = 10,face = "bold",hjust = 0.5),
        axis.title = element_text(size = 14),
        legend.position = "bottom",
        legend.text = element_text(size = 12))+
  annotation_scale(location = "bl") +
  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    rotation = 90 
  )
MAP<-MAP+
  theme(panel.background = element_rect(fill = "white", color = NA))
MAP


#ggsave("Figs/S5.svg", plot = MAP, width = 10, height = 8, dpi = 600)

