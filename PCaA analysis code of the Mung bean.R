# Step 1: Load the data (transposed)
data <- data.frame(
  MR_Sr = c(0.7623202, 0.8085587, 0.8931595, 0.8150164, 0.8189528, 0.8521453, 0.8838244, 0.9764501, 0.7582572, 1.0075665, 1.012291, 0.7163866, 0.8269511, 0.982057, 1.1292833, 0.8305051),
  NSR_S = c(8.133333, 8.4, 10, 8.8, 7.4, 7.933333, 8.2, 8.466667, 7.133333, 9.2, 11, 8.733333, 11.4, 7.933333, 10.466667, 7.533333),
  AGWP = c(2.0266667, 2.36, 1.96, 1.6, 1.66, 1.35, 1.71, 1.72, 1.6, 1.06, 1.14, 1.8, 1.26, 1.006, 0.88, 1.36),
  Plant_Height = c(1.2, 1.4, 1.8, 1.3, 1.1, 1.9, 1.7, 1.5, 1.6, 1.2, 1.9, 1.3, 1.1, 1.4, 1.7, 1.5),
  Leaf_color_chart = c(10, 12, 15, 11, 9, 16, 14, 13, 12, 10, 16, 11, 9, 12, 15, 13),
  New_Variable1 = c(0.5, 0.9, 0.7, 0.3, 0.6, 0.8, 0.4, 0.7, 0.9, 0.2, 0.6, 0.3, 0.5, 0.7, 0.8, 0.4),
  New_Variable2 = c(5, 8, 6, 4, 7, 9, 3, 6, 8, 2, 7, 4, 5, 6, 9, 3),
  row.names = c("PANT MUNG 2", "KPS-1", "VC3890A", "VC6848", "VC6369", "PRATIGYA", "SAMRAT", "VC6173C", "VC3960A-88", "VC3964A-2", "PRATAP", "VC6173C_2", "KPS-1_2", "VC3964A-2_2", "VC3960A-88_2", "PRATIGYA_2")
)

# Step 2: Install and load required packages
library(ggplot2)
library("FactoMineR")
library("factoextra")

# Step 3: Perform PCA
TazData <- PCA(data, graph = FALSE)

# Step 4: Create a scree plot
fviz_eig(TazData, addlabels = TRUE, ylim = c(0, 50))

# Step 5: Create a PCA variable plot
fviz_contrib(TazData, choice = "var", axes = 1:2,
             col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
)

# Step 6: Create a PCA individual plot
fviz_contrib(TazData, choice = "ind", axes = 1:2,
             col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)

# Step 7: Create a biplot
fviz_pca_biplot(TazData, repel = TRUE, 
                col.var = "red",
                col.ind = "black"
)

# Step 8: Create a biplot with ellipses
fviz_pca_biplot(TazData, 
                col.ind = TazData$ind$Location, 
                addEllipses = TRUE, label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "Location"
)


# Step 1: Load the data (transposed)
data <- data.frame(
  MR_Sr = c(0.7623202, 0.8085587, 0.8931595, 0.8150164, 0.8189528, 0.8521453, 0.8838244, 0.9764501, 0.7582572, 1.0075665, 1.012291, 0.7163866, 0.8269511, 0.982057, 1.1292833, 0.8305051),
  NSR_S = c(8.133333, 8.4, 10, 8.8, 7.4, 7.933333, 8.2, 8.466667, 7.133333, 9.2, 11, 8.733333, 11.4, 7.933333, 10.466667, 7.533333),
  AGWP = c(2.0266667, 2.36, 1.96, 1.6, 1.66, 1.35, 1.71, 1.72, 1.6, 1.06, 1.14, 1.8, 1.26, 1.006, 0.88, 1.36),
  Plant_Height = c(1.2, 1.4, 1.8, 1.3, 1.1, 1.9, 1.7, 1.5, 1.6, 1.2, 1.9, 1.3, 1.1, 1.4, 1.7, 1.5),
  Leaf_color_chart = c(10, 12, 15, 11, 9, 16, 14, 13, 12, 10, 16, 11, 9, 12, 15, 13),
  New_Variable1 = c(0.5, 0.9, 0.7, 0.3, 0.6, 0.8, 0.4, 0.7, 0.9, 0.2, 0.6, 0.3, 0.5, 0.7, 0.8, 0.4),
  New_Variable2 = c(5, 8, 6, 4, 7, 9, 3, 6, 8, 2, 7, 4, 5, 6, 9, 3),
  row.names = c("PANT MUNG 2", "KPS-1", "VC3890A", "VC6848", "VC6369", "PRATIGYA", "SAMRAT", "VC6173C", "VC3960A-88", "VC3964A-2", "PRATAP", "VC6173C_2", "KPS-1_2", "VC3964A-2_2", "VC3960A-88_2", "PRATIGYA_2")
)

# Step 2: Install and load required packages
library(ggplot2)
library("FactoMineR")
library("factoextra")

# Step 3: Perform PCA
TazData <- PCA(data, graph = FALSE)

# Step 4: Create a scree plot
fviz_eig(TazData, addlabels = TRUE, ylim = c(0, 50))

# Step 5: Create a PCA variable plot
fviz_contrib(TazData, choice = "var", axes = 1:2, col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE) # <-- corrected line

# Step 6: Create a PCA individual plot
fviz_contrib(TazData, choice = "ind", axes = 1:2, col.ind = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE) # Avoid text overlapping (slow if many points)

# Step 7: Create a biplot
fviz_pca_biplot(TazData, repel = TRUE, col.var = "red", col.ind = "black")

# Step 8: Create a biplot with ellipses
fviz_pca_biplot(TazData, col.ind = TazData$ind$Location, addEllipses = TRUE, label = "var", col.var = "black", repel = TRUE, legend.title = "Location")








