# Load necessary packages
library(ggplot2)
library(reshape2)

# Define the correlation matrix
corr<-data.frame(
  TY = c(1.00, -.216, .984, -.985, .962, -.218, .060, .972, .899, .777, -.590),
  PH = c(-.216, 1.00, -.332, .245, -.310, .967, .960, -.032, -.501, -.756, .912),
  STD = c(.428, -.332, 1.00, -.402, .350, .733, .910, .571, .069, -.192, .473),
  LFN = c(.984, -.983, .987, -.340, -.067, .929, .890, .850, -.670, NA, NA),
  DTF = c(-.985, .987, .215, -1.00, -.016, -.971, -.852, -.816, .612, NA, NA),
  BN = c(.962, -.340, .350, -.016, 1.00, -.293, -.059, .926, .821, .851, -.645),
  FRTL = c(-.218, .060, .733, -.971, -.293, 1.00, -.003, -.515, -.707, .867, NA),
  FRTD = c(.060, .972, .910, -.852, -.059, -.003, 1.00, .232, -.243, .565, .767),
  FNP = c(.972, .899, .571, -.816, .926, -.515, .232, 1.00, .664, -.437, NA),
  ST = c(.899, .777, .069, .612, .821, -.707, -.243, .664, 1.00, -.788, NA),
  LI = c(.777, -.590, .473, NA, .851, .867, .565, -.437, -.788, 1.00, NA)
)
rownames(corr)<-colnames(corr)<- c("TY", "PH", "STD", "LFN", "DTF", "BN", "FRTL", "FRTD", "FNP", "ST", "LI")

# Convert the correlation matrix to a data frame
corr_df <- as.data.frame(corr)

# Add row names as a column
corr_df$variable1 <- rownames(corr_df)

# Melt the data frame, setting the row names column as the id variable
melted_corr <- melt(corr_df, id.vars = "variable1", variable.name = "variable2", value.name = "value")

# Plot the heatmap
ggplot(melted_corr, aes(x=variable1, y=variable2, fill=value)) + 
  geom_tile() + 
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Pearson\nCorrelation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("correlation_matrix.emf", plot = last_plot(), device = "emf")


#here i discribe the process of making the circular correlation heat map in this 
discribtions.



#to creat the circular heat map of the correlation marix 
library(ggplot2)
library(reshape2)

# Define the correlation matrix
corr<-data.frame(
  TY = c(1.00, -.216, .984, -.985, .962, -.218, .060, .972, .899, .777, -.590),
  PH = c(-.216, 1.00, -.332, .245, -.310, .967, .960, -.032, -.501, -.756, .912),
  STD = c(.428, -.332, 1.00, -.402, .350, .733, .910, .571, .069, -.192, .473),
  LFN = c(.984, -.983, .987, -.340, -.067, .929, .890, .850, -.670, NA, NA),
  DTF = c(-.985, .987, .215, -1.00, -.016, -.971, -.852, -.816, .612, NA, NA),
  BN = c(.962, -.340, .350, -.016, 1.00, -.293, -.059, .926, .821, .851, -.645),
  FRTL = c(-.218, .060, .733, -.971, -.293, 1.00, -.003, -.515, -.707, .867, NA),
  FRTD = c(.060, .972, .910, -.852, -.059, -.003, 1.00, .232, -.243, .565, .767),
  FNP = c(.972, .899, .571, -.816, .926, -.515, .232, 1.00, .664, -.437, NA),
  ST = c(.899, .777, .069, .612, .821, -.707, -.243, .664, 1.00, -.788, NA),
  LI = c(.777, -.590, .473, NA, .851, .867, .565, -.437, -.788, 1.00, NA)
)
rownames(corr)<-colnames(corr)<- c("TY", "PH", "STD", "LFN", "DTF", "BN", "FRTL", "FRTD", "FNP", "ST", "LI")

# Convert the correlation matrix to a data frame
corr_df <- as.data.frame(corr)

# Add row names as a column
corr_df$variable1 <- rownames(corr_df)

# Melt the data frame, setting the row names column as the id variable
melted_corr <- melt(corr_df, id.vars = "variable1", variable.name = "variable2", value.name = "value")

# Define the plot
ggplot(melted_corr, aes(x=variable1, y=variable2, fill=value)) + 
  geom_tile() + 
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Pearson\nCorrelation") +
  coord_polar() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

