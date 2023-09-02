setwd("C:/Users/HP/OneDrive/Desktop/Mung bean analysis folder")
bikas<-read.csv("C:/Users/HP/OneDrive/Desktop/Mung bean analysis folder/RSratio.csv",header = TRUE)
attach(bikas)
print(bikas)
head(bikas)
#for analysi of the correlation study of the 3 
#scale variables of the mung bean
library(corrplot)
library(ggplot2)
#to create the research Design Plot  in R.
library(tidyverse)
library(magick)


bikas <- data.frame(Treatment = rep(c("A", "B", "C", "D"), 3),
                    Response = rnorm(12))

mydesign <- rcbd(4, 3)

print(mydesign)


bikas<-data.frame(MR.Sr, NSR.S, AGWP)
cor_matrix <- cor(bikas, method = "pearson")
bikas <- data.frame(MR.S = rnorm(16), NSR.S = rnorm(16), AGWP = rnorm(16))
cor_matrix <- cor(bikas)
library(Formula)
library(Hmisc)
bikas <- data.frame(
  MR.S = rnorm(16),
  NSR.S = rnorm(16),
  AGWP = rnorm(16)
)
cor_matrix <- cor(bikas)
require(ggcorrplot)
ggcorrplot(cor_matrix, type = "upper", lab = TRUE)






cor_matrix <- cor(bikas)
cor_matrix_pvals <- cor_pmat(bikas)$p
cor_matrix_pvals_adjusted <- p.adjust(cor_matrix_pvals, method = "holm")
corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45, p.mat = cor_matrix_pvals_adjusted, sig.level = 0.05)
ggplot(bikas, aes(x = MR.S, y = NSR.S)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  stat_cor(label.x.npc = 0.3, label.y.npc = 0.9, label.sep = "\n")
ggplot(bikas, aes(x = MR.S, y = AGWP)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  stat_cor(label.x.npc = 0.3, label.y.npc = 0.9, label.sep = "\n")
ggplot(bikas, aes(x = NSR.S, y = AGWP)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  stat_cor(label.x.npc = 0.3, label.y.npc = 0.9, label.sep = "\n")



ggplot(bikas, aes(x = Genotypes, y =  NSR.S)) +
  geom_point(color = "blue", size = 5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "Genotypes", y = " NSR.S", title = "Scatter Plot of Genotypes against Mean Number of secondary Roots hairs per seedling") +
  theme_bw() +
  theme(axis.text.x = element_text(face = "bold")) +
  scale_y_continuous(limits = c(5, 10), breaks = seq(5, 10))

  geom_abline(intercept = 0, slope = 1, color = "green", linetype = "dashed") +
scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10))



# Create a boxplot of AGWP by Genotypes with custom colors
  bikas$Genotypes <- factor(bikas$Genotypes)
  
ggplot(bikas, aes(x = Genotypes, y = AGWP)) +
  geom_boxplot(aes(fill = Genotypes, color = Genotypes), size = 0.6) +
  scale_fill_manual(values = c("#FF0000", "#00FF00", "#0000FF", "#800080", "#FFA500", "#FFFF00", "#00FFFF", "#FF00FF", "#808080", "#000000", "#FFFFFF", "#A52A2A", "#FFC0CB", "#808000", "#000080", "#008080")) +
  scale_color_manual(values = c("#FF0000", "#00FF00", "#0000FF", "#800080", "#FFA500", "#FFFF00", "#00FFFF", "#FF00FF", "#808080", "#000000", "#FFFFFF", "#A52A2A", "#FFC0CB", "#808000", "#000080", "#008080")) +
  labs(x = "Genotypes", y = "AGWP", title = "Box plot of Genotypes vs Below Ground White Portion(Hypocotyl color)") +
  theme_bw() +
  scale_y_continuous(limits = c(0.1, 2), breaks = seq(0.1, 2))

ggplot(na.omit(bikas), aes(x = Genotypes, y = AGWP)) +
  geom_boxplot(aes(fill = Genotypes, color = Genotypes), size = 0.6) +
  scale_fill_manual(values = c("#FF0000", "#00FF00", "#0000FF", "#800080", "#FFA500", "#FFFF00", "#00FFFF", "#FF00FF", "#808080", "#000000", "#FFFFFF", "#A52A2A", "#FFC0CB", "#808000", "#000080", "#008080")) +
  scale_color_manual(values = c("#FF0000", "#00FF00", "#0000FF", "#800080", "#FFA500", "#FFFF00", "#00FFFF", "#FF00FF", "#808080", "#000000", "#FFFFFF", "#A52A2A", "#FFC0CB", "#808000", "#000080", "#008080")) +
  labs(x = "Genotypes", y = "AGWP", title = "Box plot of Genotypes vs Below Ground White Portion(Hypocotyl color)") +
  theme_bw() +
  scale_y_continuous(limits = c(0.1, 2), breaks = seq(0.1, 2))

#violoin plot of the Code for the Genotypes to Number of secondary root hairs for seedling
ggplot(bikas, aes(x = Genotypes, y = NSR.S, color = Genotypes)) +
  geom_violin(size = 0.6, alpha = 0.8) +
  labs(x = "Genotypes", y = "NSR.S", title = "Violin Plot of Genotypes against Mean Number of secondary Roots per seedling") +
  theme_bw() +
  theme(axis.text.x = element_text(face = "bold")) +
  scale_y_continuous(limits = c(7, 10), breaks = seq(7, 10)) +
  scale_color_manual(values = c("#FF0000", "#00FF00", "#0000FF", "#800080", "#FFA500", "#FFFF00", "#00FFFF", "#FF00FF", "#808080", "#000000", "#FFFFFF", "#A52A2A", "#FFC0CB", "#808000", "#000080", "#008080")) +
  guides(color = guide_legend(title = "Genotypes"))
warning()









# scatterplot code of the Mung bean.


ggplot(bikas, aes(x = Genotypes, y = NSR.S, color = Genotypes)) +
  geom_point(size = 5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "Genotypes", y = "NSR.S", title = "Scatter Plot of 
       Genotypes against Mean Number of secondary Roots per seedling") +
  theme_bw() +
  theme(axis.text.x = element_text(face = "bold")) +
  scale_y_continuous(limits = c(5, 10), breaks = seq(5, 10)) +
  scale_color_manual(values = c("#FF0000", "#00FF00", "#0000FF", "#800080", "#FFA500", "#FFFF00", "#00FFFF", "#FF00FF", "#808080", "#000000", "#FFFFFF", "#A52A2A", "#FFC0CB", "#808000", "#000080", "#008080")) +
  guides(color = guide_legend(title = "Genotypes"))

#code for saving the file
ggsave("myplot.png", plot = last_plot(), device = "png", dpi = 300)
require(magick)
img <- image_read("C:/Users/HP/Downloads/20230512_084057.jpg")
info <- image_info(img)
info$width
info$height
img_png <- image_convert(img, "png")
img_gray <- image_convert(img, colorspace = "gray")
img_thresh <- image_threshold(img_gray, threshold = "60%", type = "black")
info <- image_info(img_thresh)
width <- info$width
height <- info$height
data <- image_data(img_thresh)
num_affected <- sum(data > 0)
severity <- num_affected / (width * height) * 100



