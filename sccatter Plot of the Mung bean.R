setwd("C:/Users/HP/OneDrive/Desktop/Mung bean analysis folder")
bikas<-read.csv("C:/Users/HP/OneDrive/Desktop/Mung bean analysis folder/MGA.csv")
attach(bikas)
print(bikas)
require(ggplot2)
ggplot(bikas, aes(x = Var, y = N0H.S, color = Var)) +
  geom_point(size = 5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "Var", y = "N0H.S", title = "Scatter Plot of 
       Genotypes against Mean Number of secondary Roots per seedling") +
  theme_bw() +
  theme(axis.text.x = element_text(face = "bold")) +
  scale_y_continuous(limits = c(5, 10), breaks = seq(5, 10)) +
  scale_color_manual(values = c("#FF0000", "#00FF00", "#0000FF", "#800080", "#FFA500", "#FFFF00", "#00FFFF", "#FF00FF", "#808080", "#000000", "#FFFFFF", "#A52A2A", "#FFC0CB", "#808000", "#000080", "#008080")) +
  guides(color = guide_legend(title = "Genotypes"))
