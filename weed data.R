WD<-read.csv("E:/Madhav sir Paper/saurajbk.csv", header = TRUE)
WD
colnames(WD)
WD$rep<-as.factor(WD$rep)
WD$trt<-as.factor(WD$trt)
library(agricolae)
library(gvlma)
require(gvlma)
fit=lm(WD$ph_30~WD$rep+WD$trt) 
gvmodelfit=gvlma(fit) 
gvmodelfit
anova(lm(WD$ph_30~(WD$rep+WD$trt)))
aggregate(WD$ph_30 ,by=list(WD$trt),mean)
aggregate(WD$ph_30 ,by=list(WD$trt),sd)
dmrtcomparison<-with(WD,duncan.test(ph_30 , trt, 30 , 11.948))      
dmrtcomparison
library(ggplot2)
library(dplyr)
library(ggpubr)
library(stringr)
########for DMRT plotting 
MSE<- 11.948                                                                                                                                                                                                                                                                                                                                                                                                                                                              
n<-220  
alpha<-0.05   
df_error<-30
q_alpha<-qt(1 - alpha/2, df_error)
LSD_value1<-q_alpha * sqrt(MSE / n)
print(LSD_value1)
meanval<-WD%>%group_by(trt)%>%summarize(mean(ph_30))
meanval
sig.let<-dmrtcomparison$groups[order(row.names(dmrtcomparison$groups)),]
sig.let

summary_data <- WD %>%
  group_by(trt) %>%
  summarize(mean_yield = mean(ph_30, na.rm = TRUE), .groups = 'drop')

# Assuming sig.let is a vector with significance letters corresponding to each VAR
# Make sure the lengths match
if (length(sig.let$groups) == nrow(summary_data)) {
  summary_data <- summary_data %>%
    mutate(significance = sig.let$groups)
} else {
  stop("Length of significance letters does not match number of groups.")
}

PH30 <- ggplot(data = WD, aes(x = trt, y = ph_30)) +
  geom_boxplot(aes(fill = trt)) +
  geom_violin(width = 0.7) +
  geom_text(data = summary_data, aes(x = trt, y = max(WD$ph_30) + 1, label = significance), vjust = 0) +
  geom_errorbar(aes(ymin = ph_30 - 0.6, ymax = ph_30 + 0.6), width = 0.2) +
  ylab("Plant height at 30 DAS") +
  stat_compare_means(method = "anova") +
  stat_compare_means(label = "p.signif", method = "anova") +
  rotate_x_text(angle = 0, size= 6) +
  annotate("text", x = 1, y = max(WD$ph_30) + 1, label = paste("LSD =", round(LSD_value1, 5)), hjust = 0, size = 3.5)+
  theme(legend.position = "none")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 12.5))+theme(
    axis.title.x = element_blank())

PH30
######Plant height 45
colnames(WD)
fit=lm(WD$ph_45~WD$rep+WD$trt) 
gvmodelfit=gvlma(fit) 
gvmodelfit
anova(lm(WD$ph_45~(WD$rep+WD$trt)))
aggregate(WD$ph_45 ,by=list(WD$trt),mean)
aggregate(WD$ph_45 ,by=list(WD$trt),sd)
dmrtcomparison<-with(WD,duncan.test(ph_45 , trt, 30 , 44.431))      
dmrtcomparison
########for DMRT plotting 
MSE<- 44.431                                                                                                                                                                                                                                                                                                                                                                                                                                                              
n<-220  
alpha<-0.05   
df_error<-30
q_alpha<-qt(1 - alpha/2, df_error)
LSD_value<-q_alpha * sqrt(MSE / n)
print(LSD_value)
meanval<-WD%>%group_by(trt)%>%summarize(mean(ph_45))
meanval
sig.let<-dmrtcomparison$groups[order(row.names(dmrtcomparison$groups)),]
sig.let

summary_data <- WD %>%
  group_by(trt) %>%
  summarize(mean_yield = mean(ph_45, na.rm = TRUE), .groups = 'drop')

## Assuming sig.let is a vector with significance letters corresponding to each VAR
##### Make sure the lengths match
if (length(sig.let$groups) == nrow(summary_data)) {
  summary_data <- summary_data %>%
    mutate(significance = sig.let$groups)
} else {
  stop("Length of significance letters does not match number of groups.")
}
colnames(WD) 
# Now create the plot

PH45 <- ggplot(data = WD, aes(x = trt, y = ph_45)) +
  geom_boxplot(aes(fill = trt)) +
  geom_violin(width = 0.7) +
  geom_text(data = summary_data, aes(x = trt, y = max(WD$ph_45) + 1, label = significance), vjust = 0) +
  geom_errorbar(aes(ymin = ph_45 - 0.6, ymax = ph_45 + 0.6), width = 0.2) +
  ylab("Plant height at 45 DAS") +
  stat_compare_means(method = "anova") +
  stat_compare_means(label = "p.signif", method = "anova") +
  rotate_x_text(angle =0, size= 6) +
  annotate("text", x = 1, y = max(WD$ph_45) + 1, label = paste("LSD =", round(LSD_value, 5)), hjust = 0, size = 3.5) +
  theme(legend.position = "none")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 12.5)) +
  theme(axis.title.x = element_blank())

PH45
library(gridExtra)
################ plant height 60
colnames(WD)

fit=lm(WD$ph_60~WD$rep+WD$trt) 
gvmodelfit=gvlma(fit) 
gvmodelfit
anova(lm(WD$ph_60~(WD$rep+WD$trt)))
aggregate(WD$ph_60 ,by=list(WD$trt),mean)
aggregate(WD$ph_60 ,by=list(WD$trt),sd)
dmrtcomparison<-with(WD,duncan.test(ph_60 , trt, 30 , 43.869))      
dmrtcomparison
########for DMRT plotting 
MSE<- 43.869                                                                                                                                                                                                                                                                                                                                                                                                                                                              
n<-220  
alpha<-0.05   
df_error<-30
q_alpha<-qt(1 - alpha/2, df_error)
LSD_value2<-q_alpha * sqrt(MSE / n)
print(LSD_value2)
meanval<-WD%>%group_by(trt)%>%summarize(mean(ph_60))
meanval
sig.let<-dmrtcomparison$groups[order(row.names(dmrtcomparison$groups)),]
sig.let

summary_data <- WD %>%
  group_by(trt) %>%
  summarize(mean_yield = mean(ph_60, na.rm = TRUE), .groups = 'drop')

## Assuming sig.let is a vector with significance letters corresponding to each VAR
##### Make sure the lengths match
if (length(sig.let$groups) == nrow(summary_data)) {
  summary_data <- summary_data %>%
    mutate(significance = sig.let$groups)
} else {
  stop("Length of significance letters does not match number of groups.")
}
colnames(WD) 
# Now create the plot

PH60 <- ggplot(data = WD, aes(x = trt, y = ph_60)) +
  geom_boxplot(aes(fill = trt)) +
  geom_violin(width = 0.7) +
  geom_text(data = summary_data, aes(x = trt, y = max(WD$ph_60) + 1, label = significance), vjust = 0) +
  geom_errorbar(aes(ymin = ph_60 - 0.6, ymax = ph_60 + 0.6), width = 0.2) +
  ylab("Plant height at 60 DAS") +
  stat_compare_means(method = "anova") +
  stat_compare_means(label = "p.signif", method = "anova") +
  rotate_x_text(angle =0, size = 6) +
  annotate("text", x = 1, y = max(WD$ph_60) + 1, label = paste("LSD =", round(LSD_value2, 5)), hjust = 0, size = 3.5) +
  theme(legend.position = "none")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 12.5))+
  theme(axis.title.x = element_blank())


PH60

############Plant height 75 DAYS 
fit=lm(WD$ph_75~WD$rep+WD$trt) 
gvmodelfit=gvlma(fit) 
gvmodelfit
anova(lm(WD$ph_75~(WD$rep+WD$trt)))
aggregate(WD$ph_75 ,by=list(WD$trt),mean)
aggregate(WD$ph_75 ,by=list(WD$trt),sd)
dmrtcomparison<-with(WD,duncan.test(ph_75 , trt, 30 , 29.882))      
dmrtcomparison
########for DMRT plotting 
MSE<- 29.882                                                                                                                                                                                                                                                                                                                                                                                                                                                              
n<-220  
alpha<-0.05   
df_error<-30
q_alpha<-qt(1 - alpha/2, df_error)
LSD_value3<-q_alpha * sqrt(MSE / n)
print(LSD_value3)
meanval<-WD%>%group_by(trt)%>%summarize(mean(ph_75))
meanval
sig.let<-dmrtcomparison$groups[order(row.names(dmrtcomparison$groups)),]
sig.let

summary_data <- WD %>%
  group_by(trt) %>%
  summarize(mean_yield = mean(ph_75, na.rm = TRUE), .groups = 'drop')

## Assuming sig.let is a vector with significance letters corresponding to each VAR
##### Make sure the lengths match
if (length(sig.let$groups) == nrow(summary_data)) {
  summary_data <- summary_data %>%
    mutate(significance = sig.let$groups)
} else {
  stop("Length of significance letters does not match number of groups.")
}
colnames(WD) 
PH75 <- ggplot(data = WD, aes(x = trt, y = ph_75)) +
  geom_boxplot(aes(fill = trt)) +
  geom_violin(width = 0.7) +
  geom_text(data = summary_data, aes(x = trt, y = max(WD$ph_60) + 1, label = significance), vjust = 0) +
  geom_errorbar(aes(ymin = ph_75 - 0.6, ymax = ph_75 + 0.6), width = 0.2) +
  ylab("Plant height at 75 DAS") +
  stat_compare_means(method = "anova") +
  stat_compare_means(label = "p.signif", method = "anova") +
  annotate("text", x = 1, y = max(WD$ph_75) + 1, label = paste("LSD =", round(LSD_value3, 5)), hjust = 0, size = 3.5) +
  theme(legend.position = "none") +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1, size = 6)
  ) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 12.5))+
  theme(axis.title.x = element_blank())



PH75
library(gridExtra)
Combined1<-grid.arrange(PH30,PH45, PH60, PH75,  ncol=2)
Combined1
getwd()
ggsave("30and 45, 60 and 75.JPG", plot = Combined1, dpi = 600, width = 25, height = 15)
