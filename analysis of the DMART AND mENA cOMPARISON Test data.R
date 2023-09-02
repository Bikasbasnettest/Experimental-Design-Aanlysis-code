setwd("C:/Users/HP/OneDrive/Desktop/Mung bean analysis folder")
bikas<-read.csv("C:/Users/HP/OneDrive/Desktop/Mung bean analysis folder/MGA.csv")
attach(bikas)
print(bikas)
summary(bikas)
aggregate(RL,by=list(Var),mean)
aggregate(SL,by=list(Var),mean)
aggregate(RL/SL,by=list(Var),mean)
aggregate(N0H.S,by=list(Var),mean)
aggregate(AGWP,by=list(Var),mean)

comparision<-with(bikas,duncan.test(N0H,Var,223,0.094555))             
comparision
comparision<-with(bikas,duncan.test(N0H,Var,223,0.094555))             
comparision

bikas$ratio<-bikas$RL/SL
summary(bikas)
bikas$Rep=as.factor(Rep)
bikas$Var=as.factor(Var)
require(gvlma)
fit=lm(RL/SL~Rep+Var)
gvmodelfit=gvlma(fit)
print(gvmodelfit)
boxplot(RL/SL)
anova(lm(RL/SL~Rep+Var))
require(agricolae)
comparision<-with(bikas,duncan.test(RL/SL,Var,223,0.094555))             
comparision 

plot(bikas$RL, bikas$SL, col = bikas$RL/SL, pch = 16, cex = 2)

ggplot(bikas, aes(x = Var, y =  RL/SL)) + 
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "Var", y = "RL/SL", title = "Scatter Plot of Genotypes against Root to Shoot Ratio") +
  theme_bw()
geom_abline(intercept = 0, slope = 1, color = "green", linetype = "dashed")

ggplot(bikas, aes(x = Var, y = RL/SL)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "Var", y = "RL/SL", title = "Scatter Plot of Var against RL/SL") +
  theme_bw()
