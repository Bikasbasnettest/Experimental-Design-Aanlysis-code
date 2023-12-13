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
###$##3RCBD 2 factor code analayiss 
pankaj<-read.csv("C:/Users/HP/Desktop/Lee.csv")
attach(pankaj)
print(pankaj)
aggregate( Yld.vine,by=list(X3G),mean)
aggregate( Yld.vine,by=list(Ethephon),mean)
aggregate( Yld.vine,by=list(Trt),mean)
pankaj$Replication=as.factor(Replication)
pankaj$X3G=as.factor(X3G)
pankaj$Ethephon=as.factor(Ethephon)
require(gvlma)
fit=lm( Yld.vine~Replication+X3G+Ethephon+X3G*Ethephon)
gvmodelfit=gvlma(fit)
print(gvmodelfit)
boxplot( Yld.vine)
anova(lm( Yld.vine~Replication+Trt+X3G+Ethephon+X3G*Ethephon))
require(agricolae)
comparision<-with(pankaj,duncan.test( Yld.vine,X3G,15,0.4035))             
comparision           
comparision<-with(pankaj,duncan.test( Yld.vine,Ethephon,15,0.4035))             
comparision
comparision<-with(pankaj,duncan.test( Yld.vine,Trt,15,0.4035))             
comparison
###333333for the split plot design
Anova for the split polot design
splitmodel<-with(bikas,sp.plot(Rep,VAR(mainplot),Ntr(subplot),X30Pl.Ht)
                 
                require(agricolae)
attach(bikas)
splitmodel<-with(bikas,sp.plot(Rep,VAR,Ntr,X30Pl.Ht))
gla=splitmodel$gl.a
glb=splitmodel$gl.b
ea=splitmodel$Ea(error of main plot)
eb=splitmodel$Eb(erroofr th4e sub plot)
#W### IF MAIN PLOT AND SUB PLOT ARE SIGNIFICANT
mainplot=with(bikas,duncan.test(X30Pl.Ht,VAR,gla,ea,console = TRUE))
subplot=with(bikas,duncan.test(X30Pl.Ht,Ntr,gla,ea,console = TRUE))
mainplot=with(bikas,LSD.test(X30Pl.Ht,VAR,gla,ea,console = TRUE))
subplot=with(bikas,LSD.test(X30Pl.Ht,Ntr,glb,eb,console = TRUE))

##Interaction if significant
interacton=with(bikas,LSD.test(X30Pl.Ht,VAR:Ntr,glb,eb,console = TRUE))
with(bikas,(interaction.plot(x.factor = VAR,trace.factor = Ntr,
                             response = bconN.,
                             fun = mean,
                             type = "b",
                             main="interaction plot of Var and Ntrogen rate 
                             representing baby corn Nitrogen %",
                             xlab = VAR,
                             ylab = bconN.,
                             pch = c(19,17,15,13),
                             leg.bty = "0",
                             col = c("black","blue","green","red"),
                             lwd = 3,
                             trace.label = "Nitrogen rate kg/ha",
                             xpd = TRUE)))

                

