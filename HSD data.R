library(readxl)
HSD<-read_excel("C:/Users/HP/Downloads/Himani Singh Data (1).xlsx")
require(ggplot2)
library(gvlma)
library(agricolae)
print(HSD, n=5)
Trt<-as.factor(HSD$Treatment)
Iri<- as.factor(HSD$Irrigation)
SaD<- as.factor(HSD$Sampling_date)
####For fitting the linear model

colnames(HSD)
fit <- lm(`ugNO3/gsoil` ~ Treatment + Irrgation + Treatment:Sampling_date, data = HSD)
gvmodelfit=gvlma(fit)
print(gvmodelfit) 
###Assumptions also not satified the Liniearity so
##Test agian the normality
###For checking the normality of errror 
qqnorm(residuals(fit))  
qqline(residuals(fit), col = "red")
shapiro.test(residuals(fit))
###Scince the data is not normally distributed, we need to transform the data
fit_transformed <- lm(log(`ugNO3/gsoil`) ~ Treatment + Irrgation + Treatment:Sampling_date, data = HSD)
qqnorm(residuals(fit_transformed))  
qqline(residuals(fit_transformed), col = "red")
shapiro.test(residuals(fit_transformed))
hist(residuals(fit_transformed), breaks = 20, main = "Histogram of Residuals (Transformed)", xlab = "Residuals")###Now the data is acceted 
###Now again test the Transform data normality
gvmodelfit=gvlma(fit_transformed)
print(gvmodelfit)
####Again squre root transform test due to p value even W is closer to one
fit_transformed_sqrt <- lm(sqrt(`ugNO3/gsoil`) ~ Treatment + Irrgation + Treatment:Sampling_date, data = HSD)
gvmodelfit=gvlma(fit_transformed_sqrt)
print(gvmodelfit)
shapiro.test(residuals(fit_transformed_sqrt))
####However log transform follow better than squre root transfom so
##We go for log trasformation.
##Now deal with anova
print(fit_transformed)
anovaRes<- anova(fit_transformed)
anovaRes
mainplot<- duncan.test(fit_transformed, "Treatment", console = TRUE)
Subplot<- duncan.test(fit_transformed, "Irrgation", console = TRUE)

#Alternate way
####Another way to deal the same data transforing the specific data
log_ugNO3_gsoil<-log(HSD$`ugNO3/gsoil`)
splitmod_log <- with(HSD, sp.plot(Sampling_date, Treatment, Irrgation, log_ugNO3_gsoil))
splitmod_log
#######For main and sub plot comparison 
mainplot_log = with(HSD, duncan.test(log_ugNO3_gsoil, Treatment, 12, 0.4546, console = TRUE))
subplot_log = with(HSD, duncan.test(log_ugNO3_gsoil, Irrgation, 14, 0.8951, console = TRUE))

