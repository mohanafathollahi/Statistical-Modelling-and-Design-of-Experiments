#create normal distribution
Dist_1 <- rnorm(50, mean = 10 , sd = 2)
Dist_2  <- rnorm(50, mean = 5 , sd = 2)
Dist_3  <- rnorm(50, mean = 10 , sd = 2 )

plot(density(Dist_1),xlim=c(-4,20),main="Three Normal distributions")
lines(density(Dist_2),col=2)
lines(density(Dist_3),col=3)

Dst_1=data.frame(x1=Dist_1, x2="D1")
Dst_2=data.frame(x1=Dist_2, x2="D2")
Dst_3=data.frame(x1=Dist_3, x2="D3")

??mergeRows
library(RcmdrMisc)
total_dst=mergeRows(Dst_1, Dst_2, common.only=FALSE)
total_dst=mergeRows(as.data.frame(total_dst), Dst_3, common.only=FALSE)
total_dst
### ANOVA on simulated data ####
AnovaModel_1 <- aov(x1 ~ x2, data=total_dst)
summary(AnovaModel_1)
Boxplot(x1~x2,data=total_dst,id=FALSE)   #when we write id= TRUE we can find exact number of outlier


library(lmtest)
## Assumptions of ANOVA 
#Durbin Watson 
dwtest(AnovaModel_1, alternative ="two.sided")

#Shapiro test
shapiro.test(residuals(AnovaModel_1))

#Breusch Pagan test
bptest(AnovaModel_1)

#Wine dataset
install.packages("readxl")
library(readxl)

winequality_red<-read.csv2("winequality-red.csv",dec=".")
winequality_white<-read.csv2("winequality-white.csv",dec=".")

colnames(winequality_red)
  
Red =data.frame(winequality_red, type="Red")
White = data.frame(winequality_white, type="White")

library(RcmdrMisc)
wine_merg = mergeRows(Red, White, common.only=FALSE)

wine_quality <- cut(wine_merg$quality, breaks = c(0,5,7,Inf), 
                          labels = c("low","medium","high"),right = FALSE)

wine_total = cbind(wine_merg, wine_quality)

head(wine_total)

# ANOVA- part 1

for (i in 1:11){
  print(colnames(wine_total)[i])
  print(summary(aov(wine_total[,i] ~ wine_quality,data=wine_total)))
}

library(lmtest)
# Assumptions of ANOVA 
#normality test with Q-Q plot
for (i in 1:11){
  qqnorm(wine_total$fixed.acidity, pch = 1, frame = FALSE,main = "fixed.acidity")
}
qqnorm(wine_total$fixed.acidity, pch = 1, frame = FALSE,main = "fixed.acidity")
qqline(wine_total$fixed.acidity, col = "red", lwd = 2)

qqnorm(wine_total$volatile.acidity, pch = 1, frame = FALSE,main = "volatile.acidity")
qqline(wine_total$volatile.acidity, col = "red", lwd = 2)

qqnorm(wine_total$citric.acid, pch = 1, frame = FALSE,main = "citric.acid")
qqline(wine_total$citric.acid, col = "red", lwd = 2)

qqnorm(wine_total$residual.sugar, pch = 1, frame = FALSE,main = "residual.sugar")
qqline(wine_total$residual.sugar, col = "red", lwd = 2)

qqnorm(wine_total$chlorides, pch = 1, frame = FALSE,main = "chlorides")
qqline(wine_total$chlorides, col = "red", lwd = 2)

qqnorm(wine_total$free.sulfur.dioxide, pch = 1, frame = FALSE,main = "free.sulfur.dioxide")
qqline(wine_total$free.sulfur.dioxide, col = "red", lwd = 2)

qqnorm(wine_total$total.sulfur.dioxide, pch = 1, frame = FALSE,main = "total.sulfur.dioxide")
qqline(wine_total$total.sulfur.dioxide, col = "red", lwd = 2)

qqnorm(wine_total$density, pch = 1, frame = FALSE,main = "density")
qqline(wine_total$density, col = "red", lwd = 2)

qqnorm(wine_total$pH, pch = 1, frame = FALSE,main = "pH")
qqline(wine_total$pH, col = "red", lwd = 2)

qqnorm(wine_total$sulphates, pch = 1, frame = FALSE,main = "sulphates")
qqline(wine_total$sulphates, col = "red", lwd = 2)

qqnorm(wine_total$alcohol, pch = 1, frame = FALSE,main = "alcohol")
qqline(wine_total$alcohol, col = "red", lwd = 2)


# Independency of observations 
library(lmtest)

for (i in 1:11){
  print(colnames(wine_total)[i])
  print(dwtest(aov(wine_total[,i] ~ wine_quality,data=wine_total), alternative ="two.sided"))
}

### Breusch Pagan test ####

for (i in 1:11){
  print(colnames(wine_total)[i])
  print(bptest(aov(wine_total[,i]~wine_quality, data=wine_total)))
}

#ANOVA-part 2
for (i in 1:11){
  print(colnames(wine_total)[i])
  print(summary(aov(wine_total[,i] ~ type, data=wine_total)))
}

# Independency of observations 
library(lmtest)

for (i in 1:11){
  print(colnames(wine_total)[i])
  print(dwtest(aov(wine_total[,i] ~ type,data=wine_total), alternative ="two.sided"))
}

### Breusch Pagan test ####
for (i in 1:11){
  print(colnames(wine_total)[i])
  print(bptest(aov(wine_total[,i]~type, data=wine_total)))
}

#Two way ANOVA_ alcohol ~ type+wine_quality/part-3 

model_mix<-aov(alcohol~type+wine_quality,data=wine_total)
summary(model_mix)

## Assumptions of ANOVA 
#Durbin Watson 
dwtest(model_mix, alternative ="two.sided")

#Breusch Pagan test
leveneTest(alcohol~type, data= wine_total) #reject H0, variances are not equal
leveneTest(alcohol~wine_quality, data= wine_total) #reject H0, variances are not equal

#Posthoc testing
#bwt alcohol and type
with(wine_total,
     {
       pairwise.t.test( alcohol, type ,p.adj="bonf") 
     })

#bwt alcohol and wine_quality
with(wine_total,
     {
       pairwise.t.test( alcohol, wine_quality ,p.adj="bonf") 
     })
#box plot
Boxplot(alcohol~wine_quality,data=wine_total,id=FALSE,col=2:(nlevels(alcohol~wine_quality)+1)) 

#Two way ANOVA_fixed acidity ~ type+wine_quality/part-4

model_mix_2<-aov(fixed.acidity~type+wine_quality,data=wine_total)
summary(model_mix_2)

#assumptions of ANOVA: 

dwtest(model_mix_2, alternative ="two.sided") 
leveneTest(fixed.acidity ~ type, data= wine_total)#reject H0, variances are not equal
leveneTest(fixed.acidity ~ wine_quality, data= wine_total)#accept H0, variances are equal

#Post-hoc testing
with(wine_total,
     {
       pairwise.t.test( fixed.acidity, type ,p.adj="bonf") 
     })

