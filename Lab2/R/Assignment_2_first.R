f1  <- rnorm(2000, mean = 5 , sd = 3 )
f2 <- rexp(2000,rate = 20)
f3  <- rnorm(2000, mean = 3 , sd = 1.5 )
f4  <- rnorm(2000, mean = 0 , sd = 1 )
f5 <- rexp(2000,rate = 10)

f6 <- f5+f3
f7 <- 2*f5+ 3*f1
f8 <- f4+ 2.5*f2+11*f3
f9 <- f3+ 6*f2
f10 <- f5+2*f4

noise <- rnorm(2000, mean = 0 , sd = 1)
Answer <- 3* f1 + 4*f7 +10* f5 +noise
total=data.frame(f1, f2,f3,f4,f5,f6,f7,f8,f9,f10, Answer)


library(FactoMineR)
total_pca<-PCA(total,scale=TRUE, graph=FALSE)
summary(total_pca)
plot(total_pca,choix="var")

total_pca$eig
total_pca$var$coord 

model_1 <-lm(Answer~f3+f6+f8+f9+f1+f7+f2)
summary(model_1)

model_2 <-lm(Answer~f1+f3+f6)
summary(model_2)

#Normality tests
library(lmtest)

#1. Normality test
qqnorm(residuals(model_2))
shapiro.test(residuals(model_2))

#2. Homogeneity of variance
plot(residuals(model_2))
bptest(model_2)

# 3.The independence of errors 
dwtest(model_2, alternative = "two.sided")
       
#Validation
pred.model <- predict(model_2)
plot(x=total$Answer, y=pred.model, type="l",col="blue", xlab="DataFrame",
     ylab="Prediction")
t.test(x = total$Answer, y = pred.model, alternative = "two.sided", var.equal = FALSE)




