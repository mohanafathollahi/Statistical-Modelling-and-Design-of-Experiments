decathlon <-read.csv("decathlon.csv", sep = ",")
colnames(decathlon)
cor(decathlon[,2:13])

decathlon.cor = cor(decathlon[,2:10])
decathlon.cor
install.packages("corrplot")
library(corrplot)
corrplot(decathlon.cor)

#Plots of importnat variables 
op<-par(mfrow=c(2,2))
plot(X1500m~Shot.put,data=decathlon)
plot(X1500m~x400m,data=decathlon)
plot(X1500m~Discus,data=decathlon)
plot(X1500m~Pole.vault,data=decathlon)

# Model 1
RegModel_1 <-lm(X1500m~X400m, data=decathlon)
summary(RegModel_1)

#Assumptions
library(lmtest)

#Assumptions
#1. Normality test
shapiro.test(residuals(RegModel_1))
qqnorm(residuals(RegModel_1), pch = 1, frame = FALSE, main = "Q-Q RegModel_1")
qqline(residuals(RegModel_1), col = "red", lwd = 2)
hist(residuals(RegModel_1))

#2. Homogeneity of variance
plot(residuals(RegModel_1))
bptest(RegModel_1)

# 3.The independence of errors 
dwtest(RegModel_1, alternative = "two.sided")

# Model 2
RegModel_2 <-lm(X1500m ~ X400m+Discus, data=decathlon)
summary(RegModel_2)

#Assumptions
#1. Normality test
shapiro.test(residuals(RegModel_2))
qqnorm(residuals(RegModel_2), pch = 1, frame = FALSE, main = "Q-Q RegModel_1")
qqline(residuals(RegModel_2), col = "red", lwd = 2)
hist(residuals(RegModel_2))

#2. Homogeneity of variance
plot(residuals(RegModel_2))
bptest(RegModel_2)

# 3.The independence of errors 
dwtest(RegModel_2, alternative = "two.sided")

# Model 3
RegModel_3 <-lm(X1500m ~ X400m+Discus+Pole.vault, data=decathlon)
summary(RegModel_3)

#Assumptions
#1. Normality test
shapiro.test(residuals(RegModel_3))
qqnorm(residuals(RegModel_3), pch = 1, frame = FALSE, main = "Q-Q RegModel_1")
qqline(residuals(RegModel_3), col = "red", lwd = 2)
hist(residuals(RegModel_3))

#2. Homogeneity of variance
plot(residuals(RegModel_3))
bptest(RegModel_3)

# 3.The independence of errors 
dwtest(RegModel_3, alternative = "two.sided")

# Model 4
RegModel_4 <-lm(X1500m ~ X400m + Discus + Pole.vault + Shot.put, data=decathlon)
summary(RegModel_4)



#Compare to real value
coef_vec<-RegModel_3$coefficients
coef_vec

confint(RegModel_3)

yhat<-RegModel_3$fitted.values
yhat
decathlon$yhat <- yhat
confint(RegModel_3)

predict.lm(RegModel_3,interval="confidence")

#Accuracy of model
n <- nrow(decathlon)

train_sample <- sample(1:n, round(0.67*n))
train_sample

train_set <- decathlon[train_sample, ] 
test_set <- decathlon[-train_sample, ] 

RegModel_3_train <- lm(X1500m ~ X400m+Discus+Pole.vault, data = train_set)
yhat<-predict(RegModel_3_train, test_set, interval="prediction")
yhat
y<-test_set$X1500m
y

error_X1500m <- cbind(yhat[,1,drop=FALSE],y,(yhat[,1]-y)^2)
sqr_err<-error_X1500m[,3]
sse<-sum(sqr_err)

RMSE_test<-sqrt(sse/(nrow(test_set)))
RMSE_test

RegModel_3_train$residuals 
RMSE_train <- sqrt(sum((RegModel_3_train$residuals)^2)/nrow(train_set))
RMSE_train
