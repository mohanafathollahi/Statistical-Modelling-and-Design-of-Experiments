library(FactoMineR)
data(decathlon)

Correlation <- cor(decathlon[,1:12])

n<-nrow(decathlon)

install.packages("psych")
library(psych)

cortest.bartlett(Correlation,n)
#Reject null hypothesis (H0:R=I), so variables are correlated

kmo <- function(x)
{
  x <- subset(x, complete.cases(x))       # Omit missing values
  r <- cor(x)                             # Correlation matrix
  r2 <- r^2                               # Squared correlation coefficients
  i <- solve(r)                           # Inverse matrix of correlation matrix
  d <- diag(i)                            # Diagonal elements of inverse matrix
  p2 <- (-i/sqrt(outer(d, d)))^2          # Squared partial correlation coefficients
  diag(r2) <- diag(p2) <- 0               # Delete diagonal elements
  KMO <- sum(r2)/(sum(r2)+sum(p2))
  MSA <- colSums(r2)/(colSums(r2)+colSums(p2))
  return(list(KMO=KMO, MSA=MSA))
}
kmo(decathlon[,1:10])
#It is recommended to have an index greater than 0.50. in this case KMO is 0.596

# graph=TRUE by default, gives all the graphs
# Standardized PCA on correlation matrix is performed

levels(decathlon$Competition)<-c(1,2)
decathlon_PCA<-PCA(decathlon[,1:10],scale=TRUE, graph=TRUE )  #error

decathlon_PCA$eig
decathlon_PCA$var$coord 

#scree plot
decathlon_pca<-princomp(decathlon[,1:10],cor=TRUE,scores=TRUE) #,cutoff=0.01)
plot(decathlon_pca)
plot(decathlon_pca,type="line")

plot(decathlon_PCA,choix="var")
plot(decathlon_PCA,choix="ind")

cor(decathlon[,1:10])

#Regression
decathlon$PC1<-decathlon_PCA$ind$coord[,1]
decathlon$PC2<-decathlon_PCA$ind$coord[,2]
decathlon$PC3<-decathlon_PCA$ind$coord[,3]
decathlon$PC4<-decathlon_PCA$ind$coord[,4]
decathlon$PC5<-decathlon_PCA$ind$coord[,5]

reg_decathlon_1<-lm(decathlon$Points ~ PC1 + PC2, data=decathlon)
summary(reg_decathlon_1)


library(lmtest)

#Assumptions for Model_1
#1. Normality
#Shapiro Wilks Test
shapiro.test(residuals(reg_decathlon_1)) # follow normal distribution

# 2. Homogenity of Variance ###
# Residual Analysis 
plot(residuals(reg_decathlon_1))
##Breusch Pagan Test
bptest(reg_decathlon_1)  #The homogenity of variances is provided.

# 3. The independence of errors ### 
dwtest(reg_decathlon_1, alternative = "two.sided")  
#There is not an autocorrelaiton in the data set (p>0.05)

##Prediction Accuracy / Model_1
n <- nrow(decathlon)
train.sample <- sample(1:n, round(0.67*n))
train.sample
train.set <- decathlon[train.sample, ] 
test.set <- decathlon[-train.sample, ] 

reg_decathlon_1_train <-lm(Points ~ PC1 + PC2, data = train.set)
yhat<-predict(reg_decathlon_1_train, test.set, interval="prediction")
yhat
y<-test.set$Points
y

error<-cbind(yhat[,1,drop=FALSE],y,(yhat[,1]-y)^2)
error
sqr_err<-error[,3]
sse<-sum(sqr_err)
sse
RMSE_test<-sqrt(sse/(nrow(test.set)))
RMSE_test


reg_decathlon_1_train$residuals
RMSE_train <- sqrt(sum((reg_decathlon_1_train$residuals)^2)/nrow(train.set))
RMSE_train

#Model_2
reg_decathlon_2<-lm (decathlon$Points ~ PC1 + PC2 + PC3 , data=decathlon)
summary(reg_decathlon_2)

#Assumptions for Model_2
#1. Normality
#Shapiro Wilks Test
shapiro.test(residuals(reg_decathlon_2)) # follow normal distribution

# 2. Homogenity of Variance ###
# Residual Analysis 
plot(residuals(reg_decathlon_2))
##Breusch Pagan Test
bptest(reg_decathlon_2)  #The homogenity of variances is provided.

# 3. The independence of errors ### 
dwtest(reg_decathlon_2, alternative = "two.sided")  
#There is not an autocorrelaiton in the data set (p>0.05)

##Prediction Accuracy / Model_2
reg_decathlon_2_train <- lm(Points ~ PC1 + PC2 + PC3 , data=train.set)
yhat_2<-predict(reg_decathlon_2_train, test.set, interval="prediction")
yhat_2
y_2<-test.set$Points
y_2

error_2<-cbind(yhat_2[,1,drop=FALSE],y_2,(yhat_2[,1]-y_2)^2)
error_2
sqr_err_2<-error_2[,3]
sse_2<-sum(sqr_err_2)
sse_2
RMSE_test_2<-sqrt(sse_2/(nrow(test.set)))
RMSE_test_2

reg_decathlon_2_train$residuals
RMSE_train_2 <- sqrt(sum((reg_decathlon_2_train$residuals)^2)/nrow(train.set))
RMSE_train_2

#Model_3
reg_decathlon_3<-lm(decathlon$Points ~ PC1 + PC2 + PC3 + PC4 , data=decathlon)
summary(reg_decathlon_3)

#Assumptions for Model_3
#1. Normality
#Shapiro Wilks Test
shapiro.test(residuals(reg_decathlon_3)) # follow normal distribution

# 2. Homogenity of Variance ###
# Residual Analysis 
plot(residuals(reg_decathlon_3))
##Breusch Pagan Test
bptest(reg_decathlon_3)  #The homogenity of variances is provided.

# 3. The independence of errors ### 
dwtest(reg_decathlon_2, alternative = "two.sided")  
#There is not an autocorrelaiton in the data set (p>0.05)

##Prediction Accuracy / Model_3

reg_decathlon_3_train <- lm(Points ~ PC1 + PC2 + PC3 + PC4 , data=train.set)
yhat_3<-predict(reg_decathlon_3_train, test.set, interval="prediction")
yhat_3
y_3<-test.set$Points
y_3

error_3<-cbind(yhat_3[,1,drop=FALSE],y_3,(yhat_3[,1]-y_3)^2)
error_3
sqr_err_3<-error_3[,3]
sse_3<-sum(sqr_err_3)
sse_3
RMSE_test_3<-sqrt(sse_3/(nrow(test.set)))
RMSE_test_3

RMSE_train_3 <- sqrt(sum((reg_decathlon_3_train$residuals)^2)/nrow(train.set))
RMSE_train_3
