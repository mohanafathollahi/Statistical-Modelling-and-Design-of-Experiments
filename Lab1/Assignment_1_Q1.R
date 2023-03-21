#Question 1:
decathlon <-read.csv("decathlon.csv", sep = ",")
head(decathlon)
boxplot(X100m~Competition, xlab="Competition", ylab="X100m", data=decathlon)
colnames(decathlon)

#Question 2: 
decathlon$x100m_2<- cut(decathlon$X100m, c(0,11,12)) 
levels(decathlon$x100m_2) <- c("<=11", ">11")
summary(decathlon$x100m_2)

#cross-table
table_comp <- table(decathlon$x100m_2, decathlon$Competition)
table_comp 

?prop.table 
prop.table(table_comp) #Joint Probability : the intersection of two variables

#Marginal Probabilities
colSums(prop.table(table_comp)) #prob of each column
rowSums(prop.table(table_comp)) #prob of each row

sweep(prop.table(table_comp),1,rowSums(prop.table(table_comp)),'/')

#Chi-Square test
chisq.test(table_comp) 

#Question 3: Visualize the distribution 

summary(decathlon$X100m)
plot(density(decathlon$X100m),main="Density curves of X100m",xlim=c(10,12))
shapiro.test(decathlon$X100m)

summary(decathlon$Long.jump)
plot(density(decathlon$Long.jump),main="Density curves of Long.jump",xlim=c(6,9))
shapiro.test(decathlon$Long.jump)

summary(decathlon$Shot.put)
plot(density(decathlon$Shot.put),main="Density curves of Shot.put",xlim=c(11,18))
shapiro.test(decathlon$Shot.put)

summary(decathlon$High.jump)
plot(density(decathlon$High.jump),main="Density curves of High.jump",xlim=c(1.5,2.5))
shapiro.test(decathlon$High.jump)

summary(decathlon$X400m)
plot(density(decathlon$X400m),main="Density curves of X400m",xlim=c(44,55))
shapiro.test(decathlon$X400m)

summary(decathlon$X110m.hurdle)
plot(density(decathlon$X110m.hurdle),main="Density curves of X110m.hurdle",xlim=c(12.5,17))
shapiro.test(decathlon$X110m.hurdle)

summary(decathlon$Discus)
plot(density(decathlon$Discus),main="Density curves of Discus",xlim=c(34,56))
shapiro.test(decathlon$Discus)

summary(decathlon$Pole.vault)
plot(density(decathlon$Pole.vault),main="Density curves of Pole.vault",xlim=c(3.5,6))
shapiro.test(decathlon$Pole.vault)

summary(decathlon$Javeline)
plot(density(decathlon$Javeline),main="Density curves of Javeline",xlim=c(45,75))
shapiro.test(decathlon$Javeline)

summary(decathlon$X1500m)
plot(density(decathlon$X1500m),main="Density curves of X1500m",xlim=c(250,330))
shapiro.test(decathlon$X1500m)

summary(decathlon$Rank)
plot(density(decathlon$Rank),main="Density curves of Rank",xlim=c(1,28))
shapiro.test(decathlon$Rank)

summary(decathlon$Points)
plot(density(decathlon$Points),main="Density curves of Points",xlim=c(7280,8890))
shapiro.test(decathlon$Points)

#Question 4: Generate normal distribution

Dist_first<-rnorm(100, mean=40, sd=4)
Dist_second<-rnorm(100, mean=40, sd=10)
Dist_third<-rnorm(100, mean=20, sd=10)

plot(density(Dist_first),xlim=c(0,60),ylim=c(0,0.1), main="Three different Normal distributed samples")
lines(density(Dist_second),col=2)
lines(density(Dist_third),col=3)

t.test(Dist_first, Dist_second,var.equal=TRUE)
t.test(Dist_first, Dist_third,var.equal=TRUE)
t.test(Dist_third, Dist_second,var.equal=TRUE)

t.test(decathlon$X400m,decathlon$X100m,var.equal=TRUE)

#Question 5: difference between two type

decathlon$competition <-unclass(factor(decathlon$Competition))

g1<-which(decathlon$competition =="1")
g2<-which(decathlon$competition =="2")

t.test(decathlon$X100m[g1], decathlon$X100m[g2],var.equal=TRUE)

plot(density(decathlon$X100m[g1]),main="Density curves of X100m Groups",xlim=c(10,12.5),ylim=c(0,2))
lines(density(decathlon$X100m[g2]),col=2)

t.test(decathlon$X400m[g1], decathlon$X400m[g2],var.equal=TRUE)

plot(density(decathlon$X400m[g1]),main="Density curves of X400m Groups",xlim=c(45,55),ylim=c(0,0.5))
lines(density(decathlon$X400m[g2]),col=2)




