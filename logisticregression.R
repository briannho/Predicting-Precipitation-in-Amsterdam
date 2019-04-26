#install.packages("caret")
library(caret)
#install.packages("boot")
library('boot')

data <- read.csv("allstations.csv")
names(data) <- c('pr1','hu1','tg1', 'slp1','pr2','hu2','tg2', 'slp2','pr3','hu3','tg3', 'slp3','pr4','hu4','tg4', 'slp4','pr5','hu5','tg5', 'slp5')

#pulls stations out of table
stat1 <- data[1:4]
stat2 <- data[5:8]
stat3 <- data[9:12]
stat4 <- data[13:16]
stat5 <- data[17:20]



logit <- function(p) log(p/(1-p))
#inital logistic regression models
model1 <- glm(stat1$pr1~.,family=binomial(logit), data=stat1)
model2 <- glm(stat2$pr2~.,family=binomial(logit), data=stat2)
model3 <- glm(stat3$pr3~.,family=binomial(logit), data=stat3)
model4 <- glm(stat4$pr4~.,family=binomial(logit), data=stat4)
model5 <- glm(stat5$pr5~.,family=binomial(logit), data=stat5)

#see what the models look like
summary(model1)
BIC1 <- BIC(model1)
summary(model2)
BIC2 <- BIC(model2)
summary(model3)
BIC3 <- BIC(model3)
summary(model4)
BIC4 <- BIC(model4)
summary(model5)
BIC5 <-BIC(model5)

#stepwise selection of models
model1_2 <- step(model1, data=stat1)
model2_2 <- step(model2, data=stat2)
model3_2 <- step(model3, data=stat3)
model4_2 <- step(model4, data=stat4)
model5_2 <- step(model5, data=stat5)

#see what they look like
summary(model1_2)
BIC(model1_2)
summary(model2_2)
BIC2_2 <- BIC(model2_2)
summary(model3_2)
BIC(model3_2)
summary(model4_2)
BIC(model4_2)
summary(model5_2)
BIC(model5_2)

#calculating accuracy and misclassification error
pr <- predict(model1, stat1, type = "response")

table <- table(actual= stat1$pr1, predicted=pr>.5)
total <- sum(table)
accuracy1 <- (table[1,1]+table[2+2])/total
error1 <- (table[1,2] + table[2,1])/total



pr <- predict(model2, stat2, type = "response")

table <- table(actual=stat2$pr2, predicted=pr>.5)
total <- sum(table)
accuracy2 <- (table[1,1]+table[2+2])/total
error2 <- (table[1,2] + table[2,1])/total




pr <- predict(model3, stat3, type = "response")

table <- table(actual=stat3$pr3, predicted=pr>.5)
total <- sum(table)
accuracy3 <- (table[1,1]+table[2+2])/total
error3 <- (table[1,2] + table[2,1])/total


pr <- predict(model4, stat4, type = "response")

table <- table(actual=stat4$pr4, predicted=pr>.5)
total <- sum(table)
accuracy4 <- (table[1,1]+table[2+2])/total
error4 <- (table[1,2] + table[2,1])/total


pr <- predict(model5, stat5, type = "response")

table <- table(actual=stat5$pr5, predicted=pr>.5)
total <- sum(table)
accuracy5 <- (table[1,1]+table[2+2])/total
error5 <- (table[1,2] + table[2,1])/total


#functions to bootstrap
f1 <- function(stat1,indicies){
  d<-stat1[indicies,]
  model1 <- glm(stat1$pr1 ~., family=binomial(logit),data=d)
  a <- BIC(model1)
 
  return(a)
}

f2 <- function(stat2,indicies){
  d<-stat2[indicies,]
  model2 <- glm(stat2$pr2 ~., family=binomial(logit),data=d)
  a <- BIC(model2)
 
  return(a)
}

f3 <- function(stat3,indicies){
  d<-stat3[indicies,]
  model3 <- glm(stat3$pr3 ~., family=binomial(logit),data=d)
  a <- BIC(model3)
 
  return(a)
}

f4 <- function(stat4,indicies){
  d<-stat4[indicies,]
  model4 <- glm(stat4$pr4 ~., family=binomial(logit),data=d)
  a <- BIC(model4)
 
  return(a)
}

f5 <- function(stat5,indicies){
  d<-stat1[indicies,]
  model5 <- glm(stat5$pr5 ~., family=binomial(logit),data=d)
  a <- BIC(model5)
 
  return(a)
}

#first round of bootstrapping
boot1res <- boot(data=stat1, statistic = f1, R=1000)


boot2res <- boot(data=stat2, statistic = f2, R=1000)


boot3res <- boot(data=stat3, statistic = f3, R=1000)


boot4res <- boot(data=stat4, statistic = f4, R=1000)


boot5res <- boot(data=stat5, statistic = f5, R=1000)

#so you can see what you did
print(boot1res)
print(boot2res)
print(boot3res)
print(boot4res)
print(boot5res)


#calculate pca for each station
pca1 <- princomp(stat1, cor=T)
summary(pca1, loadings = T)
modpca1 <- glm(formula = stat1$pr1 ~ pca1$scores[,1:4], family = binomial(logit),data = stat1)
summary(modpca1)
pcaBIC1 <- BIC(modpca1)


pca2 <- princomp(stat2, cor=T)
summary(pca2, loadings = T)
modpca2 <- glm(formula = stat2$pr2 ~ pca2$scores[,1:4], family = binomial(logit),data = stat2)
summary(modpca2)
pcaBIC2 <- BIC(modpca2)

pca3 <- princomp(stat3, cor=T)
summary(pca3, loadings = T)
modpca3 <- glm(formula = stat3$pr3 ~ pca3$scores[,1:4], family = binomial(logit),data = stat3)
summary(modpca3)
pcaBIC3 <- BIC(modpca3)

pca4 <- princomp(stat4, cor=T)
summary(pca4, loadings = T)
modpca4 <- glm(formula = stat4$pr4 ~ pca4$scores[,1:4], family = binomial(logit),data = stat4)
summary(modpca4)
pcaBIC4 <- BIC(modpca4)


pca5 <- princomp(stat5, cor=T)
summary(pca5, loadings = T)
modpca5 <- glm(formula = stat5$pr5 ~ pca5$scores[,1:4], family = binomial(logit),data = stat5)
summary(modpca5)
pcaBIC5 <- BIC(modpca5)


#check accuracy and misclassification error
pr <- predict(modpca1, stat1, type = "response")

table <- table(actual= stat1$pr1, predicted=pr>.5)
total <- sum(table)
accmod1 <- (table[1,1]+table[2+2])/total
errmod1 <- (table[1,2] + table[2,1])/total



pr <- predict(modpca2,stat2, type = "response")

table <- table(actual=stat2$pr2, predicted=pr>.5)
total <- sum(table)
accmod2 <- (table[1,1]+table[2+2])/total
errmod2 <- (table[1,2] + table[2,1])/total




pr <- predict(modpca3, stat3, type = "response")

table <- table(actual=stat3$pr3, predicted=pr>.5)
total <- sum(table)
accpca3 <- (table[1,1]+table[2+2])/total
errpca3 <- (table[1,2] + table[2,1])/total


pr <- predict(modpca4, stat4, type = "response")

table <- table(actual=stat4$pr4, predicted=pr>.5)
total <- sum(table)
accpca4 <- (table[1,1]+table[2+2])/total
errpca4 <- (table[1,2] + table[2,1])/total


pr <- predict(modpca5, stat5, type = "response")

table <- table(actual=stat5$pr5, predicted=pr>.5)
total <- sum(table)
accpca5 <- (table[1,1]+table[2+2])/total
errpca5 <- (table[1,2] + table[2,1])/total

#bootstrapping round 2
fpca1 <- function(stat1,indicies){
  d<-stat1[indicies,]
  modpca1 <- glm(formula = stat1$pr1 ~ pca1$scores[,1:4], family = binomial(logit),data = d)
  a <- BIC(modpca1)
 
  return(a)
}

fpca2 <- function(stat2,indicies){
  d<-stat2[indicies,]
  model2 <- glm(formula = stat2$pr2 ~ pca2$scores[,1:4], family=binomial(logit),data=d)
  a <- BIC(model2)
 
  return(a)
}

fpca3 <- function(stat3,indicies){
  d<-stat3[indicies,]
  model3 <- glm(stat3$pr3 ~pca3$scores[,1:4], family=binomial(logit),data=d)
  a <- BIC(model3)
 
  return(a)
}

fpca4 <- function(stat4,indicies){
  d<-stat4[indicies,]
  model4 <- glm(stat4$pr4 ~pca4$scores[,1:4], family=binomial(logit),data=d)
  a <- BIC(model4)
 
  return(a)
}

fpca5 <- function(stat5,indicies){
  d<-stat5[indicies,]
  model5 <- glm(stat5$pr5 ~pca5$scores[,1:4], family=binomial(logit),data=d)
  a <- BIC(model5)
 
  return(a)
}

boot1respca <- boot(data=stat1, statistic = fpca1, R=1000)


boot2respca <- boot(data=stat2, statistic = fpca2, R=1000)


boot3respca <- boot(data=stat3, statistic = fpca3, R=1000)


boot4respca <- boot(data=stat4, statistic = fpca4, R=1000)


boot5respca <- boot(data=stat5, statistic = fpca5, R=1000)

#so you can see what you did
print(boot1respca)
print(boot2respca)
print(boot3respca)
print(boot4respca)
print(boot5respca)
