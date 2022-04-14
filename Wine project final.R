library(MASS)
install.packages("car")
library(car)
install.packages("SignifReg")
library(SignifReg)
library(tidyverse)
library(caret)
library(nnet)
Wine = read.csv("winequality-both.csv", header = TRUE)
Wine2 = read.csv("winequality-both.csv", header = TRUE)
#Setting up Data partition
set.seed(123)
random_sample <- createDataPartition(Wine $ quality,p = 0.7, list = FALSE)
train<- Wine[random_sample, ]
test<- Wine[-random_sample, ]
#Building Multinomial logistic model
Wine$quality<-as.factor(Wine$quality)
train$quality <-relevel(train$quality, ref="3")
M1 <-multinom(quality~.-total.sulfur.dioxide-citric.acid,data = train)
summary(M1)
#Two tailed Z test
z<-summary(M1)$coefficients/summary(M1)$standard.errors
p <- (1-pnorm(abs(z),0,1))*2
p
#Misclassification matrix- Train set
pred<-predict(M1,train)
head(pred)
head(train$quality)
t<- table(train$quality,pred)
t
1 - sum(diag(t))/sum(t)
#Misclassification matrix- Test set
pred1<-predict(M1,test)
head(pred1)
head(test$quality)
t1<- table(test$quality,pred1)
t1
1 - sum(diag(t1))/sum(t1)
#Prediction and Model Assessment
n <- table(train$quality)
n/sum(n)
t3<-t/rowSums(t)
t4<-t1/rowSums(t1)
#Linear model construction
lr<-lm(quality~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+
total.sulfur.dioxide+density+pH+sulphates+alcohol+Type, data = Wine)

lf<-lm(quality~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+
total.sulfur.dioxide+density+pH+sulphates+alcohol+fixed.acidity*Type+volatile.acidity*Type+citric.acid*Type+residual.sugar*Type+chlorides*Type+free.sulfur.dioxide*Type+
total.sulfur.dioxide*Type+density*Type+pH*Type+sulphates*Type+alcohol*Type+Type, data = Wine)

summary(lr)
summary(lf)
anova(lr,lf)
#Variable selection
intercept_only<-lm(quality~1, data = train)

a_step<-stepAIC(intercept_only,scope = ~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+
total.sulfur.dioxide+density+pH+sulphates+alcohol+fixed.acidity*Type+volatile.acidity*Type+citric.acid*Type+residual.sugar*Type+chlorides*Type+free.sulfur.dioxide*Type+
total.sulfur.dioxide*Type+density*Type+pH*Type+sulphates*Type+alcohol*Type+Type,direction = "both",trace = 1)
a_step
AIC(a_step)
b_step<-stepAIC(intercept_only,scope = ~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+
total.sulfur.dioxide+density+pH+sulphates+alcohol+fixed.acidity*Type+volatile.acidity*Type+citric.acid*Type+residual.sugar*Type+chlorides*Type+free.sulfur.dioxide*Type+
total.sulfur.dioxide*Type+density*Type+pH*Type+sulphates*Type+alcohol*Type+Type,direction = "both",trace = 1,k = log(6497))
b_step
AIC(b_step)
c_R2<-SignifReg(intercept_only,scope = ~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+
total.sulfur.dioxide+density+pH+sulphates+alcohol+fixed.acidity*Type+volatile.acidity*Type+citric.acid*Type+residual.sugar*Type+chlorides*Type+free.sulfur.dioxide*Type+
total.sulfur.dioxide*Type+density*Type+pH*Type+sulphates*Type+alcohol*Type+Type,direction = "both",criterion = "r-adj",trace = TRUE)
c_R2
AIC(c_R2)
summary(a_step)
summary(b_step)
summary(c_R2)
vif(c_R2)
c_R2<-lm(quality ~ alcohol + volatile.acidity + sulphates + Type + density + free.sulfur.dioxide + 
fixed.acidity + chlorides + total.sulfur.dioxide + volatile.acidity:Type + 
Type:density + residual.sugar:Type + Type:pH + alcohol:Type + 
Type:total.sulfur.dioxide, data = train)
#Cross Validation using model
set.seed(123)
random_sample <- createDataPartition(Wine $ quality,p = 0.7, list = FALSE)
train  <- Wine[random_sample, ]
test<- Wine[-random_sample, ]
rmodel<-lm(quality ~ alcohol + volatile.acidity + sulphates + Type + density + free.sulfur.dioxide + 
             fixed.acidity + chlorides + total.sulfur.dioxide + volatile.acidity:Type + 
             Type:density + residual.sugar:Type + Type:pH + alcohol:Type + 
             Type:total.sulfur.dioxide, data = train)
predictions <- predict(rmodel, test)
predictions <- predict(c_R2, test)
data.frame( R2 = R2(predictions, test $ quality),
            RMSE = RMSE(predictions, test $ quality),
            MAE = MAE(predictions, test $ quality))
