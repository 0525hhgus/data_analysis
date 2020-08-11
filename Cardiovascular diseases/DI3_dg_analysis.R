
setwd("C:/Users/USER/Cardiovascular diseases")

data1<-read.csv("DI3_dg.csv")

attach(data1)

cor(data1[0:28])

train1<-data1[1:4000, ]
test1<-data1[4001:6030, ]

# Logistic Regression
rg_model1 <- glm(DI3_dg ~ ., data =train1, family = "binomial")
help("glm")
summary(rg_model1)

# Anova
anova(rg_model1, test="Chisq")


# ROC
library(ROCR)
p1 <- predict(rg_model1, newdata=test1, type="response")
pr1 <- prediction(p1, test1$DI3_dg)
prf1 <- performance(pr1, measure = "tpr", x.measure = "fpr")
plot(prf1)

auc1 <- performance(pr1, measure = "auc")
auc1 <- auc1@y.values[[1]]
auc1

# Random Forest
library(randomForest)
rf_out1<-randomForest(as.factor(DI3_dg)~.,data=train1, importance=T, mtry=4)
rf_out1
round(importance(rf_out1), 2)
varImpPlot(rf_out1)

library(caret)
rfpred1<-predict(rf_out1,test1)
# rfpred1

library (e1071)
confusionMatrix(as.factor(rfpred1),as.factor(test1$DI3_dg))

#svm
m1<-svm(as.factor(DI3_dg)~., data = train1, kernel = "linear")
summary(m1)
m2<-svm(as.factor(DI3_dg)~., data = train1, kernel="polynomial")
summary(m2)
m3<-svm(as.factor(DI3_dg)~., data = train1, kernel="sigmoid")
summary(m3)
m4<-svm(as.factor(DI3_dg)~., data = train1, kernel="radial")
summary(m4)

pred1<-predict(m1,test1)
confusionMatrix(pred1,as.factor(tes1t$DI3_dg))

pred2<-predict(m2,test1)
confusionMatrix(pred2,as.factor(test1$DI3_dg))

pred3<-predict(m3,test1)
confusionMatrix(pred3,as.factor(test1$DI3_dg))

pred4<-predict(m4,test1)
confusionMatrix(pred4,as.factor(test1$DI3_dg))


