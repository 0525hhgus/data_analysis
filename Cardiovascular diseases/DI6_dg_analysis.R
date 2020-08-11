
setwd("C:/Users/USER/Cardiovascular diseases")

data3<-read.csv("DI6_dg.csv")

attach(data3)

train3<-data3[1:4000, ]
test3<-data3[4001:6030, ]

# Random Forest3
library(randomForest)
rf_out3<-randomForest(as.factor(DI6_dg)~.,data=train3, importance=T, mtry=4)
rf_out3
round(importance(rf_out3), 2)
varImpPlot(rf_out3)

library(caret)
rfpred3<-predict(rf_out3,test3)
# rfpred1

library (e1071)
confusionMatrix(as.factor(rfpred3),as.factor(test3$DI6_dg))
