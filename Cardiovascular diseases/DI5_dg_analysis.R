
setwd("C:/Users/USER/Cardiovascular diseases")

data2<-read.csv("DI5_dg.csv")

attach(data2)

train2<-data2[1:4000, ]
test2<-data2[4001:6030, ]

# Random Forest2
library(randomForest)
rf_out2<-randomForest(as.factor(DI5_dg)~.,data=train2, importance=T, mtry=4)
rf_out2
round(importance(rf_out2), 2)
varImpPlot(rf_out2)

library(caret)
rfpred2<-predict(rf_out2,test2)
# rfpred2

library (e1071)
confusionMatrix(as.factor(rfpred2),as.factor(test1$DI5_dg))
