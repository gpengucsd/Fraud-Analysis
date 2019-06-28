train_list <- c()
test_list <-c()
oot_list <-c()


for (i in 10:11){for (k in c(0.1,0.3,0.5)){

result_nn <- nn(
  train_new, 
  rvar = "Fraud", 
  evar = list(a), 
  lev = "1",
  size =i,
  decay =k
)

pred <- predict(result_nn, pred_data = test)
pred$Fraud <- test$Fraud
Fraud_sum <- sum(as.numeric(as.character(test$Fraud)))
FDR_ratio <- pred %>%
  filter(pred$Prediction >unname(quantile(pred$Prediction,0.97)))
ratio_test = sum(as.numeric(as.character(FDR_ratio$Fraud)))/Fraud_sum

###### train 0.79
pred <- predict(result_nn, pred_data = train)
pred$Fraud <- train$Fraud
Fraud_sum <- sum(as.numeric(as.character(train$Fraud)))
FDR_ratio <- pred %>%
  filter(pred$Prediction >unname(quantile(pred$Prediction,0.97)))
ratio_train = sum(as.numeric(as.character(FDR_ratio$Fraud)))/Fraud_sum

####### oot 0.46
pred <- predict(result_nn, pred_data = oot)
pred$Fraud <- oot$Fraud
Fraud_sum <- sum(as.numeric(as.character(oot$Fraud)))
FDR_ratio <- pred %>%
  filter(pred$Prediction >unname(quantile(pred$Prediction,0.97)))
ratio_oot = sum(as.numeric(as.character(FDR_ratio$Fraud)))/Fraud_sum



train_list <-append(train_list,ratio_train)
test_list <-append(test_list,ratio_test)
oot_list <-append(oot_list,ratio_oot)}}

library(randomForest)
fit <- randomForest(x=train_new[,1:20],y=train_new[,21],ntree = 50)
rf.best <-best.randomForest(x=train_new[,1:20],y=train_new[,21])
pred <- predict(fit,test,type = 'prob')
pred <-data.frame(pred)
pred$Fraud <- test$Fraud
Fraud_sum <- sum(as.numeric(as.character(test$Fraud)))
FDR_ratio <- pred %>%
  filter(pred$X1 >unname(quantile(pred$X1,0.97)))
ratio_test = sum(as.numeric(as.character(FDR_ratio$Fraud)))/Fraud_sum



pred <- predict(rf.best,oot,type = 'prob')
pred <-data.frame(pred)
pred$Fraud <- oot$Fraud
Fraud_sum <- sum(as.numeric(as.character(oot$Fraud)))
FDR_ratio <- pred %>%
  filter(pred$X1 >unname(quantile(pred$X1,0.97)))
ratio_oot = sum(as.numeric(as.character(FDR_ratio$Fraud)))/Fraud_sum
###################OOT 0.41
pred <- predict(rf.best,test,type = 'prob')
pred <-data.frame(pred)
pred$Fraud <- test$Fraud
Fraud_sum <- sum(as.numeric(as.character(test$Fraud)))
FDR_ratio <- pred %>%
  filter(pred$X1 >unname(quantile(pred$X1,0.97)))
ratio_TEST = sum(as.numeric(as.character(FDR_ratio$Fraud)))/Fraud_sum
#############0.82 TEST 