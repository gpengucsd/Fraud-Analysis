library(tidyverse)
library(ModelMetrics)
data <- read.csv('vars_final.csv')
data1 <- read.csv('card transactions.csv')%>%
  select(Recnum,Date)
data <- data %>% 
  left_join(data1, by ='Recnum')%>%
  mutate(Date = as.Date(Date,'%m/%d/%y'))%>%
  mutate(Fraud = as.factor(Fraud))%>%
  select(-Recnum)

data_zscaled<-sapply(data[,2:21], function(x) scale(x))
data_zscaled<-as.data.frame(data_zscaled)
data_zscaled$Fraud = data$Fraud
data_zscaled$Date = data$Date



oot <- data_zscaled %>%
  filter(Date > as.Date("2010-11-01"))
normal <- data_zscaled%>%
  anti_join(oot)%>%
  select(-Date)
train <- sample_n(normal,nrow(normal)*0.7)
test <- normal %>%
  anti_join(train)

write.csv(test,'test.csv')
write.csv(oot,'oot.csv')

normal %>%
  group_by(Fraud)%>%
  summarise(n=n())

train_positive <- train %>%
  filter(Fraud == 1)
train_nagetive <- train %>%
  filter(Fraud== 0)
train_new <- sample_n(train_positive,replace = TRUE,size = nrow(train_nagetive))%>%
  full_join(train_nagetive)
write.csv(train,'train_full.csv')

##
a<-colnames(train_new)[-1][-1]
library(radiant)
result <- logistic(
  train_new, 
  rvar = "Fraud", 
  evar = list(a), 
  lev = "1"
)
###############train FDR 0.59
pred <- predict(result, pred_data = train)

train <- store(train, pred, name = "pred_log")
train <- train %>% mutate(prediction = ifelse(pred_log>=0.5,1,0))

Fraud_sum <- sum(as.numeric(as.character(train$Fraud)))


FDR_ratio <- train %>%
  filter(train$pred_log >unname(quantile(train$pred_log,0.97)))
ratio = sum(as.numeric(as.character(FDR_ratio$Fraud)))/Fraud_sum

##########################0.57
pred <- predict(result, pred_data = test)

test <- store(test, pred, name = "pred_log")
test <- test %>% mutate(prediction = ifelse(pred_log>=0.5,1,0))

Fraud_sum <- sum(as.numeric(as.character(test$Fraud)))


FDR_ratio <- test %>%
    filter(test$pred_log >unname(quantile(test$pred_log,0.97)))
ratio = sum(as.numeric(as.character(FDR_ratio$Fraud)))/Fraud_sum
#######0.27
pred <- predict(result, pred_data = oot)

oot <- store(oot, pred, name = "pred_log")
oot <- oot %>% mutate(prediction = ifelse(pred_log>=0.5,1,0))

Fraud_sum <- sum(as.numeric(as.character(oot$Fraud)))


FDR_ratio <- oot %>%
  filter(oot$pred_log >unname(quantile(oot$pred_log,0.97)))
ratio = sum(as.numeric(as.character(FDR_ratio$Fraud)))/Fraud_sum

###### 0.69

library(radiant)
set.seed(1234)
train_list <- c()
test_list <-c()
oot_list <-c()

a<-colnames(train_new)[-21]
result_nn <- nn(
  train_new, 
  rvar = "Fraud", 
  evar = list(a), 
  lev = "1",
  size =5,
  decay =0.1
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
oot_list <-append(oot_list,ratio_oot)


######nn ks 0.75  FDR 3% 0.67 ROC 0.93
###





set.seed(13)
result_nn2 <- nn(
  train_new, 
  rvar = "Fraud", 
  evar = list(a), 
  lev = "1",
  size =12,
  decay =0.1
)



pred <- predict(result_nn2, pred_data = test)

test <- store(test, pred, name = "pred_nn")
test <- test %>% mutate(prediction = ifelse(pred_nn>=0.5,1,0))
TabPred <- table(test$Fraud,test$prediction)
Accuracy <- (TabPred[1,1]+TabPred[2,2])/sum(TabPred)
print(Accuracy)
TabPred
print(auc(test$pred_nn,test$Fraud,lev=1))
KS(test$pred_nn,test$Fraud)
FDR_ratio <- test %>%
  filter(test$pred_nn >unname(quantile(test$pred_nn,0.97)))
ratio = sum(as.numeric(as.character(FDR_ratio$Fraud)))/Fraud_sum

####up sample nn  KS 0.79 auc 0.95 FDR 0.754

#####
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
