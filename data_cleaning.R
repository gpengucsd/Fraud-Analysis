library(dplyr)
library(readr)
data <- read_csv('card transactions.csv')%>%
  select(Recnum,Cardnum,Date,Merchnum,`Merch description`,`Merch state`,`Merch zip`,Transtype,Amount,Fraud)%>%
  mutate(Date = as.Date(Date,'%m/%d/%y'))
data <- mutate_at(data, .vars = vars(Fraud,Transtype,`Merch state`,`Merch zip`,Recnum,Cardnum,Merchnum,Date),
                  .funs = as.factor)
list <- data %>%
  group_by(`Merch description`)%>%
  summarise(Merchnum =unique(Merchnum))

D<- data %>% filter(is.na(Merchnum) != TRUE )%>%filter( Merchnum!= 0) %>%group_by(Merchnum,`Merch description`)%>% summarise(n=n())
D$rownumber = 1:nrow(D)
E<- D %>%group_by(`Merch description`)%>% summarise(max=max(n))
D<- D %>% left_join(E)%>%
  filter(max==n)%>%
  select(Merchnum,`Merch description`)

data_zero <- data %>% filter(is.na(Merchnum) == TRUE | Merchnum== 0) %>% left_join(D,by ='Merch description')
  
data_zero <- data_zero %>% mutate(Merchnum_new =ifelse(is.na(Merchnum.y),`Merch description`,as.character(Merchnum.y)))%>% right_join(data)%>%
  mutate(Merchnum =ifelse(is.na(Merchnum) == TRUE | Merchnum== 0,Merchnum_new,as.character(Merchnum)))%>%
  select(-Merchnum.x)%>%
  select(-Merchnum_new)%>%
  select(-Merchnum.y)
write.csv(data_zero,'data_clean.csv')

data_zero <- data_zero %>%
  mutate(`Merch state` =as.character(`Merch state`))

#####
D<- data_zero %>% filter(is.na(`Merch zip`) != TRUE )%>%group_by(`Merch zip`,`Merch description`)%>% summarise(n=n())
D$rownumber = 1:nrow(D)
E<- D %>%group_by(`Merch description`)%>% summarise(max=max(n))
D<- D %>% left_join(E)%>%
  filter(max==n)%>%
  select(`Merch zip`,`Merch description`)
data_zero1 <- data_zero %>% filter(is.na(`Merch zip`) == TRUE) %>% left_join(D,by ='Merch description')
data_zero1 <- data_zero1 %>% mutate(zip_new =ifelse(is.na(`Merch zip.y`),`Merch description`,as.character(`Merch zip.y`)))%>% right_join(data_zero)%>%
  mutate(`Merch zip` =ifelse(is.na(`Merch zip`) == TRUE,zip_new,as.character(`Merch zip`)))%>%
  select(-`Merch zip.x`)%>%
  select(-zip_new)%>%
  select(-`Merch zip.y`)
####
D<- data_zero1 %>% filter(is.na(`Merch state`) != TRUE )%>%group_by(`Merch state`,`Merch description`)%>% summarise(n=n())
D$rownumber = 1:nrow(D)
E<- D %>%group_by(`Merch description`)%>% summarise(max=max(n))
D<- D %>% left_join(E)%>%
  filter(max==n)%>%
  select(`Merch state`,`Merch description`)
data_zero2 <- data_zero1 %>% filter(is.na(`Merch state`) == TRUE) %>% left_join(D,by ='Merch description')
data_zero2 <- data_zero2 %>% mutate(state_new =ifelse(is.na(`Merch state.y`),`Merch description`,as.character(`Merch state.y`)))%>% right_join(data_zero1)%>%
  mutate(`Merch state` =ifelse(is.na(`Merch state`) == TRUE,state_new,as.character(`Merch state`)))%>%
  select(-`Merch state.x`)%>%
  select(-state_new)%>%
  select(-`Merch state.y`)

write.csv(data_zero2,'data_clean2.csv')
library(tidyverse)
library(ROCR)
data_new <- read.csv('data_python.csv')

data_filter <- data_new %>%
  filter(as.Date(Date) < as.Date("2010-11-01"))%>%
  dplyr::select(-X)%>%
  dplyr::select(-Recnum)
data_filter$random <- round(runif(nrow(data_filter),min=0,max=nrow(data_filter)),0)

KS <- function(pred,depvar){
  p   <- prediction(as.numeric(pred),depvar)
  perf <- performance(p, "tpr", "fpr")
  ks <- max(attr(perf, "y.values")[[1]] - (attr(perf, "x.values")[[1]]))
  return(ks)
}
Fraud_sum <- sum(data_filter$Fraud)

FDR_ratio <- data_filter %>%
  filter(sum_Cardnum_Merch.zip_7d <unname(quantile(sum_Cardnum_Merch.zip_7d,0.03)))%>%
  summarise(ratio = sum(Fraud)/Fraud_sum)

final_table<- data.frame(col_name = colnames(data_filter))

KS_list <-c()  
for (i in colnames(data_filter)){
KS_list <- append(KS_list,KS(data_filter[[i]],data_filter$Fraud))
}
final_table$KS <- KS_list

FDR_list <- c()
for (i in colnames(data_filter)){
  FDR_ratio <- data_filter %>%
    filter(data_filter[[i]] >unname(quantile(data_filter[[i]],0.97)))%>%
    summarise(ratio = sum(Fraud)/Fraud_sum)
  FDR_list <- append(FDR_list,FDR_ratio[1,1])
}
final_table$FDR <- FDR_list
final_table <- final_table%>%
  mutate(KS_rank = round(rank(KS),0))%>%
  mutate(FDR_rank = round(rank(FDR),0))%>%
  mutate(average_rank = (KS_rank+FDR_rank)/2)
write_csv(final_table,'final_table.csv')  

result <- read.csv('final_table.csv')
a<-result[2:(nrow(result)/2),] 
name_list <- as.character(unname(a$col_name))[-79]
data_finish <- data_new %>%
  dplyr::select(name_list)
write_csv(data_finish,'data_finish.csv')  
######
set.seed(1234)
train <- sample_n(data_filter,nrow(data_filter)*0.8)
test <- anti_join(data_filter,train)
library(MASS)
model <- glm(Fraud ~., data = train, family = binomial) %>%
  stepAIC(trace = FALSE,k=5,direction = ('forward'),steps = 500)
# Summarize the final selected model
summary(model)