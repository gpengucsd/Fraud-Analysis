library(dplyr)
library(dataQualityR)
library(readr)
data <- read_csv('card transactions.csv')%>%
  select(Recnum,Cardnum,Date,Merchnum,`Merch description`,`Merch state`,`Merch zip`,Transtype,Amount,Fraud)%>%
  mutate(Date = as.Date(Date,'%m/%d/%y'))
data <- mutate_at(data, .vars = vars(Fraud,Transtype,`Merch state`,`Merch zip`,Recnum,Cardnum,Merchnum,Date),
                  .funs = as.factor)

library(misc)
table <- summary(data)

checkDataQuality(data= data, out.file.num= "dq_num.csv", out.file.cat= "dq_cat.csv",numeric.cutoff = 1)
sd(data$list)

data %>% group_by(Date)%>% summarize(n=n())%>% arrange (desc(n))%>%slice(1:10) %>%ggplot(., aes(x=reorder(Date, -n), y=n))+
  geom_bar(stat='identity')+scale_x_discrete(name ="Date")+theme(axis.text=element_text(size=15),
                                                                 axis.title=element_text(size=15,face="bold"))


data %>% group_by(Merchnum)%>% summarize(n=n())%>% arrange (desc(n))%>%slice(1:10) %>%ggplot(., aes(x=reorder(Merchnum, -n), y=n))+
  geom_bar(stat='identity')+scale_x_discrete(name ="Merchnum")+theme(axis.text=element_text(size=15),
                                                                 axis.title=element_text(size=15,face="bold"))
data %>% group_by(`Merch description`)%>% summarize(n=n())%>% arrange (desc(n))%>%slice(1:10) %>%ggplot(., aes(x=reorder(`Merch description`, -n), y=n))+
  geom_bar(stat='identity')+scale_x_discrete(name ="Merch description")+theme(axis.text=element_text(size=9),
                                                                     axis.title=element_text(size=15,face="bold"))

data %>% group_by(`Merch state`)%>% summarize(n=n())%>% arrange (desc(n))%>%slice(1:10) %>%ggplot(., aes(x=reorder(`Merch state`, -n), y=n))+
  geom_bar(stat='identity')+scale_x_discrete(name ="Merch State")+theme(axis.text=element_text(size=15),
                                                                              axis.title=element_text(size=15,face="bold"))

data %>% filter(is.na(`Merch zip`)==FALSE )%>%group_by(`Merch zip`)%>% summarize(n=n())%>% arrange (desc(n))%>%slice(1:10) %>%ggplot(aes(x=reorder(`Merch zip`, -n), y=n))+
  geom_bar(stat='identity')+scale_x_discrete(name ="Merch Zip")+theme(axis.text=element_text(size=15), axis.title=element_text(size=15,face="bold"))
                                                                        
data %>% group_by(Transtype)%>% summarize(n=n())%>% arrange (desc(n))%>%slice(1:10) %>%ggplot(., aes(x=reorder(Transtype, -n), y=n))+
  geom_bar(stat='identity')+scale_y_log10()+scale_x_discrete(name ="Transtype")+theme(axis.text=element_text(size=20),
                                                                     axis.title=element_text(size=20,face="bold"))

data %>% ggplot(aes(x= Fraud))+geom_bar(stat = 'count')+scale_y_log10()+theme(axis.text=element_text(size=20),
                                                                              axis.title=element_text(size=20,face="bold"))
data%>%filter(Amount!=3102045.53) %>%ggplot(aes(x= Amount))+ geom_histogram()+scale_x_log10()+theme(axis.text=element_text(size=20),
                                                                                                    axis.title=element_text(size=20,face="bold"))

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.01, .95), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

