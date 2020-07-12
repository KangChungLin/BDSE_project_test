ecom1911_small <- read_csv("ecom1911_small.csv")

# 轉成時間的資料型態
library(lubridate)
ecom1911_small$event_time <- as_datetime(ecom1911_small$event_time)
class(ecom1911_small$event_time)

# 計算時間差(以秒為單位)
x <- difftime(ecom1911_small$event_time[1000],ecom1911_small$event_time[1],units = 'secs')
as.numeric(x)

# 計算每個session的時間(以秒為單位)
library(dplyr)
ecom1911_small %>% group_by(user_session,user_id) %>% 
  summarise(time_secs=as.numeric(difftime(max(event_time),min(event_time),units = 'secs'))) ->  session_time
mean(session_time$time_secs)
max(session_time$time_secs)
# plot
library(ggplot2)
sum(session_time$time_secs==0)/length(session_time$time_secs) # 35% one event
q1 <- quantile(session_time$time_secs/60, 0.5)                # 0.983 mins
q2 <- quantile(session_time$time_secs/60, 0.75)               # 4.12 mins
q3 <- quantile(session_time$time_secs/60, 0.9)                # 10.8 mins
ggplot(data = session_time) + 
  geom_histogram(aes(x = time_secs/60),binwidth = 0.5) +
  xlim(0,50) + ylim(0,21000) +
  geom_vline(xintercept = c(q1,q2,q3),linetype=c(2,4,5)) +
  labs(title = 'Length of Stay', x='mins')
# statics
max(ecom1911_small$event_time)-min(ecom1911_small$event_time)
# save file
write.csv(session_time,'session_time.csv',quote = F,row.names = F)
# check data
length(unique(session_time$user_session))
length(unique(session_time$user_id))
duplicated(session_time$user_session)
session_time[duplicated(session_time$user_session,fromLast = T),]


# 計算每個session的各個動作(view,cart,purchase)的次數
unique(ecom1911_small$event_type)
library(dplyr)
ecom1911_small %>% group_by(user_session,user_id) %>% 
  summarise(view=sum(event_type=='view'),cart=sum(event_type=='cart'),purchase=sum(event_type=='purchase')) -> ecom_event
head(ecom_event)
# 加總所有event
ecom_event$total_event <- rowSums(ecom_event[,3:5])
# save file
write.csv(ecom_event,'ecom_event.csv',quote = F,row.names = F)

