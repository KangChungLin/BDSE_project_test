library(readr)
word20207 <- read_csv("tf_table/20207.csv")

library(DBI)
library(RMariaDB)
library(pool)

drv <- dbDriver("MariaDB")
con <- dbPool(drv, username="root", password="", dbname ="stock", host="localhost")

res <- dbGetQuery(con, "SELECT * FROM stock20207;")
df <- res[order(res$Counts,decreasing = T),]
df <- df[1:10,]

wordcloud2::wordcloud2(res ,color = "random-light", backgroundColor = "grey" )

library(ggplot2)
library(readr)
df <- read_csv("stock_hot/20207.csv")
df <- df[order(df$Counts,decreasing = T),]
df <- df[1:10,]
ggplot(df,aes(x = reorder(Name, -Counts),y = Counts) ) +
  geom_bar(stat = "identity", colour = gray(0.5), fill = gray(0.5)) +
  labs(x = '股票名稱', y = '網路聲量') +
  theme(axis.title.x = element_text(size=14,face = "bold"),
        axis.text.x = element_text(size=10,face = "bold"),
        axis.title.y = element_text(size=14,face = "bold"),
        axis.text.y = element_text(size=10,face = "bold"))
library(dplyr)
price2330 <- read_csv("stock_price/2330.csv")
#price2330 <- subset(price2330,select = c(Date, Close))
price2330 <- price2330[(price2330$Date<as.Date('2020-08-01')) & (price2330$Date>=as.Date('2020-07-01')),]
stock_byDate <- read_csv("stock_byDate/stock_byDate.csv")
df2330 <- subset(stock_byDate,select = c(Date, 台積電))
df2330 <- df2330[(df2330$Date<as.Date('2020-08-01')) & (df2330$Date>=as.Date('2020-07-01')),]
df <- merge(df2330,price2330, by='Date',all.x = T)
colnames(df) <- c('Date','Counts','Close')

Sys.setlocale("LC_TIME", "US")
p3 <- ggplot(data = df) +
  geom_bar(aes(y = Counts, x = Date), stat = "identity")+
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d")


  
ggplot(data = price2330) +
  geom_line(aes(y = Close, x = Date), stat = "identity")+
  geom_point(aes(y = Close, x = Date))+
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d")

library(tidyverse)
library(gridExtra)
library(quantmod)
library(cowplot)

p3 <- ggplot(data = df) +
  geom_bar(aes(y = Counts, x = Date), stat = "identity",show.legend = FALSE)+
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d")+
  theme(axis.title = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

p1 <- price2330 %>%
  arrange(Date) %>%
  mutate(ma5 = SMA(Close, n = 5, align =  "right"),
         ma10 = SMA(Close, n = 10, align = "right")) %>%
  ggplot(aes(x = Date)) +
  geom_boxplot(aes(lower = pmin(Close, Open),
                   middle = Close,
                   upper = pmax(Close, Open),
                   ymin = Low,
                   ymax = High,
                   group = Date,
                   fill = Open > Close),
               stat = "identity",
               show.legend = FALSE) +
  #geom_line(aes(y = ma5), color = "blue3") +
  #geom_line(aes(y = ma10), color = "red") +
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d")+
#  scale_x_continuous(breaks = breaks,
#                     labels = NULL,
#                     expand = c(0, 0)) +
  theme(axis.ticks.x = element_blank(),
        axis.title = element_blank())

p1

p2 <- price2330 %>%
  arrange(Date) %>%
  mutate(vol_ma5 = SMA(Volume, n = 5, align =  "right"),
         vol_ma10 = SMA(Volume, n = 10, align = "right")) %>%
  ggplot(aes(x = Date, y = Volume)) +
  geom_bar(stat = "identity",
           aes(fill = Open > Close),
           show.legend = FALSE) +
#  geom_line(aes(y = vol_ma5), color = "blue3") +
#  geom_line(aes(y = vol_ma10), color = "red") +
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d")+
#  scale_x_continuous(breaks = breaks,
#                     labels = format(labels, "%m-%d"),
#                     expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0.5)) +
  theme(axis.title = element_blank())
p2

plot_grid(p3, p1, p2, align = "v", nrow = 3, rel_heights = c(2/5, 2/5, 1/5))






library(cowplot)
library(ggplot2)
library(readr)
library(dplyr)

# 讀取台積電股價檔案
price2330 <- read_csv("stock_price/2330.csv")
# 選取2020年7月的資料
price2330 <- price2330[(price2330$Date<as.Date('2020-08-01')) & (price2330$Date>=as.Date('2020-07-01')),]

# 讀取各股票聲量變化的檔案
stock_byDate <- read_csv("stock_byDate/stock_byDate.csv")
# 只取出台積電的資料
df2330 <- subset(stock_byDate,select = c(Date, 台積電))
# 選取2020年7月的資料
df <- df2330[(df2330$Date<as.Date('2020-08-01')) & (df2330$Date>=as.Date('2020-07-01')),]
# 欄位重新命名
colnames(df) <- c('Date','Counts')

# 更改系統預設語言(改成英文)
Sys.setlocale("LC_TIME", "US")

# 聲量圖
p1 <- ggplot(data = df) +
  geom_bar(aes(y = Counts, x = Date), stat = "identity",show.legend = FALSE)+
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d")+
  theme(axis.title = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
p1

price2330$Color <- ifelse(price2330$Close>price2330$Open, 'red', 'green')

# k棒圖
p2 <- price2330 %>%
  arrange(Date) %>%
  ggplot(aes(x = Date)) +
  geom_boxplot(aes(lower = pmin(Close, Open),
                   middle = Close,
                   upper = pmax(Close, Open),
                   ymin = Low,
                   ymax = High,
                   group = Date,
                   fill = Color),
               stat = "identity",
               show.legend = FALSE) +
  scale_fill_manual(values = c('#008000','#ff0000'))+
  scale_color_manual(values = c('#008000','#ff0000'))+
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d")+
  theme(axis.ticks.x = element_blank(),
        axis.title = element_blank())
p2

# 
p3 <- price2330 %>%
  arrange(Date) %>%
  ggplot(aes(x = Date, y = Volume)) +
  geom_bar(stat = "identity",
           aes(fill = Color),
           show.legend = FALSE) +
  scale_fill_manual(values = c('#008000','#ff0000'))+
  scale_color_manual(values = c('#008000','#ff0000'))+
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d")+
  scale_y_continuous(expand = c(0, 0.5)) +
  theme(axis.title = element_blank())
p3

plot_grid(p1, p2, p3, align = "v", nrow = 3, rel_heights = c(2/5, 2/5, 1/5))


df <- read_csv("C:/Users/Kang/Downloads/4743_history.csv")
df <- df[,c(1:5,8)]
colnames(df)[6] <- 'Volume'
price4743 <- df
price4743 <- price4743[(price4743$Date<as.Date('2020-08-01')) & (price4743$Date>=as.Date('2020-07-01')),]

# 讀取各股票聲量變化的檔案
stock_byDate <- read_csv("stock_byDate/stock_byDate.csv")
# 只取出合一的資料
df4743 <- subset(stock_byDate,select = c(Date, 合一))
# 選取2020年7月的資料
df <- df4743[(df4743$Date<as.Date('2020-08-01')) & (df4743$Date>=as.Date('2020-07-01')),]
# 欄位重新命名
colnames(df) <- c('Date','Counts')

# 聲量圖
p1 <- ggplot(data = df) +
  geom_bar(aes(y = Counts, x = Date), stat = "identity",show.legend = FALSE)+
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d")+
  theme(axis.title = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
p1

price4743$Color <- ifelse(price4743$Close>price4743$Open, 'red', 'green')

# k棒圖
p2 <- price4743 %>%
  arrange(Date) %>%
  ggplot(aes(x = Date)) +
  geom_boxplot(aes(lower = pmin(Close, Open),
                   middle = Close,
                   upper = pmax(Close, Open),
                   ymin = Low,
                   ymax = High,
                   group = Date,
                   fill = Color),
               stat = "identity",
               show.legend = FALSE) +
  scale_fill_manual(values = c('#008000','#ff0000'))+
  scale_color_manual(values = c('#008000','#ff0000'))+
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d")+
  theme(axis.ticks.x = element_blank(),
        axis.title = element_blank())
p2

# 
p3 <- price4743 %>%
  arrange(Date) %>%
  ggplot(aes(x = Date, y = Volume)) +
  geom_bar(stat = "identity",
           aes(fill = Color),
           show.legend = FALSE) +
  scale_fill_manual(values = c('#008000','#ff0000'))+
  scale_color_manual(values = c('#008000','#ff0000'))+
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d")+
  scale_y_continuous(expand = c(0, 0.5)) +
  theme(axis.title = element_blank())
p3

plot_grid(p1, p2, p3, align = "v", nrow = 3, rel_heights = c(2/5, 2/5, 1/5))



df <- read_csv("stock_hot/20206.csv")
df <- df[order(df$Counts,decreasing = T),]
df <- df[1:10,]
ggplot(df,aes(x = reorder(Name, -Counts),y = Counts) ) +
  geom_bar(stat = "identity", colour = gray(0.5), fill = gray(0.5)) +
  labs(x = '', y = '') +
  theme(axis.text.x = element_text(size=12,face = "bold"),
        axis.text.y = element_text(size=12,face = "bold"))

stock_byDate <- read_csv("stock_byDate/stock_byDate.csv")
temp <- stock_byDate[,-1]
total <- apply(temp, 2, sum)
df <- data.frame(name = colnames(temp),counts = total)
df <- df[order(df$counts,decreasing = T),]
df <- df[1:10,]
ggplot(df,aes(x = reorder(name, -counts),y = counts) ) +
  geom_bar(stat = "identity", colour = gray(0.5), fill = gray(0.5)) +
  labs(x = '', y = '') +
  theme(axis.text.x = element_text(size=12,face = "bold"),
        axis.text.y = element_text(size=12,face = "bold"))

df <- read_csv("stock_hot/20206.csv")
wordcloud2(df,color = "random-light",shape='diamond')
