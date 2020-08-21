library(dplyr)
stock <- subset(stock_trend_268cols,select = -Website)
stock %>% group_by(Date) %>% summarise_all(sum) -> stock
unique(stock_trend_268cols$Website)

write.csv(stock,'stock_byDate.csv',row.names=F,quote = F,fileEncoding = "UTF-8" )


# mobile01 moneydj ptt
# gihun commercialtimes businesstoday
# bussinessweekly wealth moneyweekly_final statementdog gihun_blog

stock <- stock_trend_268cols[stock_trend_268cols$Website=='mobile01' | stock_trend_268cols$Website=='ptt'| stock_trend_268cols$Website=='moneydj',]
unique(stock$Website)
stock <- subset(stock,select = -Website)
stock %>% group_by(Date) %>% summarise_all(sum) -> stock
write.csv(stock,'stock_byDate_platform.csv',row.names=F,quote = F,fileEncoding = "UTF-8" )

stock <- stock_trend_268cols[stock_trend_268cols$Website=='gihun' | stock_trend_268cols$Website=='commercialtimes'| stock_trend_268cols$Website=='businesstoday',]
unique(stock$Website)
stock <- subset(stock,select = -Website)
stock %>% group_by(Date) %>% summarise_all(sum) -> stock
write.csv(stock,'stock_byDate_news.csv',row.names=F,quote = F,fileEncoding = "UTF-8" )

stock <- stock_trend_268cols[stock_trend_268cols$Website=='bussinessweekly' | stock_trend_268cols$Website=='wealth'| stock_trend_268cols$Website=='moneyweekly_final' | stock_trend_268cols$Website=='statementdog' | stock_trend_268cols$Website=='gihun_blog',]
unique(stock$Website)
stock <- subset(stock,select = -Website)
stock %>% group_by(Date) %>% summarise_all(sum) -> stock
write.csv(stock,'stock_byDate_blog.csv',row.names=F,quote = F,fileEncoding = "UTF-8" )

# sort by month
library(lubridate)
as.Date(stock_byDate$Date[1],format='%Y-%m')
#month(stock_byDate$Date[1])
stock_byDate$year <- year(stock_byDate$Date)
stock_byDate$month <- month(stock_byDate$Date)
stock_byDate <- subset(stock_byDate,select = -Date)
stock_byDate %>% group_by(year,month) %>% summarise_all(sum) -> stock
write.csv(stock,'stock_byMonth.csv',row.names=F,quote = F,fileEncoding = "UTF-8" )
