# write chinese csv files
write.csv(df1,'C:/Users/Kang/notebooks/gihunblog_url/gihun_blog_7_1_urls.csv',fileEncoding = "UTF-8")

tw_stock <- tw_stock[,c(1,3,5)]
colnames(tw_stock) <- c('Ticker','Start_date', 'Sector')


# convert tw stock list to data frame and saved as csv file
library(readr)
tw_stock <- read_delim("tw_stock.txt", "\t", 
                       escape_double = FALSE, col_names = FALSE, 
                       trim_ws = TRUE)
new_col <- strsplit(tw_stock$Ticker,'\u3000')
col1 <- sapply(new_col, "[", 1)
col2 <- sapply(new_col, "[", 2)
tw_stock$Ticker <- col1
tw_stock$Name <- col2
tw_stock <- tw_stock[c(1,4,2,3)]
write.csv(tw_stock,'tw_stock_final.csv',fileEncoding = "UTF-8")
