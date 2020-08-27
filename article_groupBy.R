library(readr)
library(dplyr)
library(lubridate)

df <- read_csv("web_date.csv")

df %>% mutate(year = year(Date), month = month(Date)) %>%
  select(year,month,Website) %>%
  filter(year>2009) -> df

write.csv(df,'websiteByMonth.csv',row.names = F,quote = F)

df %>% group_by(year,Website) %>% summarise_all(length) -> new_df
colnames(new_df)[3] <- 'counts'


# "businesstoday"     "wantgoo"           "bussinessweekly"   "gihun"            
# "gihun_blog"        "mobile01"          "moneydj"           "ptt"              
# "moneyweekly" "statementdog"      "wealth"            "commercialtimes"  
# "gooaye"  

new_df[new_df$Website== "gooaye" ,2] = 'other'
new_df %>% group_by(year,Website) %>% summarise_all(sum) -> new_df
new_df <- new_df[new_df$year>2012,]
new_df <- new_df[new_df$counts>1000,]

library(ggplot2)
ggplot(new_df, aes(x = as.factor(year), y = counts, fill=Website , label = counts)) +
  geom_bar(stat = "identity") +
  geom_text(size=3.5 , position = position_stack(vjust=0.5) , fontface = 'bold') +
  labs(x = '', y = '') +
  theme(axis.text.x = element_text(size=12,face = "bold"),
        axis.text.y = element_text(size=12,face = "bold"))

  




