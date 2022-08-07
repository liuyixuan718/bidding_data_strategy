library(tidyverse)
library(janitor)
library(lubridate)
#install.packages('lubridate')
#install.packages('janitor')

daily <- read_csv('dailymotion_2021-10_2022-04-01.csv')
roll <- read_csv('rollingstone_2021-10_2022-04-01.csv')
taste <- read_csv('tasteofhome_2021-10_2022-04-01.csv')

#Data Profiling 
#colnames(daily)
#colnames(roll)
#colnames(taste)
#unique(daily$`Remote Feed Name`) #51 unique feed name 
#class(daily$Date)
#class(roll$Date)
#class(taste$Date)

daily$Date <- as.Date(daily$Date, "%m/%d/%y") #convert character to date 

# add a publisher name column for all 3 data frames 
daily$publisher_name <- 'dailymotion'  
roll$publisher_name <- 'rollingstone'  
taste$publisher_name <- 'tasteofhome'  

# merge dfs together 
df <- daily%>%
  bind_rows(roll)%>%
  bind_rows(taste)

# 1.clean column name: lower case + use "_" replace " " 
# 2.add 3 new columns: hour_of_day1, day_of_month, day_of_week, month 
# 3.add 2 new columns: avg_price, win_prob, catapultx_revenue 
df <- df%>%
  janitor:: clean_names()

df <- df%>%
  mutate(hour_of_day_1=hour(strptime(df$hour_of_day,'%H:%M'))
         %>% as.numeric())%>%
  mutate(day_of_month= format(date, format='%d')%>% as.numeric())%>%
  mutate(month = format(df$date,"%m")%>% as.numeric())%>%
  mutate(day_of_week= wday(date, week_start = 1))%>%
  mutate(avg_price= publisher_wins_price/publisher_impression_wins)%>%
  mutate(win_prob=publisher_impression_wins/remote_feed_responses)%>%
  mutate(catapultx_revenue = remote_feed_gross_revenue - publisher_estimated_revenue)

write_csv(df,'cleaned2.csv')









