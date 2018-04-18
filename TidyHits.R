library(tidyverse)
library(data.table)

stocklist <- list.files(path = "Stocks/")

for (stock in stocklist) {
  
  stock.A <- fread(paste('Stocks/', stock, sep = ''), header = T, sep = ',')
  stock.A$Time <- sub("(\\d+)(\\d{2})", "\\1:\\2", stock.A$Time)
  stock.A$Date <- paste(stock.A$Date, stock.A$Time)
  stock.A$Date <- lubridate::mdy_hm(stock.A$Date)
  stock.A <- stock.A %>% 
    select(-Time) %>% 
    rename(Datetime = 'Date')
  #stock.A$Datetime <- stock.A$Datetime %>% 
  #  lubridate::force_tz(tzone = 'EST')
  stock.A$Hourly <- lubridate::ymd_hm(
    paste(lubridate::date(stock.A$Datetime), 
          ' ', 
          as.character(lubridate::hour(stock.A$Datetime)),
          ':',
          as.character(((lubridate::minute(stock.A$Datetime) - 30) >= 0) * 30),
          sep = '')
  ) # 30-minutes per session
  stock.A$Daily <- lubridate::floor_date(stock.A$Datetime, unit = 'day')
  stock.A <- stock.A %>% 
    filter(lubridate::hour(Hourly) >= 10 & lubridate::hour(Hourly) < 16) %>% 
    arrange(Hourly)# drop those earlier than 10:00 and no later than 16:00
  
  DailySum <- stock.A %>% 
    group_by(Daily) %>% 
    summarise(High = max(High), Low = min(Low), Volume = sum(Volume)) %>% 
    mutate(Daily = lubridate::date(Daily))
  
  HourlySum <- stock.A %>% 
    group_by(Hourly) %>% 
    summarise(High = max(High), Low = min(Low), Volume = sum(Volume)) %>% 
    mutate(Daily = lubridate::date(Hourly)) %>% 
    mutate(Yearly = lubridate::year(Hourly)) %>% 
    left_join(DailySum, by = c('Daily' = 'Daily')) %>% 
    select(Hourly, Daily, Yearly, High = High.x, Low = Low.x, 
           Volume = Volume.x, DailyHigh = High.y, 
           DailyLow = Low.y, DailyVolume = Volume.y)
  # Now data is tidy. no NA entry.
  
  # count hits
  HourlyHit <- HourlySum %>% 
    mutate(HighHit = (High == DailyHigh), LowHit = (Low == DailyLow)) %>% 
    select(Hourly, Daily, Yearly, HighHit, LowHit) 
  
  #before screen multiple hits
  #HourlyHit %>% 
  #  group_by(Daily) %>% 
  #  select(Hourly, HighHit, LowHit) %>% 
  #  mutate(Hour = paste(lubridate::hour(Hourly), ':', lubridate::minute(Hourly), sep = '')) %>%
  #  group_by(Hour) %>% summarise(sum(HighHit), sum(LowHit))
  
  # Only count first hits
  #    rev.    cumprod.   rev.    cumsum.   first '1' 
  # 0       1           1       0         0             0
  # 1       0           0       1         1             1
  # 0  -->  1  ----->   0  -->  1  -----> 2 --------->  0
  # 1       0           0       1         3             0
  # 0       1           0       1         4             0
  #
  
  HourlyHit %>% 
    group_by(Daily) %>% 
    mutate(TransHigh_1 = !(HighHit), 
           TransLow_1 = !(LowHit)) %>% 
    mutate(TransHigh_2 = cumprod(TransHigh_1), 
           TransLow_2 = cumprod(TransLow_1)) %>% 
    mutate(TransHigh_3 = !(TransHigh_2), 
           TransLow_3 = !(TransLow_2)) %>% 
    mutate(TransHigh_4 = cumsum(TransHigh_3), 
           TransLow_4 = cumsum(TransLow_3)) %>% 
    mutate(TransHigh_5 = (TransHigh_4 == 1), 
           TransLow_5 = (TransLow_4 == 1)) %>% 
    select(Hourly,
           Yearly,
           TransHigh_5, 
           TransLow_5) %>% 
    rename(NewHighHit = TransHigh_5, 
           NewLowHit = TransLow_5) %>% 
    mutate(Hour = paste(lubridate::hour(Hourly), ':', lubridate::minute(Hourly), sep = '')) %>%
    select(-Hourly, -Daily) %>% rename(Year = Yearly) %>% ungroup(Daily) %>% 
    mutate(`Year-Hour` = paste(Year, Hour, sep = ' - ')) %>% 
    group_by(`Year-Hour`) %>% summarise(High = sum(NewHighHit), Low = sum(NewLowHit)) %>% 
    mutate(Year = stringr::str_sub(`Year-Hour`, 1, 4), Hour = stringr::str_sub(`Year-Hour`, 8, -1)) %>%
    select(Year, Hour, High, Low) %>% 
    gather(`High`, `Low`, key = 'Hits', value = 'Cases') %>% 
    add_column(Stock_Name = stringr::str_sub(stock,1,-5)) %>% 
    write.table(file="./TidyHits.csv", 
                append = T, 
                sep=',', 
                row.names=F, 
                col.names=F)
}
