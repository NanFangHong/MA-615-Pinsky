library(tidyverse)
library(data.table)
tidyHits <- fread('TidyHits.csv', header = F, sep = ',') %>% as.tibble() %>% rename(Year = V1, Hour = V2, BotTop = V3, Hit = V4, Symbol = V5)
companyProfile <- tidyHits %>% select(Symbol) %>% unique()


# ======
# add colum Name, Industry, Sector, Continental and Country 
# Africa.csv, Asia.csv, Europe.csv, 
# Austrialia_and_South_Pacific.csv, 
# Central_America.csv, Middle_East.csv,
# North_America.csv, South_America.csv
# ======


Africa <- fread('CompanyProfile/Africa.csv', header = T, sep = ',') %>% as.tibble() %>% mutate(Continent = 'Africa')
companyProfile %>% inner_join(Africa, by = c("Symbol")) %>% select(Symbol, Name, Continent, Country, Sector, Industry)
# 0 

Asia <- fread('CompanyProfile/Asia.csv', header = T, sep = ',') %>% as.tibble() %>% mutate(Continent = 'Asia')
c1 <- companyProfile %>% inner_join(Asia, by = c("Symbol")) %>% select(Symbol, Name, Continent, Country, Sector, Industry)
# 6 

Europe <- fread('CompanyProfile/Europe.csv', header = T, sep = ',') %>% as.tibble() %>% mutate(Continent = 'Europe')
c2 <- companyProfile %>% inner_join(Europe, by = c("Symbol")) %>% select(Symbol, Name, Continent, Country, Sector, Industry)
c1 <- add_row(c1, Symbol = c2$Symbol, Name = c2$Name, Continent = c2$Continent, Country = c2$Country, Sector = c2$Sector, Industry = c2$Industry)
# 8 

Austrialia_and_South_Pacific <- fread('CompanyProfile/Austrialia_and_South_Pacific.csv', header = T, sep = ',') %>% as.tibble() %>% mutate(Continent = 'Austrialia and South Pacific')
c2 <- companyProfile %>% inner_join(Austrialia_and_South_Pacific, by = c("Symbol")) %>% select(Symbol, Name, Continent, Country, Sector, Industry)
c1 <- add_row(c1, Symbol = c2$Symbol, Name = c2$Name, Continent = c2$Continent, Country = c2$Country, Sector = c2$Sector, Industry = c2$Industry)
# 1

Central_America <- fread('CompanyProfile/Central_America.csv', header = T, sep = ',') %>% as.tibble() %>% mutate(Continent = 'Central America')
c2 <- companyProfile %>% inner_join(Central_America, by = c("Symbol")) %>% select(Symbol, Name, Continent, Country, Sector, Industry)
c1 <- add_row(c1, Symbol = c2$Symbol, Name = c2$Name, Continent = c2$Continent, Country = c2$Country, Sector = c2$Sector, Industry = c2$Industry)
# 2

Middle_East <- fread('CompanyProfile/Middle_East.csv', header = T, sep = ',') %>% as.tibble() %>% mutate(Continent = 'Middle East')
c2 <- companyProfile %>% inner_join(Middle_East, by = c("Symbol")) %>% select(Symbol, Name, Continent, Country, Sector, Industry)
c1 <- add_row(c1, Symbol = c2$Symbol, Name = c2$Name, Continent = c2$Continent, Country = c2$Country, Sector = c2$Sector, Industry = c2$Industry)
# 2

North_America <- fread('CompanyProfile/North_America.csv', header = T, sep = ',') %>% as.tibble() %>% mutate(Continent = 'North America')
c2 <- companyProfile %>% inner_join(North_America, by = c("Symbol")) %>% select(Symbol, Name, Continent, Country, Sector, Industry)
c1 <- add_row(c1, Symbol = c2$Symbol, Name = c2$Name, Continent = c2$Continent, Country = c2$Country, Sector = c2$Sector, Industry = c2$Industry)
# 173

South_America <- fread('CompanyProfile/South_America.csv', header = T, sep = ',') %>% as.tibble() %>% mutate(Continent = 'South America')
c2 <- companyProfile %>% inner_join(South_America, by = c("Symbol")) %>% select(Symbol, Name, Continent, Country, Sector, Industry)
c1 <- add_row(c1, Symbol = c2$Symbol, Name = c2$Name, Continent = c2$Continent, Country = c2$Country, Sector = c2$Sector, Industry = c2$Industry)
# 1

c1 <- arrange(c1, Symbol)

# ============
# Those are not in above regional table will be singled out and search as 
# much as possible information in NASDAQ.csv, NYSE.csv and AMEX.csv
# ===========

rest_of_company <- companyProfile %>% anti_join(c1, by = c("Symbol"))

NASDAQ <- fread('CompanyProfile/NASDAQ.csv', header = T, sep = ',') %>% as.tibble() 
c3 <- rest_of_company %>% inner_join(NASDAQ, by = c("Symbol")) %>% select(Symbol, Name, Sector, Industry = industry)
# 0

NYSE <- fread('CompanyProfile/NYSE.csv', header = T, sep = ',') %>% as.tibble() 
c3 <- rest_of_company %>% inner_join(NYSE, by = c("Symbol")) %>% select(Symbol, Name, Sector, Industry = industry)
# 4

AMEX <- fread('CompanyProfile/AMEX.csv', header = T, sep = ',') %>% as.tibble() 
c4 <- rest_of_company %>% inner_join(AMEX, by = c("Symbol")) %>% select(Symbol, Name, Sector, Industry = industry)
c3 <- add_row(c3, Symbol = c4$Symbol, Name = c4$Name, Sector = c4$Sector, Industry = c4$Industry)
# 1

c3 <- arrange(c3, Symbol)

# ======
# Symbol Change History notation 
# SymbolChange.csv
# ======

SymbolChangeHistory <- fread('CompanyProfile/SymbolChange.csv', header = T, sep = ',')
SymbolChange <- SymbolChangeHistory %>% as.tibble() %>% pull(`Old Symbol`)

companyProfile %>% filter(Symbol %in% SymbolChange)
companyProfile %>% inner_join(SymbolChangeHistory, by = c("Symbol" = "Old Symbol"))
# Symbol `New Symbol` `Effective Date`
# <chr>  <chr>        <chr>           
#1 BBRY   BB           10/16/2017      
#2 COH    TPR          10/31/2017      
#3 PCLN   BKNG         2/27/2018  
# Try these three

change_name_company <- companyProfile %>% inner_join(SymbolChangeHistory, by = c("Symbol" = "Old Symbol")) %>% select(`New Symbol`) %>% rename(Symbol = `New Symbol`)
# Now try 

c5 <- change_name_company %>% inner_join(North_America, by = c("Symbol")) %>% select(Symbol, Name, Continent, Country, Sector, Industry)
c5$Symbol<- c("BBRY", "PCLN", "COH")

c5 <- arrange(c5, Symbol)

c7 <- add_row(c1, Symbol = c5$Symbol, Name = c5$Name, Continent = c5$Continent, Country = c5$Country, Sector = c5$Sector, Industry = c5$Industry)
c7 <- add_row(c7, Symbol = c3$Symbol, Name = c3$Name, Sector = c3$Sector, Industry = c3$Industry)

companyProfile <- companyProfile %>% left_join(c7, by = c("Symbol"))
companyProfile <- arrange(companyProfile, Symbol)

companyProfile %>% write.table(file='companyProfile.csv', 
                       sep=',', 
                       row.names=F, 
                       col.names=T)


