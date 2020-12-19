library(tidyverse)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

my_url <- "https://raw.githubusercontent.com/zsomborh/ba_da2/main/homework/doing_business/data/raw/business_2019_raw.csv"
df <- read_csv( my_url )

drop_id <- c("EU","HK","OE")
retain_id <- c("XK","ZA","ZM","ZW")
fl_iso2c <- substr(df$iso2c, 1, 1)


df <- df %>% 
    filter( !grepl("[[:digit:]]", df$iso2c) ) 
df<- df %>%  
    filter( !grepl( paste( drop_id , collapse="|"), df$iso2c ) )

fl_iso2c <- substr(df$iso2c, 1, 1)

df<- df %>% 
    filter( !( grepl( "X", fl_iso2c ) | grepl( "Z", fl_iso2c ) & !grepl( paste( retain_id , collapse="|"), df$iso2c ) ) ) 

df <- df %>% filter( complete.cases( df ) | is.na( df$iso2c ) )

df <-df %>% transmute( country = country,
                       population=SP.POP.TOTL/1000000,
                       gdppc=NY.GDP.PCAP.PP.KD/1000,
                       time2business = IC.REG.DURS )


write_csv(df, '../data/clean/time2business_clean.csv')
