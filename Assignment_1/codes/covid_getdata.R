library(WDI)
library(tidyverse)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#Date: 05/10/2020 
#Y: Number of registered death per capita 
#X: Number of registered case per capita

#First,  getting the csv from Johns Hopkins University's page
url <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports'
date <- '10-05-2020'
csv_location <- paste0(url,'/',date,'.csv') 

covid_data <- read_csv(csv_location)

# Then we get population data from WDI
population_data <- WDI(indicator=c('SP.POP.TOTL'), 
               country="all", start=2019, end=2019)


#finally, writing out raw files to github
write_csv(covid_data, '../data/raw/covid_20201005_raw.csv')
write_csv(population_data, '../data/raw/population_2019_raw.csv')
