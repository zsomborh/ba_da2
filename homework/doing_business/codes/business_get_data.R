library(WDI)
library(tidyverse)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Then we get population data from WDI

wdi_data <- WDI(indicator=c('NY.GDP.PCAP.PP.KD','SP.POP.TOTL',"IC.REG.DURS"), country="all", start=2019, end=2019)

#finally, writing out raw files to github
write_csv(wdi_data, '../data/raw/business_2019_raw.csv')
