These codes are for regression analysis between the number COVID-19 infected and deceased people.

covid_getdata.R - downloads the raw data from World Development Indicators maintained by World Bank and COVID related data from Johns Hopkins University and saves it to data/raw folder.

covid_clean.R - loads the raw data and clean it: creates a tidy table where each observation is a country.

covid_analysis.R - loads the clean data and executes a linear regressions with visual inspections and quantitative analysis.