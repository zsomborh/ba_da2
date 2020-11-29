library(tidyverse)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Let's clean population data first per guidance in Coding1 - Class 8 --------

## load data
df_population <- read_csv("../data/raw/population_2019_raw.csv")

## special cases
to_drop <- c("EU","HK","OE")
to_retain <- c("XK","ZA","ZM","ZW")

## filtering out grouped entitites
### a) those that have an integer in their iso2c
df_population <- df_population %>% 
    filter(!grepl("[[:digit:]]", df_population$iso2c)) 
### b) further manually spotted grouped entitites e.g. EU
df_population <- df_population %>% 
    filter(!grepl( paste( to_drop , collapse="|"), df_population$iso2c )) 
### c) most records that have X, or Z in their iso2c except `to_retain` vector
fl_iso2c <- substr(df_population$iso2c, 1, 1)
df_population <- df_population %>% 
    filter(!(grepl( "X", fl_iso2c ) | grepl( "Z", fl_iso2c ) & 
            !grepl( paste( to_retain , collapse="|"), 
                    df_population$iso2c ) ) )
### d) NAs or NaNs
df_population <- 
    df_population %>% 
        filter(complete.cases(df_population) | is.na(df_population$iso2c))

## keep columns needed
df_population <-
    df_population %>% 
    transmute( country = country,
                       population=SP.POP.TOTL)


# Let's continue with covid data ------------------------------------------

## First we load the data
df_covid <- read_csv( "../data/raw/covid_20201005_raw.csv" )

## I do 3 things: 1) group by country 2) sum records that appera multiple
## times for a given country - since they were broken out to regions and 3) 
## naturally unusued columns not defined in summarise function are dropped

df_covid <- df_covid %>% 
    mutate(country=Country_Region) %>% 
    group_by(country) %>% 
    summarise( 
        confirmed = sum( Confirmed, na.rm = TRUE ),
        deaths = sum( Deaths, na.rm = TRUE ),
        recovered = sum( Recovered , na.rm = TRUE),
        active = sum( Active, na.rm = TRUE ) )


# Merging dataframes ------------------------------------------------------

## merging dataframes with full join
joined_df <- full_join(df_covid,df_population, by = 'country')

## we will have a lot of partial matches, let's look at observations with NAs
temp_df <- joined_df %>% 
    filter(!complete.cases(joined_df)) %>%
    transmute(country=country) %>% 
    arrange(country)

## manually picking out records to be renamed
keep_names <- c(
'Brunei', 'Bahamas', 'Laos', 'Syria',
'Egypt', 'Czech Republic', 
'Congo', 'Congo', 'Congo', 'Congo',
'Kyrgyzstan', 'Korea, South', 'Iran', 'Gambia',
'Saint Lucia', 'Saint Kitts and Nevis', 'Slovakia','Russia',
'St. Vincent and the Grenadines','United States','Venezuela', 'Yemen')

change_names <- c(
'Brunei Darussalam','Bahamas, The', 'Lao PDR', 'Syrian Arab Republic',
'Egypt, Arab Rep.', 'Czechia',
'Congo, Dem. Rep.', 'Congo, Rep.', 'Congo (Kinshasa)','Congo (Brazzaville)',
'Kyrgyz Republic', 'Korea, Rep.', 'Iran, Islamic Rep.', 'Gambia, The',
'St. Lucia','St. Kitts and Nevis', 'Slovak Republic', 'Russian Federation',
'Saint Vincent and the Grenadines','US','Venezuela, RB','Yemen, Rep.')

## renaming countries with partial matches
for ( i in seq_along( keep_names ) ){
    joined_df$country[ joined_df $country == change_names[ i ] ] <- keep_names[ i ]
}

## summing up population and covid cases
joined_df<- joined_df %>% 
    group_by(country) %>% 
    summarise( 
    confirmed = sum( confirmed, na.rm = TRUE ),
    deaths = sum( deaths, na.rm = TRUE),
    recovered = sum( recovered, na.rm = TRUE ),
    active = sum( active, na.rm = TRUE ),
    population = sum(population, na.rm = TRUE))

## I still have records that should be filtered out as with full join
## I added countries that are actually not present in both tables, but due to
## summing up, they are not NA anymore. 

## I checked source db and it can't be that
##  there are 0 confirmed cases or
##  0 population for a country, so I will filter those out


final_df <- joined_df %>%  filter(
    !(population == 0 | confirmed ==0)
)

## I will check if I have any NAs left
sum(complete.cases(final_df)) == nrow(final_df)
## No NAs left

write_csv(final_df, '../data/clean/covid_pop_20201005_clean.csv')
