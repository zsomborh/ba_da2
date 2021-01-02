# Getting the raw data ----------------------------------------------------

library(tidyverse)
df <- read_delim('https://raw.githubusercontent.com/zsomborh/ba_da2/main/Assignment_2/bank-additional-full.csv', delim = ';')

# Resolve Data Quality issues ---------------------------------------------

#removing duplicate entries 
nrow(df)- nrow(unique(df)) # there are a few duplicates
df<- df[!duplicated(df),] 

# Handling missing values

#there are no NAs
sum(!complete.cases(df))

#missing values are denoted with 'uknown' string
plot_Missing <- function(data_in, title = NULL){
    temp_df <- as.data.frame(ifelse(data_in == 'unknown', 0, 1))
    temp_df <- temp_df[,order(colSums(temp_df))]
    data_temp <- expand.grid(list(x = 1:nrow(temp_df), y = colnames(temp_df)))
    data_temp$m <- as.vector(as.matrix(temp_df))
    data_temp <- data.frame(x = unlist(data_temp$x), y = unlist(data_temp$y), m = unlist(data_temp$m))
    ggplot(data_temp) + geom_tile(aes(x=x, y=y, fill=factor(m))) + scale_fill_manual(values=c("white", "black"), name="Missing\n(0=Yes, 1=No)") + theme_light() + ylab("") + xlab("") + ggtitle(title)
}

plot_Missing(df[,colSums(df == 'unknown') > 0, with = FALSE]) #default var has quite a lot of unknown, others are not that bad
df %>%  group_by(default) %>%  count() # there is a very small variation of x as only 3 obs has Y for default - I will drop it

df$default <- NULL

#see the number of unknowns in each category 
colSums(sapply(df,function(x){x == 'unknown'})) 

#it's better to drop these, there is no alternative to default them to 
df<- df %>% filter(
    !job == 'unknown',
    !marital == 'unknown',
    !education == 'unknown',
    !housing == 'unknown',
    !loan == 'unknown')

#there is a lot of cases when pdays was 999 - a numeric dummy, meaning client was not contacted
pdays<- df %>%  group_by(pdays) %>%  count()
pdays[pdays$pdays == 999,]$n/sum(pdays$n) 

#Only ca.5% of observations has a variable that is not 999 - due to small variation of x, I will drop this.
df$pdays <- NULL
rm(pdays)

# Contact has no variation at all, I will drop it
#df$contact <- NULL


# transform categorical variables -----------------------------------------


#create numeric dummies for dichotom variables
df<- df %>% mutate(
    housing = ifelse(housing == 'yes',1,0),
    loan =  ifelse(loan == 'yes',1,0),
    output = ifelse(y == 'yes',1,0),
    y = NULL) 
    


# check unique number of values in each column
sapply(sapply(df,unique),length) 

# I want to trim back education and month to less categories 

#     education: 
df %>%  group_by(education) %>%  count()
ggplot(data = df, aes(x = education)) + geom_bar()
# I will create 2 categories in a new variable - high.ed
#  1: university degree or professional course
#  0: in every other case 

df<- df %>% mutate(high.ed = ifelse(df$education == 'university.degree' | df$education == 'professional.course',1,0 ),
                   education = NULL)

#    month
df %>%  group_by(month) %>%  count()
# we will reduce 10 categories to 4, so that we have seasons and not month
df <- df %>% mutate( season =   case_when( month  == 'mar' | month == 'apr' | month == 'may' ~ "spring",
                                   month == 'dec' | month == 'jan' | month == 'feb' ~ 'winter',
                                   month =='jun' | month == 'jul' | month == 'aug' ~ 'summer',
                                   month == 'sep' | month == 'oct' | month == 'nov' ~ 'autumn',
                                   TRUE ~ as.character(month)),
                     month = NULL)
#    job
df %>%  group_by(job) %>%  count()
ggplot(data = df, aes(job, ..count..)) +geom_bar() 
# job is left as is, as there is good representation in each category + there is no easy way to merge them together

# create factors out of categorical variables for later analysis
df <- df %>% mutate(
    job = as.factor(job),
    marital = as.factor(marital),
    contact = as.factor(contact),
    day_of_week = as.factor(day_of_week),
    poutcome = as.factor(poutcome),
    season = as.factor(season)
)

summary(df)
# Overall I have ca.38k observations, mostly with categorical variables
# I fitlered out duplicate values, missing values (without the use of any imputation technique)
# I dropped variables that didn't have a lot of variation in x (e.g. 95% of the cases take the same value)
# I reduced the amount of categories in some variables to reduce the complexity of this data

write_csv(df, 'C:/Users/T450s/Desktop/programming/git/ba_da2/Assignment_2/data/clean/bankmarketing_clean.csv')