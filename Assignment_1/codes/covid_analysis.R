library(tidyverse)
require(scales)
library(lspline)
library(estimatr)
library(texreg)
library(ggthemes)
library(xtable)
library(car)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

df<- read_csv('../data/clean/covid_pop_20201015_clean.csv')


# EDA ---------------------------------------------------------------------

## histograms look to be skewed to the right 
df %>%
    keep(is.numeric) %>% 
    gather() %>% 
    ggplot(aes(value)) +
    facet_wrap(~key, scales = "free") +
    geom_histogram()+
    theme_wsj() + 
    scale_fill_wsj()

##  create per capita variables 
colnames(df)
df <- df %>%
    mutate(
        death_pc = as.numeric(deaths/(population/1000000)),
        conf_pc = as.numeric(confirmed / (population/1000000)),
        mortality = as.numeric(deaths/confirmed)
    )

## plotting
## a) level-level - scaled
ggplot( df , aes(x = conf_pc, y =death_pc )) +
    geom_point() +
    geom_smooth(method="loess") +
    labs(x='Infected per 1 m capita',y='Dead per 1m capita')
## two extreme values in terms of confirmed cases/1m people: Qatar, Bahrein
## Otherwise there seems to be a positive trend

ggplot( df , aes(x = conf_pc, y =death_pc )) +
    geom_point() +
    geom_smooth(method="loess")+
    scale_x_continuous( trans = log_trans(),  breaks = c(1,2,5,10,20,50,100,200,500,1000,10000) )+
    labs(x='Infected per 1 m capita - ln scale',y='Dead per 1m capita')

ggplot( df , aes(x = conf_pc, y =death_pc )) +
    geom_point() +
    geom_smooth(method="loess")+
    scale_x_continuous( trans = log_trans(),  breaks = c(1,2,5,10,20,50,100,200,500,1000,3000,10000) )+
    scale_y_continuous( trans = log_trans(), breaks = c(1,20,200,400) )+
    labs(x='Infected per 1 m capita - ln scale',y='Dead per 1m capita - ln scale')

## Conclusion - log-log transformation seems to be giving a good fit

## we will remove cases where nobody died, so that ln transform can happen
df <- df %>%  filter(deaths != 0)

df <- df %>% mutate( ln_conf_pc = log( conf_pc ),
                     ln_death_pc = log( death_pc ) )


# Regression tests --------------------------------------------------------

## We create variables to use polinomials later on
df <- df %>% mutate( ln_conf_pc_sq = ln_conf_pc^2,
                     ln_conf_pc_cb = ln_conf_pc^3
                     )
## linear
reg1 <- lm_robust( ln_death_pc ~ ln_conf_pc , data = df , se_type = "HC2" )
summary( reg1 )
ggplot( data = df, aes( x = ln_conf_pc, y = ln_death_pc ) ) + 
    geom_point( color='blue') +
    geom_smooth( method = lm , color = 'red' )

## quadratic
reg2 <- lm_robust( ln_death_pc ~ ln_conf_pc + ln_conf_pc_sq , data = df )
summary( reg2 )
ggplot( data = df, aes( x = ln_conf_pc, y = ln_death_pc ) ) + 
    geom_point( color='blue') +
    geom_smooth( formula = y ~ poly(x,2) , method = lm , color = 'red' )


## pls
ln_cutoff <- log(c(200,3000))
reg3 <- lm_robust(ln_death_pc ~ lspline( ln_conf_pc , ln_cutoff ), data = df )
summary( reg3 )
ggplot( data = df, aes( x = ln_conf_pc, y = ln_death_pc ) ) + 
    geom_point( color='blue') +
    geom_smooth( formula = y ~ lspline(x,ln_cutoff) , method = lm , color = 'red' )

## weighted
reg4 <- lm_robust(ln_death_pc ~ ln_conf_pc, data = df , weights = population)
summary( reg4 )

ggplot(data = df, aes(x = ln_conf_pc, y = ln_death_pc)) +
    geom_point(data = df, aes(size=population),  color = 'blue', shape = 16, alpha = 0.6,  show.legend=F) +
    geom_smooth(aes(weight = population), method = "lm", color='red')
    #scale_size(range = c(1, 15)) +
    #coord_cartesian(ylim = c(50, 85)) +
    #labs(x = "ln(GDP per capita, thousand US dollars) ",y = "Life expectancy  (years)")+
    #annotate("text", x = 4, y = 80, label = "USA", size=5)+
    #annotate("text", x = 2.7, y = 79, label = "China", size=5)+
    #annotate("text", x = 2,  y = 68, label = "India", size=5)


# Choose model ------------------------------------------------------------

data_out <- '../out/'
htmlreg( list(reg1 , reg2 , reg3 , reg4),
         type = 'html',
         custom.model.names = c("Confirmed/capita - linear","Confirmed/capita - quadratic",
                                "Confirmed/capita - PLS",'Confirmed/capita - weighted linear'),
         caption = "Modeling Covid-19 caused deaths and confirmed cases",
         file = paste0( data_out ,'model_comparison.html'), include.ci = FALSE)



# Prediction errors -------------------------------------------------------

# Get the predicted y values from the model
df$reg4_y_pred <- reg4$fitted.values
# Calculate the errors of the model
df$reg4_res <- df$ln_death_pc - df$reg4_y_pred 

# Find countries with largest negative errors
df %>% top_n( -5 , reg4_res ) %>% 
    select( country , ln_death_pc , reg4_y_pred , reg4_res )

# Find countries with largest positive errors
df %>% top_n( 5 , reg4_res ) %>% 
    select( country , ln_death_pc , reg4_y_pred , reg4_res )



# Testing hypothesis ------------------------------------------------------

# Test if beta = 0

lin <-linearHypothesis( reg1 , "ln_conf_pc = 0")

tab<- tidy(reg1)

tab <-  tab %>% 
    filter(term == 'ln_conf_pc')  %>%  
    transmute(
        variable=term,
        estimate = estimate,
        std.error = std.error,
        statistic = statistic,
        p.value = p.value,
        conf.low = conf.low,
        conf.high = conf.high)