library(tidyverse)
library(moments)
library(xtable)
library(ggpubr)
require(scales)
library(lspline)
library(estimatr)
library(texreg)
library(ggthemes)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

my_url <- "https://raw.githubusercontent.com/zsomborh/ba_da2/main/homework/doing_business/data/clean/time2business_clean.csv"
df <- read_csv( my_url )

# Descriptive statistics --------------------------------------------------


gdppc_df <- df %>% summarise(
    variable = 'gdppc',
    mean     = round(mean(gdppc),2),
    median   = round(median(gdppc),2),
    std      = round(sd(gdppc),2),
    iq_range = round(IQR(gdppc),2), 
    min      = round(min(gdppc),2),
    max      = round(max(gdppc),2),
    skew     = round(skewness(gdppc),2),
    numObs   = sum( !is.na( gdppc ) ) )

time2business_df <- df %>% summarise(
    variable = 'time2business in HUF(mm)',
    mean     = round(mean(time2business),2),
    median   = round(median(time2business),2),
    std      = round(sd(time2business),2),
    iq_range = round(IQR(time2business, na.rm= TRUE),2), 
    min      = round(min(time2business),2),
    max      = round(max(time2business),2),
    skew     = round(skewness(time2business),2),
    numObs   = sum( !is.na( time2business ) ) )

df_summary <- gdppc_df %>% add_row( time2business_df ) 
xtb <- xtable(df_summary,type = "latex", caption = "Summary statistics of examined variables")


p1<- ggplot( df , aes( x = gdppc ) ) +
    geom_density( aes(y = ..density..) , alpha = 1 , bw = 5, color = 'black' , fill="#FF6666") +
    labs(x='GDP per capita, 2019',y='Density') + ylim (0,0.045) + theme_bw()

p2<- ggplot( df , aes( x = time2business ) ) +
    geom_density( aes(y = ..density..) , alpha = 1 , bw = 5, color = 'black', fill="#56B4E9") +
    labs(x='Time to start business, 2019',y='Density') + ylim (0,0.045) + theme_bw() + ylab(NULL)

ggarrange(p1,p2)


# Scatterplots+lowess -----------------------------------------------------
df <- df %>%  mutate(
    gdppc_ln = log(gdppc),
    t2b_ln = log(time2business)
    
)



p1<- ggplot(df, aes(x = time2business, y= gdppc)) + 
    geom_point() + 
    geom_smooth(method="loess") +
    labs(x='Days required to start business',y='GDP per capita')+ 
    theme_bw()

p2<- ggplot( df , aes(x = time2business, y =gdppc )) +
    geom_point() +
    geom_smooth(method="loess") +
    scale_x_continuous( trans = log_trans(), breaks = c(1,3,25,70,140)) +
    labs(x='Days required to start business (ln scale)',y='GDP per capita')+ 
    theme_bw()

p3<- ggplot( df , aes(x = time2business, y =gdppc )) +
    geom_point() +
    geom_smooth(method="loess")+
    scale_y_continuous( trans = log_trans(), breaks = c(1,3,7,20,55))+
    labs(x='Days required to start business',y='GDP per capita (ln scale)')+
    theme_bw()#,  breaks = c(10,50,200,1000,10000) )+
    #labs(x='Infected per 1m capita (ln scale)',y='Dead per 1m capita')

p4<- ggplot( df , aes(x = time2business, y =gdppc )) +
    geom_point() +
    geom_smooth(method="loess")+
    scale_x_continuous( trans = log_trans(), breaks = c(1,3,25,70,140)) +#,  breaks = c(10,50,200,1000,10000) )+
    scale_y_continuous( trans = log_trans(), breaks = c(1,3,7,20,55))+
    labs(x='Days required to start business (ln scale)',y='GDP per capita (ln scale)')+
    theme_bw()#, breaks = c(1,20,400) )+
    #labs(x='Infected per 1m capita (ln scale)',y='Dead per 1m capita (ln scale)')

ggarrange(p1, p2,p3,p4, nrow = 2, ncol = 2 )


# regression visualisations -----------------------------------------------

cutoff <- c(10,50)

p1<- ggplot(df, aes(x = time2business, y= gdppc)) + geom_point() + 
    labs(x='Days required to start business', y= 'GDP per capita')+
    geom_smooth(method=lm) +theme_bw()

p2<- ggplot(df, aes(x = t2b_ln, y= gdppc_ln)) + geom_point() + 
    labs(x='Log number of days needed to start business', y= 'Log GDP per capita')+
    geom_smooth(method = lm, color = 'red') +theme_bw()
p3<- ggplot(df, aes(x = time2business, y= gdppc_ln)) + geom_point() +
    labs(x='Days required to start business quadratic', y= 'Log GDP per capita') +
    geom_smooth( formula = y ~ poly(x,2) , method = lm , color = 'orange') +theme_bw()
p4<- ggplot(df, aes(x = time2business, y= gdppc_ln)) + geom_point() +
    labs(x='Days required to start business (PLS)', y= 'Log GDP per capita')+
    geom_smooth( formula = y ~ lspline( x , cutoff ), method = lm , color = 'lightgreen') +theme_bw()

ggarrange(p1,p2,p3,p4,nrow=2, ncol = 2)


# Regressions - estimated coefficients ------------------------------------

df <- df %>%  mutate(
    t2b_sq = time2business **2
)

reg1 <- lm_robust( gdppc ~ time2business , data = df )
reg2 <- lm_robust( gdppc_ln ~ time2business , data = df )
reg3<-  lm_robust( gdppc_ln ~ time2business +t2b_sq, data = df )
reg4 <- lm_robust(gdppc_ln ~ lspline( time2business , cutoff ), data = df )
reg5 <- lm_robust(gdppc_ln ~ time2business, data = df , weights = population)


texreg(list(reg1 , reg2 , reg3 , reg4, reg5), table = FALSE, use.packages = FALSE, float.pos = 'h')

