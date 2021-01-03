# General message - I want to understand what determines the willingness for a person to take a term loan 
# There is a paper published that is about predicting success of campaign with ML models, my focus is understanding 
# the pattern of association between certain demographic factors and outcome of phone call 

# Materials used: 
#https://www.kaggle.com/notaapple/detailed-exploratory-data-analysis-using-r
#https://www.kaggle.com/hillabehar/titanic-analysis-with-r
#https://rpubs.com/abhaypadda/logistic-regression-using-titanic-data
#https://www.kaggle.com/notaapple/detailed-exploratory-data-analysis-using-r
#https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html

# Load libraries, df, functions -----------------------------------------------


load.libraries <- 
    c('data.table',
      'testthat',
      'gridExtra',
      'corrplot',
      'GGally',
      'tidyverse',
      'e1071',
      'viridis',
      'stargazer',
      'ggpubr',
      'estimatr',
      'mfx',
      'margins',
      'pscl',
      'modelsummary',
      'caret',
      'DescTools')

install.lib <- load.libraries[!load.libraries %in% installed.packages()]

for(libs in install.lib) install.packages(libs, dependences = TRUE)

#load and check if all is loaded
sapply(load.libraries, require, character = TRUE)

# source sum_stat function
source('https://raw.githubusercontent.com/CEU-Economics-and-Business/ECBS-5208-Coding-1-Business-Analytics/master/Class_10/codes/sum_stat.R')

df <- read_csv('https://raw.githubusercontent.com/zsomborh/ba_da2/main/Assignment_2/data/clean/bankmarketing_clean.csv')

# EDA for dataset ---------------------------------------------------------
str(df)

# create separate df with categorical values + dummies and numerical values  
quanty_cols <- c('age', 'duration', 'campaign', 'previous', 'emp.var.rate',
                 'cons.price.idx', 'cons.conf.idx', 'euribor3m', 'nr.employed')

qualy_cols<-  names(df) %in% quanty_cols
quanty_cols <- c(quanty_cols, 'output')

quant_df <- df[,quanty_cols]
qual_df <- df[!qualy_cols]


# plotting histograms/barplots
qual_plotHist <- function(data_in, i, bins = NULL,angle = 0) {
    data <- data.frame(x=data_in[[i]])
    p <- ggplot(data=data, aes(x=factor(x),fill=factor(data_in$output))) +
        stat_count(color= 'black') +
        labs(x = colnames(data_in)[i], fill = 'Sucessfull campaign?') + 
        theme_minimal() + 
        theme(axis.text.x = element_text(angle = angle, hjust =1),legend.position = 'bottom')+
        scale_fill_viridis(discrete = TRUE, option = "D")
    return (p)
}


quant_plotHist <- function(data_in, i, bins = 15, angle = NULL) {
    data <- data.frame(x=data_in[[i]])
    p <- ggplot(data=data, aes(x=x, fill = factor(data_in$output)))  +
        geom_histogram( bins=bins, color = 'black')+#, alpha = .2 , color = 'black', position = 'fill')+
        labs(x = colnames(data_in)[i], fill = 'Sucessfull campaign?') + 
        theme_minimal() +
        theme(legend.position = 'bottom')+
        scale_fill_viridis(discrete = TRUE, option = "D")
    return (p)
}


plots <- function(data,fun,from=1,to=4,ncol = 2, nrow = 2, bins = rep(15,(to-from)+1), angle = rep(0,(to-from)+1)) {
    
    plot_list <- list()
    
    for(i in from:to){
        p<- fun(data,i, bins[i], angle[i])
        plot_list <- c(plot_list, list(p))
    }
    
    return(do.call('grid.arrange', c(plot_list,ncol=ncol,nrow=nrow)))
    
}



plots(data=quant_df, fun= quant_plotHist, from = 1, to =9,ncol = 3, nrow =3, bins = c(15,15,15,7,20,15,15,15,20))

#reorder df so that output is last observation
qual_df<- qual_df[c(1,2,3,4,5,6,7,9,10,8)]
plots(data=qual_df, fun= qual_plotHist, from = 1, to =9,ncol = 3, nrow =3, angle= c(45,0,0,0,0,0,0,0,0))

# Make descriptive statistics table for quantitative variables
desc_stat <- sum_stat( quant_df , var_names = names(quant_df),
                      stats = c('mean','median','min','max','sd') )


# Checking correlations and linearity with lowess  ---------------------------------

cors = cor(quant_df)

corrplot(cors , 
         type = 'upper', 
         order = 'hclust', 
         tl.col = 'black', tl.srt= 45, number.cex = .7,
         addCoef.col = 'black',method = 'color', 
         col = colorRampPalette(c('yellow','white','purple'))(10))

# checking connections of succes vs quantitative variables with lowess 

lowess_plots <- function(data,var= 'age', xlab = 'Age') {
    
    temp_df <- data %>% 
        group_by(data[,var],output) %>%  
        mutate(weight = n()) %>% 
        dplyr::select(var,output,weight) 
    
    ggplot(data = temp_df, aes_string(x=names(temp_df)[1], y='output')) +
        geom_point(aes_string(x = names(temp_df)[1], 
                              y = 'output', 
                              size='weight'),
                   color="gold", 
                   shape = 16, 
                   alpha=1, show.legend=F, 
                   na.rm=TRUE)  +
        geom_smooth(method="loess", color="purple", se = FALSE) +
        scale_y_continuous(expand = c(0.05,0.05), limits = c(0,1), breaks = seq(0,1,0.1)) +
        labs(x = xlab,y = "Probability of success") + theme_minimal()
}

lowess_plots(df,'age','Age') 
lowess_plots(df,'duration','Duration')
lowess_plots(df,var = 'campaign','Number of contacts performed')

# Main Takeaways: 
# There is a non-linear relationship: age vs output, duration vs output
# Lower success is associated the more someone is contacted and it gets to almost zero when contacted more than 15 times

# Transformations:
# - I will use the second order polynomial of age and duration due to lowess resembling quadratic function form
# - I will log transfrom campaign, and previous variables as they look log-normal distributed

df<- df %>%  mutate(
    age_sq  = age **2,
    duration_sq = duration **2,
    campaign_ln = log(campaign)
)

# Naive model - decide on what to include ---------------------------------

demographic <-  c('age','age_sq','job','marital', 'high.ed')
fin.state <- c('housing','loan')
time<- c('day_of_week', 'season')
campaign.specs <- c('contact','duration','previous','duration_sq','campaign_ln','poutcome')
macro <- c('euribor3m','nr.employed','cons.price.idx', 'cons.conf.idx', 'emp.var.rate')

#demographic only

lpm1 <- lm( formula = output ~. , data=df[,c(demographic,'output')] )
summary(lpm1, vcov = sandwich)

# demographic +financial
lpm2 <- lm( formula = output ~. , data=df[,c(demographic,fin.state,'output')] )
summary(lpm2, vcov = sandwich)

# demo + fin + time
lpm3 <- lm( formula = output ~. , data=df[,c(demographic,fin.state,time,'output')] )
summary(lpm3, vcov = sandwich)

# demo + fin + time + campaign
lpm4 <- lm( formula = output ~. , data=df[,c(demographic,fin.state,time, campaign.specs, 'output')] )
summary(lpm4, vcov = sandwich)

# demo + fin + time + campaign + macro
lpm5 <- lm( formula = output ~. , data=df[,c(demographic,fin.state,time, campaign.specs, macro, 'output')] )
summary(lpm5, vcov = sandwich)

# reduced model - leaving out weekdays, fin state variables, cons.conf.idx, previous, marital based on VarImp
varImp(lpm5)

keep_vars <- c(demographic,fin.state,time, campaign.specs, macro, 'output') 
remove_vars <- c('day_of_week', 'loan', 'housing', 'cons.conf.idx', 'previous', 'campaign_ln', 'marital' )
keep_vars <- keep_vars[!keep_vars %in% remove_vars]

lpm6 <- lm( formula = output ~. , data=df[,keep_vars] )
summary(lpm6, vcov = sandwich)

#Create stargazer summary table with all 6 regressions 
stargazer(lpm1, lpm2,lpm3,lpm4,lpm5, lpm6, 
          digits=2, 
          out = 'C:/Users/T450s/Desktop/programming/git/ba_da2/Assignment_2/out/lpms_comparison.html' )

# we will stick to lpm5 and lpm6 but keep lpm1 as well to see how much the model improved

df$lpm1_pred <- predict(lpm1)
df$lpm5_pred <- predict(lpm5)
df$lpm6_pred <- predict(lpm6)

# prediction distr

p1<- ggplot(data=df, aes(x=lpm5_pred)) +
    geom_histogram( aes( y = ..density.. ), fill = 'navyblue', binwidth=0.02) +
    coord_cartesian(xlim = c(0, 1.2)) +
    labs(x = "Predicted probability of successfull marketing campaign (LPM)",y = "Percent")

p2<- ggplot(data=df, aes(x=lpm6_pred)) +
    geom_histogram( aes( y = ..density.. ), fill = 'navyblue', binwidth=0.02) +
    coord_cartesian(xlim = c(0, 1.2)) +
    labs(x = "Predicted probability of successfull marketing campaign (LPM)",y = "Percent")


ggarrange(p1,p2,nrow = 1)

fit_df %>% mutate( R2 = round(as.numeric(R2),2))

?round
# Checking some descriptive stats for top and bottom 1%
df <- df %>% mutate(q100_pred_lpm = ntile(lpm6_pred, 100))
b1 <- df %>% filter( q100_pred_lpm == 1 )
t1 <- df %>% filter( q100_pred_lpm == 100 )
stat_interest <- c('mean','median','sd')
sum_stat(b1,quanty_cols,stat_interest,num_obs = F)
sum_stat(t1,quanty_cols,stat_interest,num_obs = F)


# More sophisticated models - logit and probit  ---------------------------

# first, logit
logit <- glm( formula = output~. , data=df[,keep_vars], family=binomial(link="logit") )
summary(logit)
glance(logit)

df$pred_logit <- predict.glm(logit, type="response")
logit_marg <- logitmfx( formula = output~. , data=df[,keep_vars], atmean=FALSE, robust = T)

# second, probit

probit <- glm( formula = output~. , data=df[,keep_vars], family=binomial(link="probit") )
summary(probit)
glance(probit)

df$pred_probit <- predict.glm(probit, type="response")
probit_marg <- probitmfx( formula = output~. , data=df[,keep_vars], atmean=FALSE, robust = T)

#Creating model summary
cm <- c('(Intercept)' = 'Constant')
pmodels <- list(lpm5, lpm6, logit, logit_marg, probit, probit_marg)

msummary( pmodels ,
          fmt="%.3f",
          gof_omit = 'DF|Deviance|Log.Lik.|F|R2 Adj.|AIC|BIC|R2|PseudoR2',
          stars=c('*' = .05, '**' = .01),
          coef_rename = cm,
          coef_omit = 'as.factor(country)*',
          output = 'C:/Users/T450s/Desktop/programming/git/ba_da2/Assignment_2/out/all_models.html'
)

# adding pseudo R2 
glance_custom.glm <- function(x) data.frame(`PseudoR2` = pR2(x)["McFadden"])

msummary(list(lpm5, lpm6, logit, probit),
         fmt="%.3f",
         gof_omit = 'DF|Deviance|Log.Lik.|F|R2 Adj.|AIC|BIC',
         stars=c('*' = .05, '**' = .01),
         coef_rename = cm)

# visualisation
ggplot(data = df) +
    geom_point(aes(x=lpm6_pred, y=pred_probit, color="Probit"), size=1,  shape=16, color = 'gold') +
    geom_point(aes(x=lpm6_pred, y=pred_logit,  color="Logit"), size=1,  shape=16, color = 'purple') +
    geom_line(aes(x=lpm6_pred, y=lpm6_pred,    color="45 degree line"), size=1, color = 'navyblue') +
    labs(x = "Predicted probability of successfull campaign (LPM)", y="Predicted probability")+
    scale_y_continuous(expand = c(0.00,0.0), limits = c(0,1), breaks = seq(0,1,0.1)) +
    scale_x_continuous(expand = c(0.00,0.0), limits = c(0,1), breaks = seq(0,1,0.1)) +
    scale_color_manual(name = "", values=c("green", "red","blue"))+
    theme(legend.position=c(0.55,0.08),
          legend.direction = "horizontal",
          legend.text = element_text(size = 4))+
    theme_minimal() 


# Model choice ------------------------------------------------------------

# let's see baseline model that always predicts 0
df$baseline <- rep(0,nrow(df))

# create list of models and prediction col names
models <- list(lpm1,lpm5,lpm6,logit,probit)
preds <- c('lpm1_pred','lpm5_pred','lpm6_pred','pred_logit','pred_probit')

# create functoin to get all types of R squareds
get_rsquared <- function(models, preds){
    
    R2 <- c()
    R2_adjusted <- c()
    McFadden<- c()
    
    model_rank <- c(paste0('model ',1:length(models)))
    
    i=1
    
    is.null(summary(models[[1]])$r.squared)
    
    
    for(i in seq_along(models)){
        #get R2
        if (is.null(summary(models[[i]])$r.squared)) {
            R2 <- c(R2,"NA")
        } else{
            R2 <- c(R2,summary(models[[i]])$r.squared)
        }
        #Get R2 adj
        if (is.null(summary(models[[i]])$r.squared)) {
            R2_adjusted <- c(R2_adjusted,"NA")
        } else{
            R2_adjusted <- c(R2_adjusted,summary(models[[i]])$adj.r.squared)
        }
        #R2_adjusted <- c(R2_adjusted, summary(models[[i]])$adj.r.squared )
        McFadden <- c(McFadden,pR2(models[[i]])[['McFadden']])
    }
    
    return(data.frame('Model rank' = model_rank, 'R2' =R2, 'R2 adjusted' = R2_adjusted, 'McFadden R2' = McFadden))
}


# Create function for calculating Brier score
Brier_score <- function(data,var) {
    Br_df <- (data[,var]-data[,'output'])**2
    Br_score <- mean(Br_df[,var])
    
    return(Br_score)
}

# Create function for calculating Bias
Bias<- function(data,var){
    bias <- mean( data[,var][[1]] ) - mean( data[,'output'][[1]] )
    return(bias)
}

# Create function that makes a summary table for model fit metrics 
get_fit_df <- function(models,data,preds) {
    brier = c()
    for(pred in preds){brier <- c(brier,Brier_score(data, pred))}
    
    bias = c()
    for(pred in preds){bias <- c(bias,Bias(data, pred))}
    
    fit_df <- get_rsquared(models)
    fit_df <- cbind(fit_df,list('Brier score' = brier), list('Bias' = bias))
    return(fit_df)
}


fit_df<- get_fit_df(models,df,preds)

#small correction to remove mcfadden R2 for lpms

fit_df[fit_df$Model.rank %in% c('model 1','model 2', 'model 3'), 'McFadden.R2'] <- NA
fit_df <- fit_df %>% mutate(
    Model = c('Simple LPM', 'Rich LPM', 'Reduced LPM', 'Logit', 'Probit'),
    Model.rank = NULL
)

fit_df 
# Probit, has the best R squared, lowest Brier score, and Bias 

# Checking predicted values -----------------------------------------------

#base model
p1<- ggplot(data = df,aes(x=lpm1_pred)) + 
    geom_histogram(data=subset(df[df$output == 1, ]), 
                   aes(fill=as.factor(output), color=as.factor(output), y = (..count..)/sum(..count..)*100),
                   binwidth = 0.05, boundary=0, alpha=0.8) +
    geom_histogram(data=subset(df[df$output == 0, ]), 
                   aes(fill=as.factor(output), color=as.factor(output), y = (..count..)/sum(..count..)*100), 
                   binwidth = 0.05, boundary=0, alpha=0) +
    scale_fill_manual(name="", values=c("0" = "white", "1" = "red"),labels=c("Didn't succeed","Succeeded")) +
    scale_color_manual(name="", values=c("0" = "blue", "1" = "red"),labels=c("Didn't succeed","Succeeded")) +
    ylab("Percent") +
    xlab("Fitted values") +
    scale_x_continuous(expand=c(0.01,0.01) ,limits = c(0,1), breaks = seq(0,1,0.2)) +
    scale_y_continuous(expand=c(0.00,0.00) ,limits = c(0,80), breaks = seq(0,80,20))+
    theme(legend.position = c(0.3,0.9),
          legend.key.size = unit(x = 0.5, units = "cm")) + theme_minimal()

#complex lpm
p2<- ggplot(data = df,aes(x=lpm6_pred)) + 
    geom_histogram(data=subset(df[df$output == 1, ]), 
                   aes(fill=as.factor(output), color=as.factor(output), y = (..count..)/sum(..count..)*100),
                   binwidth = 0.05, boundary=0, alpha=0.8) +
    geom_histogram(data=subset(df[df$output == 0, ]), 
                   aes(fill=as.factor(output), color=as.factor(output), y = (..count..)/sum(..count..)*100), 
                   binwidth = 0.05, boundary=0, alpha=0) +
    scale_fill_manual(name="", values=c("0" = "white", "1" = "red"),labels=c("Didn't succeed","Succeeded")) +
    scale_color_manual(name="", values=c("0" = "blue", "1" = "red"),labels=c("Didn't succeed","Succeeded")) +
    ylab("Percent") +
    xlab("Fitted values") +
    scale_x_continuous(expand=c(0.01,0.01) ,limits = c(0,1), breaks = seq(0,1,0.2)) +
    scale_y_continuous(expand=c(0.00,0.00) ,limits = c(0,80), breaks = seq(0,80,20))+
    theme(legend.position = c(0.3,0.9),
          legend.key.size = unit(x = 0.5, units = "cm")) + theme_minimal()

p3<- ggplot(data = df,aes(x=pred_probit)) + 
    geom_histogram(data=subset(df[df$output == 1, ]), 
                   aes(fill=as.factor(output), color=as.factor(output), y = (..count..)/sum(..count..)*100),
                   binwidth = 0.05, boundary=0, alpha=0.8) +
    geom_histogram(data=subset(df[df$output == 0, ]), 
                   aes(fill=as.factor(output), color=as.factor(output), y = (..count..)/sum(..count..)*100), 
                   binwidth = 0.05, boundary=0, alpha=0) +
    scale_fill_manual(name="", values=c("0" = "white", "1" = "red"),labels=c("Didn't succeed","Succeeded")) +
    scale_color_manual(name="", values=c("0" = "blue", "1" = "red"),labels=c("Didn't succeed","Succeeded")) +
    ylab("Percent") +
    xlab("Fitted values") +
    scale_x_continuous(expand=c(0.01,0.01) ,limits = c(0,1), breaks = seq(0,1,0.2)) +
    scale_y_continuous(expand=c(0.00,0.00) ,limits = c(0,80), breaks = seq(0,80,20))+
    theme(legend.position = c(0.3,0.9),
          legend.key.size = unit(x = 0.5, units = "cm")) + theme_minimal()

ggarrange(p1,p2,p3, common.legend = TRUE, nrow = 1)


# Check desciptive stats of cases with and without success in models
ss_1 <- subset( df , df$output==1 )
ss_0 <- subset( df , df$output==0 )

ss_1s <- sum_stat(ss_1,c("lpm1_pred","lpm5_pred",'lpm6_pred',"pred_logit","pred_probit"),
                  c("mean","median","min","max","sd"),num_obs = F)
ss_0s <- sum_stat(ss_0,c("lpm1_pred","lpm5_pred",'lpm6_pred',"pred_logit","pred_probit"),
                  c("mean","median","min","max","sd"),num_obs = F)
ss_1s
ss_0s


# Bias and calibration curve ----------------------------------------------

# make function to create calibration curve

actual_vs_predicted <- df %>%
    ungroup() %>% 
    dplyr::select(actual = output, 
                  predicted = pred_probit) 

num_groups <- 10

calibration_d <- actual_vs_predicted %>%
    mutate(predicted_score_group = dplyr::ntile(predicted, num_groups))%>%
    group_by(predicted_score_group) %>%
    dplyr::summarise(mean_actual = mean(actual), 
                     mean_predicted = mean(predicted), 
                     num_obs = n())

g1 <- ggplot( calibration_d,aes(x = mean_actual, y = mean_predicted)) +
    geom_point( color='red', size=1.5, alpha=0.8) +
    geom_line(  color='red', size=1  , alpha=0.8) +
    geom_abline( intercept = 0, slope = 1, color='blue') +
    labs( x = "Actual event probability", y = "Predicted event probability") +
    scale_x_continuous(expand = c(0.01,0.01), limits = c(0,1), breaks = seq(0,1,0.1)) +
    scale_y_continuous(expand = c(0.01,0.01), limits = c(0,1), breaks = seq(0,1,0.1))
g1

#calibration curve shows that predicted and actual probabilities are not very far away 


# Robustness checks -------------------------------------------------------

# Determine threshold, for categorisation

threshold_finder <- function(data, breaks = c(1:20 *5), predictor = 'pred_probit') {
    
    cat_df <- data.frame('output'=df$output)
    colnames <- c()
    acc <- c()
    
    for(i in breaks){
        col<- ifelse(df[,predictor] >= i/100, 1,0)
        colnames <- c(colnames,paste0('pred_',i))
        cat_df <- cbind(cat_df,data.frame(col))
        names(cat_df)<-c('output',colnames)
        
        colname <- paste0('pred_',i)
        acc <- c(acc,sum(cat_df$output == cat_df[,colname])/nrow(cat_df))
    }
    
    acc_df <- data.frame('breakpoint' = breaks,'thresholds' = colnames, 'accuracy' = acc)
    
    acc_df <- acc_df[order(acc_df$breakpoint),]
    
    p<- ggplot(acc_df, aes(x= breakpoint,y = accuracy )) + 
        geom_bar(aes(x=breakpoint, y = accuracy), stat = 'identity')+
        labs(x = 'Potential thresholds', 'Accuracy') + 
        ylim(c(0,1)) + xlim (c(0,100)) +
        geom_text(aes(y = accuracy, label = sprintf("%0.3f", round(accuracy, digits = 3))), vjust = -0.6, size = 4)+
        theme_minimal()
    return(p)
}

threshold_finder(data= df, predictor = 'pred_probit')


# let's do a confusion matrix here with 0,4 as threshold
df$pred_probit_bin <- ifelse(df$pred_probit > 0.4, 1, 0)

print(paste0(
    'Accuracy of probit is: ',
    round(sum(df$pred_probit_bin == df$output)/nrow(df) * 100,2),
    '%, while the accuracy of baseline is: ',
    round(sum(df$baseline == df$output)/nrow(df) * 100,2),
    '%, so with using the model we can improve ',
    round(sum(df$pred_probit_bin == df$output)/nrow(df) * 100 - sum(df$baseline == df$output)/nrow(df) * 100,2),
    ' percentage points'))

cm <- confusionMatrix(factor(df$pred_probit_bin), factor(df$output))
fourfoldplot(cm$table)

summary(probit)
varImp(probit)
