#### Eriksen & Etzerodt (2018) - Example of Box-Jenkins inspired estimation procedure 





## Load data and prepare dataset for estimations  
    


    dat1 <- read_dta("Data/Data_cleaned.dta")
    
    # remove missing observations from the dataset 
    dat_lim <- dat1 %>%
        na.omit()
    



## Estimating a full model ----
    
    # Full regression with pcse and country specific intercepts 
    LM_1 <- lm(growth ~ lag_growth + growth_2 + growth_3 + growth_4 + 
                   growth_5 +  macrocorp + macrocorp_1 + macrocorp_2 + 
                   macrocorp_3 + macrocorp_4 + macrocorp_5 +
                   Inflation + Inflation_1 + Inflation_2 + Inflation_3 +
                   Inflation_4 + Inflation_5 + Investment + Investment_1 +
                   Investment_2 + Investment_3 + Investment_4 + Investment_5 +
                   as.factor(id), data = dat_lim)
    pcse_1 <- pcse(LM_1, groupN = dat_lim$id, groupT = dat_lim$Year)
    std_1 <- sqrt(diag(pcse_1$vcov))
    summary(pcse_1)
    
    # The ACF plot can be used to 
    acf(LM_1$residuals)
    dwtest(growth ~ lag_growth + growth_2 + growth_3 + growth_4 + growth_5 + 
               macrocorp + macrocorp_1 + macrocorp_2 + macrocorp_3 + macrocorp_4 + macrocorp_5 +
               Inflation + Inflation_1 + Inflation_2 + Inflation_3 + Inflation_4 + Inflation_5 + 
               Investment + Investment_1 + Investment_2 + Investment_3 + Investment_4 + Investment_5 +
               as.factor(id), data = dat_lim)


## Test if lags of investment are statistically significant (t-test suggests not)
    linearHypothesis(LM_1, c("Investment_2 = 0", "Investment_3 = 0", "Investment_4 = 0", "Investment_5 = 0"), vcov. = pcse_1$vcov) 


## Reestimate without investment lags 
    LM_2 <- lm(growth ~ lag_growth + growth_2 + growth_3 + growth_4 + growth_5 + 
                   macrocorp + macrocorp_1 + macrocorp_2 + macrocorp_3 + macrocorp_4 + macrocorp_5 +
                   Inflation + Inflation_1 + Inflation_2 + Inflation_3 + Inflation_4 + Inflation_5 + 
                   Investment +  Investment_1 +
                   as.factor(id), data = dat_lim)
    pcse_2 <- pcse(LM_2, , groupN = dat_lim$id, groupT = dat_lim$Year)
    summary(pcse_2)

## Test if lags of investment are statistically significant (t-test suggests not)
    linearHypothesis(LM_2, c("growth_2 = 0", "growth_3 = 0", "growth_4 = 0", "growth_5 = 0"), vcov. = pcse_2$vcov) ## We can seemingly remove the lags of investment 
    ## We can remove lags of growth beyond first 


## Reestimate without growth lags 
    LM_3 <- lm(growth ~ lag_growth + 
                   macrocorp + macrocorp_1 + macrocorp_2 + macrocorp_3 + macrocorp_4 + macrocorp_5 +
                   Inflation + Inflation_1 + Inflation_2 + Inflation_3 + Inflation_4 + Inflation_5 + 
                   Investment +  Investment_1 +
                   as.factor(id), data = dat_lim)
    pcse_3 <- pcse(LM_3, groupN = dat_lim$id, groupT = dat_lim$Year)
    summary(pcse_3)
    
    ## Test if lags of investment are statistically significant (t-test suggests not)
    linearHypothesis(LM_3, c("Inflation_3 = 0", "Inflation_4 = 0", "Inflation_5 = 0"), vcov. = pcse_3$vcov) ## We can seemingly remove the lags of investment 
    ## We can remove lags of inflation beyond second 


## Test if macrocorp has anything at all to do in the model
    LM_4 <- lm(growth ~ lag_growth + 
                   macrocorp + macrocorp_1 + macrocorp_2 + macrocorp_3 + macrocorp_4 + macrocorp_5 +
                   Inflation + Inflation_1 + Inflation_2 + 
                   Investment +  Investment_1 +
                   as.factor(id), data = dat_lim)
    pcse_4 <- pcse(LM_4, groupN = dat_lim$id, groupT = dat_lim$Year)
    summary(pcse_4)
    
    ## Test if lags of investment are statistically significant (t-test suggests not)
    linearHypothesis(LM_4, c("macrocorp = 0", "macrocorp_1 = 0", "macrocorp_2 = 0", "macrocorp_3 = 0", "macrocorp_4 = 0", "macrocorp_5 = 0"), vcov. = pcse_4$vcov) ## We can seemingly remove the lags of investment 
    
    






## Test for serial correlation in model specification ----
    
    # We can use the Breusch-Godfrey test to test for serial correlation in panels. 
    
    p_model <- plm(growth ~ Inflation + Investment + 
                       macrocorp, data = pdat, model = "pooling")
    p_model_1 <- plm(growth ~ lag(growth) + Inflation + 
                         Investment + macrocorp, data = pdat, 
                     model = "pooling", effect = "time")
    p_model_2 <- plm(growth ~ lag(growth) + lag(growth, 2) + Inflation + 
                         Investment + macrocorp, data = pdat, 
                     model = "pooling", effect = "time")
    pbgtest(p_model)
    pbgtest(p_model_1)
    pbgtest(p_model_2)
    
    
