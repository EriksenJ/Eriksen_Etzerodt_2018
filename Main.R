## Main file 


## Load packages and set wd ---------------------

library(tidyverse)
library(haven)
library(directlabels)
library(plm)
library(reshape2)
library(gridExtra)
library(urca)
library(stargazer)
library(pcse)
library(lmtest)
library(stats)
library(stringr)
library(foreign)
library(car)


options(scipen = 99999)



## Load data -----------
    
    dat <- read_dta("Data/dat1.dta")

    
    ## Select relevant variables and select relevant timeframe for analysis
    dat1 <- dat %>%
        select(Country, Year, id, idn, rGDPc, Inflation, Investment, macrocorp) %>%
        filter( Year < 2003) %>%
        group_by(id) %>%
        mutate(lrGDPc = lag(rGDPc),
               growth = (rGDPc/lrGDPc - 1)*100,
               lag_growth = lag(growth))
    
    
    ## Load dataset containing only DK, DE, and the US
    dat_change <- read_dta("Data/dat_change.dta")


## Load Functions ---------
    
    source("Code/Functions.R")



## Descriptive stats and plots --------------------------------------

    source("Code/Descriptive_stats.R")




# Effect of Country dummies  ----------------
    
    source("Code/Unit_dummies.R")




# Effect of Year dummies  ----------------

    source("Code/Time_dummies.R")




## Simulate Unit root series ----------------------------
    
    source("Code/Simulate_unit_roots.R")




## Show TSCS data ----------------------------------
    
    source("Code/Show_TSCS_data.R")

