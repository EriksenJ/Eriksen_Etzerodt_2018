#### Eriksen & Etzerodt (2018) - Data preparation 
## Assumes file "Settings.R" and a set working directory 


## Settings

    source("Settings.R")


    
    

#### Load and prepare data 

    dat <- read.csv("Data/Data_1980_2011.csv", stringsAsFactors = F)


## Add variables for lagged GDP per capita, the growthrate of GDP, and re-define the investment variable in the dataset 

    dat1 <- dat %>%
        select(Country, Year, id, idn, rGDPc, Inflation, csh_i, macrocorp) %>%
        filter( Year < 2003) %>%
        group_by(id) %>%
        mutate(lrGDPc = dplyr::lag(rGDPc),
               growth = (rGDPc/lrGDPc - 1)*100,
               lag_growth = dplyr::lag(growth),
               csh_i = csh_i*100) %>%
        rename(Investment = csh_i)

    
    
    
## Add lags for all variables we want to include in a regression
    
    dat1 <- dat1 %>%
        group_by(id) %>%
        mutate(growth_2 = lag(growth, 2),
               growth_3 = lag(growth, 3),
               growth_4 = lag(growth, 4),
               growth_5 = lag(growth, 5)) %>%
        mutate(Inflation_1 = lag(Inflation, 1),
               Inflation_2 = lag(Inflation, 2),
               Inflation_3 = lag(Inflation, 3),
               Inflation_4 = lag(Inflation, 4),
               Inflation_5 = lag(Inflation, 5)) %>%
        mutate(Investment_1 = lag(Investment, 1),
               Investment_2 = lag(Investment, 2),
               Investment_3 = lag(Investment, 3),
               Investment_4 = lag(Investment, 4),
               Investment_5 = lag(Investment, 5)) %>%
        mutate(macrocorp_1 = lag(macrocorp, 1),
               macrocorp_2 = lag(macrocorp, 2),
               macrocorp_3 = lag(macrocorp, 3),
               macrocorp_4 = lag(macrocorp, 4),
               macrocorp_5 = lag(macrocorp, 5)) %>%
        ungroup()

    
    
## Rename the three countries that we will use in several files to illustrate approaches
    
    
    dat1 <- dat1 %>%
        filter(id %in% c("DEN", "USA", "FRG"), Year > 1980) %>%
        mutate(Country = ifelse(str_detect(Country, pattern = "Den*"), 
                                "Danmark", Country),
               Country = ifelse(str_detect(Country, pattern = "Ger*"), 
                                "Tyskland", Country),
               Country = ifelse(str_detect(Country, pattern = "Uni*"), 
                                "USA", Country), Land = Country)

    
    
    
    
## Show what TSCS data look like ----
    
    # Select a relevant subset to plot 
    TSCS <- dat1 %>% 
        filter(Year > 1980 & Year < 1984) %>%
        select(Country, Year, rGDPc, growth, macrocorp, Inflation, Investment) %>%
        mutate(macrocorp = round(macrocorp, 2),
               Investment = round(Investment, 2),
               growth = round(growth, 2)) %>%
        as.matrix() 
    
    # Create table 
    stargazer(TSCS, type = "html", out = "Output/TSCS.htm",
              title = "Eksempel på time-series cross-section data")
    
    
    
    
    
    
## Save the final dataset ----
    
    write_dta(dat1, "Data/Data_cleaned.dta")

    
    
        