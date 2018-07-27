## Main file 


## Load packages and set wd ---------------------

    library(tidyverse)
    library(rstudioapi)
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
    
    
    
    #setwd("C:/Users/jeri/Dropbox/Economics/Bachelor_Projekt/Metode_Paper/Analysis")
    
    setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
    
    options(scipen = 99999)
    
    
    
## Load data -----------
    
    dat <- read.csv("Data/Data_1980_2011.csv", stringsAsFactors = F)
        
    
    ## Select relevant variables and select relevant timeframe for analysis
    dat1 <- dat %>%
        select(Country, Year, id, idn, rGDPc, Inflation, csh_i, macrocorp) %>%
        filter( Year < 2003) %>%
        group_by(id) %>%
        mutate(lrGDPc = lag(rGDPc),
               growth = (rGDPc/lrGDPc - 1)*100,
               lag_growth = lag(growth),
               csh_i = csh_i*100) %>%
        rename(Investment = csh_i)
    
    
    ## Create lags of growth, inflation, and investment rates, 
    ## as well as macrocorporatism variables  
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
               macrocorp_5 = lag(macrocorp, 5))
    
    
    
    
    
## Descriptive stats and plots --------------------------------------
    
    source("Code/Descriptives.R")
        
    
    
    
    

# Effect of Country dummies  ----------------
    
# Create residualized series 
    # Create country dummies 
    for(ID in unique(dat_change$id)){
        dat_change[, ID] <- ifelse(dat_change[, "id"] == ID, 1, 0)
    }
    
    
    # Residuals from country dummies 
    dat_change$growth_res <- lm(growth ~ USA + FRG, data = dat_change)$residuals
    dat_change$macrocorp_res <- lm(macrocorp ~ USA + FRG, data = dat_change)$residuals
    
    # Residuals from country dummies + covariates 
    dat_change$growth_res2 <- lm(growth ~ Inflation + Investment + USA + FRG, data = dat_change)$residuals
    dat_change$macrocorp_res2 <- lm(macrocorp ~ Inflation + Investment + USA + FRG, data = dat_change)$residuals
    
    # Residuals from country dummies + covariates 
    dat_change$growth_res3 <- lm(growth ~ Inflation + Investment, data = dat_change)$residuals
    dat_change$macrocorp_res3 <- lm(macrocorp ~ Inflation + Investment, data = dat_change)$residuals
    
    
# Correlations between growth rates 
    d <- dat_change %>%
        select(growth, Country, Year) %>%
        spread(Country, growth) %>%
        mutate(DK_DE = cor(Danmark, Tyskland),
               DE_US = cor(Tyskland, USA),
               DK_US = cor(Danmark, USA)) %>%
        select(DK_DE, DE_US, DK_US) %>%
        head(1)
    
    d <- dat_change %>%
        select(growth_res, Country, Year) %>%
        spread(Country, growth_res) %>%
        mutate(DK_DE = cor(Danmark, Tyskland),
               DE_US = cor(Tyskland, USA),
               DK_US = cor(Danmark, USA)) %>%
        select(DK_DE, DE_US, DK_US) %>%
        head(1)
    
    d <- dat_change %>%
        select(macrocorp, growth, Country) %>%
        group_by(Country) %>%
        summarize(Mean_koordinering = mean(macrocorp), Mean_growth = mean(growth))
        
        
        
# Plot the different series 
    
    dat_change <- dat_change %>%
        mutate(Land = Country)
    
    d1 <- ggplot(dat_change, aes(Year, growth, color = Land, linetype = Land)) +
        geom_line() + 
        theme_classic() +
        ggtitle("(a) Vækst i BNP per Indbygger") +
        xlab("År") + 
        ylab("Vækstrate") +
        ylim(-5.1,7) + 
        theme(legend.position = "none")
    

    d2 <- ggplot(dat_change, aes(Year, growth_res, color = Land, linetype = Land)) +
        geom_line() + 
        theme_classic() +
        ggtitle("(b) Vækst i BNP per Indbygger \n Kontrol for  Lande") +
        xlab("År") + 
        ylab("Vækstrate") +
        ylim(-5.1,7) 
    
    d3 <- ggplot(dat_change, aes(Year, macrocorp, color = Land, linetype = Land)) +
        geom_line() + 
        theme_classic() +
        ggtitle("(c) Koordinering") +
        xlab("År") + 
        ylab("Index") +
        ylim(-2,2)  + 
        theme(legend.position = "none") 
    
    d4 <- ggplot(dat_change, aes(Year, macrocorp_res, color = Land, linetype = Land)) +
        geom_line() + 
        theme_classic() +
        ggtitle("(d) Koordinering - Kontrol for Lande") +
        xlab("År") + 
        ylab("Index") +
        ylim(-2,2) 
    
    
    d5 <- ggplot(dat_change, aes(macrocorp, growth)) +
        geom_point(aes(color = Land)) + 
        theme_classic() +
        ggtitle("(e) Scatterplot") +
        xlab("Koordineringe") + 
        ylab("Vækstrate") +
        xlim(-2,2) +
        ylim(-5.1,7) +
        geom_smooth(method = "lm") +
        theme(legend.position = "none") 
    
    
    d6 <- ggplot(dat_change, aes(macrocorp_res, growth_res)) +
        geom_point(aes(color = Land)) + 
        theme_classic() +
        ggtitle("(f) Scatterplot - Kontrol for Lande") +
        xlab("Koordinering") + 
        ylab("Vækstrate") +
        geom_smooth(method = "lm") +
        xlim(-0.7, 0.2) 
    
    d <- grid.arrange(d1, d2, d3, d4, d5, d6, nrow = 3)
    ggsave(plot = d, "Plot/timeseries_scatterplot.png", device = "png", width = 12, height = 10)
    
    
    # Histograms of macrocorp and macrocorp_res
    dat_mc <- dat_change %>%
        select(Country, macrocorp, macrocorp_res) %>%
        group_by(Country) %>%
        gather(-Country, key = key, value = value)
    d <- ggplot(dat_mc, aes(value, fill = as.factor(Country))) + 
        geom_histogram() +
        facet_grid(. ~ key) +
        theme_classic() + 
        xlab("Værdi") + 
        ylab("Frekvens") +
        scale_fill_discrete(name = "Land") +
        theme(legend.position = "bottom")
    plot(d)
    ggsave(filename = "Plot/hist_macrocorp_res.png", device = "png", width = 7, height = 4)
    
    
    # Create corroborating regression tables 
    dat2 <- filter(dat1, !is.na(growth), Year < 2002)
    dat3 <- filter(dat1, !is.na(lag_growth), Year < 2002)
    g_m_1 <- lm(growth ~ macrocorp, data = dat2)
    g_m_2 <- lm(growth ~ macrocorp + Investment + Inflation, data = dat2)
    g_m_3 <- lm(growth ~ macrocorp + Investment + Inflation + as.factor(id), data = dat2)
    g_m_4 <- lm(growth ~ lag_growth + macrocorp + Investment + Inflation + as.factor(id), data = dat3)
    
    # Panel corrected standard errors 
    pcse_1 <- pcse(g_m_1, groupN = dat2$id, groupT = dat2$Year)
    pcse_2 <- pcse(g_m_2, groupN = dat2$id, groupT = dat2$Year)
    pcse_3 <- pcse(g_m_3, groupN = dat2$id, groupT = dat2$Year)
    pcse_4 <- pcse(g_m_4, groupN = dat3$id, groupT = dat3$Year)
    std_1 <- sqrt(diag(pcse_1$vcov))
    std_2 <- sqrt(diag(pcse_2$vcov))
    std_3 <- sqrt(diag(pcse_3$vcov))
    std_4 <- sqrt(diag(pcse_4$vcov))
    
    (wald_result_1 <- waldtest(g_m_1, vcov = pcse_1$vcov))
    (wald_result_2 <- waldtest(g_m_2, vcov = pcse_2$vcov))
    (wald_result_3 <- waldtest(g_m_3, vcov = pcse_3$vcov))
    (wald_result_4 <- waldtest(g_m_4, vcov = pcse_4$vcov))
    
    
    stargazer(g_m_1, g_m_2, g_m_3, 
              se = list(std_1, std_2, std_3), 
              omit.stat = c("ll", "f"),
              add.lines = list(c("Wald Statistic", 
                                 str_c(round(wald_result_1[2, 3], 2)), 
                                 str_c(round(wald_result_2[2, 3], 2)), 
                                 str_c(round(wald_result_3[2, 3], 2)))),
              notes = "The models are estimated with panel corrected standard errors.",
              type = "html", out = "Output/growth_macrocorp.htm")
    
    
    
    
    ## show scatterplots of macrocorp and growth rate after controlling 
    dat2$res_1 <- lm(macrocorp ~ Investment + Inflation + as.factor(id), data = dat2)$residuals
    dat2$res_2 <- lm(growth ~ Investment + Inflation + as.factor(id), data = dat2)$residuals
    
    d <- ggplot(dat2, aes(x = res_1, y = res_2)) +
        geom_point(aes(color = id)) +
        geom_smooth(method = "lm")
    plot(d)
    
    d <- ggplot(dat2, aes(x = macrocorp)) +
        geom_histogram(aes(fill = id))
    plot(d)
    
    d <- ggplot(dat2, aes(x = macrocorp)) +
        geom_histogram(aes(fill = id))
    plot(d)
    
    
    
    
    
    
    # Effect of Year dummies  ----------------
    
    # Create Year dummies
    Years <- 0
    i = 1
    for(Y in unique(dat_change$Year)){
        dat_change[, str_c("Y", Y)] <- ifelse(dat_change[, "Year"] == Y, 1, 0)
        Years[i] <- str_c("Y", Y)
        i = i + 1
    }
    
    # Create residual series from regression onto year dummies
    datY <- dat_change[, c("growth", Years[2:length(Years)])]
    dat_change$growth_resY <- lm(growth ~ ., data = datY)$residuals
    datY <- dat_change[, c("macrocorp", Years[2:length(Years)])]
    dat_change$macrocorp_resY <- lm(macrocorp ~ ., data = datY)$residuals
    
    
    # Plotting the series 
    d1 <- ggplot(dat_change, aes(Year, growth, color = Land, linetype = Land)) +
        geom_line() + 
        theme_classic() +
        ggtitle("(a) Vækst i BNP per Indbygger") +
        xlab("År") + 
        ylab("Vækstrate") +
        theme(legend.position = "none")
    
    
    d2 <- ggplot(dat_change, aes(Year, growth_resY, color = Land, linetype = Land)) +
        geom_line() + 
        theme_classic() +
        ggtitle("(b) Vækst i BNP per Indbygger \n Kontrol for År") +
        xlab("År") + 
        ylab("Vækstrate") 
    
    d3 <- ggplot(dat_change, aes(Year, macrocorp, color = Land, linetype = Land)) +
        geom_line() + 
        theme_classic() +
        ggtitle("(c) Koordinering") +
        xlab("År") + 
        ylab("Index") +
        theme(legend.position = "none") 
    
    d4 <- ggplot(dat_change, aes(Year, macrocorp_resY, color = Land, linetype = Land)) +
        geom_line() + 
        theme_classic() +
        ggtitle("(d) Koordinering - Kontrol for År") +
        xlab("År") + 
        ylab("Index") 
    
    
    d5 <- ggplot(dat_change, aes(macrocorp, growth)) +
        geom_point(aes(color = Land)) + 
        theme_classic() +
        ggtitle("(e) Scatterplot") +
        xlab("Koordinering") + 
        ylab("Vækstrate") +
        geom_smooth(method = "lm") +
        theme(legend.position = "none") 
    
    
    d6 <- ggplot(dat_change, aes(macrocorp_resY, growth_resY)) +
        geom_point(aes(color = Land)) + 
        theme_classic() +
        ggtitle("(f) Scatterplot - Kontrol for År") +
        xlab("Koordinering (residual)") + 
        ylab("Vækstrate (residual)") +
        geom_smooth(method = "lm") 
    
    d <- grid.arrange(d1, d2, d3, d4, d5, d6, nrow = 3)
    ggsave(plot = d, "Plot/timeseries_year_scatterplot.png", device = "png", width = 12, height = 10)
    
    # Plot growth and macrocorp residualized series 
    plot_ts_vars(dataset = dat_change, var = "growth_resY", ytext = "Residual vækstrate",  filenam = "plot/rGDPc_growth_resY.png", ylim = c(-3, 6))
    plot_ts_vars(dataset = dat_change, var = "macrocorp_resY", ytext = "Residual makro korporatisme", filenam = "plot/macrocorp_resY.png")
    
    # Scatterplot of partial growth ~ macrocorp
    d <- ggplot(dat_change, aes(x = macrocorp_resY, y = growth_resY)) +
        geom_point(aes(color = Land, shape = Land)) +
        theme_classic() + 
        xlab("Makro Korporatisme (residual)") +
        ylab("Vækstrate i real BNP per Indbygger \n (residual)") +
        theme(legend.position = "bottom") +
        geom_smooth(method = "lm", color = "black")
    plot(d)
    ggsave(filename = "Plot/Scatterplot_g_m_resY.png", device = "png", width = 5.5, height = 4)
    
    # Create corroborating regression tables 
    g_m_1 <- lm(growth ~ macrocorp, data = dat_change)
    datY <- dat_change[, c("growth", "macrocorp", Years[2:length(Years)])]
    g_m_2 <- lm(growth ~ macrocorp + ., data = datY)
    
    # Panel corrected standard errors 
    pcse_1 <- pcse(g_m_1, groupN = dat_change$Land, groupT = dat_change$Year)
    pcse_2 <- pcse(g_m_2, groupN = dat_change$Land, groupT = dat_change$Year)
    std_1 <- sqrt(diag(pcse_1$vcov))
    std_2 <- sqrt(diag(pcse_2$vcov))
    
    (wald_result_1 <- waldtest(g_m_1, vcov = pcse_1$vcov))
    (wald_result_2 <- waldtest(g_m_2, vcov = pcse_2$vcov))
    
    
    
    stargazer(g_m_1, g_m_2, 
              se = list(std_1, std_2), 
              omit.stat = c("ll", "f"),
              add.lines = list(c("F statistic (df = 1; 65)", 
                                 str_c(round(wald_result_1[2, 3], 2)), 
                                 str_c(round(wald_result_2[2, 3], 2)))),
              notes = "The models are estimated with panel corrected standard errors.",
              type = "html", out = "Output/growth_macrocorp_yeardum.htm")
    
    
    
    # Checking correlations: 
    dat_g <- dat_change %>%
        select(Country, Year, growth_resY) %>%
        spread(key = Country, value = growth_resY)
    
    cor(dat_g$Danmark, dat_g$Tyskland)
    cor(dat_g$Danmark, dat_g$USA)
    cor(dat_g$Tyskland, dat_g$USA)
    
    
    # Checking acf for g_m_3
    A <- g_m_3$residuals
    acf(A)
    


    
## Estimating a full model
    
    dat_lim <- dat1 %>%
        na.omit()
    
    ## Full regression with pcse
    LM_1 <- lm(growth ~ lag_growth + growth_2 + growth_3 + growth_4 + growth_5 + 
                   macrocorp + macrocorp_1 + macrocorp_2 + macrocorp_3 + macrocorp_4 + macrocorp_5 +
                   Inflation + Inflation_1 + Inflation_2 + Inflation_3 + Inflation_4 + Inflation_5 + 
                   Investment + Investment_1 + Investment_2 + Investment_3 + Investment_4 + Investment_5 +
                   as.factor(id), data = dat_lim)
    pcse_1 <- pcse(LM_1, groupN = dat_lim$id, groupT = dat_lim$Year)
    std_1 <- sqrt(diag(pcse_1$vcov))
    summary(pcse_1)
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
    ## It appears that we can remove all but the concurrent observation
    
    
    
    
    
    
    
    
    
    
    
    
    
    
## Unit roots ----------------------------
# Create plots of series w/o unit root and w/o trend ----------------------
    
    # Create length of time series
    length = 50
    time <- 1:length
    
    # Create constant (timetrend)
    Alpha = 10
    
    # Trend series
    Y0 = 0 ## Initial Value
    Y1 <-  as.data.frame(cbind(matrix(0, nrow = length, 10), time)) ## Unit Root
    Y2 <-  as.data.frame(cbind(matrix(0, nrow = length, 10), time)) ## Stationary
    Y3 <-  as.data.frame(cbind(matrix(0, nrow = length, 10), time)) ## Unit root with drift  
    Y4 <-  as.data.frame(cbind(matrix(0, nrow = length, 10), time)) ## Trend stationary 
    
    # Set number of simulations per series
    B = 10
    
    # Run simulations, setting initial values of series to Y0
    for(c in 1:10){
        
        for(i in 2:length) {
            if(i == 2) { # Set intial values
                Y1[i-1, c] = Y0 ## Unit root 
                Y2[i-1, c] = Y0 ## Stationary 
                Y3[i-1, c] = Y0 ## Unit root with drift
                Y4[i-1, c] = Y0 ## Trend stationary 
            }
            
            error <- rnorm(1, 0, 30) # Error with mean 0 and variance 1
            
            Y1[i, c] = Y1[i-1, c] + error                               ## Unit root 
            Y2[i, c] = error                                            ## stationary series 
            Y3[i, c] = Y3[i-1, c] + time[i] + error               ## Unit root with drift
            Y4[i, c] = Alpha*time[i] + error                            ## Trend stationary series 
        }
        
        colnames(Y1)[c] <- paste("Simulation ", c, sep = "")    ## Rename columns in accordance wih simulation run 
        colnames(Y2)[c] <- paste("Simulation ", c, sep = "")
        colnames(Y3)[c] <- paste("Simulation ", c, sep = "")
        colnames(Y4)[c] <- paste("Simulation ", c, sep = "")
    }
    
    
    # To plot the series I need them i long format 
    Y1 <- gather(Y1, -time, key = "Simulation", value = "value") 
    Y2 <- gather(Y2, -time, key = "Simulation", value = "value") 
    Y3 <- gather(Y3, -time, key = "Simulation", value = "value") 
    Y4 <- gather(Y4, -time, key = "Simulation", value = "value") 
        
    # Create the plots of each series (not plotting until the end)
    plot_ur_series <- function(dataset, title = "Unit root", UR_Stationary = F){
        d <- ggplot(dataset, aes(dataset[, "time"], dataset[, "value"], linetype = dataset[, "Simulation"])) +
            geom_line() +
            xlab("Tid") +
            ylab("Værdi") +
            ggtitle(title) +
            theme_classic() +
            theme(plot.title = element_text(hjust = 0.5),
                  legend.position = "none")
        if(isTRUE(UR_Stationary)){
            d <- d + ylim(-400, 400)
        }
        return(d)
        plot(d)
    }
    
    y1 <- plot_ur_series(Y1, title = "(a) Unit root", UR_Stationary = T)
    y2 <- plot_ur_series(Y2, title = "(b) Stationær", UR_Stationary = T)
    y3 <- plot_ur_series(Y3, title = "(c) Unit root med drift")
    y4 <- plot_ur_series(Y4, title = "(d) Trend Stationær")
    
    grid.arrange(y1, y2, y3, y4, ncol = 2)
    
    
    g <- arrangeGrob(y1, y2, y3, y4, ncol = 2)
    ggsave(filename = "Plot/Unit_trend_plots.png", g, width = 10, height = 10)
    
    
# Test for unit root in rGDPc and growth ----------------------
    
    # set as pdata 
    pdat <- plm::pdata.frame(dat1, index = "Country")
    pdat1 <- plm::pdata.frame(filter(dat1, Year > 1980), index = "Country")
    glimpse(pdat)
    
    # test rGDPc 
    (pur_rGDPc_level <- plm::purtest(rGDPc ~ 1, data = pdat, test = "ips", pmax = 1))
    
    y <- data.frame(split(pdat1$growth, pdat1$Country))
    (pur_growth <- plm::purtest(y, 
                               test = "ips", pmax = 4,  exo = "trend"))
    
    
# test for unit roots by country and series
    
    # Write function that tests series by countries separately
    adf_test <- function(dataset, variable){ # Function to calculate individual ADF test sizes and compare with 5% sign. level cval
        # Set up storage vectors
        reject_n = c(0)
        reject_d = c(0)
        reject_t = c(0)
        stat_n <- c(0)
        stat_d <- c(0)
        stat_t <- c(0)
        crit_n <- c(0)
        crit_d <- c(0)
        crit_t <- c(0)
        Country = c("c")
        i = 1 # Count for loop
        
        for(C in as.character(unique(dataset[, "Country"]))){ # Loop over all countries, performing test for each 
            Country[i] = C # Set name 
            d <- NULL # Remove old name 
            d <- pdat %>% filter(Country == C) %>% select_(variable) %>% as.matrix() # Subset to relevant country-series
            if(sum(is.na(d) > 0)){
                d <- d[-which(is.na(d))] # If the series contains NA's, remove these (diff series or similar)
            }
            
            X <- ur.df(d, type = c("none"), selectlags = "AIC") # Calculate ADF for "none" option and store values (below)
            reject_n[i] = ifelse(X@teststat[1, 1] < X@cval[1, 2], "Yes", "No")
            stat_n[i] <- round(X@teststat[1, 1], 3)
            crit_n[i] <- as.numeric(X@cval[2])
            
            X <- ur.df(d, type = c("drift"), selectlags = "AIC") # Calculate ADF for "drift" option and store values (below)
            reject_d[i] = ifelse(X@teststat[1, 1] < X@cval[1, 2], "Yes", "No")
            stat_d[i] <- round(X@teststat[1, 1], 3)
            crit_d[i] <- X@cval[1, 2]
            
            X <- ur.df(d, type = c("trend"), selectlags = "AIC") # Calculate ADF for "trend" option and store values (below)
            reject_t[i] = ifelse(X@teststat[1, 1] < X@cval[1, 2], "Yes", "No")
            stat_t[i] <- round(X@teststat[1, 1], 3)
            crit_t[i] <- X@cval[1, 2]
            
            i = i + 1
        }
        
        adf <- cbind(Country, 
                     stat_n, crit_n, reject_n, 
                     stat_d, crit_d, reject_d,
                     stat_t, crit_t, reject_t)
        
        return(adf)
    }
    
    # Run the function for rGDPc and growth
    (adf_rGDPc <- adf_test(pdat, "rGDPc"))
    (adf_growth <- adf_test(pdat, "growth"))
    
    
# Plot ACF for each country for series 
    # rGDPc
    for(C in unique(dat1$Country)){
        d <- filter(dat1, Country == C)
        acf(d[, "rGDPc"])
    }
    
    # growth
    for(C in unique(dat1$Country)){
        d <- filter(dat1, Country == C) %>%
            group_by() %>%
            select(growth) %>%
            drop_na()
        acf(d)
    }
    
# Plot pacf for each country for series 
    
    # growth
    for(C in unique(dat1$Country)){
        d <- filter(dat1, Country == C) %>%
            group_by() %>%
            select(growth) %>%
            drop_na()
        acf(d, type = "partial")
    }
    
    acf()
    
    

## Illustration of ADL and FDL series -------------------
    
    
    # Create length, X, beta, and initial value 
    length = 50         # Total length of simulation
    time <- 1:length    # Time-series 
    X = rep(0, 50)      # Baseline X
    X[10] = 1           # Temp shock in period 10
    X2 = rep(0, 50)     # Create baseline for X2 
    X2[10:length] = 1   # Create permenent shock in X2
    beta =50            # Set beta 
    Y0 = 0              # Initial Value
    phi = 0.7           # Set phi for adl
    rho = 0.7           # Set rho for sc
    beta_2 = 30         # Set beta_2 for fdl
    error = rnorm(length, 0, 1) # Error term series
    
    # Create series 
    for(i in 2:length){
        if (i-1 == 1){ # Set initial value of series 
            Y_adl = Y0
            Y_fdl = Y0
            Y_sc = Y0
            Y_adl_2 = Y0
            Y_fdl_2 = Y0
            Y_sc_2 = Y0
        }
        
        # Temporary shock to X 
        Y_adl[i] = phi*Y_adl[i-1] + beta*X[i] + error[i] 
        Y_fdl[i] = beta*X[i] + beta_2*X[i-1] + error[i] 
        Y_sc[i] = beta*X[i] + rho*Y_sc[i-1] - rho*beta*X[i-1] + error[i]  
        
        # Permanent shock to X
        Y_adl_2[i] = phi*Y_adl_2[i-1] + beta*X2[i] + error[i]
        Y_fdl_2[i] = beta*X2[i] + beta_2*X2[i-1] + error[i]
        Y_sc_2[i] = beta*X2[i] + rho*Y_sc[i-1] - rho*beta*X[i-1] + error[i]   
    }
    
    # Merge into dataframe and plot series 
    d_temp <- cbind(time, Y_adl, Y_fdl, Y_sc) %>%
        as.data.frame() %>%
        rename(ADL = Y_adl, 
               FDL = Y_fdl,
               SC = Y_sc) %>%
        gather(-time, key = "Model", value = "value")
    
    d_perm <- cbind(time, Y_adl_2, Y_fdl_2, Y_sc_2) %>%
        as.data.frame() %>%
        rename(ADL = Y_adl_2, 
               FDL = Y_fdl_2,
               SC = Y_sc_2) %>%
        gather(-time, key = "Model", value = "value")
    
    # Plot models 
    plot_models <- function(dataset, title = "Temporary shock", shock = "temp"){
        p <- ggplot(dataset, aes(dataset[, "time"], dataset[, "value"], linetype = Model)) +
            geom_line() +
            xlab("Tid") +
            ylab("Værdi") +
            ggtitle(title) +
            theme_classic() +
            theme(plot.title = element_text(hjust = 0.5),
                  legend.position = "bottom")
        plot(p)
        ggsave(filename = paste0("Plot/ADL_FDL_", shock, ".png"), device = "png", width = 5.5, height = 4)
    }
    
    plot_models(d_temp, title = "Midlertidigt Shock", shock = "temp")
    plot_models(d_perm, title = "Permanent Shock", shock = "perm")
    
    
    
    
    
## Test for serial correlation in model specification -----------------------
    
    # We can use the Breusch-Godfrey test to test for serial correlation in panels. 
    
    p_model <- plm(growth ~ Inflation + Investment + macrocorp, data = pdat, model = "pooling")
    p_model_1 <- plm(growth ~ lag(growth) + Inflation + Investment + macrocorp, data = pdat, model = "pooling", effect = "time")
    p_model_2 <- plm(growth ~ lag(growth) + lag(growth, 2) + Inflation + Investment + macrocorp, data = pdat, model = "pooling", effect = "time")
    pbgtest(p_model)
    pbgtest(p_model_1)
    pbgtest(p_model_2)
    
    

## Show what TSCS data look like ----------------------------------
    
    TSCS <- dat_change %>% 
        filter(Year < 1984) %>%
        select(Country, Year, rGDPc, growth, macrocorp, Inflation, Investment) %>%
        mutate(macrocorp = round(macrocorp, 2),
               Investment = round(Investment, 2),
               growth = round(growth, 2)) %>%
        as.matrix() 
        
    
    
    stargazer(TSCS,
              type = "html", out = "Output/TSCS.htm")
    stargazer(TSCS,
              type = "latex", out = "Output/TSCS.tex",
              title = "Eksempel på time-series cross-section data")
    
    
    
## Save the dataset we use ----------------------------
    
    write.dta(dat1, "Data/Dat1.dta")
    
    