#### Eriksen & Etzerodt (2018) - Fixed Effects Example







#### Exemplify the effects of Including fixed effects 
    
    

## Load data and filter dataset 

    # Read data 
    dat1 <- read_dta("Data/Data_cleaned.dta")
    
    
    # Limit dataset to 2002 (available dataset)
    dat_change <- dat1 %>% 
        filter(Year < 2003 & Year > 1980) %>%
        select(Year, Country, id, rGDPc, macrocorp, 
               growth, Inflation, Investment) %>%
        filter(!is.na(macrocorp))




    # Create country dummies 
        for(ID in unique(dat_change$id)){
            dat_change[, ID] <- ifelse(dat_change[, "id"] == ID, 1, 0)
        }
    


## Effect of Country dummies  ----------------
    
    
    ## Create residualized series 
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
    dat_change %>%
        select(growth, Country, Year) %>%
        spread(Country, growth) %>%
        mutate(DK_DE = cor(Danmark, Tyskland),
               DE_US = cor(Tyskland, USA),
               DK_US = cor(Danmark, USA)) %>%
        select(DK_DE, DE_US, DK_US) %>%
        head(1)
    
    dat_change %>%
        select(growth_res, Country, Year) %>%
        spread(Country, growth_res) %>%
        mutate(DK_DE = cor(Danmark, Tyskland),
               DE_US = cor(Tyskland, USA),
               DK_US = cor(Danmark, USA)) %>%
        select(DK_DE, DE_US, DK_US) %>%
        head(1)
    
    dat_change %>%
        select(macrocorp, growth, Country) %>%
        group_by(Country) %>%
        summarize(Mean_koordinering = mean(macrocorp), Mean_growth = mean(growth))
    
    
    
    # Plot the different series 
    
    dat_change <- dat_change %>%
        mutate(Land = Country)
    
    d1 <- ggplot(dat_change, aes(Year, growth, linetype = Land)) +
        geom_line() + 
        theme_classic() +
        ggtitle("(a) Vækst i BNP per Indbygger") +
        xlab("År") + 
        ylab("Vækstrate") +
        ylim(-5.1,7) + 
        theme(legend.position = "none")
    
    
    d2 <- ggplot(dat_change, aes(Year, growth_res, linetype = Land)) +
        geom_line() + 
        theme_classic() +
        ggtitle("(b) Vækst i BNP per Indbygger \n Kontrol for  Lande") +
        xlab("År") + 
        ylab("Vækstrate") +
        ylim(-5.1,7) 
    
    d3 <- ggplot(dat_change, aes(Year, macrocorp, linetype = Land)) +
        geom_line() + 
        theme_classic() +
        ggtitle("(c) Koordinering") +
        xlab("År") + 
        ylab("Index") +
        ylim(-2,2)  + 
        theme(legend.position = "none" )
    
    d4 <- ggplot(dat_change, aes(Year, macrocorp_res, linetype = Land)) +
        geom_line() + 
        theme_classic() +
        ggtitle("(d) Koordinering - Kontrol for Lande") +
        xlab("År") + 
        ylab("Index") +
        ylim(-2,2) 
    
    
    d5 <- ggplot(dat_change, aes(macrocorp, growth)) +
        geom_point(aes(shape = Land)) + 
        scale_shape_manual(values=1:length(unique(dat_change$id))) +
        theme_classic() +
        ggtitle("(e) Scatterplot") +
        xlab("Koordineringe") + 
        ylab("Vækstrate") +
        xlim(-2,2) +
        ylim(-5.1,7) +
        geom_smooth(method = "lm", color = "black") +
        theme(legend.position = "none") 
    
    
    d6 <- ggplot(dat_change, aes(macrocorp_res, growth_res)) +
        geom_point(aes(shape = Land)) + 
        scale_shape_manual(values=1:length(unique(dat_change$id))) +
        theme_classic() +
        ggtitle("(f) Scatterplot - Kontrol for Lande") +
        xlab("Koordinering") + 
        ylab("Vækstrate") +
        geom_smooth(method = "lm", color = "black") +
        xlim(-0.7, 0.2) 
    
    d <- grid.arrange(d1, d2, d3, d4, d5, d6, nrow = 3)
    ggsave(plot = d, "Plot/timeseries_scatterplot.png", device = "png", width = 12, height = 10)
    
    
    # Histograms of macrocorp and macrocorp_res
    dat_mc <- dat_change %>%
        select(Land, macrocorp, macrocorp_res) %>%
        group_by(Land) %>%
        gather(-Land, key = key, value = value)
    
    d <- ggplot(dat_mc, aes(value, fill = Land)) + 
        geom_histogram() +
        facet_grid(. ~ key) +
        theme_classic() + 
        xlab("Værdi") + 
        ylab("Frekvens") +
        scale_fill_discrete(name = "Land") +
        theme(legend.position = "bottom") +
        scale_color_brewer(palette="Dark2") +
        scale_fill_grey(start = 0.2, end = 0.8, na.value = "red")
    plot(d)
    ggsave(filename = "Plot/hist_macrocorp_res.png", 
           device = "png", width = 7, height = 4)
    
    
## Create regression tables corroborating results  
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
              type = "html", out = "Output/FE_example_growth_macrocorp.htm")
    
    
    
    
    



## Effect of Year dummies  ----------------
    
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
    d1 <- ggplot(dat_change, aes(Year, growth, linetype = Land)) +
        geom_line() + 
        theme_classic() +
        ggtitle("(a) Vækst i BNP per Indbygger") +
        xlab("År") + 
        ylab("Vækstrate") +
        theme(legend.position = "none")
    
    
    d2 <- ggplot(dat_change, aes(Year, growth_resY, linetype = Land)) +
        geom_line() + 
        theme_classic() +
        ggtitle("(b) Vækst i BNP per Indbygger \n Kontrol for År") +
        xlab("År") + 
        ylab("Vækstrate") 
    
    d3 <- ggplot(dat_change, aes(Year, macrocorp, linetype = Land)) +
        geom_line() + 
        theme_classic() +
        ggtitle("(c) Koordinering") +
        xlab("År") + 
        ylab("Index") +
        theme(legend.position = "none") 
    
    d4 <- ggplot(dat_change, aes(Year, macrocorp_resY, linetype = Land)) +
        geom_line() + 
        theme_classic() +
        ggtitle("(d) Koordinering - Kontrol for År") +
        xlab("År") + 
        ylab("Index") 
    
    
    d5 <- ggplot(dat_change, aes(macrocorp, growth)) +
        geom_point(aes(shape = Land)) + 
        theme_classic() +
        ggtitle("(e) Scatterplot") +
        xlab("Koordinering") + 
        ylab("Vækstrate") +
        geom_smooth(method = "lm", color = "black") +
        theme(legend.position = "none") 
    
    
    d6 <- ggplot(dat_change, aes(macrocorp_resY, growth_resY)) +
        geom_point(aes(shape = Land)) + 
        theme_classic() +
        ggtitle("(f) Scatterplot - Kontrol for År") +
        xlab("Koordinering (residual)") + 
        ylab("Vækstrate (residual)") +
        geom_smooth(method = "lm", color = "black") 
    
    d <- grid.arrange(d1, d2, d3, d4, d5, d6, nrow = 3)
    ggsave(plot = d, "Plot/timeseries_year_scatterplot.png", device = "png", width = 12, height = 10)
    
    
    
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
              type = "html", out = "Output/FE_time_growth_macrocorp.htm")
    
    

    
