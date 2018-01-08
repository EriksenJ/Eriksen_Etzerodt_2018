## Time Dummies Effect

    
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