## Descriptive statistics and plots 

    
    # Prepare dataset with stats by country 
    desc <- dat1 %>%
        select(Country, Year, rGDPc, growth, Inflation, Investment, macrocorp) %>%
        gather(rGDPc, growth, Inflation, Investment, macrocorp, key = "Variable", value = "value") %>%
        group_by(Country, Variable) %>%
        summarize(Mean = round(mean(value, na.rm = T), 2), "Std Dev" = round(sd(value, na.rm = T), 2), T = n(), Na = sum(is.na(value)))
    
    # Prepare dataset with total data 
    desc2 <- dat1 %>%
        select(Country, Year, rGDPc, growth, Inflation, Investment, macrocorp) %>%
        gather(rGDPc, growth, Inflation, Investment, macrocorp, key = "Variable", value = "value") %>%
        group_by(Variable) %>%
        summarize(Mean = round(mean(value, na.rm = T), 2), "Std Dev" = round(sd(value, na.rm = T), 2), Obs = n(), Na = sum(is.na(value)))
    
    # Store descriptive stats in html format 
    stargazer(as.matrix(desc),
              type = "html", 
              out = "Output/desc_stats.htm")
    
    # Store descriptive stats in html format 
    stargazer(as.matrix(desc2),
              type = "html", 
              out = "Output/desc2_stats.htm")
    
    
    # Plot series 
    d <- ggplot(dat1, aes(x = Year, y = growth)) +
        geom_line(aes(color = id)) +
        theme_classic() + 
        labs(title = "(a) Vækstrate i BNP per Indbygger", 
             x = "År",
             y = "Vækstrate") +
        xlim(1980, 2006) 
    d <- direct.label(p = d, method = "last.qp")
    ggsave(filename = "Plot/descriptive_growth.png", plot = d, device = "png", height = 4.2, width = 5.3)
    
    
    d <- ggplot(dat1, aes(x = Year, y = rGDPc)) +
        geom_line(aes(color = id)) +
        theme_classic() + 
        labs(title = "(b) Real BNP per Indbygger", 
             x = "År",
             y = "PPP Dollars") +
        xlim(1980, 2006) 
    d <- direct.label(p = d, method = "last.qp")
    ggsave(filename = "Plot/descriptive_GDPc.png", plot = d, device = "png", height = 4.2, width = 5.3)
    
    
    d <- ggplot(dat1, aes(x = Year, y = macrocorp)) +
        geom_line(aes(color = id)) +
        theme_classic() + 
        labs(title = "(c) Makrokorporatisme", 
             x = "År",
             y = "Index") +
        xlim(1980, 2006) +
        theme(legend.position = "bottom")
    ggsave(filename = "Plot/descriptive_mcorp.png", plot = d, device = "png", height = 5.5, width = 5.3)
