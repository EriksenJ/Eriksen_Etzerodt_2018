## Show example of TSCS data 

    TSCS <- dat_change %>% 
        filter(Year < 1984) %>%
        select(Country, Year, rGDPc, growth, macrocorp, Inflation, Investment) %>%
        mutate(macrocorp = round(macrocorp, 2),
               Investment = round(Investment, 2),
               growth = round(growth, 2)) %>%
        as.matrix() 
    
    stargazer(TSCS,
              type = "html", out = "Output/TSCS.htm")
    
