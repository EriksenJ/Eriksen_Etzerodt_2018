#### Eriksen & Etzerodt (2018) - Descriptive analyses








## Load dataset 
    
    dat1 <- read_dta("Data/Data_cleaned.dta") 

    
    

## Tables with descriptives by country and for total dataset 

    # Create a datasete with relevant variables and merge all variables (eases construction of summary statistics in tidyverse approach)
    desc <- dat1 %>%
        select(Country, Year, rGDPc, growth, Inflation, Investment, macrocorp) %>%
        gather(rGDPc, growth, Inflation, Investment, macrocorp, key = "Variable", value = "value") 
    
    # Country 
    desc %>%
        group_by(Country, Variable) %>%
        summarize(Mean = round(mean(value, na.rm = T), 2), 
                  "Std Dev" = round(sd(value, na.rm = T), 2), T = n(), Na = sum(is.na(value))) %>%
        as.matrix() %>%    
        stargazer(., type = "html", out = "Output/desc_stats_country.htm")
    
    # Total 
    desc %>%
        group_by(Variable) %>%
        summarize(Mean = round(mean(value, na.rm = T), 2), 
                  "Std Dev" = round(sd(value, na.rm = T), 2), T = n(), Na = sum(is.na(value))) %>%
        as.matrix() %>%    
        stargazer(., type = "html", out = "Output/desc_stats_total.htm")

    


    
## Plot real GDP, growth, and macrocorporatism by year  
    
    vars <- c("growth", "rGDPc", "macrocorp")
    ylabl <- c("Vækstrate", "PPP Dollars", "Index")
    top_label <- c("(a) Vækstrate i BNP per Indbygger", "(b) Real BNP per Indbygger",
                   "(c) Makrokorporatisme")
    d <- list()
    
    for(i in 1:3){
        d[[i]] <- ggplot(dat1, aes_string(x = "Year", y = vars[i])) +
            geom_line(aes(class = id)) +
            theme_classic() + 
            labs(title = top_label[i], 
                 x = "År",
                 y = ylabl[i])
        plot(d[[i]])
        ggsave(filename = paste0("Plot/descriptive_", vars[i], ".png"), 
               plot = d[[i]], device = "png", height = 4.2, width = 5.3)
    }
    
    # Store a collective graph for growth and rGDPc 
    
    d1 <- grid.arrange(d[[1]], d[[2]], ncol = 2)
    ggsave(filename = "Plot/descriptive_growth_rGDPc.png", 
           plot = d1, device = "png", height = 4.2, width = 10)




