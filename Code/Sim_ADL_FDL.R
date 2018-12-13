#### Eriksen & Etzerodt (2018) - ADL and FDL series and the effect of shocks 





## Simulate shocks as expressed in ADL and FDL series -------------------
    
    
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
    
    
    
    # Create function to plot effects of shocks 
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
    
    # Plot effects of shocks 
    plot_models(d_temp, title = "Midlertidigt Shock", shock = "temp")
    plot_models(d_perm, title = "Permanent Shock", shock = "perm")
    
    
