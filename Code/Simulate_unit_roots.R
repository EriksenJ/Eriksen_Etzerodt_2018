## Simulate unit root and stationary series 


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
