## Functions 
    
    # Function to plot consistent series
    plot_ts_vars <- function(dataset = dat_change, var, 
                             ytext = "Vækstrate i real BNP per indbygger", 
                             xtext = "År", 
                             filenam = "plot/rGDPc_growth.png",
                             ylim = NULL){
        d <- ggplot(dataset, aes(x = dataset[, "Year"], y = dataset[, var])) + 
            geom_line(aes(color = Land, linetype = Land)) +
            theme_classic() +
            xlab(xtext) +
            ylab(ytext) + 
            theme(legend.position = "bottom")
        if(!is.null(ylim)){
            d <- d + ylim(ylim)        
        }
        plot(d)
        ggsave(filename = filenam, device = "png", width = 5.5, height = 4)
    }
