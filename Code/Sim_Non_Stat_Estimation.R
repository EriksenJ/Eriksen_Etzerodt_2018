#### E&E Method Paper - Monte Carlo Experiments with Non-stationary series 




## Create a set of functions to simulate data and calculate beta_values ----


## Functions for storing values and naming ----
    # Function to store variables and parameters after simulation
    store_vals <- function(X_ = X, Y_ = Y, beta_ = beta){
        sim <- list() 
        sim$X <- X_
        sim$Y <- Y_ 
        sim$beta <- beta_
        return(sim)
    }    
    
    # Function to add types to the dataframes
    add_type <- function(Types_vec, List){
        for(i in 1:length(List)){
            List[[i]]$Type = Types_vec[i]
        }
        return(List)
    }
    
    # Save Monte Carlo experiment results when stored in list 
    write_MC <- function(MCs, filenam, reg_type){
        for(i in 1:length(MCs)){
            data.table::fwrite(MCs[[i]], file = paste0(filenam, reg_type[i], ".csv"))
        }
    }


## Functions to collect regression coefficients -------
    # Function to collect regression coefficient 
    reg_coef <- function(Y_, X_){
        beta <- lm(Y_ ~ X_)$coefficients[2]
        return(beta)
    }
    
    # Fixed effects reg at unit level  
    reg_coef_FE_Unit <- function(Y_, X_, Unit_){
        beta <- felm(Y_ ~ X_ | factor(Unit_))$coefficients
        return(beta)
    }
    
    # Fixed effects at time level 
    reg_coef_FE_Time <- function(Y_, X_, Periods_){
        beta <- felm(Y_ ~ X_ | factor(Periods_))$coefficients
        return(beta)
    }
    
    # Fixed effects at unit and time level 
    reg_coef_FE_Unit_Time <- function(Y_, X_, Unit_, Periods_){
        beta <- felm(Y_ ~ X_ | factor(Unit_) + factor(Periods_))$coefficients
        return(beta)
    }


## Simulation functions -------

    # Function to create stationary X variable 
    X_s <- function(N_ = N) {
        x <- rnorm(N_, 0, 2)
        return(x)
    }
    
    # Function to create stationary Y variable
    Y_s <- function(X_, N_ = N, true_beta_ = true_beta) {
        y <- true_beta_ * X_ + rnorm(N_, 0, 1) 
        return(y)
    }
    
    
    # Function to create nonstationary X variable 
    X_ns <- function(N_ = N) {
        x <- rep(0, N_)
        for(i in 2:N_){
            x[i] <- x[i-1] + rnorm(1, 0, 2)
        }
        return(x)
    }
    
    # Function to create nonstationary y variable
    Y_ns <- function(X_, N_ = N) {
        y = rep(0, N_)
        for(i in 2:N_){
            y[i] <- y[i-1] + true_beta * X_[i] + rnorm(1, 0, 1)
        }
        return(y)
    }
    
    # Function to create Non-stationary X variable of type 2 
    X_ns_2 <- function(N_ = N) {
        unit_root_1 <- arima.sim(list(order = c(0, 1, 0)), sd = 1, n = N_) %>%
            as.vector() %>% 
            .[-1]
        x <- unit_root_1 + rnorm(n = N_, mean = 0, sd = 2)
        return(x)
    }
    
    # Function to create non-stationary Y variable of type 2
    Y_ns_2 <- function(X_, N_ = N, true_beta_ = true_beta) {
        unit_root_2 <- arima.sim(list(order = c(0, 1, 0)), sd = 1, n = N_) %>%
            as.vector() %>% 
            .[-1]
        y <- unit_root_2 + true_beta_ * X_ + rnorm(N_, 0, 1) 
        return(y)
    }






## Function for simulation 
Simulate_ts <- function(Stationarity = "NS", Time = seq(10,100, 5), Runs = 100){
    
    # Set the parameters for the experiment  
    N = Time # Time periods 
    C = Runs          
    N_obs <- 1:length(N)
    N_vec <- rep(0, NU)
    
    beta <- rep(0, last(N_obs))
    beta_sd <- rep(0, last(N_obs))
    
    for(i in N_obs) {
        # Storage for X and Y
        X <- matrix(data = NA, nrow = N[i], ncol = C)
        Y <- matrix(data = NA, nrow = N[i], ncol = C)
        betas <- rep(0, C)
        
        # Simulate data for N_obs, C times, and calculate the beta values
        if(Stationarity == "NS"){
            for(c in 1:C){
                X[, c] <- X_ns(N_ = N[i])
                Y[, c] = Y_ns(X[, c], N_ = N[i])
                betas[c] <- reg_coef(Y[, c], X[, c])
            }   
        }
        if(Stationarity == "S"){
            for(c in 1:C){
                X[, c] <- X_s(N_ = N[i])
                Y[, c] = Y_s(X[, c], N_ = N[i])
                betas[c] <- reg_coef(Y[, c], X[, c])
            }   
        }
        if(Stationarity == "X_NS"){
            for(c in 1:C){
                X[, c] <- X_ns(N_ = N[i])
                Y[, c] = Y_s(X[, c], N_ = N[i])
                betas[c] <- reg_coef(Y[, c], X[, c])
            }   
        }
        if(Stationarity == "Y_NS"){
            for(c in 1:C){
                X[, c] <- X_s(N_ = N[i])
                Y[, c] = Y_ns(X[, c], N_ = N[i])
                betas[c] <- reg_coef(Y[, c], X[, c])
            }   
        }
        if(Stationarity == "NS_2"){
            for(c in 1:C){
                X[, c] <- X_ns_2(N_ = N[i])
                Y[, c] = Y_ns_2(X[, c], N_ = N[i])
                betas[c] <- reg_coef(Y[, c], X[, c])
            }   
        }
        if(Stationarity == "X_NS_2"){
            for(c in 1:C){
                X[, c] <- X_ns_2(N_ = N[i])
                Y[, c] = Y_s(X[, c], N_ = N[i])
                betas[c] <- reg_coef(Y[, c], X[, c])
            }   
        }
        if(Stationarity == "Y_NS_2"){
            for(c in 1:C){
                X[, c] <- X_s(N_ = N[i])
                Y[, c] = Y_ns_2(X[, c], N_ = N[i])
                betas[c] <- reg_coef(Y[, c], X[, c])
            }   
        }
        
        # Store mean and sd of estimated beta values 
        beta[i] <- mean(betas)
        beta_sd[i] <- sd(betas)
    }
    
    # Return the beta and beta_sd matrices 
    return(list(beta = beta, beta_sd = beta_sd))
}








#### MC Panel function ---------





## Panel simulation function 
sim_panel <- function(Units, Periods, Stationarity = "NS", True_Beta = 2){
    # Storage for X and Y
    x <- matrix(data = NA, nrow = Periods, ncol = Units)
    y <- matrix(data = NA, nrow = Periods, ncol = Units)
    
    # Set up simulation parameters 
    true_beta = True_Beta
    
    # Simulate time series for each unit
    for(b in 1:Units){
        if(Stationarity == "NS"){
            x[, b] <- X_ns(N_ = Periods)
            y[, b] <- Y_ns(x[, b], N_ = Periods)    
        }
        if(Stationarity == "S"){
            x[, b] <- X_s(N_ = Periods)
            y[, b] <- Y_s(x[, b], N_ = Periods)    
        }
        if(Stationarity == "X_NS"){
            x[, b] <- X_ns(N_ = Periods)
            y[, b] <- Y_s(x[, b], N_ = Periods)    
        }
        if(Stationarity == "Y_NS"){
            x[, b] <- X_s(N_ = Periods)
            y[, b] <- Y_ns(x[, b], N_ = Periods)    
        }
        if(Stationarity == "NS_2"){
            x[, b] <- X_ns_2(N_ = Periods)
            y[, b] <- Y_ns_2(x[, b], N_ = Periods)    
        }
        if(Stationarity == "Y_NS_2"){
            x[, b] <- X_s(N_ = Periods)
            y[, b] <- Y_ns_2(x[, b], N_ = Periods)    
        }
        if(Stationarity == "X_NS_2"){
            x[, b] <- X_ns_2(N_ = Periods)
            y[, b] <- Y_s(x[, b], N_ = Periods)    
        }
    }
    
    # Store as panel data 
    x <- as.data.frame(x) %>% 
        mutate(Period = seq(1, Periods)) %>%
        gather(-Period, key = Unit, value = x) 
    y <- as.data.frame(y) %>% 
        mutate(Period = seq(1, Periods)) %>%
        gather(-Period, key = Unit, value = y)
    d <- left_join(x, y, by = c("Unit", "Period"))
    
    # Return dataset with panel 
    return(d)
}


## MC simulation function 
MC_panel <- function(Stationarity = "NS", Time = seq(10,100, 5), 
                     Units = seq(10, 100, 5), Runs = 100, 
                     reg_type = c("OLS", "FE_Unit", "FE_Time", "FE_Unit_Time")){
    
    # Set the parameters for the experiment  
    N = Time # Time periods
    U = Units # Units  
    C = Runs          
    
    N_obs <- 1:length(N)
    U_obs <- 1:length(U)
    NU <- last(N_obs) * last(U_obs)
    N_vec <- rep(0, NU)
    U_vec <- rep(0, NU)
    
    beta <- matrix(data = NA, last(N_obs), last(U_obs))
    beta_sd <- matrix(data = NA, last(N_obs), last(U_obs))
    
    # Set timer 
    tic.clearlog()
    tic("Total simulation time", )
    
    # Run simulations 
    for(u in 1:last(U_obs)){
        for(i in 1:last(N_obs)) {
            Units <- U[u]
            Periods <- N[i]
            
            tic(paste("For ", Units, " units, ", Periods, 
                      "periods, ", C, " Runs"))
            
            # Storage for X, Y, and b
            X <- matrix(data = NA, nrow = Periods*Units, ncol = 1)
            Y <- matrix(data = NA, nrow = Periods*Units, ncol = 1)
            betas <- rep(0, C)
            
            # Simulate series 
            for(c in 1:C){
                d <- sim_panel(Units = Units, Periods = Periods, Stationarity = Stationarity)
                X[, 1] <- d$x
                Y[, 1] <- d$y
                if(reg_type == "FE_Unit"){
                    betas[c] <- reg_coef_FE_Unit(Y[, 1], X[, 1], Unit_ = d[, "Unit"])
                }
                else if(reg_type == "FE_Time"){
                    betas[c] <- reg_coef_FE_Time(Y[, 1], X[, 1], Periods_ = d[, "Period"])
                }
                else if(reg_type == "FE_Unit_Time"){
                    betas[c] <- reg_coef_FE_Unit_Time(Y[, 1], X[, 1], Unit_ = d[, "Unit"], Periods_ = d[, "Period"])
                }
                else if(reg_type == "OLS"){
                    betas[c] <- reg_coef(Y[, 1], X[, 1])
                }
                else {
                    stop("Specify reg_type as OLS, FE_unit, FE_Time or OLS")
                }
            }
            
            # Store mean and sd of estimated beta values 
            beta[i, u] <- mean(betas)
            beta_sd[i, u] <- sd(betas)
            
            toc(log = TRUE)
        }    
    }
    toc(log = TRUE)
    
    # turn into data frames 
    beta <- as.data.frame(beta) 
    beta_sd <- as.data.frame(beta_sd) 
    
    # Number columns by Units 
    colnames(beta) <- U
    colnames(beta_sd) <- U
    
    # Create periods column
    beta$Periods <- N 
    beta_sd$Periods <- N
    
    # Gather into three columns (Units, Periods, value)
    beta <- beta %>%
        gather(-Periods, key = Units, value = beta)
    beta_sd <- beta_sd %>%
        gather(-Periods, key = Units, value = beta_sd)
    
    # Merge the two 
    d <- left_join(beta, beta_sd, by = c("Units", "Periods"))
    
    
    # Return the beta and beta_sd matrices 
    return(d)
}








#### Monte Carlo Experiment for Time Series : The effect of non-stationary on OLS estimator efficiency and consistency -------------

# Set the parameters for the experiment 
true_beta = -1          # The true beta value that we want to estimate 
N = seq(100, 1000, 5)   # the number of observations (a sequence to investigate behavior as n increases)
N_len = length(N)       # parameter used for storing results of simulations     
C = 1000                # The amount of simulations per n 


## Nonstationary data 
MC_result <- Simulate_ts(Stationarity = "NS", Time = N, Runs = C)

df <- as_tibble(list(Obs = N,
                     Mean = MC_result$beta,
                     SD = MC_result$beta_sd,
                     Type = rep("Begge unit root", N_len),
                     Y_stat = rep("Y unit root", N_len),
                     X_stat = rep("X unit root", N_len)))



## Y nonstationary 
MC_result <- Simulate_ts(Stationarity = "Y_NS", Time = N, Runs = C)

df_new <- as_tibble(list(Obs = N,
                         Mean = MC_result$beta,
                         SD = MC_result$beta_sd,
                         Type = rep("Y unit root", N_len),
                         Y_stat = rep("Y unit root", N_len),
                         X_stat = rep("X ikke unit root", N_len)))

df <- bind_rows(df, df_new)




## X nonstationary
MC_result <- Simulate_ts(Stationarity = "X_NS", Time = N, Runs = C)

df_new <- as_tibble(list(Obs = N,
                         Mean = MC_result$beta,
                         SD = MC_result$beta_sd,
                         Type = rep("x unit root", N_len),
                         Y_stat = rep("Y ikke unit root", N_len),
                         X_stat = rep("X unit root", N_len)))

df <- bind_rows(df, df_new)




## X & Y stationary 
MC_result <- Simulate_ts(Stationarity = "S", Time = N, Runs = C)

df_new <- as_tibble(list(Obs = N,
                         Mean = MC_result$beta,
                         SD = MC_result$beta_sd,
                         Type = rep("Ingen unit root", N_len),
                         Y_stat = rep("Y ikke unit root", N_len),
                         X_stat = rep("X ikke unit root", N_len)))

df <- bind_rows(df, df_new)




## X & Y non-stationary type 2
MC_result <- Simulate_ts(Stationarity = "NS_2", Time = N, Runs = C)

df_2 <- as_tibble(list(Obs = N,
                       Mean = MC_result$beta,
                       SD = MC_result$beta_sd,
                       Type = rep("Begge unit root (2)", N_len),
                       Y_stat = rep("Y unit root (2)", N_len),
                       X_stat = rep("X unit root (2)", N_len)))


## Y non-stationary type 2
MC_result <- Simulate_ts(Stationarity = "Y_NS_2", Time = N, Runs = C)

df_2_new <- as_tibble(list(Obs = N,
                           Mean = MC_result$beta,
                           SD = MC_result$beta_sd,
                           Type = rep("Y unit root (2)", N_len),
                           Y_stat = rep("Y unit root (2)", N_len),
                           X_stat = rep("X ikke unit root", N_len)))

df_2 <- bind_rows(df_2, df_2_new)


## X non-stationary type 2
MC_result <- Simulate_ts(Stationarity = "X_NS_2", Time = N, Runs = C)

df_2_new <- as_tibble(list(Obs = N,
                           Mean = MC_result$beta,
                           SD = MC_result$beta_sd,
                           Type = rep("X unit root (2)", N_len),
                           Y_stat = rep("Y ikke unit root", N_len),
                           X_stat = rep("X unit root (2)", N_len)))

df_2 <- bind_rows(df_2, df_2_new)



## Plot the results ----

# Bind the type 1 and 2 nonstationary simulations 
df_3 <- bind_rows(df, df_2)

# Make a plottable dataset 
df_plot <- df_3 %>%
    gather(key = Statistik, value = value, -Obs, -Type, -Y_stat, -X_stat) %>% 
    filter(Obs >= 100) %>%
    mutate(Statistik = ifelse(Statistik == "Mean", "Middelværdi", "Standardafvigelse"),
           Type = case_when(Type == "Y Ikke-stationær" ~ "Y unit root (1)",
                            Type == "Kointegration" ~ "X unit root (1)",
                            Type == "Begge Stationære" ~ "Begge stationære",
                            Type == "Begge Unit root" ~ "Begge unit root (1)",
                            Type == "Begge har type 2 unit root" ~ "Begge unit root (2)",
                            Type == "X unit root (2) Begge har type 2 unit root" ~ "X unit root (2)",
                            TRUE ~ Type))



# Plot 
d <- ggplot(df_plot, aes(x = Obs, y = value, linetype = Statistik)) + 
    geom_hline(yintercept = true_beta, alpha = 0.01) +
    geom_line() +
    theme_classic() + 
    scale_y_continuous(position = "right") +
    expand_limits(y=0) +
    labs(x = "Observationer (T)",
         title = "OLS Estimater med stationære og ikke-stationære variable") +
    ylab(NULL) +
    theme(legend.position = "bottom") +
    facet_grid(Type ~ .,
               scales = "free_y", switch = "y")
plot(d)
ggsave(filename = "Plot/Reg_Koefs_by_T.png", width = 5.5, height = 11)









#### Monte Carlo Experiment for Time Series : The effect of non-stationary on OLS estimator efficiency and consistency -------------
    
    # Set the parameters for the experiment 
    true_beta   = 2                     # The true beta value that we want to estimate 
    Units       = c(10, 30, 50, 70, 100, 200, 300, 400, 500)
    Periods     = c(10, 30, 50, 70, 100, 200, 300, 400, 500)
    U_len       = length(Units)
    P_len       = length(Periods)        
    Runs        = 500                          # The number of simulations per set (Units, Periods)


## Run Experiments 
    specs <- c("S", "NS", "Y_NS", "X_NS", "NS_2", "Y_NS_2", "X_NS_2")
    Types <- c("Stationær", 
               "Begge unit root (1)", "Y unit root (1)", "X unit root (1)",
               "Begge unit root (2)", "Y unit root (2)", "X unit root (2)")
    
    reg_types <- c("OLS", "FE_Unit", "FE_Time", "FE_Unit_Time")
    
    
    # Monte Carlo simulations
    MC_OLS <- lapply(specs, function(x) MC_panel(Stationarity = x, Time = Periods, Units = Units, 
                                              Runs = Runs, reg_type = "OLS"))
    MC_FE_U <- lapply(specs, function(x) MC_panel(Stationarity = x, Time = Periods, Units = Units, 
                                                 Runs = Runs, reg_type = "FE_Unit"))
    MC_FE_T <- lapply(specs, function(x) MC_panel(Stationarity = x, Time = Periods, Units = Units, 
                                                 Runs = Runs, reg_type = "FE_Time"))
    MC_FE_UT <- lapply(specs, function(x) MC_panel(Stationarity = x, Time = Periods, Units = Units, 
                                                 Runs = Runs, reg_type = "FE_Unit_Time"))
    
    
    # Store as list of lists and rename 
    MCs <- list(MC_OLS, MC_FE_U, MC_FE_T, MC_FE_UT)
    
    

## Add names and types
  
    # Change names and add types 
    MCs <- lapply(MCs, function(x){names(x) = specs; return(x) })
    MCs <- lapply(MCs, function(x) add_type(Types, x))
    
    
## Bind together and save 
    
    # Bind 
    MC_d <- lapply(MCs, function(x){d <- data.table::rbindlist(x) %>% as_tibble(); return (d)})
    
    # Save as csv datasets 
    write_MC(MC_d, "Data/MC_", reg_types)




## Plot the results ----
    
    
    # Plot coefficients 
    Units_lev <- c(10, 50, 100, 300, 500)
    
    for(i in 1:length(reg_types)){
        # Gather coefficients 
        d1 <- MC_d[[1]] %>% 
            gather(-Periods, -Units, -Type, key = Statistik, value = value) %>% 
            filter(Units %in% Units_lev) %>%
            mutate(Statistik = case_when(
                Statistik == "beta" ~ "Estimat",
                TRUE ~ "Standard Fejl"
            )) %>%
            rename(Enheder = Units,
                   Perioder = Periods)
        d1$Type <- factor(d1$Type, levels = Types)
        d1$Enheder = factor(as.character(d1$Enheder), levels = unique(Units_lev))
        
        ggplot(d1, aes(x = Perioder, y = value, linetype = Enheder)) +
            geom_line() + 
            facet_grid(Type ~ Statistik, 
                       scales = "free_y", switch = "y") +
            labs(y = "Statistik") +
            theme_light() +
            scale_y_continuous(limits = c(0, NA), 
                               expand = expand_scale(0.2))  +
            theme(legend.position = "bottom")
        ggsave(filename = paste0("Plot/Reg_Koefs_Panel_", reg_types[i], ".png"), width = 7.5, height = 10)
    }
    





























