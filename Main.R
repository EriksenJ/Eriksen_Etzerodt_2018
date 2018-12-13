#### Eriksen and Etzerodt (2018) - Main file (R)




#### Working Directory, settings, and packages 

    setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
    
    source("Code/Settings.R")
    
    source("Code/Libraries.R")

    

    
#### Data preparation 
    
    source("Code/Data_final.R")

    
#### Descriptive Statistics 
    
    source("Code/Desc_final.R")
    
    
#### Simulations 
    
    source("Code/Sim_Non_Stat_Estimation.R")
    source("Code/Sim_Unit_root.R")
    source("Code/Sim_ADL_FDL.R")
    
    

    
#### Examples 
    
    source("Code/Example_Fixed_Effects.R")
    source("Code/Reg_BoxJenkins_Example.R")
    
    
        
    