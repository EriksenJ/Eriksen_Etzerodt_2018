# Code and data for Eriksen and Etzerodt (2018): Time-Series Cross-Section Analysis for Komparativ Politisk Økonomi 

 This page contains the code and data for Eriksen and Etzerodt (2018). 

 The project is developed in R with the exception of unit root tests run in Stata. To run all R files, run the file "Main.R". "Unit_root_test.do" must be run separately. 

 In addition, the project contains the used and simulated datasets from the paper. The datasets can be found in the folder "Data". Plots appearing in the project can be found in the folder "Plot". Tables with Summary Statistics in "Output". 
 
 The project consists of the following code files that should be run in the indicated order: 
 - Main.R
 - Settings.R
 - Libraries.R
 - Data_final.R
 - Desc_final.R
 - Sim_Non_Stat_Estimation.R
 - Sim_Unit_root.R
 - Sim_ADL_FDL.R
 - Example_Fixed_Effects.R
 - Reg_BoxJenkins_Example.R
 - Unit_root_test.do

 To run the code, start with opening the RProject file "EE_2018.RProject". This will set the working directory for your computer. Then, run the R file "Main.R". The file loads the data into R, and then runs the code for the paper by parts. 
 
 A single Stata .do file contains code for running IPS unit root tests. 
 
 