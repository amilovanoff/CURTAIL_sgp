# Folder description
The "inputs/data" folder contains raw data derived from other sources.  
The "inputs/model" folder contains data that are processed and formated by the scripts from the "inputs/scripts" folder to be used by the model. This step is sometimes necessary if the raw data are too large or require processing.  
The "inputs/user" folder contains inputs data that are created by the user.  

# Data management systems
The CURTAIL model pre-loads the inputs data in an R environment to simplify and fasten the simulations. The data_input_management.xlsx file specifies all the inputs data to be pre-loaded and the associated dataframe names in the R environmnent. The input_data_environment.RDATA is the pre-loaded environment. The save_input_data_f() function of the utils/utils_f.R script reads and saves all inputs in the associated R environment. If the data are edited in the inputs folder, the save_input_data_f() function should be ran afterwards to ensure that the latest data are loaded in the R environment.  

# Formatting scripts
The scripts contained in the "inputs/scripts" folder convert raw inputs data of the "inputs/data" folder into formatted data inputs in the "inputs/model" folder. The descriptions of the scripts are included in the scripts. They need to be simulated in the correct order (e.g., first 1_ then 2_ ).
