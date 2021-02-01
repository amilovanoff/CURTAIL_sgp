# Repository Introduction
This repository contains the input data, R codes and results of the Climate change constrained URban passenger TrAnsport Integrated Life cycle assessment (CURTAIL) model developed in the paper "**Greenhouse Gas Emission Mitigation Pathways for Urban Passenger Land Transport under Ambitious Climate Targets**" published in Environmental Science and Technology (2021) by Alexandre Milovanoff<sup>a</sup>, Laura Minet<sup>a</sup>, Lynette Cheah<sup>b</sup>, I. Daniel Posen<sup>a</sup>, Heather L. MacLean<sup>a</sup>, Rajasekhar Balasubramanian<sup>c</sup>.  
<sup>a</sup> Department of Civil & Mineral Engineering, University of Toronto, Canada  
<sup>b</sup> Engineering Systems and Design, Singapore University of Technology and Design, Singapore 
<sup>c</sup> Civil & Environmental Engineering, National University of Singapore, Singapore

# How to use this repository
This repository will not be updated but can be cloned, duplicated and extended. Feel free me to contact if you have any questions via email (alexandre.milovanoff@mail.utoronto.ca) or by GitHub @amilovanoff.

# Description of the model
The CURTAIL model is composed of interconnected R functions (in the functions folder) with specific tasks. The functions transform the inputs (in the inputs folder) into outputs (in the outputs folder). Variables (contained in architecture/attribute_value.csv) can be defined to specify inputs and/or assumptions. The functions can be used individually (by calling their names directly in an R environment) or can be simulated given a specific set of scenarios (with outputs/simulation_script.R). The set of scenarios can be defined in outputs/scenarios.csv.

# How to set up the model the first time
* Download the model from this repository on your computer.
* The model was developed using R and RStudio. Install RStudio: https://www.rstudio.com/products/rstudio/. I use the RStudio Desktop Open Source License (free).
* Once R and Rstudio are operationals, open the repository folder on your computer and open the file 'project_setup.RProj' in the main folder. It opens RStudio and sets up the working directory to the repository folder.
* Open and run "architecture/library_dependencies.R". It installs all the packages used at some point in the model (i.e., in the functions or to create the plots). The model is operational.

# How to run the functions individually
* Once the model is set up, you can run the functions of the model indivudially by opening "project_setup.RProj" in the main folder. It opens RStudio and sets up the working directory. 
* Then run the "model_setup.R" script. It loads the functions in the R environment, the function attributes and the data inputs.
* You can now run the functions individually by calling their names (e.g., transport_lca_ghg_f()).

# How to run simulations
* Once the model is set up, you can create simulations and run the selected functions across the specified scenarios.
* Scenarios can be created in the outputs/scenarios.csv files by specifying the set of attributes and their values.
* Then you can use the simulation script to run simulations and to save them in the outputs folder.

# If you want to extract the numerical values of the manuscript and supporting information figures
You can download them here (outputs/raw_data).

# Repository description
The repository comprises 6 folders.
* architecture: Contains scripts and .csv files to create the model framework. Compiles a list of functions included in the model, their inputs and their interdependencies (i.e., how are the functions linked?).
* functions: Contains the R scripts of the different functions of the model. Each function has a specific goal (e.g., calculate the prospective transport activity by transport mode, simulate the stock of private cars by model year and vehicle type).
* inputs: Contains the exogenous inputs of the functions and some R scripts to format the input data from raw inputs data. The inputs are associated with the case study of the CURTAIL model of Singapore.
* outputs: Contains the intermediate or final outputs of the model functions. Most of the outputs are in .Rdata format and contains default results or results from simulations and sensitivity analysis. The folder also contains the raw data of the figures presented in the manuscript and in the SI.
* reports: Contains some RMarkedown reports generated directly from the functions outputs. In this folder, the codes to generate the plots included in the manuscript and SI are available.
* utils: Contains additional functions to run the model. These functions are operating functions, meaning that they do not directly contribute to the model's results but help load and run the functions (e.g., loading the inputs data in the environment, loading the function attribute values, running the simulations and saving the results).

