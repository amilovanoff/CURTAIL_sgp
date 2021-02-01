# Folder description
The 'outputs' folder contains the output files suhc as the simulation results.

# Files to define and run the simulations
Excel files that are used to define the values of the function's attributes for the simulations.
* simulations.xlsx: Excel spreadsheets that contain the list of attributes and their values associated with simulation numbers. There are two spreadsheets for simulations that are based on discrete attributes and simulations that are based on continuous attributes (attributes with ranges). All attributes not specified have default values.
	* 'Simulation' column contains the simulation number.
	* 'Attribute' column contains 'name' as the second row of the simulations then the list of attributes to be udpated.
	* 'Type' column contains the type of attribute.
	* The other columns define the values of the attributes by run in the simulation. The first row of the simulation defined the name of the run, then the other rows are assoociated with the attributes and define the attribute values to use.
	
* scenarios.xlsx: Excel spreadsheets that define scenarios with multiple runs. In the model, functions possess one single set of outputs for a given set of inputs. A scenario is defined as multiple set of inputs to generate multiple sets of outputs. The default scenario is the baseline of the simulations and is used to simulate the default outputs of the functions. Scenarios can be used with the "simulations.xlsx" to run multiple simulations of different scenarios.
	* 'Scenario' columns contains the name of the scenario.
	* 'Attribute' column contains the name of the attributes.
	* 'Type' column contains the type of the attributes.
	* The other columns define the multiple runs with the first row beeing the name of the runs and the other rows beeing the attribute's values.

* simulations_script.R: Contains the lines to run to obtain the simulation results. Use the "utils/write_f.R" functions.

# Other folders
* out_def: Contains the default results in .RData by function.
* out_sim: Contains the simulation results in .Rdata by function and simulation number.
* plots: Contains the plots of the manuscript and SI
* raw_data: Contains the raw data of the plots of the manuscript and SI
