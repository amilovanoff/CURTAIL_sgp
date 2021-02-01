# Folder description
This folder contains the function's scripts of the model. The functions contained in the 'utils' folder are operating functions, i.e., they help to run the model but do not directly contribute to the model's results.

# Function script description
* Each script is a function. The names have to be the same between script name and function name. Otherwise, the model will not work properly.
* The description of the scipt or function is preceded by "#' Function"
* Each function has attributes:
	* Endogenous attributes (i.e., outputs of another function used as inputs) and they do no have default values;
	* Exogenous attributes (i.e., attributes defined by the user in 'architecture/attribute_value.csv') marked with the NA default value in the function;
	* Attributes only associated with simulation parameters (i.e., such as fast_mode which indicates if default inputs should be used to fasten the simulations).
* To initiate the function, all the attributes need to be specified. The first line of the function simulates 'attribute_f' with the name of the model's function. This forces the values of the exogenous attributes, which are not already specified, from 'architecture/attribute_value.csv'.

# Class
The "class.R" script defines the classes defined and used in the model.