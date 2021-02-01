# Folder description
This folder contains the files that contain the variables of the simulations, the ones that describe the model architecture and the all the ones essential for the simulations.

# Files description

## Files to update manually
* attribute_value.csv: List of all exogenous function's attributes with their abbreviated name (Attribute), their type (Type, i.e., 'cha' means character, 'num' means numeric), their value to be used in the current simulations (Value), their default value (Default), all working values (All) and their description (Description).  

## Scripts or functions
* model_framework_setup.R: Runs the utils/model_builder.R script to get all attributes from all functions, sort them in the attribute_value.csv file, then construct the other .csv files (described below).
* function_descriptions.R: Creates a .csv file with the descriptions of the model's functions.
* library_dependencies.R: List and install the set of libraries used at some point in the model.

## Files automatically generated
from utils/model_builder.R:
* function_attributes.csv: Contains the list of model's functions and their attributes by type ('Internal' for endogenous attributes - outputs of other model's functions; 'External' for exogenous attributes and 'Model' for attributes only associated with simulation parameters).
* function_attributes_matrix.csv: Contains the same information than function_attributes.csv but only for external attributes and in matrix format (with the rows beeing the functions and the columns the external attributes).
* function_explicit_attributes.csv: Contains the list of model's functions with the external attributes.
* function_inputs.csv: Contains the list of model's functions and their inputs by type (.csv, .xlsx, .R).
* function_inputs_matrix.csv: Contains the same information than function_inputs.csv but in matrix format (with the rows beeing the inputs and the columns the model's functions).
* function_sources_matrix.csv: Contains the same information than function_inputs_matrix.csv but only for model's function as inputs. This matrix shows the interdepencies of the functions and is used by some "utils" functions to fasten the simulations.
 
from script_function_descriptions.R:
* functions_descriptions.csv: Contains the list of model's functions and their functional descriptions.
