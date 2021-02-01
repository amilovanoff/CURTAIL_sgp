#' write_def_outputs_f
#'
#' Run and write outputs of functions for a given scenario with default attribute values.
#' @param function_tbc List of functions to simulate
#' @param scen_tbc Scenario to consider
#' @export
write_def_outputs_f <- function(function_tbc,
                                scen_tbc){
  #Load input data
  load_input_data_f()
  #Load attribute values
  load_attribute_value()
  #Get scenario attributes with values
  scen_attributes <- get_scenario_attributes(scen_tbc)
  #Get the list of depend functions. Useful to erase data.
  list_depend_fct <- get_dependent_functions(attribute_list=rownames(scen_attributes))
  for (i in 1:length(function_tbc)){
    #Output files
    results_l <- list()
    fct <- function_tbc[i]
    #Delete all saved results
    del_fun_res("all")
    #Loop on scenarios
    for (scen_number in seq_len(ncol(scen_attributes))){
      #Reset all attribute values to default values
      reset_attribute_values()
      #Update the attribute values with the scenario's values
      update_attribute_values(scen_attributes[,scen_number,drop=FALSE])
      #Delete all saved results
      del_fun_res(list_depend_fct)
      #Get function results
      results <- append(do.call(get(fct), list()), setNames(list(sapply(rownames(scen_attributes),function(x)scen_attributes[x,scen_number])),"Scenario"))
      #Store results in a new list element
      results_l[[length(results_l)+1]] <- results
    }
    save(list="results_l",file=paste0("outputs/out_def/",fct,"_",scen_tbc,".RData"))
  }
  #Reset all attribute values to default values
  reset_attribute_values()
  #Delete all saved results
  del_fun_res("all")
}

#' write_simulation_f
#'
#' Run and write outputs of functions for a given scenario and a given simulation number.
#' @param function_tbc List of functions to simulate
#' @param scen_tbc Scenario to consider
#' @param sim_tbc Simulation to consider
#' @param sim_type Type of simulation discrete or continuous
#' @export
write_simulation_f <- function(function_tbc,
                               sim_tbc,
                               sim_type,
                               scen_tbc){
  #Load input data
  load_input_data_f()
  #Load attribute values
  load_attribute_value()
  #Get simulation attributes with values
  if (sim_type=="discrete"){
    sim_attributes <- get_discrete_simulation_attributes(sim_tbc)
  } else if(sim_type=="continuous"){
    sim_attributes <- get_continuous_simulation_attributes(sim_tbc)
  }
  list_depend_fct_simulation <- get_dependent_functions(attribute_list=rownames(sim_attributes))
  #Get scenario attributes with values
  scen_attributes <- get_scenario_attributes(scen_tbc)
  list_depend_fct_scenario <- get_dependent_functions(attribute_list=rownames(scen_attributes))
  #Output files
  results_l <- list()
  #Loop on scenarios
  for (scen_number in seq_len(ncol(scen_attributes))){
    #Reset all attribute values to default values
    reset_attribute_values()
    #Update the attribute values with the scenario's values
    update_attribute_values(scen_attributes[,scen_number,drop=FALSE])
    #Clear environment with function related to scenario attributes
    del_fun_res(list_depend_fct_scenario)
    #Loop on sensitivty analysis attributes
    for (sim_number in seq_len(ncol(sim_attributes))){
      #Update the attribute values with the simulation's values
      update_attribute_values(sim_attributes[,sim_number,drop=FALSE])
      #Clear the functions' results
      del_fun_res(list_depend_fct_simulation)
      #Get function results
      results <- append(do.call(get(function_tbc), list()),
                        append(setNames(list(sapply(rownames(sim_attributes),function(x)sim_attributes[x,sim_number])),"Simulation"),
                               setNames(list(sapply(rownames(scen_attributes),function(x)scen_attributes[x,scen_number])),"Scenario")))
      #Store results in a new list element
      results_l[[length(results_l) + 1]] <- results
    }
  }
  #Delete all saved results
  del_fun_res("all")
  #Write results
  save(list="results_l",file=paste0("outputs/out_sim/",function_tbc,"_",scen_tbc,"_",sim_tbc,"_",sim_type,".RData"))
  reset_attribute_values()
}
