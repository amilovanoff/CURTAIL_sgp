#' optimization_pathway_f
#' Function: 
#' @export
optimization_pathway_f <- function(carbon_budget_mdl=NA,first_yr=NA,last_yr=NA,optimization_scen=NA){
  attribute_f("optimization_pathway_f")

  #Parameters for optimization
  #Get the attribute to function to change from the optimization scenario
  optimization_attribute <- switch(optimization_scen,
                                   modal_share="pkt_proj_modal_share_scen",
                                   technology="techno_ms_proj_car",
                                   fc="fc_proj_scen",
                                   travel_demand="pkt_proj_tot_scen",
                                   electricity="ef_elec_scen")
  #Get the cut_off of the attribute
  attribute_cut_off <- as.numeric(switch(optimization_scen,
                                         modal_share="0.0001",
                                         technology="1",
                                         fc="0.0005",
                                         travel_demand="0.0005",
                                         electricity="0.0001"))
  #Get the maximum value of the attribute
  attribute_value_max <- as.numeric(switch(optimization_scen,
                                           modal_share="0.5",
                                           technology="2030",
                                           fc="0.1",
                                           travel_demand="0.15",
                                           electricity="1"))
  #Get the name of the optimization variable
  optimization_variable <- switch(optimization_scen,
                                  modal_share="modal_share_variable",
                                  technology="electrification_year",
                                  fc="fc_variable",
                                  travel_demand="pkt_tot_variable",
                                  electricity="ef_elec_variable")
  
  ##Update the atribute in the appropriate function to allow for optimization
  update_attribute_values(setNames(as.list("optimization"),optimization_attribute))
  #Get the list of depend functions. Useful to erase data.
  list_depend_fct <- get_dependent_functions(attribute_list=optimization_attribute)
  #Delete all previously saved data that are sensitive to optimization_attribute
  del_fun_res(list_depend_fct)
  
  #Output
  dt_col <- c("Attribute","Attribute_value","Target","Score")
  seek_and_find_results <- setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
  dt_col <- c("Attribute","Target_achieved","Attribute_value","Target","Score")
  target_results <- setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)

  # Calculate transport emissions without optimization
  
  transport_lca_ghg_tot <- do.call(fun_res_f,list(fun_name="transport_lca_ghg_f"))[["transport_lca_ghg_tot"]]
  
  # Calculate the emission target -------------------------------------------
  
  score_target <- do.call(fun_res_f,list(fun_name="transport_lca_ghg_budget_f"))[["budget"]]
  target_year <- 2018:last_yr
  # Calculate the emission in default case --------------------------

  score_def <- sum(subset(transport_lca_ghg_tot,Year%in%target_year)$Value)
  ##Get the current value of the attribute to change. Then update the list of attribtues with value
  attribute_value_def <- as.numeric(get_attribute_value(optimization_variable))
  ##Save the values in data.frame
  seek_and_find_results[nrow(seek_and_find_results)+1,] <- c(optimization_attribute,attribute_value_def,score_target,score_def)
  
  ##We seek an attribute value that achieves more reductions (so more negative changes) than the target
  ##If change_value_def is lower than target, target is achieved for default value
  if (score_def <= score_target){
    #Save no result
    target_results[nrow(target_results)+1,] <- c(optimization_attribute,"y by def",attribute_value_def,score_target,score_def)
  ##Otherwise, calculate case with high value
  } else {
    
    # Calculate the emission changes in extreme case --------------------------
    
    ##Update value to maximum then calculate value
    update_attribute_values(setNames(as.list(attribute_value_max),optimization_variable))
    ##Clean the results of dependent functions in the saved environment and local
    del_fun_res(list_depend_fct)
    ##Get function results
    transport_lca_ghg_tot <- do.call(fun_res_f,list(fun_name="transport_lca_ghg_f"))[["transport_lca_ghg_tot"]]
    score_max <- sum(subset(transport_lca_ghg_tot,Year%in%target_year)$Value)
    ##Save the values in data.frame
    seek_and_find_results[nrow(seek_and_find_results)+1,] <- c(optimization_attribute,attribute_value_max,score_target,score_max)
    
    ##If High reductions, proceed by dichotomy to find the value
    if (score_max <= score_target){
      lower_bound <- attribute_value_def
      higher_bound <- attribute_value_max
      attribute_value <- (lower_bound+higher_bound)/2
      ##As long as attribute value bounds are higher than cut-off interval
      while ((higher_bound-lower_bound)>attribute_cut_off){
        ##Calculate the emission changes for the new attribute value
        update_attribute_values(setNames(as.list(attribute_value),optimization_variable))
        del_fun_res(list_depend_fct)
        
        transport_lca_ghg_tot <- do.call(fun_res_f,list(fun_name="transport_lca_ghg_f"))[["transport_lca_ghg_tot"]]
        score <- sum(subset(transport_lca_ghg_tot,Year%in%target_year)$Value)
        seek_and_find_results[nrow(seek_and_find_results)+1,] <- c(optimization_attribute,attribute_value,score_target,score)
        #Update lower and higher bounds
        if (score <= score_target){
          lower_bound <- lower_bound
          higher_bound <- attribute_value
        } else{
          lower_bound <- attribute_value
          higher_bound <- higher_bound
        }
        attribute_value <- (lower_bound+higher_bound)/2
      }
      #Save final result
      target_results[nrow(target_results)+1,] <- c(optimization_attribute,"y",attribute_value,score_target,score)
    ##If change_value_max is higher than target, target never achieved
    } else {
      #Save no result
      target_results[nrow(target_results)+1,] <- c(optimization_attribute,"n",NA,score_target,attribute_value_max)
    }
  }
  ##Update value back to default value
  update_attribute_values(setNames(as.list(attribute_value_def),optimization_variable))
  
  # Format final output -----------------------------------------------------
  
  dt_col <- c("Attribute_value","Target","Score")
  seek_and_find_results[,dt_col] <- sapply(dt_col,function(x) as.numeric(seek_and_find_results[,x]))
  target_results[,dt_col] <- sapply(dt_col,function(x) as.numeric(target_results[,x]))
  
  # Function results to save
  function_tbc <- c("transport_activity_f","transport_veh_pop_f","vehicle_module_f","ef_electricity_f","transport_lca_ghg_f","transport_on_road_emissions_f")
  if(target_results$Target_achieved=="n"){
    return(setNames(rep(list(NULL),times=length(function_tbc)),function_tbc))
  } else if(target_results$Target_achieved%in%c("y","y by def")){
    #Get results from considered functions
    return(setNames(lapply(function_tbc,function(x)do.call(fun_res_f,list(fun_name=x))),function_tbc))
  }
  
}
