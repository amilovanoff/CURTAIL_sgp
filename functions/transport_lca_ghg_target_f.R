#' transport_lca_ghg_target_f
#' Function: Calculate the life cycle GHG emission budget of the passenger transport
#' @import modelframework
#' @import tidyr
#' @export
transport_lca_ghg_target_f<-function(carbon_budget_mdl=NA){
  attribute_f("transport_lca_ghg_target_f")
  #Function's output
  transport_lca_ghg_tot <- do.call(fun_res_f,list(fun_name="transport_lca_ghg_f"))[["transport_lca_ghg_tot"]]
  if (carbon_budget_mdl=="ndc"){
    pop_dt <- get_input_f("population")
    #The target is to reach 36% reductions in emission intensity of 2005 level in 2030. Assume emission intensity to kg CO2/capita
    i_year <- 2005
    target_year <- 2030
    #Calculate emission target
    score_target <- (subset(transport_lca_ghg_tot,Year==i_year)$Value/subset(pop_dt,Year==i_year)$Value)*(1-0.36)*subset(pop_dt,Scenario=="Medium" & Year==target_year)$Value
  }
  
  return(list(score_target=score_target))
}
