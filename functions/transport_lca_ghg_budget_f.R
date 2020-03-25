#' transport_lca_ghg_budget_f
#' Function: Calculate the life cycle GHG emission budget of the passenger transport from 2018 to last_year
#' @import modelframework
#' @import tidyr
#' @export
transport_lca_ghg_budget_f<-function(carbon_budget_mdl=NA,last_yr=NA){
  attribute_f("transport_lca_ghg_budget_f")
  #Import data
  co2_budget <- get_input_f(input_name = 'co2_budget')
  #Function's output
  transport_lca_ghg_tot <- do.call(fun_res_f,list(fun_name="transport_lca_ghg_f"))[["transport_lca_ghg_tot"]]
  scenario_tbc <- switch(carbon_budget_mdl,
                         "ndc"="SSP1-26",
                         "2C_low"="SSP1-26",
                         "2C_high"="SSP1-26",
                         "1.5C_low"="SSP1-19",
                         "1.5C_high"="SSP1-19")
  conv_year <- switch(carbon_budget_mdl,
                         "ndc"="2040",
                         "2C_low"="2030",
                         "2C_high"="2040",
                         "1.5C_low"="2030",
                         "1.5C_high"="2040")
  #Calculate the national 2018-last year budget and adjust it for the transport sector assuming proportional
  adj_factor <- subset(transport_lca_ghg_tot,Year==2018)$Value/subset(co2_budget,Scenario==scenario_tbc & Convergence_year==conv_year & Year==2018)$Value
  #Unit: Kg CO2
  budget <- sum(subset(co2_budget,Scenario==scenario_tbc & Convergence_year==conv_year & Year%in%c(2018:last_yr))$Value)*adj_factor
  
  return(list(budget=budget))
}
