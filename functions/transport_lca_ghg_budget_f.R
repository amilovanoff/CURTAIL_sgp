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
  target_tbc <- switch(carbon_budget_mdl,
                         "2C_low"="2C",
                         "2C_high"="2C",
                         "1.5C_low"="1.5C",
                         "1.5C_high"="1.5C",
                       carbon_budget_mdl)
  quantile_tbc <- as.numeric(switch(carbon_budget_mdl,
                               "2C_low"="0.25",
                               "2C_high"="0.75",
                               "1.5C_low"="0.25",
                               "1.5C_high"="0.75",
                          "0.5"))
  #Calculate the national 2018-last year budget and adjust it for the transport sector assuming proportional
  adj_factor <- subset(transport_lca_ghg_tot,Year==2018)$Value/(subset(co2_budget,Scenario=="INDC" & Year==2018)$Value*10^9)
  
  tot_budgets <- subset(co2_budget,Target==target_tbc & Year%in%c(2018:last_yr)) %>%
    aggregate(formula=Value~Target+Convergence_year+Scenario+Model,data=.,FUN=sum)
  #Unit: Kg CO2
  budget <- as.numeric(quantile(x=tot_budgets$Value,probs=quantile_tbc))*10^9*adj_factor
  #Return
  return(list(budget=budget))
}
