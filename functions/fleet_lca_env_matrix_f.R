#' fleet_lca_env_matrix_f
#' Function: Create the environmental matrix of the fleet to calculate the fleet life cycle GHG emissions
#' @import modelframework
#' @export
fleet_lca_env_matrix_f <- function(fleet,first_yr=NA,last_yr=NA,ef_elec_scen=NA,fast_mode="n"){
  attribute_f("fleet_lca_env_matrix_f")
  #Input files
  lca_process  <- get_input_f(input_name = 'lca_process')
  ef_mobile_combustion  <- get_input_f(input_name = 'ef_mobile_combustion')
  ef_mobile_combustion$Fuel <- get_matching_names(ef_mobile_combustion$`Fuel Type`,matching_type = "fuel_type",original_source = "Asian_bank_name", matched_source = "Fuel")
  EF_lit <- get_input_f(input_name = 'EF_lit_review')
  ef_greet <- get_input_f(input_name = 'ef_greet')
  #
  #lca_ef_elec_f_res <- do.call(fun_res_f,list(fun_name="lca_ef_elec_f",fast_mode=fast_mode))
  #lca_ef_elec <- subset(lca_ef_elec_f_res[["lca_ef_elec"]],Year%in%first_yr:last_yr)
  #Output
  fleet$lca_env_matrix <- matrix(0,ncol = length(first_yr:last_yr),nrow = nrow(lca_process),dimnames = list(paste(lca_process$Phase,lca_process$Process),first_yr:last_yr))
  #Battery production and assembly
  fleet$lca_env_matrix[lca_process$Process=="Battery production and assembly",] <- subset(EF_lit,`Battery type`=="Li_ion LMO" & Process=="Battery production" & Modele=="def")$LCI
  #fill env matrix with GREET emission factors for fuel production
  for (i in which(lca_process$Source=="GREET")){
    fleet$lca_env_matrix[i,] <- as.numeric(subset(ef_greet,Process==lca_process[i,"Process"])$Value)
  }
  #Mobile combustion emission factors from Asian Development Bank
  for (i in which(lca_process$Source=="Asian Development Bank")){
    fleet$lca_env_matrix[i,] <- as.numeric(subset(ef_mobile_combustion,Fuel==lca_process[i,"Process"] & Unit=="kg CO2 / Liter")[,"Emission Factor"])
  }
  #Fill environmental matrix with ecoInvent emissions factors for electricity production
  #fleet_env_matrix[which(lca_process$Phase=="Fuel Production" & lca_process$Process=="Electricity"),] <- lca_ef_elec$Value[order(lca_ef_elec$Year)]
  if (ef_elec_scen=="constant"){
    fleet$lca_env_matrix[which(lca_process$Phase=="Fuel Production" & lca_process$Process=="Electricity"),] <- 0.425
  } else if(ef_elec_scen=="renewable"){
    fleet$lca_env_matrix[which(lca_process$Phase=="Fuel Production" & lca_process$Process=="Electricity"),as.character(2005:2019)] <- 0.425
    fleet$lca_env_matrix[which(lca_process$Phase=="Fuel Production" & lca_process$Process=="Electricity"),as.character(2020:2030)] <- approx(x=c(2019,2030), y=c(0.425,0.425*0.5) , xout=2020:2030, method = "linear")$y
  }
  return(fleet)
}
