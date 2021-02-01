#' fleet_lca_env_matrix_f
#' Function: Create the environmental matrix of the given fleet to calculate the fleet life cycle GHG emissions
#' @import modelframework
#' @export
fleet_lca_env_matrix_f <- function(mode,first_yr=NA,last_yr=NA,fast_mode="n"){
  attribute_f("fleet_lca_env_matrix_f")
  #Input files
  lca_process  <- get_input_f(input_name = 'lca_process')
  transport_mode <- get_input_f(input_name = 'model_matching_passenger_transport_mode')
  mm_fuel <- get_input_f(input_name = 'model_matching_fuel_type')
  ef_mobile_combustion  <- get_input_f(input_name = 'ef_mobile_combustion')
  EF_lit <- get_input_f(input_name = 'EF_lit_review')
  conv <- get_input_f(input_name = 'conversion_units')
  fuel_specs <- get_input_f(input_name = 'greet_fuel_specs')
  EF_fuel_GREET <- get_input_f(input_name = 'EF_fuel_greet')
  #Get GHG emission factors of electricity generation
  ef_electricity_f_res <- do.call(fun_res_f,list(fun_name="ef_electricity_f"))
  #Get lists of processes
  lca_process <- subset(lca_process,Mode%in%c("all",subset(transport_mode,Mode==mode)$Mode_type))
  #Output
  lca_env_matrix <- matrix(0,ncol = length(first_yr:last_yr),nrow = nrow(lca_process),dimnames = list(paste(lca_process$Phase,lca_process$Process),first_yr:last_yr))
  #Mobile combustion emission factors from IPCC
  for (i in which(lca_process$Source=="IPCC")){
    lca_env_matrix[i,] <- as.numeric(subset(ef_mobile_combustion,Fuel==lca_process[i,"Process"] & Unit=="kg CO2/L")[,"Emission Factor"])
  }
  #fill env matrix with GREET emission factors for fuel production
  for (i in which(lca_process$Source=="GREET")){
    lca_env_matrix[i,] <- subset(EF_fuel_GREET, Data=="WTP" & LCI=="GHGs")[,subset(mm_fuel,Fuel==lca_process[i,"Process"])$GREET_name]/10^3*conv["mmBTU","1 BTU"]*as.numeric(fuel_specs[lca_process[i,"Process"],"LHV"])*conv["gal","1 L"]
  }
  #Fill environmental matrix with ecoinvent emissions factors for electricity production
  lca_env_matrix[which(lca_process$Phase=="Fuel Production" & lca_process$Process=="Electricity"),] <- acast(data=ef_electricity_f_res[["ef_elec_dt"]], Unit ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)[1,as.character(first_yr:last_yr)]
  #Battery production
  lca_env_matrix[lca_process$Process=="Battery production",] <- subset(EF_lit,Process=="Battery production" & `Functional Unit`=="kWh")$Score
  if (any(grepl("production, without battery",lca_process$Process))){
    #Vehicle production
    lca_env_matrix[lca_process$Phase=="Vehicle production" & lca_process$Process!="Battery production",] <- subset(EF_lit,Process!="Battery production" & Process%in%lca_process$Process)$Score
  }
   return(lca_env_matrix)
}
