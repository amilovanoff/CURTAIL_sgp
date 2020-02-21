#' fleet_lca_env_matrix_f
#' Function: Create the environmental matrix of the fleet to calculate the fleet life cycle GHG emissions
#' @import modelframework
#' @export
fleet_lca_env_matrix_f <- function(mode,first_yr=NA,last_yr=NA,ef_elec_scen=NA,fast_mode="n"){
  attribute_f("fleet_lca_env_matrix_f")
  #Input files
  lca_process  <- get_input_f(input_name = 'lca_process')
  transport_mode <- get_input_f(input_name = 'model_matching_passenger_transport_mode')
  ef_mobile_combustion  <- get_input_f(input_name = 'ef_mobile_combustion')
  EF_lit <- get_input_f(input_name = 'EF_lit_review')
  ef_greet <- get_input_f(input_name = 'ef_greet')
  #
  lca_process <- subset(lca_process,Mode%in%c("all",subset(transport_mode,Mode==mode)$Mode_type))
  #Output
  lca_env_matrix <- matrix(0,ncol = length(first_yr:last_yr),nrow = nrow(lca_process),dimnames = list(paste(lca_process$Phase,lca_process$Process),first_yr:last_yr))
  #Mobile combustion emission factors from IPCC
  for (i in which(lca_process$Source=="IPCC")){
    lca_env_matrix[i,] <- as.numeric(subset(ef_mobile_combustion,Fuel==lca_process[i,"Process"] & Unit=="kg CO2/L")[,"Emission Factor"])
  }
  #fill env matrix with GREET emission factors for fuel production
  for (i in which(lca_process$Source=="GREET")){
    lca_env_matrix[i,] <- as.numeric(subset(ef_greet,Process==lca_process[i,"Process"])$Value)
  }
  #Fill environmental matrix with ecoInvent emissions factors for electricity production
  #fleet_env_matrix[which(lca_process$Phase=="Fuel Production" & lca_process$Process=="Electricity"),] <- lca_ef_elec$Value[order(lca_ef_elec$Year)]
  if (ef_elec_scen=="constant"){
    lca_env_matrix[which(lca_process$Phase=="Fuel Production" & lca_process$Process=="Electricity"),] <- 0.413
  } else if(ef_elec_scen=="renewable"){
    lca_env_matrix[which(lca_process$Phase=="Fuel Production" & lca_process$Process=="Electricity"),as.character(2005:2019)] <- 0.413
    lca_env_matrix[which(lca_process$Phase=="Fuel Production" & lca_process$Process=="Electricity"),as.character(2020:2030)] <- approx(x=c(2019,2030), y=c(0.413,0.413*0.5) , xout=2020:2030, method = "linear")$y
  }
  #Battery production
  lca_env_matrix[lca_process$Process=="Battery production",] <- subset(EF_lit,Process=="Battery production" & `Functional Unit`=="kWh")$Score
  if (any(grepl("production, without battery",lca_process$Process))){
    #Vehicle production
    lca_env_matrix[lca_process$Phase=="Vehicle production" & lca_process$Process!="Battery production",] <- subset(EF_lit,Process!="Battery production" & Process%in%lca_process$Process)$Score
  }
   return(lca_env_matrix)
}
