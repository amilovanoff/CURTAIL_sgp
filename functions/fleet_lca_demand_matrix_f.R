#' fleet_lca_demand_matrix_f
#' Function: Creates the demand matrix of the given fleet to calculate the life cycle GHG emissions by life cycle processes and stages
#' @import tidyr
#' @importFrom reshape2 acast
#' @export
fleet_lca_demand_matrix_f<-function(fleet,first_yr=NA,last_yr=NA,fast_mode="n"){
  attribute_f("fleet_lca_demand_matrix_f")
  #Input files
  transport_mode <- get_input_f(input_name = 'model_matching_passenger_transport_mode')
  lca_process  <- get_input_f(input_name = 'lca_process')
  lca_process <- subset(lca_process,Mode%in%c("all",subset(transport_mode,Mode==fleet$mode)$Mode_type))
  #Create the demand matrix
  fleet$lca_demand_matrix <- matrix(0,ncol = length(first_yr:last_yr),nrow = nrow(lca_process),dimnames = list(paste(lca_process$Phase,lca_process$Process),first_yr:last_yr)) 
  #Fill the demand matrix
  fleet$lca_demand_matrix[paste("Fuel Production",rownames(fleet$fuel_use)),colnames(fleet$fuel_use)] <- fleet$fuel_use
  fleet$lca_demand_matrix[paste("Fuel Use",rownames(fleet$fuel_use)),colnames(fleet$fuel_use)] <- fleet$fuel_use
  if (any(grepl("production, without battery",lca_process$Process))){
    fleet$lca_demand_matrix[paste("Vehicle production",subset(lca_process, Phase == "Vehicle production" & Process!="Battery production")$Process),] <- colSums(fleet$sales)
    fleet$lca_demand_matrix[paste("Vehicle production","Battery production"),] <- colSums(fleet$battery_flow)
  }
  return(fleet)
}
