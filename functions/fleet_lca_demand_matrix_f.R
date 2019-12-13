#' fleet_lca_demand_matrix_f
#' Function: Calculate the life cycle GHG emissions of the light-duty fleet by life cycle processes, stages and total
#' @import modelframework
#' @import tidyr
#' @importFrom reshape2 acast
#' @export
fleet_lca_demand_matrix_f<-function(fleet,first_yr=NA,last_yr=NA,fast_mode="n"){
  attribute_f("fleet_lca_demand_matrix_f")
  #Input files
  lca_process  <- get_input_f(input_name = 'lca_process')
  #Create the demand matrix
  fleet$lca_demand_matrix <- matrix(0,ncol = length(first_yr:last_yr),nrow = nrow(lca_process),dimnames = list(paste(lca_process$Phase,lca_process$Process),first_yr:last_yr)) 
  #Fill the demand matrix
  fleet$lca_demand_matrix[paste("Fuel Production",rownames(fleet$fuel_use)),colnames(fleet$fuel_use)] <- fleet$fuel_use
  fleet$lca_demand_matrix[paste("Fuel Use",rownames(fleet$fuel_use)),colnames(fleet$fuel_use)] <- fleet$fuel_use
  return(fleet)
}
