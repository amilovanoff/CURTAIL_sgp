#' fleet_lca_ghg_f
#' Function: Calculate the life cycle GHG emissions of the given fleet by life cycle processes, stages and total
#' @import tidyr
#' @export
fleet_lca_ghg_f<-function(fleet,fast_mode="n"){
  attribute_f("fleet_lca_f")
  #Upload demand matrix
  fleet <- do.call(fleet_lca_demand_matrix_f,list(fleet=fleet))
  #Upload environmental matrix
  fleet$lca_env_matrix <- do.call(fleet_lca_env_matrix_f,list(mode=fleet$mode))
  #Calculate LCA score
  fleet$calculate_lca_score()
  return(fleet)
}
