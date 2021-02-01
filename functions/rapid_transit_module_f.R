#' rapid_transit_module_f
#' Function: Calculates the life cycle GHG emissions of rapid transit systems
#' @export
rapid_transit_module_f <- function(first_yr=NA,last_yr=NA){
  attribute_f("rapid_transit_module_f")
  #Travel demand for LRT and MRT
  transport_activity_f_res <- do.call(fun_res_f,list(fun_name="transport_activity_f"))
  #Electrical consumption
  vehicle_module_f_res <- do.call(fun_res_f,list(fun_name="vehicle_module_f"))
  fleet_fc_dt <- vehicle_module_f_res[["fleet_fc_dt"]]
  fleet_uf_dt <- vehicle_module_f_res[["fleet_uf_dt"]]
  #Output
  rapid_transit_fuel_use_dt <- NULL
  rapid_transit_lca_ghg_dt <- NULL
  for (mode in c("MRT","LRT")){
    #Initialize the fleet class
    fleet <- new("fleetClass")
    fleet$mode <- mode
    #Create matrix of fuel use
    fleet$fuel_use <- matrix(NA,nrow=1,ncol=length(first_yr:last_yr),dimnames=list("Electricity",first_yr:last_yr))
    #Get matrix of fuel consumption, utility factor and adjusted fuel consumption by technology and fuel and age. Unit: L or kWh / 100km
    mat_fc <- acast(subset(fleet_fc_dt,Mode==mode), Fuel ~ Model_year , value.var='Value',fun.aggregate=sum, margins=FALSE)
    mat_uf <- acast(subset(fleet_uf_dt,Mode==mode), Fuel ~ Model_year , value.var='Value',fun.aggregate=sum, margins=FALSE)
    #Unit: L/km
    mat_fC_uf <- mat_fc/100 * mat_uf
    fleet$fuel_use["Electricity",] <- mat_fC_uf[,colnames(fleet$fuel_use)]*acast(data=subset(transport_activity_f_res[["transport_vkt"]],Mode==mode & Year%in%colnames(fleet$fuel_use)), Mode ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)[mode,colnames(fleet$fuel_use)]*1000
    #Get dataframe of fuel use
    rapid_transit_fuel_use_dt <- rbind(rapid_transit_fuel_use_dt,fleet$get_data_frame("fuel_use"))
    #Calculate LCA score
    fleet <- do.call(fleet_lca_demand_matrix_f,list(fleet=fleet))
    #Upload environmental matrix
    fleet$lca_env_matrix <- do.call(fleet_lca_env_matrix_f,list(mode=fleet$mode))
    #Calculate LCA score
    fleet$calculate_lca_score()
    #Get dataframe of LCA
    rapid_transit_lca_ghg_dt <- rbind(rapid_transit_lca_ghg_dt,fleet$get_dataframe_lca_process())
  }
  return(list(rapid_transit_fuel_use_dt=rapid_transit_fuel_use_dt,
              rapid_transit_lca_ghg_dt=rapid_transit_lca_ghg_dt))
}
