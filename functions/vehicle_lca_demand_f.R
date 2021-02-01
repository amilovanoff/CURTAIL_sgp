#' vehicle_lca_demand_f
#' Function: Calculates the life cycle GHG emissions of a given vehicle object by life cycle processes, stages and total
#' @export
vehicle_lca_demand_f <- function(mode,technology,model_year,first_yr=NA,last_yr=NA){
  attribute_f("vehicle_lca_demand_f")
  #Input files
  lca_process  <- get_input_f(input_name = 'lca_process')
  transport_mode <- get_input_f(input_name = 'model_matching_passenger_transport_mode')
  lca_process <- subset(lca_process,Mode%in%c("all",subset(transport_mode,Mode==mode)$Mode_type))
  deg_fac_dt <- get_input_f("on_road_degradation_factors")
  #Functions' Outputs
  vehicle_module_f_res <- do.call(fun_res_f,list(fun_name="vehicle_module_f"))
  fleet_fc_dt <- vehicle_module_f_res[["fleet_fc_dt"]]
  fleet_uf_dt <- vehicle_module_f_res[["fleet_uf_dt"]]
  fleet_specs_dt <- vehicle_module_f_res[["fleet_specs_dt"]]
  transport_activity_f_res <- do.call(fun_res_f,list(fun_name="transport_activity_f"))
  kt_per_veh <- transport_activity_f_res[["transport_kt_per_veh"]]
  mat_kt_per_veh <- acast(data=subset(kt_per_veh,Mode==mode & Year>=model_year),Mode~Year,value.var='Value',fun.aggregate=sum, margins=FALSE)
  #Other parameters
  #lc_vkt is the VKMT over the lifetime of the technology
  lc_vkt <- as.numeric(switch(subset(transport_mode,Mode==mode)$Mode_type,
                              Car="150000",
                              Bus="500000",
                              Motorcycle="150000"))
  #lifetime is the lifetime of the vehicle to achieve lc_vkt (in year). We add one the annual mileage because not an age, but years
  vehicle_op_years <- model_year:colnames(mat_kt_per_veh)[min(which(cumsum(mat_kt_per_veh)>lc_vkt))]
  #Create the demand matrix
  vehicle_demand_matrix <- matrix(0,ncol = length(first_yr:last_yr),nrow = nrow(lca_process),dimnames = list(paste(lca_process$Phase,lca_process$Process),first_yr:last_yr))
  
  # Filling matrix ----------------------------------------------------------

  #Fill fuel use
  mat_fc <- acast(subset(fleet_fc_dt,Mode==mode & Technology==technology & Model_year%in%model_year), Fuel ~ Model_year , value.var='Value',fun.aggregate=sum, margins=FALSE)
  mat_uf <- acast(subset(fleet_uf_dt,Mode==mode & Technology==technology & Model_year%in%model_year), Fuel ~ Model_year , value.var='Value',fun.aggregate=sum, margins=FALSE)
  #Get on-road degradation factors
  if (technology%in%c("ICEV-G")){
    on_road_deg_fac = 1.25
  } else if (technology %in% unique(deg_fac_dt$Technology)){
    on_road_deg_fac = subset(deg_fac_dt,Technology==technology)$Value
  } else {
    on_road_deg_fac = 1
  }
  #
  mat_vkt <- mat_kt_per_veh[,as.character(vehicle_op_years),drop=FALSE]
  mat_vkt[,as.character(max(vehicle_op_years))] <- mat_vkt[,as.character(max(vehicle_op_years))] - (sum(mat_vkt) - lc_vkt)
  #
  mat_fuel_use <- (mat_fc/100 * mat_uf * on_road_deg_fac) %*% mat_vkt
  #
  vehicle_demand_matrix[paste("Fuel Production",rownames(mat_fuel_use)),as.character(vehicle_op_years)] <- mat_fuel_use
  vehicle_demand_matrix[paste("Fuel Use",rownames(mat_fuel_use)),as.character(vehicle_op_years)] <- mat_fuel_use
  #
  vehicle_demand_matrix["Vehicle production Battery production",as.character(model_year)] <-  subset(fleet_specs_dt,Mode==mode & Technology==technology & Model_year==model_year)$Value
  #
  vehicle_demand_matrix[lca_process$Phase=="Vehicle production" & lca_process$Process!="Battery production",as.character(model_year)] <- 1
  return(list(vehicle_demand_matrix=vehicle_demand_matrix))
}
