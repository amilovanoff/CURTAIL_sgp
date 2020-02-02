#' vehicle_lca_demand_f
#' Function: Calculate the life cycle GHG emissions of the light-duty fleet by life cycle processes, stages and total
#' @export
vehicle_lca_demand_f <- function(mode,technology,model_year,first_yr=NA,last_yr=NA){
  attribute_f("vehicle_lca_demand_f")
  #Input files
  lca_process  <- get_input_f(input_name = 'lca_process')
  #Functions' Outputs
  vehicle_module_f_res <- do.call(fun_res_f,list(fun_name="vehicle_module_f"))
  fleet_fc_dt <- vehicle_module_f_res[["fleet_fc_dt"]]
  fleet_uf_dt <- vehicle_module_f_res[["fleet_uf_dt"]]
  transport_activity_f_res <- do.call(fun_res_f,list(fun_name="transport_activity_f"))
  kt_per_veh <- transport_activity_f_res[["transport_kt_per_veh"]]
  mat_kt_per_veh <- acast(data=subset(kt_per_veh,Mode==mode & Year>=model_year),Mode~Year,value.var='Value',fun.aggregate=sum, margins=FALSE)

  #Other parameters
  #lc_vkt is the VKMT over the lifetime of the technology
  lc_vkt <- 150000
  #lifetime is the lifetime of the vehicle to achieve lc_vkt (in year). We add one the annual mileage because not an age, but years
  vehicle_op_years <- model_year:colnames(mat_kt_per_veh)[min(which(cumsum(mat_kt_per_veh)>lc_vkt))]
  #Create the demand matrix
  vehicle_demand_matrix <- matrix(0,ncol = length(first_yr:last_yr),nrow = nrow(lca_process),dimnames = list(paste(lca_process$Phase,lca_process$Process),first_yr:last_yr))

  # Filling matrix ----------------------------------------------------------

  #Fill fuel use
  mat_fc <- acast(subset(fleet_fc_dt,Mode==mode & Technology==technology & Model_year%in%vehicle_op_years), Fuel ~ Model_year , value.var='Value',fun.aggregate=sum, margins=FALSE)
  mat_uf <- acast(subset(fleet_uf_dt,Mode==mode & Technology==technology & Model_year%in%vehicle_op_years), Fuel ~ Model_year , value.var='Value',fun.aggregate=sum, margins=FALSE)
  #
  mat_vkt <- mat_kt_per_veh[,as.character(vehicle_op_years),drop=FALSE]
  mat_vkt[,as.character(max(vehicle_op_years))] <- mat_vkt[,as.character(max(vehicle_op_years))] - (sum(mat_vkt) - lc_vkt)
  #
  mat_fuel_use <- (mat_fc/100 * mat_uf) * (matrix(1,nrow=nrow(mat_fc),ncol=1) %*% mat_vkt)
  #
  vehicle_demand_matrix[paste("Fuel Production",rownames(mat_fuel_use)),as.character(vehicle_op_years)] <- mat_fuel_use
  vehicle_demand_matrix[paste("Fuel Use",rownames(mat_fuel_use)),as.character(vehicle_op_years)] <- mat_fuel_use

  return(list(vehicle_demand_matrix=vehicle_demand_matrix))
}
