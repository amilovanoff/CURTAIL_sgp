#' vehicle_initialize_f
#' Function: Initializes a vehicle object
#'  @export
vehicle_initialize_f = function(mode,technology,first_yr=NA,last_yr=NA){
  attribute_f("vehicle_initialize_f")
  #Inputs
  battery_capacity <- get_input_f(input_name = 'battery_capacity')
  #Initialize the vehicle class
  vehicle <- new("vehicleClass",mode=mode,technology=technology)
  #Create fuel_consumption field and fill matrix with historical values
  vehicle <- do.call(vehicle_hist_fc_f,list(vehicle=vehicle))
  #Utility factor
  age_tbc = as.numeric(switch(vehicle$mode,
                              "MRT"="0",
                              "LRT"="0",
                              20))
  first_hist_yr = first_yr-age_tbc
  #Utility factor
  vehicle$utility_factor <- matrix(NA,nrow=length(vehicle$fuel_type),ncol=last_yr-first_hist_yr+1,dimnames=list(vehicle$fuel_type,first_hist_yr:last_yr))
  if (length(vehicle$fuel_type)==1){
    vehicle$utility_factor[,as.character(first_hist_yr:last_yr)] <- 1
  } else {
    vehicle$utility_factor["Gasoline",as.character(first_hist_yr:last_yr)] <- 1-do.call(phev_uf_f,list(range_km=45))
    vehicle$utility_factor[setdiff(vehicle$fuel_type,"Gasoline"),as.character(first_hist_yr:last_yr)] <- vehicle$utility_factor["Gasoline",as.character(first_hist_yr:last_yr)]
  }
  #Specifications
  specs_list <- "Battery_capacity"
  vehicle$specifications <- matrix(0,nrow=length(specs_list),ncol=last_yr-first_hist_yr+1,dimnames=list(specs_list,first_hist_yr:last_yr))
  if (technology %in% battery_capacity$Technology){
    vehicle$specifications["Battery_capacity",] <- subset(battery_capacity,Technology==technology)$Value
  }
  return(vehicle)
}
