#' vehicle_initialize_f
#' 
#'  @import modelframework
#'  @export
vehicle_initialize_f = function(mode,technology,first_yr=NA,last_yr=NA){
  attribute_f("vehicle_initialize_f")
  #Initialize the vehicle class
  vehicle <- new("vehicleClass",mode=mode,technology=technology)
  #Create fuel_consumption field and fill matrix with historical values
  vehicle <- do.call(vehicle_hist_fc_f,list(vehicle=vehicle))
  #Create specifications field and fill matrix with values
  vehicle <- do.call(vehicle_hist_ef_f,list(vehicle=vehicle))
  #vehicle <- do.call(vehicle_specifications_f,list(vehicle=vehicle))
  #Utility factor
  age_tbc = as.numeric(switch(vehicle$mode,
                              "Private car" = "15",
                              "Taxi"="15",
                              "Motorcycle" = "20",
                              "Public bus" = "20",
                              "Private bus" = "20",
                              "School bus" = "20",
                              "MRT"="0",
                              "LRT"="0"))
  first_hist_yr = first_yr-age_tbc
  vehicle$utility_factor <- matrix(NA,nrow=length(vehicle$fuel_type),ncol=last_yr-first_hist_yr+1,dimnames=list(vehicle$fuel_type,first_hist_yr:last_yr))
  if (length(vehicle$fuel_type)==1){
    vehicle$utility_factor[,as.character(first_hist_yr:last_yr)] <- 1
  } else {
    vehicle$utility_factor["Gasoline",as.character(first_hist_yr:last_yr)] <- 1-do.call(phev_uf_f,list(range_km=50))
    vehicle$utility_factor[setdiff(vehicle$fuel_type,"Gasoline"),as.character(first_hist_yr:last_yr)] <- vehicle$utility_factor["Gasoline",as.character(first_hist_yr:last_yr)]
  }
  return(vehicle)
}
