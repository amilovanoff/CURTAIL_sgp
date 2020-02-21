#' vehicle_hist_fc_f
#' Function: Update the historical fuel consumption of the vehicle
#' @export
vehicle_hist_fc_f = function(vehicle,first_yr=NA,last_yr=NA){
  attribute_f("vehicle_hist_fc_f")
  hist_fc_list_input_name <- switch(vehicle$mode,
                                    "Private car"="car_hist_fc",
                                    "Private hire car"="car_hist_fc",
                                    "Taxi"="car_hist_fc",
                                    "Motorcycle"="moto_hist_fc",
                                    "Public bus"="bus_hist_fc",
                                    "Private bus"="bus_hist_fc",
                                    "School bus"="bus_hist_fc",
                                    "MRT"="mrt_hist_fc",
                                    "LRT"="lrt_hist_fc")
  vehicle_hist_fc_list <- readRDS(paste0("inputs/model/",hist_fc_list_input_name,".RDS"))
  #Other parameters
  age_tbc = as.numeric(switch(vehicle$mode,
                              "MRT"="0",
                              "LRT"="0",
                              20))
  first_hist_yr = first_yr-age_tbc
  #Create matrix of fuel consumption in the class fields for fuel_type1
  vehicle$fuel_consumption <- matrix(NA,nrow=length(vehicle$fuel_type),ncol=last_yr-first_hist_yr+1,dimnames=list(vehicle$fuel_type,first_hist_yr:last_yr))
  #Fill with historical values
  vehicle$fuel_consumption[rownames(vehicle_hist_fc_list[[vehicle$technology]]),colnames(vehicle_hist_fc_list[[vehicle$technology]])] <- vehicle_hist_fc_list[[vehicle$technology]]
  return(vehicle)
}
