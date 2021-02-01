#' vehicle_hist_fc_f
#' Function: Updates the historical fuel consumption of a vehicle object
#' @export
vehicle_hist_fc_f = function(vehicle,first_yr=NA,last_yr=NA){
  attribute_f("vehicle_hist_fc_f")
  mode_type <- switch(vehicle$mode,
                      "Private car"="Car",
                      "Private hire car"="Car",
                      "Taxi"="Car",
                      "Motorcycle"="Motorcycle",
                      "Public bus"="Bus",
                      "Private bus"="Bus",
                      "School bus"="Bus",
                      "MRT"="MRT",
                      "LRT"="LRT")
  hist_fc <- get_input_f(input_name = 'hist_fc')
  last_hist_yr <- 2019
  #Other parameters
  age_tbc = as.numeric(switch(vehicle$mode,
                              "MRT"="0",
                              "LRT"="0",
                              20))
  first_hist_yr = first_yr-age_tbc
  #Create matrix of fuel consumption in the class fields for fuel_type1
  vehicle$fuel_consumption <- matrix(NA,nrow=length(vehicle$fuel_type),ncol=last_yr-first_hist_yr+1,dimnames=list(vehicle$fuel_type,first_hist_yr:last_yr))
  #Fill with historical values
  vehicle$fuel_consumption[subset(hist_fc,Mode==mode_type & Technology==vehicle$technology)$Fuel,as.character(first_hist_yr:last_hist_yr)] <- as.matrix(subset(hist_fc,Mode==mode_type & Technology==vehicle$technology)[,as.character(first_hist_yr:last_hist_yr)])
  return(vehicle)
}
