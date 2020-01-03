#' vehicle_hist_ef_f
#' Function: Update the historical emission factor of the vehicle
#' @export
vehicle_hist_ef_f = function(vehicle,first_yr=NA,last_yr=NA){
  attribute_f("vehicle_hist_ef_f")
  ef_hist_dt <- get_input_f("air_pollutant_ef_hist")
  air_pollutant_dt <- get_input_f("model_matching_air_pollutant")
  #Other parameters
  age_tbc = as.numeric(switch(vehicle$mode,
                              "Private car" = "15",
                              "Taxi" = "15",
                              "Motorcycle" = "20",
                              "Public bus" = "20",
                              "Private bus" = "20",
                              "School bus" = "20",
                              "MRT"="0",
                              "LRT"="0"))
  first_hist_yr = first_yr-age_tbc
  #Create matrix of pollutant_emission_factor in the class fields
  vehicle$pollutant_emission_factor <- matrix(NA,nrow=length(air_pollutant_dt$Pollutant),ncol=last_yr-first_hist_yr+1,dimnames=list(air_pollutant_dt$Pollutant,first_hist_yr:last_yr))
  if (!vehicle$mode%in%c("MRT","LRT")){
    #Fill with historical values
    vehicle$pollutant_emission_factor[subset(ef_hist_dt,Mode==vehicle$mode & Technology==vehicle$technology)$Pollutant,as.character(first_hist_yr:2020)] <- as.matrix(subset(ef_hist_dt,Mode==vehicle$mode & Technology==vehicle$technology)[,as.character(first_hist_yr:2020)])
  } else {
    vehicle$pollutant_emission_factor[,] <- 0
  }
  return(vehicle)
}
