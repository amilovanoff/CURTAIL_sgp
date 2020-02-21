#' vehicle_module_f
#' Function:
#' @import modelframework
#' @export
vehicle_module_f = function(){
  attribute_f("vehicle_module_f")
  #Input - Get list of all transport mode with vehicles
  transport_mode <- get_input_f(input_name = 'model_matching_passenger_transport_mode')
  transport_mode_list <- transport_mode$Mode
  for (mode in transport_mode_list){
    #Get list of all technologies by transport mode
    if (mode%in%c("MRT","LRT")){
      technology_list <- mode
    } else {
      input_data_name <- switch (mode,
                                 "Private car"="model_matching_vehicle_technology",
                                 "Private hire car"="model_matching_vehicle_technology",
                                 "Taxi"="model_matching_vehicle_technology",
                                 "Motorcycle"="model_matching_moto_technology",
                                 "Public bus"="model_matching_bus_technology",
                                 "Private bus"="model_matching_bus_technology",
                                 "School bus"="model_matching_bus_technology"
      )
      vh_techno <- get_input_f(input_name = input_data_name)
      technology_list <- unique(vh_techno$Technology)
    }
    for (technology in technology_list){
      #Get the historical vehicle attributes
      vehicle <- do.call(vehicle_initialize_f,list(mode=mode,technology=technology))
      last_hist_yr <- max(as.numeric(colnames(vehicle$fuel_consumption)[!is.na(vehicle$fuel_consumption[1,])]))
      #Proj fuel consumption
      vehicle <- do.call(vehicle_fc_proj_f,list(vehicle=vehicle))
      #Proj emisison factors
      vehicle <- do.call(vehicle_ef_proj_f,list(vehicle=vehicle))
      #Save output of vehicle attributes
      fleet_fc_dt <- rbind(get0("fleet_fc_dt"),vehicle$get_data_frame("fuel_consumption"))
      #Save output of vehicle attributes
      fleet_ef_dt <- rbind(get0("fleet_ef_dt"),vehicle$get_data_frame("pollutant_emission_factor"))
      #Format NAs into 0
      fleet_fc_dt[is.na(fleet_fc_dt)] <- 0
      fleet_uf_dt <- rbind(get0("fleet_uf_dt"),vehicle$get_data_frame("utility_factor"))
      #
      fleet_specs_dt <- rbind(get0("fleet_specs_dt"),vehicle$get_data_frame("specifications"))
      }
    }
  return(list(fleet_fc_dt=fleet_fc_dt,fleet_uf_dt=fleet_uf_dt,fleet_ef_dt=fleet_ef_dt,fleet_specs_dt=fleet_specs_dt))  
}
