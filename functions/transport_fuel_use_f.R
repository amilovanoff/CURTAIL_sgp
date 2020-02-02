#' transport_fuel_use_f
#' Function: Get the fuel use of transport
#' @import modelframework
#' @import tidyr
#' @export
transport_fuel_use_f<-function(){
  attribute_f("transport_fuel_use_f")
  #Inputs
  transport_mode <- get_input_f(input_name = 'model_matching_passenger_transport_mode')
  transport_fuel_use_dt <- NULL
  #Loop on transport mode with fleet
  for (mode in c("Private car","Taxi","Private bus","School bus","Public bus","Motorcycle")){
    #Calculate fleet module
    fleet <- do.call(fleet_module_f,list(mode=mode))
    #Get dataframe of stock
    transport_fuel_use_dt <- rbind(transport_fuel_use_dt,fleet$get_data_frame("fuel_use"))
  }
  #Get LCA score of rapid transit
  rapid_transit_module_f_res <- do.call(fun_res_f,list(fun_name="rapid_transit_module_f"))
  transport_fuel_use_dt <- rbind(transport_fuel_use_dt,rapid_transit_module_f_res[["rapid_transit_fuel_use_dt"]])
  
  return(list(transport_fuel_use_dt=transport_fuel_use_dt))
}
