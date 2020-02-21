#' transport_veh_pop_f
#' Function: Get the stock of vehicle population
#' @import modelframework
#' @import tidyr
#' @export
transport_veh_pop_f<-function(){
  attribute_f("transport_veh_pop_f")
  #Inputs
  transport_mode <- get_input_f(input_name = 'model_matching_passenger_transport_mode')
  transport_veh_pop_dt <- NULL
  transport_vint_veh_pop_dt <- NULL
  #Loop on transport mode
  #for (mode in unique(transport_mode$Mode)){
  for (mode in c("Private car","Private hire car","Taxi","Private bus","School bus","Public bus","Motorcycle")){
    #Calculate fleet module
    fleet <- do.call(fleet_module_f,list(mode=mode))
    #Get dataframe of stock
    transport_veh_pop_dt <- rbind(transport_veh_pop_dt,fleet$get_data_frame("on_road_stock"))
    #Get dataframe of vintage stock
    transport_vint_veh_pop_dt <- rbind(transport_vint_veh_pop_dt,fleet$get_data_frame("vint_stock"))
  }
  return(list(transport_veh_pop_dt=transport_veh_pop_dt,
              transport_vint_veh_pop_dt=transport_vint_veh_pop_dt))
}
