#' fleet_module_f
#' 
#' @import modelframework
#' @export
fleet_module_f <- function(mode){
  attribute_f("fleet_module_f")
  #Get stock, vkt and fuel use of cars
  fleet <- do.call(fleet_vint_stock_f,list(mode=mode))
  #Calculate the flow of battery
  fleet <- do.call(fleet_battery_flow_f,list(fleet=fleet)) 
  #Calculate the vkt of the vintaged stock
  fleet <- do.call(fleet_vkt_f,list(fleet=fleet))
  #Calculate the fuel use of the vintaged stock
  fleet <- do.call(fleet_fuel_u_f,list(fleet=fleet))
  return(fleet)
}
