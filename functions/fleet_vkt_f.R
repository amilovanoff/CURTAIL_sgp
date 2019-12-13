#' fleet_vkt_f
#' Function: Calculates the distance traveled by technology, size, age and year in kilometers of the entire fleet.
#' @export
fleet_vkt_f<-function (fleet){
  attribute_f("fleet_vkt_f")
  #Get transport activity annual mileage
  transport <- fun_res_f(fun_name = "transport_activity_f")
  for (year in names(fleet$vint_stock)){
    fleet$vint_vkt[[year]] <- fleet$vint_stock[[year]]*transport$kt_per_veh[fleet$mode,year]
  }
  return(fleet)
}
