#' fleet_stock_proj_f
#' @import modelframework
#' @export
fleet_stock_proj_f <- function(fleet,last_yr=NA){
  attribute_f("fleet_stock_proj_f")
  #Projected the vehicle population based on the travel demand by transport mode
  transport <- fun_res_f(fun_name = "transport_activity_f")
  first_proj_yr <- min(as.numeric(colnames(fleet$on_road_stock_tot)[is.na(fleet$on_road_stock_tot[1,])]))
  #Stock is the vkt divided by km travelled by one vehicle over a year
  fleet$on_road_stock_tot["Total",as.character(first_proj_yr:last_yr)] <- transport$vkt[fleet$mode,as.character(first_proj_yr:last_yr)]*1000/transport$kt_per_veh[fleet$mode,as.character(first_proj_yr:last_yr)]
  return(fleet)
}
