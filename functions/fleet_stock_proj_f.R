#' fleet_stock_proj_f
#' @import modelframework
#' @export
fleet_stock_proj_f <- function(fleet,last_yr=NA){
  attribute_f("fleet_stock_proj_f")
  #Projected the vehicle population based on the travel demand by transport mode
  transport_activity_f_res <- do.call(fun_res_f,list(fun_name="transport_activity_f"))
  first_proj_yr <- min(as.numeric(colnames(fleet$on_road_stock_tot)[is.na(fleet$on_road_stock_tot[1,])]))
  #Stock is the vkt divided by km travelled by one vehicle over a year
  fleet$on_road_stock_tot["Total",as.character(first_proj_yr:last_yr)] <- acast(data=subset(transport_activity_f_res[["transport_vkt"]],Mode==fleet$mode & Year%in%first_proj_yr:last_yr), Mode ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)*1000/acast(data=subset(transport_activity_f_res[["transport_kt_per_veh"]],Mode==fleet$mode & Year%in%first_proj_yr:last_yr), Mode ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)
  return(fleet)
}
