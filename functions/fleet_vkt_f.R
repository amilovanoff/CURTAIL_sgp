#' fleet_vkt_f
#' Function: Calculates the distance traveled by technology, size, age and year in kilometers of the entire fleet.
#' @export
fleet_vkt_f<-function (fleet){
  attribute_f("fleet_vkt_f")
  #Get annual distance travelled by one vehicle from transport_activity_f
  transport_activity_f_res <- do.call(fun_res_f,list(fun_name="transport_activity_f"))
  for (year in names(fleet$vint_stock)){
    fleet$vint_vkt[[year]] <- fleet$vint_stock[[year]]*subset(transport_activity_f_res[["transport_kt_per_veh"]],Mode==fleet$mode & Year==year)$Value
  }
  return(fleet)
}
