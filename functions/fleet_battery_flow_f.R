#' fleet_battery_flow_f
#' Function: Calculates the flow of electric batteries
#' @import reshape2
#' @export
fleet_battery_flow_f <- function(fleet){
  attribute_f("fleet_battery_flow_f")
  #Inputs
  vehicle_module_f_res <- do.call(fun_res_f,list(fun_name="vehicle_module_f"))
  fleet_specs_dt <- vehicle_module_f_res[["fleet_specs_dt"]]
  #Create matrix
  fleet$battery_flow <- matrix(0,nrow=nrow(fleet$sales),ncol=ncol(fleet$sales),dimnames=list(rownames(fleet$sales),colnames(fleet$sales)))
  #Battery capacity
  mat_bat_cap <- acast(subset(fleet_specs_dt,Mode==fleet$mode & Model_year%in%colnames(fleet$battery_flow)), Technology ~ Model_year , value.var='Value',fun.aggregate=sum, margins=FALSE)
  #Calculate the incoming flows of battery
  fleet$battery_flow[rownames(mat_bat_cap),colnames(mat_bat_cap)] <- mat_bat_cap * fleet$sales[rownames(mat_bat_cap),colnames(mat_bat_cap)]
  return(fleet)
}
