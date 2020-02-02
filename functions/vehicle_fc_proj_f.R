#' vehicle_fc_proj_f
#' @import modelframework
#' @export
vehicle_fc_proj_f <- function(vehicle,last_yr=NA,fc_proj_scen=NA,optimization_fc_mode=NA,optimization_fc_techno=NA,fc_variable=NA){
  attribute_f("vehicle_fc_proj_f")
  first_proj_yr <- min(as.numeric(colnames(vehicle$fuel_consumption)[is.na(vehicle$fuel_consumption[1,])]))
  #Case: Optimization
  if (fc_proj_scen=="optimization" & grepl(vehicle$mode,optimization_fc_mode) & grepl(vehicle$technology,optimization_fc_techno)){
    vehicle$fuel_consumption[,as.character(first_proj_yr:last_yr)] <- vehicle$fuel_consumption[,as.character(first_proj_yr-1)]*sapply(1-fc_variable*0:(last_yr-first_proj_yr),function(x)ifelse(x>=0,x,0))
    
  #Case: constant fuel consumption
  } else {
    vehicle$fuel_consumption[,as.character(first_proj_yr:last_yr)] <- vehicle$fuel_consumption[,as.character(first_proj_yr-1)]
  }
  return(vehicle)
}
