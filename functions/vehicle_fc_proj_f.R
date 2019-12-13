#' vehicle_fc_proj_f
#' @import modelframework
#' @export
vehicle_fc_proj_f <- function(vehicle,fc_impro=NA,last_yr=NA){
  attribute_f("vehicle_fc_proj_f")
  first_proj_yr <- min(as.numeric(colnames(vehicle$fuel_consumption)[is.na(vehicle$fuel_consumption[1,])]))
  if (fc_impro=="constant"){
    #Update fleet total stock
    vehicle$fuel_consumption[,as.character(first_proj_yr:last_yr)] <- vehicle$fuel_consumption[,as.character(first_proj_yr-1)] 
  }
  return(vehicle)
}
