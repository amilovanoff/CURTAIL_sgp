#' vehicle_ef_proj_f
#' @import modelframework
#' @export
vehicle_ef_proj_f <- function(vehicle,last_yr=NA){
  attribute_f("vehicle_ef_proj_f")
  if (!vehicle$mode%in%c("MRT","LRT")){
    first_proj_yr <- min(as.numeric(colnames(vehicle$pollutant_emission_factor)[is.na(vehicle$pollutant_emission_factor[1,])]))
    #Update emission factors
    vehicle$pollutant_emission_factor[,as.character(first_proj_yr:last_yr)] <- vehicle$pollutant_emission_factor[,as.character(first_proj_yr-1)] 
  }
  return(vehicle)
}
