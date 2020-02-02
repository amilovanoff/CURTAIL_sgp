#' vehicle_ef_proj_f
#' @import modelframework
#' @export
vehicle_ef_proj_f <- function(vehicle,last_yr=NA,fc_proj_scen=NA,optimization_fc_mode=NA,optimization_fc_techno=NA,fc_variable=NA){
  attribute_f("vehicle_ef_proj_f")
  if (!vehicle$mode%in%c("MRT","LRT")){
    first_proj_yr <- min(as.numeric(colnames(vehicle$pollutant_emission_factor)[is.na(vehicle$pollutant_emission_factor[1,])]))
    #Case: Optimization
    if (fc_proj_scen=="optimization" & grepl(vehicle$mode,optimization_fc_mode) & grepl(vehicle$technology,optimization_fc_techno)){
      vehicle$pollutant_emission_factor[,as.character(first_proj_yr:last_yr)] <- vehicle$pollutant_emission_factor[,as.character(first_proj_yr-1),drop=FALSE] %*% matrix(sapply(1-fc_variable*0:(last_yr-first_proj_yr),function(x)ifelse(x>=0,x,0)),nrow=1)
      
    #Case: constant emission factors
    } else {
    #Update emission factors
    vehicle$pollutant_emission_factor[,as.character(first_proj_yr:last_yr)] <- vehicle$pollutant_emission_factor[,as.character(first_proj_yr-1)] 
    }
  }
  return(vehicle)
}
