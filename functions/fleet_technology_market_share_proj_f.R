#' fleet_technology_market_share_proj_f
#' 
#' @import modelframework
#' @import reshape2
#' @export
fleet_technology_market_share_proj_f <- function(fleet,last_yr=NA){
  attribute_f("fleet_technology_market_share_proj_f")
  first_proj_yr <- min(as.numeric(colnames(fleet$technology_market_share)[is.na(fleet$technology_market_share[1,])]))
  fleet$technology_market_share[,as.character(first_proj_yr:last_yr)] <- fleet$technology_market_share[,as.character(first_proj_yr-1)]
  return(fleet)
}
