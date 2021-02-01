#' fleet_vint_stock_f
#' Function: Simulates the vintaged fleet stock of the specified mode
#' @export
fleet_vint_stock_f <- function(mode,last_yr=NA){
  attribute_f("fleet_vint_stock_f")
  #Initialize the fleet object with historical data for stock
  fleet <- do.call(fleet_initialize_f,list(mode=mode))
  #Project the total car stock
  fleet <- do.call(fleet_stock_proj_f,list(fleet=fleet))
  #Project the technology market share
  fleet <- do.call(fleet_technology_market_share_proj_f,list(fleet=fleet))
  #Project the vintaged stock
  first_proj_yr <- max(as.numeric(names(fleet$vint_stock)))+1
  for (year in first_proj_yr:last_yr){
    fleet <- do.call(fleet_vint_stock_update_f,list(fleet=fleet,year=year))
  }
  return(fleet)
}
