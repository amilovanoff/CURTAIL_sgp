#' transport_priv_veh_load_factors_f
#' Fill the load factors for private transport mode
#' @export
transport_priv_veh_load_factors_f <- function(transport,first_yr=NA){
  attribute_f("transport_priv_veh_load_factors_f")
  last_hist_yr <- 2019
  #ASSUMPTIONS
  transport$load_factors["Motorcycle",as.character(first_yr:last_hist_yr)] <- 1
  transport$load_factors["Private car",as.character(first_yr:last_hist_yr)] <- 1.75
  transport$load_factors["Private hire car",as.character(first_yr:last_hist_yr)] <- 0.8
  transport$load_factors["Private bus",as.character(first_yr:last_hist_yr)] <- 10
  transport$load_factors["School bus",as.character(first_yr:last_hist_yr)] <- 15
  transport$load_factors["Taxi",as.character(first_yr:last_hist_yr)] <- 1.75
  return(transport)
}
