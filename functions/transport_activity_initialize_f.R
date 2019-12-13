#' transport_activity_initialize_f
#' Create a transport object  fill it with historical data
#' @export
transport_activity_initialize_f <- function(first_yr=NA,last_yr=NA){
  attribute_f("transport_activity_initialize_f")
  #Inputs
  transport_mode <- get_input_f(input_name = 'model_matching_passenger_transport_mode')
  #Initialize the fleet class
  transport <- new("transportClass")
  for (field_name in setdiff(names(transportClass$fields()),"units")){
    transport[[field_name]] <- matrix(NA,nrow=length(unique(transport_mode$Mode)),ncol=length(first_yr:last_yr),dimnames=list(unique(transport_mode$Mode),first_yr:last_yr))
  }
  #Fill load factors for private vehicles
  transport <- do.call(transport_priv_veh_load_factors_f,list(transport=transport))
  #Fill historical data for private vehicles
  transport <- do.call(transport_priv_veh_hist_f,list(transport=transport))
  #Fill historical vkt, pkt and load factors for public transit
  transport <- do.call(transport_public_transit_hist_f,list(transport=transport))
  #Fill historical km travelled by vehicles for public transit
  transport <- do.call(transport_public_transit_hist_kt_per_veh_f,list(transport=transport))
  return(transport)
}
