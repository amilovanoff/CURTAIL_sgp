#' transport_activity_f
#' Function: Simulates the transport activity for all transport modes
#' @export
transport_activity_f <- function(){
  attribute_f("transport_activity_f")
  #Initialize the fleet object with historical data
  transport <- do.call(transport_activity_initialize_f,list())
  #Project the transport activity
  transport <- do.call(transport_activity_proj_f,list(transport=transport))
  return(transport$get_list_dataframe())
}
