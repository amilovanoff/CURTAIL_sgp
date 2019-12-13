#' get_transport_activity_f
#' 
#' @import modelframework
#' @export
get_transport_activity_f <- function(){
  attribute_f("get_transport_activity_f")
  #Initialize the fleet object with historical data
  transport <- do.call(transport_activity_f,list())
  #Project the transport activity
  transport <- do.call(transport_activity_proj_f,list(transport=transport))
  
}