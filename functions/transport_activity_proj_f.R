#' transport_activity_proj_f
#' @import modelframework
#' @export
transport_activity_proj_f <- function(transport,last_yr=NA,transport_proj=NA){
  attribute_f("transport_activity_proj_f")
  first_proj_yr <- min(as.numeric(colnames(transport$vkt)[is.na(transport$vkt[1,])]))
  if (transport_proj=="constant"){
    for (field_name in setdiff(names(transportClass$fields()),"units")){
      transport[[field_name]][,as.character(first_proj_yr:last_yr)] <- transport[[field_name]][,as.character(first_proj_yr-1)]
    }
  }
  return(transport)
}
