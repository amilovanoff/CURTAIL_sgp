#' transport_public_transit_hist_kt_per_veh_f
#' 
#' @import reshape2
#' @export
transport_public_transit_hist_kt_per_veh_f <- function(transport,first_yr=NA){
  attribute_f("transport_public_transit_hist_kt_per_veh_f")
  #Input
  hist_veh_pop <- get_input_f(input_name = 'hist_veh_pop')
  hist_veh_pop$type <- rename_values(hist_veh_pop$type,list(Taxi="Taxis",`Public bus`="Omnibuses"))
  #Other parameters 
  last_hist_yr <- 2018
  #Create matrix of vehicle population for taxi and bus distance per trip
  mat_pt_veh_pop <- reshape2::acast(data=subset(hist_veh_pop,year%in%c(first_yr:last_hist_yr) & type%in%c("Public bus","Taxi")), type ~ year , value.var='number',fun.aggregate=sum, margins=FALSE)
  #Calculate in thousand pkt
  transport$kt_per_veh[rownames(mat_pt_veh_pop),colnames(mat_pt_veh_pop)] <- transport$vkt[rownames(mat_pt_veh_pop),colnames(mat_pt_veh_pop)]*1000/mat_pt_veh_pop
  return(transport)
}
