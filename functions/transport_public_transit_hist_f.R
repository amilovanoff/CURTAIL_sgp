#' transport_public_transit_hist_f
#' 
#' @import reshape2
#' @export
transport_public_transit_hist_f <- function(transport,first_yr=NA){
  attribute_f("transport_public_transit_hist_f")
  #Input
  pt_tot_vkt <- get_input_f(input_name = 'hist_public_transit_vkt')
  pt_tot_vkt <- reshape2::melt(pt_tot_vkt, id.vars = c("year","unit"),variable.name = "Mode",value.name = "Value")
  pt_tot_vkt$Mode<- as.character(pt_tot_vkt$Mode)
  pt_tot_vkt[pt_tot_vkt$Mode=="Bus","Mode"] <- "Public bus"
  pt_ridership <- get_input_f(input_name = 'hist_public_transit_ridership')
  pt_ridership[pt_ridership$type_of_public_transport=="Bus","type_of_public_transport"] <- "Public bus"
  pt_trip_km <- get_input_f(input_name = 'hist_public_transit_trip_distance')
  pt_trip_km[pt_trip_km$mode=="Bus","mode"] <- "Public bus"
  #Other parameters 
  last_hist_yr <- 2018
  #Create matrix of total vkt (km)
  tmp_mat_pt_tot_vkt <- reshape2::acast(data=subset(pt_tot_vkt,year%in%c(first_yr:last_hist_yr)), Mode ~ year , value.var='Value',fun.aggregate=sum, margins=FALSE)
  #Create matrix of ridership (Passenger.trips)
  mat_pt_ridership <- reshape2::acast(data=subset(pt_ridership,year%in%c(first_yr:last_hist_yr)), type_of_public_transport ~ year , value.var='average_ridership',fun.aggregate=sum, margins=FALSE)*365.25
  #Create matrix of trip distance (km/trip) from data
  mat_pt_trip_km <- reshape2::acast(data=subset(pt_trip_km,year%in%c(first_yr:last_hist_yr)), mode ~ year , value.var='ave_distance_per_trip',fun.aggregate=sum, margins=FALSE)
  #Fill load factors with given data
  transport$load_factors[rownames(tmp_mat_pt_tot_vkt),colnames(mat_pt_trip_km)] <- mat_pt_ridership[rownames(tmp_mat_pt_tot_vkt),colnames(mat_pt_trip_km)]*mat_pt_trip_km[rownames(tmp_mat_pt_tot_vkt),]/tmp_mat_pt_tot_vkt[,colnames(mat_pt_trip_km)]
  #Complete Bus and MRT load factor with average
  transport$load_factors[c("Public bus","MRT"),as.character(c(2015:2018))] <- rowMeans(transport$load_factors[c("Public bus","MRT"),as.character(2005:2014)])
  #Assume LRT same than mean
  transport$load_factors["LRT",as.character(c(2005:2008,2015:2018))] <- mean(transport$load_factors["LRT",as.character(2009:2014)])
  #Create extended matrix of average distance per trip
  mat_pt_trip_km_ex <- matrix(NA,nrow=nrow(mat_pt_ridership),ncol=ncol(mat_pt_ridership),dimnames=dimnames(mat_pt_ridership))
  mat_pt_trip_km_ex[rownames(mat_pt_trip_km),colnames(mat_pt_trip_km)] <- mat_pt_trip_km
  mat_pt_trip_km_ex[,as.character(c(2015:2018))] <- rowMeans(mat_pt_trip_km_ex[,as.character(2005:2014)])
  #Calculate in thousand km
  transport$vkt[rownames(mat_pt_ridership),colnames(mat_pt_ridership)] <- mat_pt_ridership*mat_pt_trip_km_ex/transport$load_factors[rownames(mat_pt_ridership),colnames(mat_pt_ridership)]/1000
  #Calculate in thousand pkt
  transport$pkt[rownames(mat_pt_ridership),colnames(mat_pt_ridership)] <- mat_pt_ridership*mat_pt_trip_km_ex/1000
  return(transport)
}
