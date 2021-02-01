#' transport_priv_veh_hist_f
#' Function: Loads historical values for the private transport modes of a given transport object
#' @import readxl
#' @import reshape2
transport_priv_veh_hist_f <- function(transport,first_yr=NA){
  attribute_f("transport_priv_veh_hist_f")
  #Input
  pop_vehicle <- get_input_f(input_name = 'hist_veh_pop')
  pop_vehicle_add <- get_input_f(input_name = 'hist_veh_pop_add')
  ann_mileage <- get_input_f(input_name = 'hist_kt_per_veh')
  #Format input
  pop_vehicle$Mode <- get_matching_names(original_values=pop_vehicle$type,matching_type="passenger_transport_mode",original_source="Type",matched_source="Mode")
  ann_mileage$Mode <- get_matching_names(original_values=ann_mileage$vehicle_type,matching_type="passenger_transport_mode",original_source="Type",matched_source="Mode")
  #Other parameters
  last_hist_yr <- 2019
  #Create matrix of vehicle population
  mat_veh_pop <- reshape2::acast(data=subset(pop_vehicle,!Mode%in%c(NA,"Public bus","Taxi") & year%in%c(first_yr:last_hist_yr)), Mode ~ year , value.var='number',fun.aggregate=sum, margins=FALSE)
  #Adjust data for private car and private hire car
  for (year in 2005:2017){
    #Update values for private cars
    mat_veh_pop["Private car",as.character(year)] <- subset(pop_vehicle_add,Year==year)$Private_car
    mat_veh_pop["Private hire car",as.character(year)] <- subset(pop_vehicle_add,Year==year)$Hire_car
  }
  #Fill kilometers traveled by vehicles 
  transport$kt_per_veh[setdiff(unique(ann_mileage$Mode),NA),as.character(unique(ann_mileage$year))] <- reshape2::acast(data=subset(ann_mileage,!Mode%in%c(NA,"Public bus","Taxi")), Mode ~ year , value.var='average_annual_mileage',fun.aggregate=sum, margins=FALSE)[setdiff(unique(ann_mileage$Mode),NA),]
  #Assumption: Mileage for private cars is twice higher than mileage for private cars
  ride_hailing_km_factor <- 2
  transport$kt_per_veh["Private hire car",] <- transport$kt_per_veh["Private car",]*ride_hailing_km_factor
  #Assumption: Same in 2019 than in 2018
  transport$kt_per_veh[,"2019"] <- transport$kt_per_veh[,"2018"]
  #Calculate vehicle kilometers travelled in thousand vkt
  transport$vkt[rownames(mat_veh_pop),colnames(mat_veh_pop)] <- mat_veh_pop*(transport$kt_per_veh[rownames(mat_veh_pop),colnames(mat_veh_pop)]/10^3)
  #Calculate passenger kilometers travelled in thousand pkt
  transport$pkt[rownames(mat_veh_pop),colnames(mat_veh_pop)] <- transport$vkt[rownames(mat_veh_pop),colnames(mat_veh_pop)]*transport$load_factors[rownames(mat_veh_pop),colnames(mat_veh_pop)]
  return(transport)
}
