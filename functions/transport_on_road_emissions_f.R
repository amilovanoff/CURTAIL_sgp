#' transport_on_road_emissions_f
#' Function: Calculate the on-road emissions of the light-duty fleet by air pollutant
#' @import modelframework
#' @import tidyr
#' @export
transport_on_road_emissions_f<-function(){
  attribute_f("transport_on_road_emissions_f")
  #Inputs
  transport_mode <- get_input_f(input_name = 'model_matching_passenger_transport_mode')
  transport_on_road_emissions <- NULL
  #Loop on transport mode
  #for (mode in unique(transport_mode$Mode)){
  for (mode in c("Private car","Taxi","Private bus","School bus","Public bus","Motorcycle")){
    #Calculate fleet module
    fleet <- do.call(fleet_module_f,list(mode=mode))
    #Calculate the GHG emissions of the fleet
    fleet <- do.call(fleet_on_road_emissions_f,list(fleet=fleet))
    #Get dataframe of LCA
    transport_on_road_emissions <- rbind(transport_on_road_emissions,fleet$get_data_frame(field_name="on_road_emissions"))
  }
  #
  agg.formula <- reformulate(termlabels = setdiff(colnames(transport_on_road_emissions),c("Mode","Value")),response = "Value")
  transport_on_road_emissions_tot <- aggregate(data = transport_on_road_emissions,agg.formula,FUN=sum)
  
  return(list(transport_on_road_emissions=transport_on_road_emissions,
              transport_on_road_emissions_tot=transport_on_road_emissions_tot))
}
