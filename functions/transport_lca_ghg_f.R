#' transport_lca_ghg_f
#' Function: Calculate the life cycle GHG emissions of the light-duty fleet by life cycle processes, stages and total
#' @import modelframework
#' @import tidyr
#' @export
transport_lca_ghg_f<-function(){
  attribute_f("transport_lca_ghg_f")
  #Inputs
  transport_mode <- get_input_f(input_name = 'model_matching_passenger_transport_mode')
  transport_lca_ghg_process <- NULL
  #Loop on transport mode
  #for (mode in unique(transport_mode$Mode)){
  for (mode in c("Private car","Private hire car","Taxi","Private bus","School bus","Public bus","Motorcycle")){
    #Calculate fleet module
    fleet <- do.call(fleet_module_f,list(mode=mode))
    #Calculate the GHG emissions of the fleet
    fleet <- do.call(fleet_lca_ghg_f,list(fleet=fleet))
    #Get dataframe of LCA
    transport_lca_ghg_process <- rbind(transport_lca_ghg_process,fleet$get_dataframe_lca_process())
  }
  #Get LCA score of rapid transit
  rapid_transit_module_f_res <- do.call(fun_res_f,list(fun_name="rapid_transit_module_f"))
  transport_lca_ghg_process <- rbind(transport_lca_ghg_process,rapid_transit_module_f_res[["rapid_transit_lca_ghg_dt"]])
  #Aggregate
  agg.formula <- reformulate(termlabels = setdiff(colnames(transport_lca_ghg_process),c("Process","Value")),response = "Value")
  transport_lca_ghg_phase <- aggregate(data = transport_lca_ghg_process,agg.formula,FUN=sum)
  agg.formula <- reformulate(termlabels = setdiff(colnames(transport_lca_ghg_phase),c("Phase","Value")),response = "Value")
  transport_lca_ghg_sector <- aggregate(data = transport_lca_ghg_phase,agg.formula,FUN=sum)
  agg.formula <- reformulate(termlabels = setdiff(colnames(transport_lca_ghg_sector),c("Sector","Value")),response = "Value")
  transport_lca_ghg_mode <- aggregate(data = transport_lca_ghg_sector,agg.formula,FUN=sum)
  agg.formula <- reformulate(termlabels = setdiff(colnames(transport_lca_ghg_mode),c("Mode","Value")),response = "Value")
  transport_lca_ghg_tot <- aggregate(data = transport_lca_ghg_mode,agg.formula,FUN=sum)
  
  return(list(transport_lca_ghg_process=transport_lca_ghg_process,
              transport_lca_ghg_phase=transport_lca_ghg_phase,
              transport_lca_ghg_sector=transport_lca_ghg_sector,
              transport_lca_ghg_mode=transport_lca_ghg_mode,
              transport_lca_ghg_tot=transport_lca_ghg_tot))
}
