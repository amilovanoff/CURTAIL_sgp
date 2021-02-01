#' vehicle_lca_ghg_f
#' Function: Calculates the single-vehicle life cycle GHG emissions by vehicle with and without temporal distributions
#' @import modelframework
#' @export
vehicle_lca_ghg_f <- function(){
  attribute_f("vehicle_lca_ghg_f")
  #Input
  lca_process_general  <- get_input_f(input_name = 'lca_process')
  transport_mode <- get_input_f(input_name = 'model_matching_passenger_transport_mode')
  transport_mode_list <- c("Private car","Private hire car","Taxi","Motorcycle","Public bus","School bus","Private bus")
  #Other parameters
  model_year <- 2018
  #Out
  vehicle_lca_dt <- NULL
  for (mode in transport_mode_list){
    #
    lca_process <- subset(lca_process_general,Mode%in%c("all",subset(transport_mode,Mode==mode)$Mode_type))
    #
    lca_env_matrix <- do.call(fleet_lca_env_matrix_f,list(mode=mode))
    #Get list of all technologies by transport mode
    if (mode%in%c("Private car","Private hire car","Private bus","School bus","Taxi","Public bus","Motorcycle")){
      input_data_name <- switch (mode,
                                 "Private car"="model_matching_vehicle_technology",
                                 "Private hire car"="model_matching_vehicle_technology",
                                 "Taxi"="model_matching_vehicle_technology",
                                 "Motorcycle"="model_matching_moto_technology",
                                 "Public bus"="model_matching_bus_technology",
                                 "Private bus"="model_matching_bus_technology",
                                 "School bus"="model_matching_bus_technology"
      )
      vh_techno <- get_input_f(input_name = input_data_name)
      technology_list <- unique(vh_techno$Technology)
    } else {
      technology_list <- mode
    }
    for (technology in technology_list){
      #Get demand matrix
      vehicle_lca_demand_f_res <- do.call(vehicle_lca_demand_f,list(mode=mode,technology=technology,model_year=model_year))
      vehicle_demand_matrix <- vehicle_lca_demand_f_res[["vehicle_demand_matrix"]]
      #Calculate the Life Cycle Impacts matrix
      vehicle_lca_matrix <- vehicle_demand_matrix * lca_env_matrix
      #Create long table
      #Get dataframe
      tmp_vehicle_lca <- as.data.frame(vehicle_lca_matrix) %>%
        cbind(lca_process[,c("Sector","Phase","Process")],stringsAsFactors = FALSE) %>%
        gather("Year","Value",-c(Sector,Phase,Process),convert=TRUE) %>%
        aggregate(formula=Value~Sector+Phase+Process,data=.,FUN=sum) %>%
        cbind(Unit="kg CO2 eq",Mode=mode,Technology=technology,stringsAsFactors = FALSE) %>%
        subset(.,Value!=0)
      #Merge it with final output
      vehicle_lca_dt <- rbind(get0("vehicle_lca_dt"),tmp_vehicle_lca)
    }
  }
  #Calculate emissions by vkt
  vehicle_lca_vkt <- vehicle_lca_dt
  vehicle_lca_vkt$Value <- sapply(1:nrow(vehicle_lca_vkt),function(x)vehicle_lca_dt[x,"Value"]/as.numeric(switch(subset(transport_mode,Mode==vehicle_lca_dt[x,"Mode"])$Mode_type,Car="150000",Bus="500000",Motorcycle="150000")))
  vehicle_lca_vkt$Unit <- "kg CO2/vkt"
  #Calculate emissions by pkt
  transport_activity_f_res <- do.call(fun_res_f,list(fun_name="transport_activity_f"))
  load_factors <- transport_activity_f_res[["transport_load_factors"]]
  vehicle_lca_pkt <- vehicle_lca_vkt
  vehicle_lca_pkt$Value <- sapply(1:nrow(vehicle_lca_pkt),function(x)vehicle_lca_vkt[x,"Value"]/subset(load_factors,Mode==vehicle_lca_vkt[x,"Mode"] & Year==model_year)$Value)
  vehicle_lca_pkt$Unit <- "kg CO2/pkt"
  return(list(vehicle_lca_dt=vehicle_lca_dt,
              vehicle_lca_vkt=vehicle_lca_vkt,
              vehicle_lca_pkt=vehicle_lca_pkt))
}
