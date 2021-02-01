#' transportClass
#' Class: Defines objects that contain all transport modes and related activities (e.g., pkt, vkt, modal share, load factors)
#' @import tidyr
#' @export
transportClass <- setRefClass("transportClass",
                              fields = list(kt_per_veh="matrix",
                                            load_factors="matrix",
                                            vkt="matrix",
                                            pkt="matrix",
                                            units="character"),
                              methods = list(
                                ###>Function: Initialize the object
                                initialize = function(){
                                  .self$units <- c(kt_per_veh="km/(veh.year)",
                                                   load_factors="passenger/veh",
                                                   vkt="thousand km",
                                                   pkt="thousand passenger")
                                },
                                ###>Function: Return the fields of the vehicle as data.frame
                                get_data_frame = function(field_name){
                                  field_values <- .self$field(field_name)
                                  #Convert matrix into long dataframe
                                  field_dt <- as.data.frame(field_values,stringsAsFactors = FALSE) %>% 
                                    cbind(Mode=rownames(field_values),stringsAsFactors = FALSE) %>% 
                                    gather("Year","Value",-Mode,convert=TRUE) %>%
                                    cbind(Unit=as.character(.self$units[field_name]),stringsAsFactors = FALSE)
                                  return(field_dt)
                                },
                                ###Function: Return all the fields into list of dataframe
                                get_list_dataframe = function(){
                                  return(list(transport_kt_per_veh=.self$get_data_frame("kt_per_veh"),
                                              transport_load_factors=.self$get_data_frame("load_factors"),
                                              transport_vkt=.self$get_data_frame("vkt"),
                                              transport_pkt=.self$get_data_frame("pkt")))
                                }
                              ))
#' fleetClass
#' Class: Defines objects that contain all data related to the fleet of a given transport mode (e.g., stock, sales, fuel use, GHG emissions)
#' @import tidyr
#' @export
fleetClass <- setRefClass("fleetClass",
                          fields = list(mode="character",
                                        vint_stock="list",
                                        vint_scrap="list",
                                        technology_market_share="matrix",
                                        sales="matrix",
                                        on_road_stock="matrix",
                                        on_road_stock_tot="matrix",
                                        battery_flow="matrix",
                                        vint_vkt="list",
                                        vint_fuel_use="list",
                                        fuel_use="matrix",
                                        lca_env_matrix="matrix",
                                        lca_demand_matrix="matrix",
                                        lca_score="matrix"),
                          methods = list(
                            ###>Function: Initialize the object
                            initialize = function(){
                            },
                            ###>Function: Calculate LCA score
                            calculate_lca_score = function(){
                              .self$lca_score <- .self$lca_demand_matrix * .self$lca_env_matrix
                            },
                            ###>Function: Return dataframe of lca score by process
                            get_dataframe_lca_process = function(){
                              #Input
                              transport_mode <- get_input_f(input_name = 'model_matching_passenger_transport_mode')
                              lca_process  <- get_input_f(input_name = 'lca_process')
                              lca_process <- subset(lca_process,Mode%in%c("all",subset(transport_mode,Mode==.self$mode)$Mode_type))
                              #Get dataframe
                              fleet_lca_process <- as.data.frame(.self$lca_score) %>% 
                                cbind(lca_process[,c("Sector","Phase","Process")],stringsAsFactors = FALSE) %>% 
                                gather("Year","Value",-c(Sector,Phase,Process),convert=TRUE) %>% 
                                cbind(Unit="kg CO2 eq",Mode=.self$mode,stringsAsFactors = FALSE)
                              return(fleet_lca_process)
                            },
                            ###>Function: Return dataframe of lca score by phase
                            get_dataframe_lca_phase = function(){
                              fleet_lca_process <- .self$get_dataframe_lca_process()
                              #Aggregate by process
                              agg.formula <- reformulate(termlabels = setdiff(colnames(fleet_lca_process),c("Process","Value")),response = "Value")
                              fleet_lca_phase <- aggregate(data = fleet_lca_process,agg.formula,FUN=sum)
                              return(fleet_lca_phase)
                            },
                            ###>Function: Return dataframe of lca score 
                            get_dataframe_lca_sector = function(){
                              fleet_lca_phase <- .self$get_dataframe_lca_phase()
                              #Aggregate by sector
                              agg.formula <- reformulate(termlabels = setdiff(colnames(fleet_lca_phase),c("Phase","Value")),response = "Value")
                              fleet_lca_sector <- aggregate(data = fleet_lca_phase,agg.formula,FUN=sum)
                              return(fleet_lca_sector)
                            },
                            ###>Function: Return dataframe of total LCA score
                            get_dataframe_lca_tot = function(){
                              fleet_lca_sector <- .self$get_dataframe_lca_sector()
                              #Aggregate by sector
                              agg.formula <- reformulate(termlabels = setdiff(colnames(fleet_lca_sector),c("Sector","Value")),response = "Value")
                              fleet_lca_tot <- aggregate(data = fleet_lca_sector,agg.formula,FUN=sum)
                              return(fleet_lca_tot)
                            },
                            ###>Function: Return the fields of the vehicle as data.frame
                            get_data_frame = function(field_name){
                              field_values <- .self$field(field_name)
                              if (is.matrix(field_values)){
                                #Convert matrix into long dataframe
                                field_dt <- as.data.frame(field_values,stringsAsFactors = FALSE) %>% 
                                  cbind(Data=rownames(field_values),stringsAsFactors = FALSE) %>% 
                                  gather("Year","Value",-Data,convert=TRUE) %>%
                                  cbind(Mode=.self$mode)
                                #Update colnames
                                colnames(field_dt)[colnames(field_dt)=="Data"] <- switch(field_name,
                                                                                         "fuel_use"="Fuel",
                                                                                         "Technology")
                              } else if(class(field_values)=="list"){
                                for (i in seq_len(length(field_values))){
                                  tmp_stock_dt <- as.data.frame(field_values[[i]]) %>% 
                                    cbind(Technology=rownames(field_values[[i]]),stringsAsFactors = FALSE) %>% 
                                    gather("Age","Value",-Technology,convert=TRUE) %>%
                                    cbind(Year=as.numeric(names(field_values)[i]))%>%
                                    cbind(Mode=.self$mode)
                                  #Update output 
                                  field_dt <- rbind(get0("field_dt"),tmp_stock_dt)
                                }
                              }
                              return(field_dt)
                            }
                          ))

#' vehicleClass
#' Class: Defines all data related to a specific technology of a transport mode (e.g., fuel consumption, utility factor, fuel used, battery type)
#' @import tidyr
#' @import modelframework
#' @export
vehicleClass <- setRefClass("vehicleClass",
                             fields = list(mode="character",
                                           technology="character",
                                           fuel_type="character",
                                           fuel_consumption="matrix",
                                           utility_factor="matrix",
                                           specifications="matrix",
                                           battery_type="character"),
                             methods = list(
                               ###>Function: Initialize the object
                               initialize = function(mode=as.character(),technology=as.character(),...){
                                 #Update fields
                                 .self$mode <<- mode
                                 .self$technology <<- technology
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
                                   .self$fuel_type <<- unlist(strsplit(subset(vh_techno,Technology==technology)[,"Fuel"],";"))
                                 } else {
                                   .self$fuel_type <<-"Electricity"
                                 }
                               },
                               ###>Function: Return the fields of the vehicle as data.frame
                               get_data_frame = function(field_name){
                                 field_values <- .self$field(field_name)
                                 if (is.matrix(field_values)){
                                   #Convert matrix into long dataframe
                                   field_dt <- as.data.frame(field_values,stringsAsFactors = FALSE) %>% 
                                     cbind(Data=rownames(field_values),stringsAsFactors = FALSE) %>% 
                                     gather("Model_year","Value",-Data,convert=TRUE) %>%
                                     cbind(Mode=.self$mode,Technology=.self$technology,stringsAsFactors = FALSE) %>%
                                     subset(.,select=c(Mode,Technology,Model_year,Data,Value))
                                   #Update colnames
                                   colnames(field_dt)[colnames(field_dt)=="Data"] <- switch(field_name,
                                                                                            "fuel_consumption"="Fuel",
                                                                                            "specifications"="Attribute",
                                                                                            "utility_factor"="Fuel")
                                 } else if(class(field_values)=="data.frame"){
                                   field_dt <- field_values %>%
                                     cbind(Mode=.self$mode,Technology=.self$technology,stringsAsFactors = FALSE)
                                 }
                                 return(field_dt)
                               }
                             ))
