#' fleet_initialize_f
#' Create a fleet object for light-duty vehicles and fill it with historical data
#' @export
fleet_initialize_f <- function(mode,first_yr=NA,last_yr=NA){
  attribute_f("fleet_initialize_f")
  #Inputs
  techno_input_name <- switch(mode,
                              "Private car"="model_matching_vehicle_technology",
                              "Taxi"="model_matching_vehicle_technology",
                              "Motorcycle"="model_matching_moto_technology",
                              "Public bus"="model_matching_bus_technology",
                              "Private bus"="model_matching_bus_technology",
                              "School bus"="model_matching_bus_technology")
  vh_techno <- get_input_f(input_name = techno_input_name)
  vint_stock_input_name <- switch(mode,
                                  "Private car"="hist_car_vint_stock",
                                  "Taxi"="hist_taxi_vint_stock",
                                  "Motorcycle"="hist_moto_vint_stock",
                                  "Public bus"="hist_pub_bus_vint_stock",
                                  "Private bus"="hist_priv_bus_vint_stock",
                                  "School bus"="hist_school_bus_vint_stock")
  hist_vint_stock  <- get_input_f(input_name = vint_stock_input_name)
  #Initialize the fleet class
  fleet <- new("fleetClass")
  fleet$mode <- mode
  #
  vint_stock_list_input_name <- switch(mode,
                                       "Private car"="car_hist_vint_stock",
                                       "Taxi"="taxi_hist_vint_stock",
                                       "Motorcycle"="moto_hist_vint_stock",
                                       "Public bus"="pub_bus_hist_vint_stock",
                                       "Private bus"="priv_bus_hist_vint_stock",
                                       "School bus"="school_bus_hist_vint_stock")
  fleet$vint_stock <- readRDS(paste0("inputs/model/",vint_stock_list_input_name,".RDS"))
  #Create matrix of stock by technology, and fill it with historical data
  fleet$on_road_stock <- matrix(NA,nrow=length(unique(vh_techno$Technology)),ncol=length(first_yr:last_yr),dimnames=list(unique(vh_techno$Technology),first_yr:last_yr))
  matrix_stock_hist <- acast(data=subset(hist_vint_stock), Technology ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)
  fleet$on_road_stock[rownames(matrix_stock_hist),colnames(matrix_stock_hist)] <- matrix_stock_hist
  #Create matrix of total stock and fill it
  fleet$on_road_stock_tot <- matrix(NA,nrow=1,ncol=length(first_yr:last_yr),dimnames=list("Total",first_yr:last_yr))
  fleet$on_road_stock_tot["Total",colnames(matrix_stock_hist)] <- colSums(matrix_stock_hist)
  #Create matrix of sales by technology, and fill it with historical data
  fleet$sales <- matrix(NA,nrow=length(unique(vh_techno$Technology)),ncol=length(first_yr:last_yr),dimnames=list(unique(vh_techno$Technology),first_yr:last_yr))
  matrix_sales_hist <- acast(data=subset(hist_vint_stock,Age==0), Technology ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)
  fleet$sales[rownames(matrix_sales_hist),colnames(matrix_sales_hist)] <- matrix_sales_hist
  #Create matrix of technology market share, and fill it
  fleet$technology_market_share <- matrix(NA,nrow=length(unique(vh_techno$Technology)),ncol=length(first_yr:last_yr),dimnames=list(unique(vh_techno$Technology),first_yr:last_yr))
  fleet$technology_market_share[rownames(matrix_sales_hist),colnames(matrix_sales_hist)] <- matrix_sales_hist %*% diag(x=vapply(colSums(matrix_sales_hist),function(x)ifelse(x==0,0,1/x),FUN.VALUE = 1))
  #Create matrix of fuel use
  fleet$fuel_use <- matrix(NA,nrow=length(unique(unlist(strsplit(vh_techno$Fuel,";")))),ncol=length(first_yr:last_yr),dimnames=list(unique(unlist(strsplit(vh_techno$Fuel,";"))),first_yr:last_yr))
  return(fleet)
}
