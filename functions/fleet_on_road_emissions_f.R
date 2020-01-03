#' fleet_on_road_emissions_f
#' Function: Calculates the on-road emisisons technology,pollutant and age
#' @import reshape2
#' @export
fleet_on_road_emissions_f<-function (fleet){
  attribute_f("fleet_on_road_emissions_f")
  #Functions' Outputs: Get fuel consumption and utility factors
  vehicle_module_f_res <- do.call(fun_res_f,list(fun_name="vehicle_module_f"))
  fleet_ef_dt <- vehicle_module_f_res[["fleet_ef_dt"]]
  max_age_tbc = as.numeric(switch(fleet$mode,
                              "Private car" = "15",
                              "Taxi"="15",
                              "Motorcycle" = "20",
                              "Public bus" = "20",
                              "Private bus" = "20",
                              "School bus" = "20"))
  #Loop for technology
  for (year in names(fleet$vint_vkt)) {
    #Extract matrix of vkt by technology and age for year
    matrix_vkt <- fleet$vint_vkt[[year]]
    colnames(matrix_vkt) <- as.numeric(year)-as.numeric(colnames(matrix_vkt))
    #Get matrix of fuel consumption, utility factor and adjusted fuel consumption by technology and fuel and age. Unit: L or kWh / 100km
    mat_ef <- acast(subset(fleet_ef_dt,Mode==fleet$mode & Model_year%in%c((as.numeric(year)-max_age_tbc):as.numeric(year))), Technology + Pollutant ~ Model_year , value.var='Value',fun.aggregate=sum, margins=FALSE)
    #Create a transition matrix to convert matrix_vkt with mat_ef rows
    mat_conv <- matrix(NA, nrow=nrow(mat_ef),ncol=nrow(matrix_vkt),dimnames = list(rownames(mat_ef),rownames(matrix_vkt)))
    mat_conv[rownames(mat_conv),colnames(mat_conv)] <- vapply(colnames(mat_conv),function(x)as.numeric(grepl(x,rownames(mat_conv))),FUN.VALUE=numeric(nrow(mat_conv)))
    #Calculate fuel use by technology and fuel, and fill vint_fuel_use
    fleet$vint_on_road_emissions[[year]] <- mat_ef * (mat_conv[rownames(mat_ef),] %*% matrix_vkt[colnames(mat_conv),colnames(mat_ef)])
    #Calculate total fuel use by year
    mat_tot_conv <- t(matrix(vapply(rownames(fleet$on_road_emissions),function(x)as.numeric(grepl(x,rownames(fleet$vint_on_road_emissions[[year]]))),FUN.VALUE=numeric(nrow(fleet$vint_on_road_emissions[[year]]))),
                            nrow=nrow(fleet$vint_on_road_emissions[[year]]),
                            ncol=nrow(fleet$on_road_emissions),
                            dimnames=list(rownames(fleet$vint_on_road_emissions[[year]]),rownames(fleet$on_road_emissions))))
    fleet$on_road_emissions[,year] <- mat_tot_conv[rownames(fleet$on_road_emissions),rownames(fleet$vint_on_road_emissions[[year]])] %*% matrix(rowSums(fleet$vint_on_road_emissions[[year]]),dimnames = list(rownames(fleet$vint_on_road_emissions[[year]]),"Total"))
  }
  return(fleet)
}
