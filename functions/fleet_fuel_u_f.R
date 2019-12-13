#' fleet_fuel_u_f
#' Function: Calculates the on-road fuel use by technology, fuel type and age
#' @import reshape2
#' @export
fleet_fuel_u_f<-function (fleet){
  attribute_f("fleet_fuel_u_f")
  #Functions' Outputs: Get fuel consumption and utility factors
  vehicle_module_f_res <- do.call(fun_res_f,list(fun_name="vehicle_module_f"))
  fleet_fc_dt <- vehicle_module_f_res[["fleet_fc_dt"]]
  fleet_uf_dt <- vehicle_module_f_res[["fleet_uf_dt"]]
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
    mat_fc <- acast(subset(fleet_fc_dt,Mode==fleet$mode & Model_year%in%c((as.numeric(year)-max_age_tbc):as.numeric(year))), Technology + Fuel ~ Model_year , value.var='Value',fun.aggregate=sum, margins=FALSE)
    mat_uf <- acast(subset(fleet_uf_dt,Mode==fleet$mode & Model_year%in%c((as.numeric(year)-max_age_tbc):as.numeric(year))), Technology + Fuel ~ Model_year , value.var='Value',fun.aggregate=sum, margins=FALSE)
    mat_fC_uf <- mat_fc/100 * mat_uf
    #Create a transition matrix to convert matrix_vkt with mat_fc_uf rows
    mat_conv <- matrix(NA, nrow=nrow(mat_fC_uf),ncol=nrow(matrix_vkt),dimnames = list(rownames(mat_fC_uf),rownames(matrix_vkt)))
    mat_conv[rownames(mat_conv),colnames(mat_conv)] <- vapply(colnames(mat_conv),function(x)as.numeric(grepl(x,rownames(mat_conv))),FUN.VALUE=numeric(nrow(mat_conv)))
    #Calculate fuel use by technology and fuel, and fill vint_fuel_use
    fleet$vint_fuel_use[[year]] <- mat_fC_uf * (mat_conv[rownames(mat_fC_uf),] %*% matrix_vkt[colnames(mat_conv),colnames(mat_fC_uf)])
    #Calculate total fuel use by year
    mat_fuel_conv <- t(matrix(vapply(rownames(fleet$fuel_use),function(x)as.numeric(grepl(x,rownames(fleet$vint_fuel_use[[year]]))),FUN.VALUE=numeric(nrow(fleet$vint_fuel_use[[year]]))),
                            nrow=nrow(fleet$vint_fuel_use[[year]]),
                            ncol=nrow(fleet$fuel_use),
                            dimnames=list(rownames(fleet$vint_fuel_use[[year]]),rownames(fleet$fuel_use))))
    fleet$fuel_use[,year] <- mat_fuel_conv[rownames(fleet$fuel_use),rownames(fleet$vint_fuel_use[[year]])] %*% matrix(rowSums(fleet$vint_fuel_use[[year]]),dimnames = list(rownames(fleet$vint_fuel_use[[year]]),"Total"))
  }
  return(fleet)
}
