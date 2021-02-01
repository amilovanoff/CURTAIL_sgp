#' fleet_fuel_u_f
#' Function: Calculates the on-road fuel use by technology, fuel type and age for a given fleet
#' @import reshape2
#' @export
fleet_fuel_u_f<-function (fleet){
  attribute_f("fleet_fuel_u_f")
  #Inputs
  deg_fac_dt <- get_input_f(input_name = "on_road_degradation_factors")
  deg_fac_icevg_dt <- get_input_f(input_name = "on_road_degradation_factors_icevg")
  #Functions' Outputs: Get fuel consumption and utility factors
  vehicle_module_f_res <- do.call(fun_res_f,list(fun_name="vehicle_module_f"))
  fleet_fc_dt <- vehicle_module_f_res[["fleet_fc_dt"]]
  fleet_uf_dt <- vehicle_module_f_res[["fleet_uf_dt"]]
  max_age_tbc = 20
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
    mat_conv[rownames(mat_conv),colnames(mat_conv)] <- vapply(colnames(mat_conv),function(x)as.numeric(sapply(1:nrow(mat_conv),function(y)x %in% strsplit(rownames(mat_conv)[y],"_")[[1]])),FUN.VALUE=numeric(nrow(mat_conv)))
    #Create on-road degradation factor matrix
    mat_deg_factor <- matrix(0,nrow=nrow(mat_fC_uf),ncol=nrow(mat_fC_uf),dimnames=list(rownames(mat_fC_uf),rownames(mat_fC_uf)))
    if (fleet$mode %in% c("Public bus","Private bus","School bus")){
      mat_deg_factor[rownames(mat_deg_factor),colnames(mat_deg_factor)] <- vapply(colnames(mat_deg_factor),function(x)as.numeric(grepl(x,rownames(mat_deg_factor)))*subset(deg_fac_dt,Mode=="Bus" & Technology==strsplit(x,split="_")[[1]][1])$Value,FUN.VALUE=numeric(nrow(mat_deg_factor)))
    } else if (fleet$mode %in% c("Private car","Private hire car","Taxi")){
      #Fill degradation factors except ICEV-G
      mat_deg_factor[rownames(mat_deg_factor),setdiff(colnames(mat_deg_factor),"ICEV-G_Gasoline")] <- vapply(setdiff(colnames(mat_deg_factor),"ICEV-G_Gasoline"),function(x)as.numeric(grepl(x,rownames(mat_deg_factor)))*subset(deg_fac_dt,Mode=="Car" & Technology==strsplit(x,split="_")[[1]][1])$Value,FUN.VALUE=numeric(nrow(mat_deg_factor)))
      #Fill ICEV-G
      mat_deg_factor["ICEV-G_Gasoline","ICEV-G_Gasoline"] <- ifelse(year%in%deg_fac_icevg_dt$Year,subset(deg_fac_icevg_dt,Year==year)$Value,1.25)
    #All other technologies without degradation  
    } else {
      mat_deg_factor[rownames(mat_deg_factor),colnames(mat_deg_factor)] <- vapply(colnames(mat_deg_factor),function(x)as.numeric(grepl(x,rownames(mat_deg_factor))),FUN.VALUE=numeric(nrow(mat_deg_factor)))
      }
    #Calculate fuel use by technology and fuel, and fill vint_fuel_use
    fleet$vint_fuel_use[[year]] <- mat_deg_factor[,rownames(mat_fC_uf)] %*% (mat_fC_uf * (mat_conv[rownames(mat_fC_uf),] %*% matrix_vkt[colnames(mat_conv),colnames(mat_fC_uf)]))
    #Calculate total fuel use by year
    mat_fuel_conv <- t(matrix(vapply(rownames(fleet$fuel_use),function(x)as.numeric(grepl(x,rownames(fleet$vint_fuel_use[[year]]))),FUN.VALUE=numeric(nrow(fleet$vint_fuel_use[[year]]))),
                            nrow=nrow(fleet$vint_fuel_use[[year]]),
                            ncol=nrow(fleet$fuel_use),
                            dimnames=list(rownames(fleet$vint_fuel_use[[year]]),rownames(fleet$fuel_use))))
    fleet$fuel_use[,year] <- mat_fuel_conv[rownames(fleet$fuel_use),rownames(fleet$vint_fuel_use[[year]])] %*% matrix(rowSums(fleet$vint_fuel_use[[year]]),dimnames = list(rownames(fleet$vint_fuel_use[[year]]),"Total"))
  }
  return(fleet)
}
