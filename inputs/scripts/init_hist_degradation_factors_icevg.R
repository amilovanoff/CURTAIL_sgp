#' Script to calculate the historical degradation factors of ICEV-G
#Inputs
on_road_fc_icevg <- read.csv("inputs/model/hist_fc_onroad_icevg.csv",stringsAsFactors = FALSE,check.names = FALSE)

#Functions' Outputs: Get fuel consumption and utility factors
vehicle_module_f_res <- do.call(fun_res_f,list(fun_name="vehicle_module_f"))
fleet_fc_dt <- vehicle_module_f_res[["fleet_fc_dt"]]
fleet_uf_dt <- vehicle_module_f_res[["fleet_uf_dt"]]

max_age_tbc = 20
last_yr=2017
first_yr=2005

mode_tbc <- c("Private car","Private hire car")

mat_fuel_use <- matrix(0,nrow=length(mode_tbc),ncol=last_yr-first_yr+1,dimnames=list(mode_tbc,first_yr:last_yr))

for (mode in mode_tbc){
  #Get stock, vkt and fuel use of cars
  fleet <- do.call(fleet_vint_stock_f,list(mode=mode))
  #Calculate the vkt of the vintaged stock
  fleet <- do.call(fleet_vkt_f,list(fleet=fleet))
  #Calculate the vkt of the vintaged stock
  
  for (year in as.character(2005:2017)) {
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
  #Fill
  mat_fuel_use[mode,as.character(first_yr:last_yr)] <- fleet$fuel_use["Gasoline",as.character(first_yr:last_yr)]
}
 
#Matrix real_worl gasoline use

mat_real_gas_use <- acast(data=on_road_fc_icevg,  Source ~ Year , value.var='Car_fuel_use',fun.aggregate=sum, margins=FALSE)

#Calculate degradation
mat_deg_factors <- as.matrix(mat_real_gas_use[,colnames(mat_fuel_use)] / colSums(mat_fuel_use))
colnames(mat_deg_factors) <- "Value"
#Save
out_dt <- as.data.frame(mat_deg_factors) %>% 
  cbind(Year=rownames(mat_deg_factors),Technology="ICEV-G",stringsAsFactors = FALSE)

write.csv(out_dt,"inputs/model/hist_on_road_degradation_factors_icevg.csv",row.names = FALSE)
