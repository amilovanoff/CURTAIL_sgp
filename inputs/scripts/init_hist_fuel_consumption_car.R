#Script to calculate the average on-road fuel consumption of private cars ICEV-G
source("model_script_run.R")
modelframework::load_input_data_f()
first_yr = 1990
last_yr = 2017

#1) From first simulations, calculate the ratio of gasoline used by private cars compared to all other technologies

#get results of first simulations
res <- do.call(transport_fuel_use_f,list())
fuel_use <- res[["transport_fuel_use_dt"]]
#Calculate ratio
matrix_gasoline_ratio <- matrix(0,nrow=1,ncol=last_yr-first_yr+1,dimnames=list("Private car Gasoline",first_yr:last_yr))
matrix_gasoline_ratio[,as.character(2005:last_yr)] <- sapply(2005:last_yr,function(x)subset(fuel_use,Mode=="Private car" & Fuel=="Gasoline" & Year==x)$Value/sum(subset(fuel_use,Fuel=="Gasoline" & Year==x)$Value))
matrix_gasoline_ratio[,as.character(1990:2004)] <- 0.95

#2) From fleet module, calculate the ratio of ICEV-G cars in the fleet for private cars
res <- do.call(transport_veh_pop_f,list())
fleet_car <- subset(res[["transport_veh_pop_dt"]],Mode=="Private car")
#Calculate ratio
matrix_icevg_ratio <- matrix(0,nrow=1,ncol=last_yr-first_yr+1,dimnames=list("Private car ICEV-G",first_yr:last_yr))
matrix_icevg_ratio[,as.character(2005:last_yr)]<- sapply(2005:last_yr,function(x)subset(fleet_car,Technology=="ICEV-G" & Year==x)$Value/sum(subset(fleet_car,Year==x)$Value))
matrix_icevg_ratio[,as.character(1990:2004)] <- 1
#Calculate total stock of Private car
annual_car_population_9004 <- read.csv("inputs/data/annual_car_population_1990-2004.csv",stringsAsFactors = FALSE,check.names = FALSE)

matrix_car_stock <- matrix(0,nrow=1,ncol=last_yr-first_yr+1,dimnames=list("Private car",first_yr:last_yr))
matrix_car_stock[,as.character(first_yr:last_yr)]<- sapply(first_yr:last_yr,function(x)sum(subset(annual_car_population_9004,Year==x)$Value))


#3) Get annual mileage
ann_mileage <- get_input_f(input_name = 'hist_kt_per_veh')
matrix_annual_mileage <- matrix(0,nrow=1,ncol=last_yr-first_yr+1,dimnames=list("Private car ICEV-G",first_yr:last_yr))

matrix_annual_mileage[,as.character(2005:last_yr)]<- sapply(2005:last_yr,function(x)subset(ann_mileage,year==x & vehicle_type=="Cars")$average_annual_mileage)
matrix_annual_mileage[,as.character(1990:2004)] <- 20500

#4) Calculate average on-road fuel consumption
#Input national gasoline consumption
iea_oil_dt <- read.csv("inputs/data/iea_oil_final_consumption_sgp.csv",stringsAsFactors = FALSE,check.names = FALSE)
#Convert ktoe in L
conv <- get_input_f("conversion_units")
fuel_conv <- get_input_f("greet_fuel_specs")
iea_oil_dt$Value <- iea_oil_dt$`Motor gasoline`*10^3*44800*10^6/as.numeric(fuel_conv["Gasoline","LHV Conv"])
matrix_gasoline_use <- matrix(0,nrow=1,ncol=last_yr-first_yr+1,dimnames=list("Gasoline",first_yr:last_yr))
matrix_gasoline_use[,as.character(first_yr:last_yr)] <- sapply(first_yr:last_yr,function(x)subset(iea_oil_dt,Year==x)$Value)
#Average on-road fuel consumption

matrix_average_onroad_fc <- matrix(0,nrow=1,ncol=2017-1990+1,dimnames=list("Private Car ICEV-G",1990:2017))
matrix_average_onroad_fc[,] <- (matrix_gasoline_use*matrix_gasoline_ratio)/(matrix_car_stock*matrix_icevg_ratio*matrix_annual_mileage)*100

mat_fc <- matrix_average_onroad_fc
mat_fc[,as.character(2005:2017)] <- NA





#DOES NOT WORK) Calculate the fuel consumption of new vehicles
#Get fleet of cars by fuel and usage
fleet_car <- fleet_vint_stock_f(mode="Private car")
fleet_car <- do.call(fleet_vkt_f,list(fleet=fleet_car))

#Loop for technology
for (year in as.character(2005:2017)) {
  #Extract matrix of vkt by technology and age for year
  matrix_vkt <- fleet_car$vint_vkt[[year]]["ICEV-G",,drop=FALSE]/sum(fleet_car$vint_vkt[[year]]["ICEV-G",,drop=FALSE])
  colnames(matrix_vkt) <- as.numeric(year)-as.numeric(colnames(matrix_vkt))
  #Calculate fuel use by technology and fuel, and fill vint_fuel_use
  #Get partial fuel consumption
  mat_fC_uf <- mat_fc[,as.character((as.numeric(year)-15):year),drop=FALSE]/100
  #Calculate the resulting fuel consumption to achieve top-down gasoline consumption
  new_fc <- (matrix_average_onroad_fc[,year]/100 - sum((mat_fC_uf * matrix_vkt[,colnames(mat_fC_uf)])[,colnames(matrix_vkt)[-1]]))/matrix_vkt[,year]*100
  
  # if (new_fc>11){
  #   mat_fc["ICEV-G_Gasoline",year] <- 11
  # } else if (new_fc<8) {
  #   mat_fc["ICEV-G_Gasoline",year] <- 8
  # } else {
  #   mat_fc["ICEV-G_Gasoline",year] <- new_fc
  # }
  # 
  mat_fc[1,year] <- new_fc
}

#DEPRECIATED
#we calculate the fuel consumption of new vehicles
#Assumption: Vehicles before are assumed at 1990 levels
mat_fc[,as.character(1985:1990)] <- subset(annual_veh_pop,Year==1990)$Car_on_road_fc
last_year_on_road <- 2004
#Assumption: Vehicles between 1990 and 2004 are equals to on-road fuel consumption
mat_fc[,as.character(1990:last_year_on_road)] <- subset(annual_veh_pop,Year%in%c(1990:last_year_on_road))$Car_on_road_fc
#Get vintage population
res <- read_def_outputs_f(function_tbc="transport_veh_pop_f",scen_tbc="def")
dt <- subset(res[["transport_vint_veh_pop_dt"]],Mode%in%c("Private car","Private hire car") & Technology=="ICEV-G")
max_age_tbc <- 20
for (year in (last_year_on_road+1):2017){
  #Matrix of vintage stock
  mat_vint_stock <- acast(data=subset(dt,Year==year), Mode ~ Age , value.var='Value',fun.aggregate=sum, margins=FALSE)
  on_road_fc <- subset(annual_veh_pop,Year==year)$Car_on_road_fc
  #
  new_fc <- (on_road_fc*(sum(mat_vint_stock["Private car",as.character(0:max_age_tbc)])+km_hire_factor*sum(mat_vint_stock["Private hire car",as.character(0:max_age_tbc)])) - sum((mat_vint_stock["Private car",as.character(1:max_age_tbc)]+km_hire_factor*mat_vint_stock["Private hire car",as.character(1:max_age_tbc)])*mat_fc[,as.character(year-1:max_age_tbc)]))/(mat_vint_stock["Private car",as.character(0)]+km_hire_factor*mat_vint_stock["Private hire car",as.character(0)])
  # if (new_fc < mat_fc[,as.character(year-1)]-1){
  #   mat_fc[,as.character(year)] <- mat_fc[,as.character(year-1)]-1
  # } else {
  mat_fc[,as.character(year)] <- new_fc
  #}
  #
}
#use Wei and Cheah for prior values with vehicle population by quota share (for 2003 and 2004 data, use 2005 share)
mat_fc[,"2003"] <- 0.61*8.3+0.39*12.5
mat_fc[,"2004"] <- 0.61*10+0.39*12.9
mat_fc[,"2007"] <- 0.59*9.6+0.41*12
mat_fc[,"2008"] <- 0.58*9.4+0.42*11.5
#Assume 2005 and 2006 to be linear interpolation between 2004 and 2007 values
#Assume linear regression from 2008 to 2015
mat_fc[,as.character(2005:2006)] <- sapply(2005:2006,function(x)(mat_fc[,"2007"]-mat_fc[,"2004"])/(2007-2004)*(x-2004)+mat_fc[,"2004"])
