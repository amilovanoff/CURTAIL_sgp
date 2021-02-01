#Script that create a dataset of fuel consumption from 1990 to 2018 for transport mode by technology
source("utils/data_processing_f.R")
source("utils/utils_f.R")
load_input_data_f()
first_yr <- 1985
last_yr <- 2019
dt_col <- c("Mode","Technology","Fuel",first_yr:last_yr)
output <- setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)

# I) Private car ----------------------------------------------------------

#A) Fuel consumption for Diesel vehicles.
#' Main assumption is we consider the mean of fuel economy for the models by make and body. Variations of fuel economy by model are not too important

#1) Aggregate population by year, make, body_type and diesel type
new_car_pop <- read.csv("inputs/data/new-registration-of-cars-by-make.csv",stringsAsFactors = FALSE)
new_car_pop <- subset(new_car_pop,number!=0)
colnames(new_car_pop)[colnames(new_car_pop)=="number"] <- "Value"
colnames(new_car_pop)[colnames(new_car_pop)=="year"] <- "Year"
new_car_pop_diesel <- subset(new_car_pop,fuel=="Diesel")

#2) Get fuel economy data and calculate the number of models, the mean,median,min, 1st quartile,3rd and max of the distribution of fuel economy
fe_data <- read.csv("inputs/model/fuel_economy_singapore.csv",stringsAsFactors = FALSE)
fe_data$Vehicle_make <- get_matching_names(fe_data$Make,matching_type="vehicle_make",original_source="LTA_fe",matched_source="LTA_population")

new_car_pop_diesel$Count_fe <- sapply(1:nrow(new_car_pop_diesel),function(x)nrow(subset(fe_data,Vehicle_make==new_car_pop_diesel[x,"make"] & Body==new_car_pop_diesel[x,"type"] & Fuel==new_car_pop_diesel[x,"fuel"])))
new_car_pop_diesel <- subset(new_car_pop_diesel,Count_fe!=0)
for (fun_name in c("mean", "min", "max")){
  fun <- switch (fun_name,
                 mean = mean,
                 min = min,
                 max = max
  )
  new_car_pop_diesel[,fun_name] <- sapply(1:nrow(new_car_pop_diesel),function(x)fun(subset(fe_data,Vehicle_make==new_car_pop_diesel[x,"make"] & Body==new_car_pop_diesel[x,"type"] & Fuel==new_car_pop_diesel[x,"fuel"])$fe_combined))
}

new_car_pop_diesel$wgt_fe_combined <- sapply(1:nrow(new_car_pop_diesel),function(x)new_car_pop_diesel[x,"mean"]*new_car_pop_diesel[x,"Value"]/sum(subset(new_car_pop_diesel,Year==new_car_pop_diesel[x,"Year"] & fuel==new_car_pop_diesel[x,"fuel"])$Value))
diesel_fc_dt <- aggregate(formula = wgt_fe_combined~Year+fuel,data = new_car_pop_diesel,FUN=sum)
#Fill historical values in matrix
mat_fc <- matrix(0,nrow=1,ncol=last_yr-first_yr+1,dimnames=list("Diesel",first_yr:last_yr))
for (i in 1:nrow(diesel_fc_dt)){
  mat_fc[,as.character(diesel_fc_dt[i,"Year"])] <- round(diesel_fc_dt[i,"wgt_fe_combined"],digits = 2)
}
#Assume constant prior to 2015
mat_fc[,as.character(first_yr:2014)] <- mat_fc[,"2015"]
#Fill output
tmp_dt <- setNames(data.frame(matrix(c("Car","ICEV-D","Diesel",mat_fc),ncol = length(dt_col), nrow = 1),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
output <- rbind(output,tmp_dt)

#B) Fuel consumption for conventional petrol vehicles
mat_fc <- matrix(0,nrow=1,ncol=last_yr-first_yr+1,dimnames=list("Gasoline",first_yr:last_yr))

#Calculate sales-weighted average from 2015 to 2019
#Assumption: Mean of model does not provide good results as many models for one combination of make/body
#We choose a representative vehicles for the major make/body combination (based on sources or personal expertise)
#1) Aggregate population by year, make, for petrol
new_car_pop <- read.csv("inputs/data/new-registration-of-cars-by-make.csv",stringsAsFactors = FALSE)
new_car_pop <- subset(new_car_pop,number!=0)
colnames(new_car_pop)[colnames(new_car_pop)=="year"] <- "Year"
colnames(new_car_pop)[colnames(new_car_pop)=="number"] <- "Value"
new_car_pop_petrol <- subset(new_car_pop,fuel=="Petrol")

#2) Match major make and body with models
fe_data <- read.csv("inputs/model/fuel_economy_singapore.csv",stringsAsFactors = FALSE)
fe_data$Vehicle_make <- get_matching_names(fe_data$Make,matching_type="vehicle_make",original_source="LTA_fe",matched_source="LTA_population")
fe_data <- subset(fe_data,Fuel=="Petrol" & Hybrid=="NO")
new_car_pop_petrol$Count_fe <- sapply(1:nrow(new_car_pop_petrol),function(x)nrow(subset(fe_data,Vehicle_make==new_car_pop_petrol[x,"make"] & Body==new_car_pop_petrol[x,"type"] & Fuel==new_car_pop_petrol[x,"fuel"])))
new_car_pop_petrol <- subset(new_car_pop_petrol,Count_fe!=0)
#Relative market share
new_car_pop_petrol$Rel_market_share <- sapply(1:nrow(new_car_pop_petrol),function(x)new_car_pop_petrol[x,"Value"]/sum(subset(new_car_pop_petrol,Year==new_car_pop_petrol[x,"Year"])$Value))
rownames(new_car_pop_petrol) <- 1:nrow(new_car_pop_petrol)
make_model_matching <- read.csv("inputs/user/make_body_model_matching.csv",stringsAsFactors = FALSE,check.names = FALSE)
make_model_matching <- gather(data=make_model_matching,key="Vehicle_size",value="Representative_model",-c(Fuel,Make),convert=TRUE)
make_model_matching <- subset(make_model_matching,Representative_model!="")
#If representative vehicle, use the fuel economy. Otherwise, take mean of possible models.
for (i in 1:nrow(new_car_pop_petrol)){
  if (nrow(subset(make_model_matching,Make==new_car_pop_petrol[i,"make"] & Vehicle_size==new_car_pop_petrol[i,"type"]))>0){
    new_car_pop_petrol[i,"fe_combined"] <- mean(subset(fe_data, Make==new_car_pop_petrol[i,"make"] & Body==new_car_pop_petrol[i,"type"] & Fuel==new_car_pop_petrol[i,"fuel"] & Model==subset(make_model_matching,Make==new_car_pop_petrol[i,"make"] & Vehicle_size==new_car_pop_petrol[i,"type"])$Representative_model)$fe_combined)
  } else {
    new_car_pop_petrol[i,"fe_combined"] <- mean(subset(fe_data,Make==new_car_pop_petrol[i,"make"] & Body==new_car_pop_petrol[i,"type"] & Fuel==new_car_pop_petrol[i,"fuel"])$fe_combined)
  }
}

new_car_pop_petrol$wgt_fe_combined <- sapply(1:nrow(new_car_pop_petrol),function(x)new_car_pop_petrol[x,"fe_combined"]*new_car_pop_petrol[x,"Value"]/sum(subset(new_car_pop_petrol,Year==new_car_pop_petrol[x,"Year"] & fuel==new_car_pop_petrol[x,"fuel"])$Value))
petrol_fc_dt <- aggregate(formula = wgt_fe_combined~Year+fuel,data = new_car_pop_petrol,FUN=sum)

#Fill sales weighted average in matrix
for (i in 1:nrow(petrol_fc_dt)){
  mat_fc[,as.character(petrol_fc_dt[i,"Year"])] <- round(petrol_fc_dt[i,"wgt_fe_combined"],digits=2)
}

#Assumption: For older model years, we use top-down approach from national gasoline use
iea_oil_dt <- read.csv("inputs/data/iea_oil_final_consumption_sgp.csv",stringsAsFactors = FALSE,check.names = FALSE)
#Convert ktoe in L
conv <- get_input_f("conversion_units")
fuel_conv <- get_input_f("greet_fuel_specs")
iea_oil_dt$Value <- iea_oil_dt$`Motor gasoline`*10^3*conv["J","1 toe"]/as.numeric(fuel_conv["Gasoline","LHV Conv"])
#Assumption: Gasoline is only consumer by private cars and Motorcyles
#Get car and motorcyle population
annual_veh_pop <- read.csv("inputs/data/annual_car_motorcyle_population_hist.csv",stringsAsFactors = FALSE,check.names = FALSE)
annual_veh_pop <- subset(annual_veh_pop,Year%in%unique(iea_oil_dt$Year))
annual_private_vehicle_mileage <- read.csv("inputs/data/annual_private_vehicle_mileage.csv",stringsAsFactors = FALSE,check.names = FALSE)
#Calculate fuel use by motorcycle
#Assumption: We use 2.43 L/100 km. Unit: L
annual_veh_pop$Moto_fuel_use <- sapply(1:nrow(annual_veh_pop),function(x)annual_veh_pop[x,"Motorcycle"]*subset(annual_private_vehicle_mileage,vehicle_type=="Motorcycles" & year==ifelse(annual_veh_pop[x,"Year"]%in%annual_private_vehicle_mileage$year,annual_veh_pop[x,"Year"],"2005"))$average_annual_mileage*2.43/100)
#Calculate private car consumption
annual_veh_pop$Car_fuel_use <- sapply(1:nrow(annual_veh_pop),function(x)subset(iea_oil_dt,Year==annual_veh_pop[x,"Year"])$Value-annual_veh_pop[x,"Moto_fuel_use"])
#Estimate on-road vehicle fuel consumption
#Assumption: Hire car have twice 
km_hire_factor <- 2
annual_veh_pop$Car_on_road_fc <- sapply(1:nrow(annual_veh_pop),function(x)annual_veh_pop[x,"Car_fuel_use"]/(annual_veh_pop[x,"Private_car"]/100*subset(annual_private_vehicle_mileage,vehicle_type=="Cars" & year==ifelse(annual_veh_pop[x,"Year"]%in%annual_private_vehicle_mileage$year,annual_veh_pop[x,"Year"],"2005"))$average_annual_mileage+annual_veh_pop[x,"Hire_car"]/100*km_hire_factor*subset(annual_private_vehicle_mileage,vehicle_type=="Cars" & year==ifelse(annual_veh_pop[x,"Year"]%in%annual_private_vehicle_mileage$year,annual_veh_pop[x,"Year"],"2005"))$average_annual_mileage))

#Write
write.csv(annual_veh_pop,"inputs/model/hist_fc_onroad_icevg.csv",row.names = FALSE)

#We apply a relative factor between 2000 to 2015
rel_factor <- subset(annual_veh_pop,Year==2000)$Car_on_road_fc/subset(annual_veh_pop,Year==2015)$Car_on_road_fc

#
mat_fc[,as.character(1985:2000)] <- round(rel_factor*mat_fc[,as.character(2015)],digits=2)

#Linear intepolation
mat_fc[,as.character(2001:2014)] <- sapply(2001:2014,function(x)round((mat_fc[,"2015"]-mat_fc[,"2000"])/(2015-2000)*(x-2000)+mat_fc[,"2000"],digits=2))

#Fill output
tmp_dt <- setNames(data.frame(matrix(c("Car","ICEV-G","Gasoline",mat_fc),ncol = length(dt_col), nrow = 1),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
output <- rbind(output,tmp_dt)

#C) Fuel consumption of HEV-G
#Assumption: 
fe_data <- read.csv("inputs/model/fuel_economy_singapore.csv",stringsAsFactors = FALSE)
mat_fc <- matrix(0,nrow=1,ncol=last_yr-first_yr+1,dimnames=list("Gasoline",first_yr:last_yr))
#Fill 2019 value with Prius Hybrid vehicle.
mat_fc[,"2019"] <- subset(fe_data,Make=="TOYOTA" & Model=="PRIUS C HYBRID 1.5")$fe_combined
mat_fc[,"2009"] <- mat_fc[,"2019"]*1.13
mat_fc[,as.character(first_yr:2008)] <- mat_fc[,"2009"]
#Assume linear regression from 2009 to 2019
mat_fc[,as.character(2010:2018)] <- sapply(2010:2018,function(x)round((mat_fc[,"2019"]-mat_fc[,"2009"])/(2019-2009)*(x-2009)+mat_fc[,"2009"],digits=2))
#Fill output
tmp_dt <- setNames(data.frame(matrix(c("Car","HEV-G","Gasoline",mat_fc),ncol = length(dt_col), nrow = 1),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
output <- rbind(output,tmp_dt)

#D) Fuel consumption of BEV (Wh/km for BEV). 
fe_data <- read.csv("inputs/model/fuel_economy_singapore.csv",stringsAsFactors = FALSE)
fe_data_bev <- subset(fe_data,Fuel=="Electric")
fc_bev <- subset(fe_data_bev, Make=="NISSAN" & Model=="LEAF EV")$fe_combined*100/1000
mat_fc <- matrix(fc_bev,nrow=1,ncol=last_yr-first_yr+1,dimnames=list("Electricity",first_yr:last_yr))
#Fill output
tmp_dt <- setNames(data.frame(matrix(c("Car","BEV","Electricity",mat_fc),ncol = length(dt_col), nrow = 1),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
output <- rbind(output,tmp_dt)

#E) Fuel consumption of PHEV-G
mat_fc <- matrix(0,nrow=2,ncol=last_yr-first_yr+1,dimnames=list(c("Gasoline","Electricity"),first_yr:last_yr))
mat_fc["Gasoline",] <- 6
mat_fc["Electricity",] <- 20.8
#Fill output
tmp_dt <- setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 2),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
tmp_dt$Mode <- "Car"
tmp_dt$Technology <- "PHEV"
tmp_dt$Fuel <- rownames(mat_fc)
tmp_dt[,as.character(colnames(mat_fc))] <- mat_fc
output <- rbind(output,tmp_dt)

#F) Fuel consumption of HCNG
#Gasoline FC in L CNG / 100 km
mat_fc <- matrix(0,nrow=1,ncol=last_yr-first_yr+1,dimnames=list("CNG",first_yr:last_yr))
#Assumption: Toyota Camry. Value obtained on fueleconomy.org
mat_fc["CNG",] <- 14.12
#Fill output
tmp_dt <- setNames(data.frame(matrix(c("Car","CNG","CNG",mat_fc),ncol = length(dt_col), nrow = 1),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
output <- rbind(output,tmp_dt)

# II) Bus ------------------------------------------------------------------
#A) ICEB-D

mat_fc <- matrix(0,nrow=1,ncol=last_yr-first_yr+1,dimnames=list("Diesel",first_yr:last_yr))
#Assumption: from Zhang et al. (2014) in L/100km
mat_fc[1,] <- 32.6
tmp_dt <- setNames(data.frame(matrix(c("Bus","ICEB-D","Diesel",mat_fc),ncol = length(dt_col), nrow = 1),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
output <- rbind(output,tmp_dt)

#B) HEB-D
mat_fc <- matrix(0,nrow=1,ncol=last_yr-first_yr+1,dimnames=list("Diesel",first_yr:last_yr))
#Assumption: from Zhang et al. (2014) in L/100km
mat_fc[1,] <- 24.3
tmp_dt <- setNames(data.frame(matrix(c("Bus","HEB-D","Diesel",mat_fc),ncol = length(dt_col), nrow = 1),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
output <- rbind(output,tmp_dt)

#C) CNGB
mat_fc <- matrix(0,nrow=1,ncol=last_yr-first_yr+1,dimnames=list("CNG",first_yr:last_yr))
#Assumption: from Zhang et al. (2014) in L/100km
fuel_conv <- read.csv("inputs/user/fuel_conversion.csv",stringsAsFactors = FALSE)
mat_fc[1,] <- round(16.1*100/(subset(fuel_conv,Data=="Conversion factor" & Fuel=="CNG")$Value/10^6),digits=2)
tmp_dt <- setNames(data.frame(matrix(c("Bus","CNGB","CNG",mat_fc),ncol = length(dt_col), nrow = 1),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
output <- rbind(output,tmp_dt)

#D) EB
mat_fc <- matrix(0,nrow=1,ncol=last_yr-first_yr+1,dimnames=list("Electricity",first_yr:last_yr))
#Assumption: from literature review Laura
mat_fc[1,] <- 130
tmp_dt <- setNames(data.frame(matrix(c("Bus","EB","Electricity",mat_fc),ncol = length(dt_col), nrow = 1),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
output <- rbind(output,tmp_dt)

#  III) Motorcycle ----------------------------------------------------------
#A) ICEM-G
mat_fc <- matrix(0,nrow=1,ncol=last_yr-first_yr+1,dimnames=list("Gasoline",first_yr:last_yr))
#Assumption: from (Koossalapeerom et al., 2019) in L/100km
mat_fc[1,] <- 2.43
tmp_dt <- setNames(data.frame(matrix(c("Motorcycle","ICEM-G","Gasoline",mat_fc),ncol = length(dt_col), nrow = 1),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
output <- rbind(output,tmp_dt)

#II) EM
mat_fc <- matrix(0,nrow=1,ncol=last_yr-first_yr+1,dimnames=list("Electricity",first_yr:last_yr))
#Assumption: from (Koossalapeerom et al., 2019) in kWh/100km
mat_fc[1,] <- 2.8
tmp_dt <- setNames(data.frame(matrix(c("Motorcycle","EM","Electricity",mat_fc),ncol = length(dt_col), nrow = 1),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
output <- rbind(output,tmp_dt)

# IV) MRT -----------------------------------------------------------------
mat_fc <- matrix(0,nrow=1,ncol=last_yr-first_yr+1,dimnames=list("Electricity",first_yr:last_yr))
#Assumption: TEDB
mat_fc[1,] <- 497
tmp_dt <- setNames(data.frame(matrix(c("MRT","MRT","Electricity",mat_fc),ncol = length(dt_col), nrow = 1),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
output <- rbind(output,tmp_dt)

# V) LRT -----------------------------------------------------------------

mat_fc <- matrix(0,nrow=1,ncol=last_yr-first_yr+1,dimnames=list("Electricity",first_yr:last_yr))
#Assumption: TEDB
mat_fc[1,] <- 497
tmp_dt <- setNames(data.frame(matrix(c("LRT","LRT","Electricity",mat_fc),ncol = length(dt_col), nrow = 1),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
output <- rbind(output,tmp_dt)

#Save output
write.csv(output,"inputs/model/hist_fc.csv",row.names = FALSE)
