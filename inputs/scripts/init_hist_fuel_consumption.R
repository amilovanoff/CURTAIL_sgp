#Script that create a dataset of fuel consumption from 1990 to 2018 for transport mode by technology

# I) Private car ----------------------------------------------------------

hist_veh_fuel_cons_l <- list()
first_yr <- 1985
last_yr <- 2019

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
write.csv(diesel_fc_dt,"inputs/model/hist_fc_icevd.csv",row.names = FALSE)
#Fill historical values in matrix
mat_fc <- matrix(0,nrow=1,ncol=last_yr-first_yr+1,dimnames=list("Diesel",first_yr:last_yr))

for (i in 1:nrow(diesel_fc_dt)){
  mat_fc[,as.character(diesel_fc_dt[i,"Year"])] <- diesel_fc_dt[i,"wgt_fe_combined"]
}
#Assume constant prior to 2015
mat_fc[,as.character(first_yr:2014)] <- mat_fc[,"2015"]
#Fill output
hist_veh_fuel_cons_l[["ICEV-D"]] <- mat_fc

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
write.csv(petrol_fc_dt,"inputs/model/hist_fc_icevg.csv",row.names = FALSE)

#Format make model matching
for (year in 2015:2019){
  make_model_matching[,as.character(year)] <- sapply(1:nrow(make_model_matching),function(x)subset(new_car_pop_petrol,Year==year & make==make_model_matching[x,"Make"] & type==make_model_matching[x,"Vehicle_size"])$Value/sum(subset(new_car_pop_petrol,Year==year)$Value))
}
make_model_matching$FC <- sapply(1:nrow(make_model_matching),function(x)subset(fe_data,Make==make_model_matching[x,"Make"] & Body==make_model_matching[x,"Vehicle_size"] & Model==make_model_matching[x,"Representative_model"])$fe_combined[1])
write.csv(make_model_matching,"inputs/model/make_body_model_market_share_fc.csv",row.names = FALSE)

#Fill sales weighted average in matrix
for (i in 1:nrow(petrol_fc_dt)){
  mat_fc[,as.character(petrol_fc_dt[i,"Year"])] <- petrol_fc_dt[i,"wgt_fe_combined"]
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
mat_fc[,as.character(1985:2000)] <- rel_factor*mat_fc[,as.character(2015)]

#Linear intepolation
mat_fc[,as.character(2001:2014)] <- sapply(2001:2014,function(x)(mat_fc[,"2015"]-mat_fc[,"2000"])/(2015-2000)*(x-2000)+mat_fc[,"2000"])

#Fill output
hist_veh_fuel_cons_l[["ICEV-G"]] <- mat_fc


#C) Fuel consumption of HEV-G
#Assumption: 
fe_data <- read.csv("inputs/model/fuel_economy_singapore.csv",stringsAsFactors = FALSE)
mat_fc <- matrix(0,nrow=1,ncol=last_yr-first_yr+1,dimnames=list("Gasoline",first_yr:last_yr))
#Fill 2019 value with Prius Hybrid vehicle.
mat_fc[,"2019"] <- subset(fe_data,Make=="TOYOTA" & Model=="PRIUS C HYBRID 1.5")$fe_combined
mat_fc[,"2009"] <- mat_fc[,"2019"]*1.13
mat_fc[,as.character(first_yr:2008)] <- mat_fc[,"2009"]
#Assume linear regression from 2009 to 2019
mat_fc[,as.character(2010:2018)] <- sapply(2010:2018,function(x)(mat_fc[,"2019"]-mat_fc[,"2009"])/(2019-2009)*(x-2009)+mat_fc[,"2009"])
#Fill output
hist_veh_fuel_cons_l[["HEV-G"]] <- mat_fc

#D) Fuel consumption of BEV (Wh/km for BEV). 
fe_data <- read.csv("inputs/model/fuel_economy_singapore.csv",stringsAsFactors = FALSE)
fe_data_bev <- subset(fe_data,Fuel=="Electric")
fc_bev <- subset(fe_data_bev, Make=="NISSAN" & Model=="LEAF EV")$fe_combined*100/1000
mat_fc <- matrix(fc_bev,nrow=1,ncol=last_yr-first_yr+1,dimnames=list("Electricity",first_yr:last_yr))
#Fill output
hist_veh_fuel_cons_l[["BEV"]] <- mat_fc

#E) Fuel consumption of PHEV-G
mat_fc <- matrix(0,nrow=2,ncol=last_yr-first_yr+1,dimnames=list(c("Gasoline","Electricity"),first_yr:last_yr))
mat_fc["Gasoline",] <- 6
mat_fc["Electricity",] <- 20.8
#Fill output
hist_veh_fuel_cons_l[["PHEV"]] <- mat_fc

#F) Fuel consumption of HCNG
fe_us_data <- read.csv("inputs/data/fuel_economy_dataset_us.csv",stringsAsFactors = FALSE)
conv <- read.csv("inputs/user/conversion_units.csv",stringsAsFactors = FALSE,row.names = 1,check.names = FALSE)
fuel_conv <-  read.csv("inputs/user/fuel_conversion.csv",stringsAsFactors = FALSE,check.names = FALSE)
fe_data_cng <- subset(fe_us_data,fuelType=="CNG")
#Gasoline FC in L CNG / 100 km
fe_data_cng$comb08_fc <- 1/fe_data_cng$comb08*conv["L","1 gal"]*conv["mile","1 km"]*100*subset(fuel_conv,Data=="Conversion factor" & Fuel=="Gasoline")$Value/subset(fuel_conv,Data=="Conversion factor" & Fuel=="CNG")$Value
mat_fc <- matrix(0,nrow=1,ncol=last_yr-first_yr+1,dimnames=list("CNG",first_yr:last_yr))
#Assumption: Toyota Camry (no justification)

mat_fc["CNG",] <- subset(fe_data_cng,model=="Camry CNG" & year==2000)$comb08_fc
#Fill output
hist_veh_fuel_cons_l[["CNG"]] <- mat_fc

#Save output
saveRDS(hist_veh_fuel_cons_l,file="inputs/model/car_hist_fc.RDS")


# II) Bus ------------------------------------------------------------------
hist_veh_fuel_cons_l <- list()
first_yr <- 1985
last_yr <- 2019

#A) ICEB-D

mat_fc <- matrix(0,nrow=1,ncol=last_yr-first_yr+1,dimnames=list("Diesel",first_yr:last_yr))
#Assumption: from Zhang et al. (2014) in L/100km
mat_fc[1,] <- 32.6
hist_veh_fuel_cons_l[["ICEB-D"]] <- mat_fc

#C) HEB-D
mat_fc <- matrix(0,nrow=1,ncol=last_yr-first_yr+1,dimnames=list("Diesel",first_yr:last_yr))
#Assumption: from Zhang et al. (2014) in L/100km
mat_fc[1,] <- 24.3
hist_veh_fuel_cons_l[["HEB-D"]] <- mat_fc

#E) CNGB
mat_fc <- matrix(0,nrow=1,ncol=last_yr-first_yr+1,dimnames=list("CNG",first_yr:last_yr))
#Assumption: from Zhang et al. (2014) in L/100km
fuel_conv <- read.csv("inputs/user/fuel_conversion.csv",stringsAsFactors = FALSE)
mat_fc[1,] <- 16.1*100/(subset(fuel_conv,Data=="Conversion factor" & Fuel=="CNG")$Value/10^6)
hist_veh_fuel_cons_l[["CNGB"]] <- mat_fc

#F) EB
mat_fc <- matrix(0,nrow=1,ncol=last_yr-first_yr+1,dimnames=list("Electricity",first_yr:last_yr))
#Assumption: from literature review Laura
mat_fc[1,] <- 130
hist_veh_fuel_cons_l[["EB"]] <- mat_fc

#Save output
saveRDS(hist_veh_fuel_cons_l,file="inputs/model/bus_hist_fc.RDS")


#  III) Motorcycle ----------------------------------------------------------
hist_veh_fuel_cons_l <- list()
first_yr <- 1985
last_yr <- 2019
#A) ICEM-G
mat_fc <- matrix(0,nrow=1,ncol=last_yr-first_yr+1,dimnames=list("Gasoline",first_yr:last_yr))
#Assumption: from (Koossalapeerom et al., 2019) in L/100km
mat_fc[1,] <- 2.43
hist_veh_fuel_cons_l[["ICEM-G"]] <- mat_fc

#II) EM
mat_fc <- matrix(0,nrow=1,ncol=last_yr-first_yr+1,dimnames=list("Electricity",first_yr:last_yr))
#Assumption: from (Koossalapeerom et al., 2019) in kWh/100km
mat_fc[1,] <- 2.8
hist_veh_fuel_cons_l[["EM"]] <- mat_fc

#Save output
saveRDS(hist_veh_fuel_cons_l,file="inputs/model/moto_hist_fc.RDS")


# IV) MRT -----------------------------------------------------------------

hist_veh_fuel_cons_l <- list()
first_yr <- 2005
last_yr <- 2019

mat_fc <- matrix(0,nrow=1,ncol=last_yr-first_yr+1,dimnames=list("Electricity",first_yr:last_yr))
#Assumption: TEDB
mat_fc[1,] <- 497
hist_veh_fuel_cons_l[["MRT"]] <- mat_fc

#Save output
saveRDS(hist_veh_fuel_cons_l,file="inputs/model/mrt_hist_fc.RDS")

# V) LRT -----------------------------------------------------------------

hist_veh_fuel_cons_l <- list()
first_yr <- 2005
last_yr <- 2019

mat_fc <- matrix(0,nrow=1,ncol=last_yr-first_yr+1,dimnames=list("Electricity",first_yr:last_yr))
#Assumption: TEDB
mat_fc[1,] <- 497
hist_veh_fuel_cons_l[["LRT"]] <- mat_fc

#Save output
saveRDS(hist_veh_fuel_cons_l,file="inputs/model/lrt_hist_fc.RDS")
