#Script that create a dataset of fuel consumption from 1990 to 2018 for transport mode by technology

# I) Private car ----------------------------------------------------------

hist_veh_fuel_cons_l <- list()
first_yr <- 1990
last_yr <- 2019

#A) Fuel consumption for Diesel vehicles.
#' Main assumption is we consider the mean of fuel economy for the models by make and body. Variations of fuel economy by model are not too important

#1) Aggregate population by year, make, body_type and diesel type
new_car_pop <- read.csv("inputs/data/new-registration-of-cars-by-make.csv",stringsAsFactors = FALSE)
new_car_pop <- subset(new_car_pop,number!=0)
new_car_pop$Year <- substring(new_car_pop$month,0,as.numeric(regexpr(pattern="-",new_car_pop$month))-1)
colnames(new_car_pop)[colnames(new_car_pop)=="number"] <- "Value"
new_car_pop_diesel <- aggregate(formula=Value~Year+make+vehicle_type+fuel_type,data=subset(new_car_pop,fuel_type=="Diesel"),FUN=sum)

#2) Get fuel economy data and calculate the number of models, the mean,median,min, 1st quartile,3rd and max of the distribution of fuel economy
fe_data <- read.csv("inputs/model/fuel_economy_singapore.csv",stringsAsFactors = FALSE)
fe_data$Vehicle_make <- get_matching_names(fe_data$Make,matching_type="vehicle_make",original_source="LTA_fe",matched_source="LTA_population")

new_car_pop_diesel$Count_fe <- sapply(1:nrow(new_car_pop_diesel),function(x)nrow(subset(fe_data,Vehicle_make==new_car_pop_diesel[x,"make"] & Body==new_car_pop_diesel[x,"vehicle_type"] & Fuel==new_car_pop_diesel[x,"fuel_type"])))
new_car_pop_diesel <- subset(new_car_pop_diesel,Count_fe!=0)
for (fun_name in c("mean", "min", "max")){
  fun <- switch (fun_name,
                 mean = mean,
                 min = min,
                 max = max
  )
  new_car_pop_diesel[,fun_name] <- sapply(1:nrow(new_car_pop_diesel),function(x)fun(subset(fe_data,Vehicle_make==new_car_pop_diesel[x,"make"] & Body==new_car_pop_diesel[x,"vehicle_type"] & Fuel==new_car_pop_diesel[x,"fuel_type"])$fe_combined))
}

new_car_pop_diesel$wgt_fe_combined <- sapply(1:nrow(new_car_pop_diesel),function(x)new_car_pop_diesel[x,"mean"]*new_car_pop_diesel[x,"Value"]/sum(subset(new_car_pop_diesel,Year==new_car_pop_diesel[x,"Year"] & fuel_type==new_car_pop_diesel[x,"fuel_type"])$Value))
diesel_fc_dt <- aggregate(formula = wgt_fe_combined~Year+fuel_type,data = new_car_pop_diesel,FUN=sum)
#Fill historical values in matrix
mat_fc <- matrix(0,nrow=1,ncol=last_yr-first_yr+1,dimnames=list("Diesel",first_yr:last_yr))
for (i in 1:nrow(diesel_fc_dt)){
  mat_fc[,as.character(diesel_fc_dt[i,"Year"])] <- diesel_fc_dt[i,"wgt_fe_combined"]
}
#Assume constant prior to 2016
mat_fc[,as.character(first_yr:2015)] <- mat_fc[,"2016"]
#Fill output
hist_veh_fuel_cons_l[["ICEV-D"]] <- mat_fc

#B) Fuel consumption for conventional petrol vehicles
#Assumption: Mean of model does not provide good results as many models for one combination of make/body
#We choose a representative vehicles for the major make/body combination (based on sources or personal expertise)
#1) Aggregate population by year, make, for petrol
new_car_pop <- read.csv("inputs/data/new-registration-of-cars-by-make.csv",stringsAsFactors = FALSE)
new_car_pop <- subset(new_car_pop,number!=0)
new_car_pop$Year <- substring(new_car_pop$month,0,as.numeric(regexpr(pattern="-",new_car_pop$month))-1)
colnames(new_car_pop)[colnames(new_car_pop)=="number"] <- "Value"
new_car_pop_petrol <- aggregate(formula=Value~Year+make+fuel_type+vehicle_type,data=subset(new_car_pop,fuel_type=="Petrol"),FUN=sum)

#2) Match major make and body with models
fe_data <- read.csv("inputs/model/fuel_economy_singapore.csv",stringsAsFactors = FALSE)
fe_data$Vehicle_make <- get_matching_names(fe_data$Make,matching_type="vehicle_make",original_source="LTA_fe",matched_source="LTA_population")
fe_data <- subset(fe_data,Fuel=="Petrol" & Hybrid=="NO")
new_car_pop_petrol$Count_fe <- sapply(1:nrow(new_car_pop_petrol),function(x)nrow(subset(fe_data,Vehicle_make==new_car_pop_petrol[x,"make"] & Body==new_car_pop_petrol[x,"vehicle_type"] & Fuel==new_car_pop_petrol[x,"fuel_type"])))
new_car_pop_petrol <- subset(new_car_pop_petrol,Count_fe!=0)
rownames(new_car_pop_petrol) <- 1:nrow(new_car_pop_petrol)
make_model_matching <- read.csv("inputs/user/make_body_model_matching.csv",stringsAsFactors = FALSE)
make_model_matching <- subset(make_model_matching,Representative_model!="")
#If representative vehicle, use the fuel economy. Otherwise, take mean of possible models.
for (i in 1:nrow(new_car_pop_petrol)){
  if (nrow(subset(make_model_matching,Make==new_car_pop_petrol[i,"make"] & Vehicle_size==new_car_pop_petrol[i,"vehicle_type"]))>0){
    new_car_pop_petrol[i,"fe_combined"] <- mean(subset(fe_data, Make==new_car_pop_petrol[i,"make"] & Body==new_car_pop_petrol[i,"vehicle_type"] & Fuel==new_car_pop_petrol[i,"fuel_type"] & Model==subset(make_model_matching,Make==new_car_pop_petrol[i,"make"] & Vehicle_size==new_car_pop_petrol[i,"vehicle_type"])$Representative_model)$fe_combined)
  } else {
    new_car_pop_petrol[i,"fe_combined"] <- mean(subset(fe_data,Make==new_car_pop_petrol[i,"make"] & Body==new_car_pop_petrol[i,"vehicle_type"] & Fuel==new_car_pop_petrol[i,"fuel_type"])$fe_combined)
  }
}

new_car_pop_petrol$wgt_fe_combined <- sapply(1:nrow(new_car_pop_petrol),function(x)new_car_pop_petrol[x,"fe_combined"]*new_car_pop_petrol[x,"Value"]/sum(subset(new_car_pop_petrol,Year==new_car_pop_petrol[x,"Year"] & fuel_type==new_car_pop_petrol[x,"fuel_type"])$Value))
petrol_fc_dt <- aggregate(formula = wgt_fe_combined~Year+fuel_type,data = new_car_pop_petrol,FUN=sum)

#Fill historical values in matrix
mat_fc <- matrix(0,nrow=1,ncol=last_yr-first_yr+1,dimnames=list("Gasoline",first_yr:last_yr))
for (i in 1:nrow(petrol_fc_dt)){
  mat_fc[,as.character(petrol_fc_dt[i,"Year"])] <- petrol_fc_dt[i,"wgt_fe_combined"]
}
#use Wei and Cheah for prior values with vehicle population by quota share (for 2003 and 2004 data, use 2005 share)
mat_fc[,"2003"] <- 0.61*8.3+0.39*12.5
mat_fc[,"2004"] <- 0.61*10+0.39*12.9
mat_fc[,"2007"] <- 0.59*9.6+0.41*12
mat_fc[,"2008"] <- 0.58*9.4+0.42*11.5
#Assume constant prior to 2003
mat_fc[,as.character(first_yr:2003)] <- mat_fc[,"2003"]
#Assume 2004 level in 2005
mat_fc[,c("2005","2006")] <- mat_fc[,"2004"]
#Assume linear regression from 2008 to 2015
mat_fc[,as.character(2009:2015)] <- sapply(2009:2015,function(x)(mat_fc[,"2016"]-mat_fc[,"2008"])/(2016-2008)*(x-2008)+mat_fc[,"2008"])
#Fill output
hist_veh_fuel_cons_l[["ICEV-G"]] <- mat_fc


#C) Fuel consumption of HEV-G
#Assumption: 
fe_data <- read.csv("inputs/model/fuel_economy_singapore.csv",stringsAsFactors = FALSE)
mat_fc <- matrix(0,nrow=1,ncol=last_yr-first_yr+1,dimnames=list("Gasoline",first_yr:last_yr))
#Fill 2019 value with Prius Hybrid vehicle
mat_fc[,"2019"] <- subset(fe_data,Make=="TOYOTA" & Model=="PRIUS C HYBRID 1.5")$fe_combined
mat_fc[,"2009"] <- mat_fc[,"2019"]*1.13
mat_fc[,as.character(first_yr:2008)] <- mat_fc[,"2009"]
#Assume linear regression from 2009 to 2019
mat_fc[,as.character(2010:2018)] <- sapply(2010:2018,function(x)(mat_fc[,"2019"]-mat_fc[,"2009"])/(2019-2009)*(x-2009)+mat_fc[,"2009"])
#Fill output
hist_veh_fuel_cons_l[["HEV-G"]] <- mat_fc

#D) Fuel consumption of BEV (Wh/km for BEV)
fe_data <- read.csv("inputs/model/fuel_economy_singapore.csv",stringsAsFactors = FALSE)
fe_data_bev <- subset(fe_data,Fuel=="Electric")
fc_bev <- subset(fe_data_bev, Make=="NISSAN" & Model=="LEAF EV")$fe_combined*100/1000
mat_fc <- matrix(fc_bev,nrow=1,ncol=last_yr-first_yr+1,dimnames=list("Electricity",first_yr:last_yr))
#Fill output
hist_veh_fuel_cons_l[["BEV"]] <- mat_fc

#E) Fuel consumption of PHEV-G
fe_data <- read.csv("inputs/model/fuel_economy_singapore.csv",stringsAsFactors = FALSE)
#Assumption: HEV for gasoline and EV for electricity
fc_hev <- subset(fe_data,Make=="TOYOTA" & Model=="PRIUS C HYBRID 1.5")$fe_combined
fc_bev <- subset(fe_data_bev, Make=="NISSAN" & Model=="LEAF EV")$fe_combined*100/1000
mat_fc <- matrix(0,nrow=2,ncol=last_yr-first_yr+1,dimnames=list(c("Gasoline","Electricity"),first_yr:last_yr))
mat_fc["Gasoline",] <- fc_hev
mat_fc["Electricity",] <- fc_bev
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

#G) Fuel consumption of HEV-D
fe_data <- read.csv("inputs/model/fuel_economy_singapore.csv",stringsAsFactors = FALSE)
fe_data_diesel <- subset(fe_data,Fuel=="Diesel")
#Assumpion: Consider minimum fuel consumption CLIO 4 1.5 DCI 6AT
mat_fc <- matrix(0,nrow=1,ncol=last_yr-first_yr+1,dimnames=list("Diesel",first_yr:last_yr))
#Assumpion: Consider minimum fuel consumption CLIO 4 1.5 DCI 6AT
mat_fc["Diesel",] <- subset(fe_data_diesel,Model=="CLIO 4 1.5 DCI 6AT")$fe_combined
#Fill output
hist_veh_fuel_cons_l[["HEV-D"]] <- mat_fc

#Save output
saveRDS(hist_veh_fuel_cons_l,file="inputs/model/car_hist_fc.RDS")


# II) Bus ------------------------------------------------------------------

hist_veh_fuel_cons_l <- list()
first_yr <- 1985
last_yr <- 2019

#A) ICEB-D
#Passenger capacity of buses
bus_pop_capacity <- read.csv("inputs/data/annual-bus-population-by-passenger-capacity.csv",stringsAsFactors = FALSE)
#Bus population by capacity is for all buses (so no only public buses)
mat_bus_pop_cap <- acast(bus_pop_capacity,capacity~year,value.var='number',fun.aggregate=sum, margins=FALSE)

#Make of buses
bus_pop_make <- read.csv("inputs/data/new-registration-of-goods-vehicles-buses-by-make.csv",stringsAsFactors = FALSE)
#Bus population by capacity is for all buses (so no only public buses)
mat_bus_pop_make <- acast(subset(bus_pop_make,vehicle_type=="Buses"),make + fuel_type~month,value.var='number',fun.aggregate=sum, margins=FALSE)

rowSums(mat_bus_pop_make)[order(rowSums(mat_bus_pop_make))]

mat_fc <- matrix(0,nrow=1,ncol=last_yr-first_yr+1,dimnames=list("Diesel",first_yr:last_yr))
#Assumption: from Zhang et al. (2014) in L/100km
mat_fc[1,] <- 32.6*1.228
hist_veh_fuel_cons_l[["ICEB-D"]] <- mat_fc

#B) ICEB-G
mat_fc <- matrix(0,nrow=1,ncol=last_yr-first_yr+1,dimnames=list("Gasoline",first_yr:last_yr))
#Assumption: We convert diesel FC in gasoline only based on energy content of fuel
fuel_conv <- read.csv("inputs/user/fuel_conversion.csv",stringsAsFactors = FALSE)
mat_fc[1,] <- 32.6*1.228*subset(fuel_conv,Data=="Conversion factor" & Fuel=="Diesel")$Value/subset(fuel_conv,Data=="Conversion factor" & Fuel=="Gasoline")$Value
hist_veh_fuel_cons_l[["ICEB-G"]] <- mat_fc

#C) HEB-D
mat_fc <- matrix(0,nrow=1,ncol=last_yr-first_yr+1,dimnames=list("Diesel",first_yr:last_yr))
#Assumption: from Zhang et al. (2014) in L/100km
mat_fc[1,] <- 24.3*1.482
hist_veh_fuel_cons_l[["HEB-D"]] <- mat_fc

#D) HEB-G
mat_fc <- matrix(0,nrow=1,ncol=last_yr-first_yr+1,dimnames=list("Gasoline",first_yr:last_yr))
#Assumption: We convert diesel FC in gasoline only based on energy content of fuel
fuel_conv <- read.csv("inputs/user/fuel_conversion.csv",stringsAsFactors = FALSE)
mat_fc[1,] <- 24.3*1.482*subset(fuel_conv,Data=="Conversion factor" & Fuel=="Diesel")$Value/subset(fuel_conv,Data=="Conversion factor" & Fuel=="Gasoline")$Value
hist_veh_fuel_cons_l[["HEB-G"]] <- mat_fc

#E) CNGB
mat_fc <- matrix(0,nrow=1,ncol=last_yr-first_yr+1,dimnames=list("CNG",first_yr:last_yr))
#Assumption: from Zhang et al. (2014) in L/100km
fuel_conv <- read.csv("inputs/user/fuel_conversion.csv",stringsAsFactors = FALSE)
mat_fc[1,] <- 16.1*1.09*100/(subset(fuel_conv,Data=="Conversion factor" & Fuel=="CNG")$Value/10^6)
hist_veh_fuel_cons_l[["CNGB"]] <- mat_fc

#F) EB
mat_fc <- matrix(0,nrow=1,ncol=last_yr-first_yr+1,dimnames=list("Electricity",first_yr:last_yr))
#Assumption: from literature review Laura
mat_fc[1,] <- 172
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
#Assumption: PErsonal calculations
mat_fc[1,] <- 7220
hist_veh_fuel_cons_l[["MRT"]] <- mat_fc

#Save output
saveRDS(hist_veh_fuel_cons_l,file="inputs/model/mrt_hist_fc.RDS")

# V) LRT -----------------------------------------------------------------

hist_veh_fuel_cons_l <- list()
first_yr <- 2005
last_yr <- 2019

mat_fc <- matrix(0,nrow=1,ncol=last_yr-first_yr+1,dimnames=list("Electricity",first_yr:last_yr))
#Assumption: PErsonal calculations
mat_fc[1,] <- 7220
hist_veh_fuel_cons_l[["LRT"]] <- mat_fc

#Save output
saveRDS(hist_veh_fuel_cons_l,file="inputs/model/lrt_hist_fc.RDS")
