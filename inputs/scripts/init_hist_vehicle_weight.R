#Script that create a dataset of vehicle weight 1985 to 2019 for transport mode by technology

# I) Private car ----------------------------------------------------------

hist_veh_weight_l <- list()
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
#Aggregate by body type
new_car_pop_diesel <- aggregate(formula=Value~type+Year+fuel,data=new_car_pop_diesel,FUN=sum)

#2) Get weight data and calculate the number of models, the mean,median,min, 1st quartile,3rd and max of the distribution of fuel economy
weight_data <- read.csv("inputs/data/vehicle_weight_model.csv",stringsAsFactors = FALSE)
new_car_pop_diesel$Weight <- sapply(1:nrow(new_car_pop_diesel),function(x)subset(weight_data,Body==new_car_pop_diesel[x,"type"])$Weight)
new_car_pop_diesel$Weight_combined <- sapply(1:nrow(new_car_pop_diesel),function(x)new_car_pop_diesel[x,"Weight"]*new_car_pop_diesel[x,"Value"]/sum(subset(new_car_pop_diesel,Year==new_car_pop_diesel[x,"Year"] & fuel==new_car_pop_diesel[x,"fuel"])$Value))
diesel_weight_dt <- aggregate(formula = Weight_combined~Year+fuel,data = new_car_pop_diesel,FUN=sum)
#Fill historical values in matrix
mat_weight <- matrix(0,nrow=1,ncol=last_yr-first_yr+1,dimnames=list("Diesel",first_yr:last_yr))

for (i in 1:nrow(diesel_weight_dt)){
  mat_weight[,as.character(diesel_weight_dt[i,"Year"])] <- diesel_weight_dt[i,"Weight_combined"]
}
#Assume constant prior to 2015
mat_weight[,as.character(first_yr:2014)] <- mat_weight[,"2015"]
#Fill output
hist_veh_weight_l[["ICEV-D"]] <- mat_weight

#B) Fuel consumption for conventional petrol vehicles

#Calculate sales-weighted average from 2015 to 2019
#Assumption: Mean of model does not provide good results as many models for one combination of make/body
#We choose a representative vehicles for the major make/body combination (based on sources or personal expertise)
#1) Aggregate population by year, make, for petrol
new_car_pop <- read.csv("inputs/data/new-registration-of-cars-by-make.csv",stringsAsFactors = FALSE)
new_car_pop <- subset(new_car_pop,number!=0)
colnames(new_car_pop)[colnames(new_car_pop)=="year"] <- "Year"
colnames(new_car_pop)[colnames(new_car_pop)=="number"] <- "Value"
new_car_pop_petrol <- subset(new_car_pop,fuel=="Petrol")
#Aggregate by body type
new_car_pop_petrol <- aggregate(formula=Value~type+Year+fuel,data=new_car_pop_petrol,FUN=sum)

#2) Get weight data and calculate the number of models, the mean,median,min, 1st quartile,3rd and max of the distribution of fuel economy
weight_data <- read.csv("inputs/data/vehicle_weight_model.csv",stringsAsFactors = FALSE)
new_car_pop_petrol$Weight <- sapply(1:nrow(new_car_pop_petrol),function(x)subset(weight_data,Body==new_car_pop_petrol[x,"type"])$Weight)
new_car_pop_petrol$Weight_combined <- sapply(1:nrow(new_car_pop_petrol),function(x)new_car_pop_petrol[x,"Weight"]*new_car_pop_petrol[x,"Value"]/sum(subset(new_car_pop_petrol,Year==new_car_pop_petrol[x,"Year"] & fuel==new_car_pop_petrol[x,"fuel"])$Value))
petrol_weight_dt <- aggregate(formula = Weight_combined~Year+fuel,data = new_car_pop_petrol,FUN=sum)
#Fill historical values in matrix
mat_weight <- matrix(0,nrow=1,ncol=last_yr-first_yr+1,dimnames=list("Gasoline",first_yr:last_yr))

for (i in 1:nrow(petrol_weight_dt)){
  mat_weight[,as.character(petrol_weight_dt[i,"Year"])] <- petrol_weight_dt[i,"Weight_combined"]
}
#Assume constant prior to 2015
mat_weight[,as.character(first_yr:2014)] <- mat_weight[,"2015"]

#Fill output
hist_veh_weight_l[["ICEV-G"]] <- mat_weight

#C) Fuel consumption of HEV-G
#Assumption: 
fe_data <- read.csv("inputs/model/fuel_economy_singapore.csv",stringsAsFactors = FALSE)
mat_weight <- matrix(0,nrow=1,ncol=last_yr-first_yr+1,dimnames=list("Gasoline",first_yr:last_yr))
#Fill 2019 value with Prius Hybrid vehicle.
mat_weight[,"2019"] <- subset(fe_data,Make=="TOYOTA" & Model=="PRIUS C HYBRID 1.5")$fe_combined
mat_weight[,"2009"] <- mat_weight[,"2019"]*1.13
mat_weight[,as.character(first_yr:2008)] <- mat_weight[,"2009"]
#Assume linear regression from 2009 to 2019
mat_weight[,as.character(2010:2018)] <- sapply(2010:2018,function(x)(mat_weight[,"2019"]-mat_weight[,"2009"])/(2019-2009)*(x-2009)+mat_weight[,"2009"])
#Fill output
hist_veh_weight_l[["HEV-G"]] <- mat_weight

#D) Fuel consumption of BEV (Wh/km for BEV). 
fe_data <- read.csv("inputs/model/fuel_economy_singapore.csv",stringsAsFactors = FALSE)
fe_data_bev <- subset(fe_data,Fuel=="Electric")
fc_bev <- subset(fe_data_bev, Make=="NISSAN" & Model=="LEAF EV")$fe_combined*100/1000
mat_weight <- matrix(fc_bev,nrow=1,ncol=last_yr-first_yr+1,dimnames=list("Electricity",first_yr:last_yr))
#Fill output
hist_veh_weight_l[["BEV"]] <- mat_weight

#E) Fuel consumption of PHEV-G
mat_weight <- matrix(0,nrow=2,ncol=last_yr-first_yr+1,dimnames=list(c("Gasoline","Electricity"),first_yr:last_yr))
mat_weight["Gasoline",] <- 6
mat_weight["Electricity",] <- 20.8
#Fill output
hist_veh_weight_l[["PHEV"]] <- mat_weight

#F) Fuel consumption of HCNG
fe_us_data <- read.csv("inputs/data/fuel_economy_dataset_us.csv",stringsAsFactors = FALSE)
conv <- read.csv("inputs/user/conversion_units.csv",stringsAsFactors = FALSE,row.names = 1,check.names = FALSE)
fuel_conv <-  read.csv("inputs/user/fuel_conversion.csv",stringsAsFactors = FALSE,check.names = FALSE)
fe_data_cng <- subset(fe_us_data,fuelType=="CNG")
#Gasoline FC in L CNG / 100 km
fe_data_cng$comb08_fc <- 1/fe_data_cng$comb08*conv["L","1 gal"]*conv["mile","1 km"]*100*subset(fuel_conv,Data=="Conversion factor" & Fuel=="Gasoline")$Value/subset(fuel_conv,Data=="Conversion factor" & Fuel=="CNG")$Value
mat_weight <- matrix(0,nrow=1,ncol=last_yr-first_yr+1,dimnames=list("CNG",first_yr:last_yr))
#Assumption: Toyota Camry (no justification)

mat_weight["CNG",] <- subset(fe_data_cng,model=="Camry CNG" & year==2000)$comb08_fc
#Fill output
hist_veh_weight_l[["CNG"]] <- mat_weight

#Save output
saveRDS(hist_veh_weight_l,file="inputs/model/car_hist_fc.RDS")


# II) Bus ------------------------------------------------------------------
hist_veh_weight_l <- list()
first_yr <- 1985
last_yr <- 2019

#A) ICEB-D

mat_weight <- matrix(0,nrow=1,ncol=last_yr-first_yr+1,dimnames=list("Diesel",first_yr:last_yr))
#Assumption: from Zhang et al. (2014) in L/100km
mat_weight[1,] <- 32.6
hist_veh_weight_l[["ICEB-D"]] <- mat_weight

#C) HEB-D
mat_weight <- matrix(0,nrow=1,ncol=last_yr-first_yr+1,dimnames=list("Diesel",first_yr:last_yr))
#Assumption: from Zhang et al. (2014) in L/100km
mat_weight[1,] <- 24.3
hist_veh_weight_l[["HEB-D"]] <- mat_weight

#E) CNGB
mat_weight <- matrix(0,nrow=1,ncol=last_yr-first_yr+1,dimnames=list("CNG",first_yr:last_yr))
#Assumption: from Zhang et al. (2014) in L/100km
fuel_conv <- read.csv("inputs/user/fuel_conversion.csv",stringsAsFactors = FALSE)
mat_weight[1,] <- 16.1*100/(subset(fuel_conv,Data=="Conversion factor" & Fuel=="CNG")$Value/10^6)
hist_veh_weight_l[["CNGB"]] <- mat_weight

#F) EB
mat_weight <- matrix(0,nrow=1,ncol=last_yr-first_yr+1,dimnames=list("Electricity",first_yr:last_yr))
#Assumption: from literature review Laura
mat_weight[1,] <- 130
hist_veh_weight_l[["EB"]] <- mat_weight

#Save output
saveRDS(hist_veh_weight_l,file="inputs/model/bus_hist_fc.RDS")


#  III) Motorcycle ----------------------------------------------------------
hist_veh_weight_l <- list()
first_yr <- 1985
last_yr <- 2019
#A) ICEM-G
mat_weight <- matrix(0,nrow=1,ncol=last_yr-first_yr+1,dimnames=list("Gasoline",first_yr:last_yr))
#Assumption: from (Koossalapeerom et al., 2019) in L/100km
mat_weight[1,] <- 2.43
hist_veh_weight_l[["ICEM-G"]] <- mat_weight

#II) EM
mat_weight <- matrix(0,nrow=1,ncol=last_yr-first_yr+1,dimnames=list("Electricity",first_yr:last_yr))
#Assumption: from (Koossalapeerom et al., 2019) in kWh/100km
mat_weight[1,] <- 2.8
hist_veh_weight_l[["EM"]] <- mat_weight

#Save output
saveRDS(hist_veh_weight_l,file="inputs/model/moto_hist_fc.RDS")


# IV) MRT -----------------------------------------------------------------

hist_veh_weight_l <- list()
first_yr <- 2005
last_yr <- 2019

mat_weight <- matrix(0,nrow=1,ncol=last_yr-first_yr+1,dimnames=list("Electricity",first_yr:last_yr))
#Assumption: TEDB
mat_weight[1,] <- 360
hist_veh_weight_l[["MRT"]] <- mat_weight

#Save output
saveRDS(hist_veh_weight_l,file="inputs/model/mrt_hist_fc.RDS")

# V) LRT -----------------------------------------------------------------

hist_veh_weight_l <- list()
first_yr <- 2005
last_yr <- 2019

mat_weight <- matrix(0,nrow=1,ncol=last_yr-first_yr+1,dimnames=list("Electricity",first_yr:last_yr))
#Assumption: TEDB
mat_weight[1,] <- 360
hist_veh_weight_l[["LRT"]] <- mat_weight

#Save output
saveRDS(hist_veh_weight_l,file="inputs/model/lrt_hist_fc.RDS")
