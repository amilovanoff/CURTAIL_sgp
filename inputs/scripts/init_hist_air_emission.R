
air_ef <- read.csv("inputs/data/air_emission_factors.csv",stringsAsFactors = FALSE, check.names = FALSE)
timing_euro_standards <- read.csv("inputs/data/timing_euro_standards.csv",stringsAsFactors = FALSE, check.names = FALSE)
#ASSUMPTION: We assume starting year of Euro1 to be 1985
timing_euro_standards[timing_euro_standards$Standard=="Euro 1","Start year"] <- 1985
first_yr = 1985
last_yr = 2020
air_pollutant_dt <- get_input_f("model_matching_air_pollutant")
#Output
dt_col <- c("Scenario","Mode","Technology","Pollutant",as.character(first_yr:last_yr))
out_ef_dt <- setNames(data.frame(matrix(NA,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)

#Create a prefill function:
prefill_ef_f <- function(mode,technology_list,pollutant_list,size_tbc){
  ef_dt <- setNames(data.frame(matrix(NA,ncol = length(dt_col), nrow = length(technology_list)*length(pollutant_list)),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
  ef_dt[,c("Technology","Pollutant")] <- expand.grid(Technology=technology_list, Pollutant=pollutant_list)
  for (techno in technology_list){
    for (pollutant  in pollutant_list){
      if (nrow(subset(air_ef,Mode==mode & Technology==techno & Size%in%size_tbc & Pollutant==pollutant))!=0){
        for (year in as.character(first_yr:last_yr)){
          euro_standard <- subset(timing_euro_standards,`Start year`<=as.numeric(year) & `End year`>=as.numeric(year))$Standard
          ef_dt[ef_dt$Technology==techno & ef_dt$Pollutant==pollutant,year] <- subset(air_ef,Mode==mode & Technology==techno & Size%in%size_tbc & Pollutant==pollutant)[,euro_standard]
        }
      }
    }
  }
  #Format: Convert empty into NA
  ef_dt[ef_dt==""] <- NA
  ef_dt$Mode <- mode
  return(ef_dt)
}

# I) Private cars -------------------------
#Get list of technology
mode="Private car"
veh_techno <- get_input_f("model_matching_vehicle_technology")

ef_dt <- prefill_ef_f(mode=mode,technology_list=veh_techno$Technology,pollutant_list=air_pollutant_dt$Pollutant,size_tbc=c("Medium","/"))
#Further assumptions:
#HEV-G constant emission factors before euro 4
techno = "HEV-G"
for (pollutant  in air_pollutant_dt$Pollutant){
  ef_dt[ef_dt$Technology==techno & ef_dt$Pollutant==pollutant,as.character(first_yr:2005)] <- ef_dt[ef_dt$Technology==techno & ef_dt$Pollutant==pollutant,"2006"]
}
#Assumption: EF of PM for HEV-G is similar to ICEV-G (TO REFINE)
ef_dt[ef_dt$Technology==techno & ef_dt$Pollutant=="PM",as.character(first_yr:2020)] <- ef_dt[ef_dt$Technology=="ICEV-G" & ef_dt$Pollutant=="PM",as.character(first_yr:2020)]
#HEV-D have the same emission factors that HEV-G
techno = "HEV-D"
for (pollutant  in air_pollutant_dt$Pollutant){
  ef_dt[ef_dt$Technology==techno & ef_dt$Pollutant==pollutant,as.character(first_yr:2020)] <- ef_dt[ef_dt$Technology=="HEV-G" & ef_dt$Pollutant==pollutant,as.character(first_yr:2020)]
}
#PHEV similar emission factors than HEV-G
techno = "PHEV"
for (pollutant  in air_pollutant_dt$Pollutant){
  ef_dt[ef_dt$Technology==techno & ef_dt$Pollutant==pollutant,as.character(first_yr:2020)] <- ef_dt[ef_dt$Technology=="HEV-G" & ef_dt$Pollutant==pollutant,as.character(first_yr:2020)]
}
#CNG constant emission factors before euro 4
techno = "CNG"
for (pollutant  in air_pollutant_dt$Pollutant){
  ef_dt[ef_dt$Technology==techno & ef_dt$Pollutant==pollutant,as.character(first_yr:2005)] <- ef_dt[ef_dt$Technology==techno & ef_dt$Pollutant==pollutant,"2006"]
}
#Format 
out_ef_dt <- rbind(out_ef_dt,ef_dt)


# II) Taxi ----------------------------------------------------------------
#Assumption: Same than Private cars
mode="Taxi"
ef_dt$Mode <- mode
out_ef_dt <- rbind(out_ef_dt,ef_dt)

# III)Motorcyles---------------------------------------------------------------------
#Get list of technology
mode="Motorcycle"
veh_techno <- get_input_f("model_matching_moto_technology")
ef_dt <- prefill_ef_f(mode=mode,technology_list=veh_techno$Technology,pollutant_list=air_pollutant_dt$Pollutant,size_tbc=c("4-stroke <250 cm3","/"))
#Format 
out_ef_dt <- rbind(out_ef_dt,ef_dt)


# IV) Public bus ----------------------------------------------------------------
mode="Public bus"
veh_techno <- get_input_f("model_matching_bus_technology")
ef_dt <- prefill_ef_f(mode=mode,technology_list=veh_techno$Technology,pollutant_list=air_pollutant_dt$Pollutant,size_tbc=c(""))
#Further assumptions
#HEB-G have the same emission factors that ICEB-G
techno = "HEB-G"
for (pollutant  in air_pollutant_dt$Pollutant){
  ef_dt[ef_dt$Technology==techno & ef_dt$Pollutant==pollutant,as.character(first_yr:2020)] <- ef_dt[ef_dt$Technology=="ICEB-G" & ef_dt$Pollutant==pollutant,as.character(first_yr:2020)]
}
#HEB-D have the same emission factors that ICEB-D
techno = "HEB-D"
for (pollutant  in air_pollutant_dt$Pollutant){
  ef_dt[ef_dt$Technology==techno & ef_dt$Pollutant==pollutant,as.character(first_yr:2020)] <- ef_dt[ef_dt$Technology=="ICEB-D" & ef_dt$Pollutant==pollutant,as.character(first_yr:2020)]
}

#Format 
out_ef_dt <- rbind(out_ef_dt,ef_dt)


# V) Private and school buses ---------------------------------------------
for (mode in c("Private bus","School bus")){
  veh_techno <- get_input_f("model_matching_bus_technology")
  ef_dt <- prefill_ef_f(mode="Coach bus",technology_list=veh_techno$Technology,pollutant_list=air_pollutant_dt$Pollutant,size_tbc=c(""))
  #Change name of mode
  ef_dt$Mode <- mode
  #Further assumptions
  #HEB-G have the same emission factors that ICEB-G
  techno = "HEB-G"
  for (pollutant  in air_pollutant_dt$Pollutant){
    ef_dt[ef_dt$Technology==techno & ef_dt$Pollutant==pollutant,as.character(first_yr:2020)] <- ef_dt[ef_dt$Technology=="ICEB-G" & ef_dt$Pollutant==pollutant,as.character(first_yr:2020)]
  }
  #HEB-D have the same emission factors that ICEB-D
  techno = "HEB-D"
  for (pollutant  in air_pollutant_dt$Pollutant){
    ef_dt[ef_dt$Technology==techno & ef_dt$Pollutant==pollutant,as.character(first_yr:2020)] <- ef_dt[ef_dt$Technology=="ICEB-D" & ef_dt$Pollutant==pollutant,as.character(first_yr:2020)]
  }
  #CNGB have the same emission factors than public bus CNGB
  techno = "CNGB"
  for (pollutant  in air_pollutant_dt$Pollutant){
    ef_dt[ef_dt$Technology==techno & ef_dt$Pollutant==pollutant,as.character(first_yr:2020)] <- out_ef_dt[out_ef_dt$Mode=="Public bus" & out_ef_dt$Technology==techno & out_ef_dt$Pollutant==pollutant,as.character(first_yr:2020)]
  }
  #Out 
  out_ef_dt <- rbind(out_ef_dt,ef_dt)
}

write.csv(out_ef_dt,"inputs/model/air_pollutant_emission_factors_historical.csv",row.names = FALSE)
