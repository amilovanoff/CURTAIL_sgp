#' survival_rate_f
#' Function: Gives the probability of a vehicle of age "age" to survive knowing that it survives until age "age -1".
#' @export
survival_rate_f <- function(mode,
                            age,
                            year=NA,
                            survival_rate_mdl=NA,
                            scrap_policy_mdl_public_bus=NA,
                            scrap_policy_mdl_taxi=NA,
                            cumulative_rate="n",
                            scrappage_rate="n"){
  attribute_f(fun_name="survival_rate_f")
  survival_rates  <- get_input_f(input_name = 'survival_rates')
  mode_tbc <- switch(mode,
                     "Private hire car"="Private car",
                     "Taxi"="Private car",
                     "Public bus"="Bus",
                     "Private bus"="Bus",
                     "School bus"="Bus",
                     mode)
  #Extract survival rates data associated with source
  if (year%in%subset(survival_rates,Mode==mode_tbc)$Year){
    survival_rates_dt <- subset(survival_rates,Mode==mode_tbc & Year==year)
  } else {
    survival_rates_dt <- subset(survival_rates,Mode==mode_tbc & Model==survival_rate_mdl)
  }
  if(cumulative_rate=="y"){
    age_l <- 1:age
  } else if (cumulative_rate=="n"){
    age_l <- age
  }
  survival_rate <- 1
  for (a in age_l){
    #Check if age in dataset. Otherwise assume constant annual survival rate after the maximum age
    if (a>max(unique(survival_rates_dt$Age))){
      age_tbc <- max(unique(survival_rates_dt$Age))
    } else {
      age_tbc <- a
    }
    #Calculate survival_rate
    survival_rate <- survival_rate*as.numeric(subset(survival_rates_dt,Data=="Annual survival rate" & Age==age_tbc,select=Value))
  }
  #Scrappage of survival
  if(scrappage_rate=="y"){
    rate <- 1-survival_rate
  } else {
    rate <- survival_rate
  }
  #High scrappage policies imply full renewal by 2030. So rate is 0 for 
  if (scrap_policy_mdl_public_bus=="high" & mode=="Public bus" & age>=(2030-year+10)){
    rate <- 0
  } else if(scrap_policy_mdl_taxi=="high" & mode=="Taxi" & age>=(2030-year+10)){
    rate <- 0
  }
  return(rate)
}


