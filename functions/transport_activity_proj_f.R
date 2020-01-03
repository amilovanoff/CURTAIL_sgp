#' transport_activity_proj_f
#' @import modelframework
#' @export
transport_activity_proj_f <- function(transport,first_yr=NA,last_yr=NA,pkt_proj_model_share_scen=NA,pkt_proj_tot_scen=NA){
  attribute_f("transport_activity_proj_f")
  first_proj_yr <- min(as.numeric(colnames(transport$vkt)[is.na(transport$vkt[1,])]))
  #First, scenario on total prospective PKT
  if (pkt_proj_tot_scen=="bau"){
    pop_dt <- get_input_f("population")
    #Create matrix of total pkt (in thousand pkt)
    mat_tot_pkt <- t(as.matrix(colSums(transport$pkt)))
    #Create matrix of population
    mat_pop <- subset(pop_dt,Scenario=="Medium" & Year <= last_yr) %>%
      acast(data=., Country ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)
    #Calculate linear regression
    lin_reg <- lm(mat_tot_pkt[,as.character(first_yr:(first_proj_yr-1))]~mat_pop[,as.character(first_yr:(first_proj_yr-1))])
    #Project total pkt
    mat_tot_pkt[,as.character(first_proj_yr:last_yr)] <- lin_reg$coefficients[1]+lin_reg$coefficients[2]*mat_pop[,as.character(first_proj_yr:last_yr)]
  }
  #Assume School bus and private bus PKT to stay constant
  for (mode in c("School bus","Private bus")){
    transport$kt_per_veh[mode,as.character(first_proj_yr:last_yr)] <- transport$kt_per_veh[mode,as.character(first_proj_yr-1)]
    transport$load_factors[mode,as.character(first_proj_yr:last_yr)] <- transport$load_factors[mode,as.character(first_proj_yr-1)]
    #Assumed constant total population of Private car.
    transport$vkt[mode,as.character(first_proj_yr:last_yr)] <- transport$vkt[mode,as.character(first_proj_yr-1)]
    transport$pkt[mode,as.character(first_proj_yr:last_yr)] <- transport$pkt[mode,as.character(first_proj_yr-1)]
  }
  #Adjust propsective PKT of other modes depending on the scenario
  if (pkt_proj_model_share_scen=="constant"){
    #Assume constant model share in pkt for other modes
    mode_tbc <- rownames(transport$pkt)[is.na(transport$pkt[,as.character(first_proj_yr)])]
    transport$pkt[mode_tbc,as.character(first_proj_yr:last_yr)] <- (transport$pkt[mode_tbc,as.character(first_proj_yr-1),drop=FALSE]/sum(transport$pkt[mode_tbc,as.character(first_proj_yr-1)])) %*% (mat_tot_pkt[,as.character(first_proj_yr:last_yr),drop=FALSE]-colSums(transport$pkt[,as.character(first_proj_yr:last_yr)],na.rm=TRUE))
    #Assume constant load factors
    transport$load_factors[mode_tbc,as.character(first_proj_yr:last_yr)] <- transport$load_factors[mode_tbc,as.character(first_proj_yr-1)]
    #Calculate resulting vkt
    transport$vkt[mode_tbc,as.character(first_proj_yr:last_yr)] <- transport$pkt[mode_tbc,as.character(first_proj_yr:last_yr)]/transport$load_factors[mode_tbc,as.character(first_proj_yr:last_yr)]
    #Assume constant annual mileage for all except private cars
    transport$kt_per_veh[setdiff(mode_tbc,"Private car"),as.character(first_proj_yr:last_yr)] <- transport$kt_per_veh[setdiff(mode_tbc,"Private car"),as.character(first_proj_yr-1)]
    #Calculate the corresponding annual mileage for a fixed stock of private cars to achieve pkt
    transport$kt_per_veh["Private car",as.character(first_proj_yr:last_yr)] <- transport$vkt["Private car",as.character(first_proj_yr:last_yr)]/(transport$vkt["Private car",as.character(first_proj_yr-1)]/transport$kt_per_veh["Private car",as.character(first_proj_yr-1)])
    
  } else if(pkt_proj_model_share_scen=="lta_masterplan"){
    #Assume public transit to continue its increasing trends up to 2030. Assume trends from 2012 to 2018
    i_year_trend=2012
    f_year_trend=2016
    mode_pt <- c("Public bus","MRT","LRT")
    transport$pkt[mode_pt,as.character(first_proj_yr:last_yr)] <-  transport$pkt[mode_pt,as.character(first_proj_yr-1),drop=FALSE] %*% matrix(1,nrow=1,ncol=last_yr-first_proj_yr+1) + ((transport$pkt[mode_pt,as.character(f_year_trend),drop=FALSE]-transport$pkt[mode_pt,as.character(i_year_trend),drop=FALSE])/(f_year_trend-i_year_trend+1)) %*% matrix(1:(last_yr-first_proj_yr+1),nrow=1)
    #Assume other transport mode decrease proportionally
    mode_tbc <- rownames(transport$pkt)[is.na(transport$pkt[,as.character(first_proj_yr)])]
    transport$pkt[mode_tbc,as.character(first_proj_yr:last_yr)] <- (transport$pkt[mode_tbc,as.character(first_proj_yr-1),drop=FALSE]/sum(transport$pkt[mode_tbc,as.character(first_proj_yr-1)])) %*% (mat_tot_pkt[,as.character(first_proj_yr:last_yr),drop=FALSE]-colSums(transport$pkt[,as.character(first_proj_yr:last_yr)],na.rm=TRUE))
    #Assume constant load factors
    transport$load_factors[c(mode_pt,mode_tbc),as.character(first_proj_yr:last_yr)] <- transport$load_factors[c(mode_pt,mode_tbc),as.character(first_proj_yr-1)]
    #Calculate resulting vkt
    transport$vkt[c(mode_pt,mode_tbc),as.character(first_proj_yr:last_yr)] <- transport$pkt[c(mode_pt,mode_tbc),as.character(first_proj_yr:last_yr)]/transport$load_factors[c(mode_pt,mode_tbc),as.character(first_proj_yr:last_yr)]
    #Assume constant annual mileage for all modes
    transport$kt_per_veh[c(mode_pt,mode_tbc),as.character(first_proj_yr:last_yr)] <- transport$kt_per_veh[c(mode_pt,mode_tbc),as.character(first_proj_yr-1)]
  } else if(pkt_proj_model_share_scen=="uber"){
    #Assume the increase in PKT is met by increasing vehicle usage in private car. Other modes stay at 2018 levels
    mode_tbc <- setdiff(rownames(transport$pkt)[is.na(transport$pkt[,as.character(first_proj_yr)])],"Private car")
    transport$pkt[mode_tbc,as.character(first_proj_yr:last_yr)] <- transport$pkt[mode_tbc,as.character(first_proj_yr-1)]
    transport$pkt["Private car",as.character(first_proj_yr:last_yr)] <- mat_tot_pkt[,as.character(first_proj_yr:last_yr),drop=FALSE]-colSums(transport$pkt[,as.character(first_proj_yr:last_yr)],na.rm=TRUE)
    #Assume constant load factors for all modes.
    transport$load_factors[c(mode_tbc,"Private car"),as.character(first_proj_yr:last_yr)] <- transport$load_factors[c(mode_tbc,"Private car"),as.character(first_proj_yr-1)]
    #Calculate resulting vkt
    transport$vkt[c(mode_tbc,"Private car"),as.character(first_proj_yr:last_yr)] <- transport$pkt[c(mode_tbc,"Private car"),as.character(first_proj_yr:last_yr)]/transport$load_factors[c(mode_tbc,"Private car"),as.character(first_proj_yr:last_yr)]
    #Assume constant annual mileage for all except private cars
    transport$kt_per_veh[mode_tbc,as.character(first_proj_yr:last_yr)] <- transport$kt_per_veh[mode_tbc,as.character(first_proj_yr-1)]
    #Calculate the corresponding annual mileage for a fixed stock of private cars to achieve pkt
    transport$kt_per_veh["Private car",as.character(first_proj_yr:last_yr)] <- transport$vkt["Private car",as.character(first_proj_yr:last_yr)]/(transport$vkt["Private car",as.character(first_proj_yr-1)]/transport$kt_per_veh["Private car",as.character(first_proj_yr-1)])
  }
  return(transport)
}
