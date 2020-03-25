#' transport_activity_proj_f
#' @import modelframework
#' @export
transport_activity_proj_f <- function(transport,first_yr=NA,last_yr=NA,pkt_proj_tot_scen=NA,pkt_tot_variable=NA,pkt_proj_modal_share_scen=NA,modal_share_variable=NA){
  attribute_f("transport_activity_proj_f")
  first_proj_yr <- min(as.numeric(colnames(transport$vkt)[is.na(transport$vkt[1,])]))
  #First, scenario on total prospective PKT
  pop_dt <- get_input_f("population")
  #Create matrix of total pkt (in thousand pkt)
  mat_tot_pkt <- t(as.matrix(colSums(transport$pkt)))
  #Create matrix of population
  mat_pop <- subset(pop_dt,Scenario=="Medium" & Year <= last_yr) %>%
    acast(data=., Country ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)
  if (pkt_proj_tot_scen=="bau"){  
    constant_ratio <- mat_tot_pkt[,as.character(first_proj_yr-1)]/mat_pop[,as.character(first_proj_yr-1)]
    #Project total pkt
    mat_tot_pkt[,as.character(first_proj_yr:last_yr)] <- constant_ratio*mat_pop[,as.character(first_proj_yr:last_yr)]
  } else if (pkt_proj_tot_scen=="mean_ratio"){  
    constant_ratio <- mean(mat_tot_pkt[,as.character(first_yr:(first_proj_yr-1))]/mat_pop[,as.character(first_yr:(first_proj_yr-1))])
    #Project total pkt
    mat_tot_pkt[,as.character(first_proj_yr:last_yr)] <- constant_ratio*mat_pop[,as.character(first_proj_yr:last_yr)]
  }else if (pkt_proj_tot_scen=="linear_reg"){
    #Calculate linear regression
    lin_reg <- lm(mat_tot_pkt[,as.character(first_yr:(first_proj_yr-1))]~mat_pop[,as.character(first_yr:(first_proj_yr-1))])
    #Project total pkt
    mat_tot_pkt[,as.character(first_proj_yr:last_yr)] <- lin_reg$coefficients[1]+lin_reg$coefficients[2]*mat_pop[,as.character(first_proj_yr:last_yr)]
  } else if (pkt_proj_tot_scen=="optimization"){
    #Project total pkt with decrease in pkt/pop
    #pkt_tot_variable is the % annual % in decrease
    mat_tot_pkt[,as.character(first_proj_yr:last_yr)] <- (lin_reg$coefficients[1]+lin_reg$coefficients[2]*mat_pop[,as.character(first_proj_yr:last_yr)])*sapply(first_proj_yr:last_yr,function(x)(1-pkt_tot_variable)^(x-first_proj_yr+1))
  }
  #Assume School bus and private bus PKT to stay constant
  constant_mode <- c("School bus","Private bus")
  for (mode in constant_mode){
    transport$kt_per_veh[mode,as.character(first_proj_yr:last_yr)] <- transport$kt_per_veh[mode,as.character(first_proj_yr-1)]
    transport$load_factors[mode,as.character(first_proj_yr:last_yr)] <- transport$load_factors[mode,as.character(first_proj_yr-1)]
    #Assumed constant total population.
    transport$vkt[mode,as.character(first_proj_yr:last_yr)] <- transport$vkt[mode,as.character(first_proj_yr-1)]
    transport$pkt[mode,as.character(first_proj_yr:last_yr)] <- transport$pkt[mode,as.character(first_proj_yr-1)]
  }
  #Adjust propsective PKT of other modes depending on the scenario
  if (pkt_proj_modal_share_scen=="constant"){
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
    
  } else if(pkt_proj_modal_share_scen=="baseline"){
    #Assume public transit to continue its increasing trends up to 2030.
    #MRT and LRT are linked to rail development
    #LTA pledge 360 km of rail to be constructed in 2030 compared with 228km currently. We assume 330 km of MRT.
    #MRT: 67000 thousand of pkt/km of rail on average between 2013 and 2018. We assume linear interpolation
    mode = "MRT"
    transport$pkt[mode,"2030"] <- 330*67000
    transport$pkt[mode,as.character((first_proj_yr-1):2030)] <- approx(x=c(first_proj_yr-1,2030),y=transport$pkt[mode,c(as.character(first_proj_yr-1),"2030")],method="linear",xout=(first_proj_yr-1):2030)$y
    transport$pkt[mode,as.character(2030:last_yr)] <- transport$pkt[mode,"2030"]
    #LRT and Bus
    i_year_trend=2016
    f_year_trend=2019
    mode_pt <- c("Public bus","LRT")
    #Assumed absolute continuing trends for each up to 2030
    for (mode in mode_pt){
      #Absolute
      #transport$pkt[mode,as.character(first_proj_yr:last_yr)] <-  transport$pkt[mode,as.character(first_proj_yr-1)]+sapply(first_proj_yr:last_yr, function(x)(x-first_proj_yr+1)*(transport$pkt[mode,as.character(f_year_trend)]-transport$pkt[mode,as.character(i_year_trend)])/(f_year_trend-i_year_trend))
      #Relative
      transport$pkt[mode,as.character(first_proj_yr:2030)] <-  mat_tot_pkt[,as.character(first_proj_yr:2030)]*(transport$pkt[mode,as.character(first_proj_yr-1)]/sum(transport$pkt[,as.character(first_proj_yr-1)])+sapply(first_proj_yr:2030, function(x)(x-first_proj_yr+1)*(transport$pkt[mode,as.character(f_year_trend)]/sum(transport$pkt[,as.character(f_year_trend)])-transport$pkt[mode,as.character(i_year_trend)]/sum(transport$pkt[,as.character(i_year_trend)]))/(f_year_trend-i_year_trend)))
      #Constant after 2030
      transport$pkt[mode,as.character(2030:last_yr)] <- transport$pkt[mode,"2030"]
    }
    #Assume other transport adjust proportionally proportionally
    mode_tbc <- rownames(transport$pkt)[is.na(transport$pkt[,as.character(first_proj_yr)])]
    transport$pkt[mode_tbc,as.character(first_proj_yr:last_yr)] <- (transport$pkt[mode_tbc,as.character(first_proj_yr-1),drop=FALSE]/sum(transport$pkt[mode_tbc,as.character(first_proj_yr-1)])) %*% (mat_tot_pkt[,as.character(first_proj_yr:last_yr),drop=FALSE]-colSums(transport$pkt[,as.character(first_proj_yr:last_yr)],na.rm=TRUE))
    #Assume constant load factors
    transport$load_factors[c(mode_pt,mode_tbc),as.character(first_proj_yr:last_yr)] <- transport$load_factors[c(mode_pt,mode_tbc),as.character(first_proj_yr-1)]
    #Calculate resulting vkt
    transport$vkt[c(mode_pt,mode_tbc),as.character(first_proj_yr:last_yr)] <- transport$pkt[c(mode_pt,mode_tbc),as.character(first_proj_yr:last_yr)]/transport$load_factors[c(mode_pt,mode_tbc),as.character(first_proj_yr:last_yr)]
    #Assume constant annual mileage for all modes
    transport$kt_per_veh[c(mode_pt,mode_tbc),as.character(first_proj_yr:last_yr)] <- transport$kt_per_veh[c(mode_pt,mode_tbc),as.character(first_proj_yr-1)]
  
  } else if(pkt_proj_modal_share_scen=="uber"){
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
  
  } else if (pkt_proj_modal_share_scen=="optimization"){
    #Create modal share scenario from one attribute.
    #modal_share_variable is the share of public transit in PKT
    mode_pt <- c("Public bus","MRT","LRT")
    #i_share is the share of public transit in the varying modes 
    i_share <- sum(transport$pkt[mode_pt,as.character(first_proj_yr-1)])/sum(transport$pkt[!rownames(transport$pkt)%in%constant_mode,as.character(first_proj_yr-1)])
    #Calculate the prospective PKT of public transit.
    #Assumption: Constant share in Bus/MRT/LRT in public transit
    transport$pkt[mode_pt,as.character(first_proj_yr:last_yr)] <-  (transport$pkt[mode_pt,as.character(first_proj_yr-1),drop=FALSE] / sum(transport$pkt[mode_pt,as.character(first_proj_yr-1)])) %*% (matrix(sapply(i_share+modal_share_variable*0:(last_yr-first_proj_yr),function(x)ifelse(x<=1,x,1)),nrow=1,ncol=last_yr-first_proj_yr+1) * (mat_tot_pkt[,as.character(first_proj_yr:last_yr),drop=FALSE]-colSums(transport$pkt[constant_mode,as.character(first_proj_yr:last_yr)],na.rm=TRUE)))
    #Assume other transport mode decrease proportionally
    mode_tbc <- rownames(transport$pkt)[is.na(transport$pkt[,as.character(first_proj_yr)])]
    transport$pkt[mode_tbc,as.character(first_proj_yr:last_yr)] <- (transport$pkt[mode_tbc,as.character(first_proj_yr-1),drop=FALSE]/sum(transport$pkt[mode_tbc,as.character(first_proj_yr-1)])) %*% matrix(sapply(first_proj_yr:last_yr,function(x)ifelse(mat_tot_pkt[,as.character(x)]-sum(transport$pkt[,as.character(x)],na.rm=TRUE)>0,mat_tot_pkt[,as.character(x)]-sum(transport$pkt[,as.character(x)],na.rm=TRUE),0)),nrow=1,ncol=last_yr-first_proj_yr+1)
    #Assume constant load factors
    transport$load_factors[c(mode_pt,mode_tbc),as.character(first_proj_yr:last_yr)] <- transport$load_factors[c(mode_pt,mode_tbc),as.character(first_proj_yr-1)]
    #Calculate resulting vkt
    transport$vkt[c(mode_pt,mode_tbc),as.character(first_proj_yr:last_yr)] <- transport$pkt[c(mode_pt,mode_tbc),as.character(first_proj_yr:last_yr)]/transport$load_factors[c(mode_pt,mode_tbc),as.character(first_proj_yr:last_yr)]
    #Assume constant annual mileage for all modes
    transport$kt_per_veh[c(mode_pt,mode_tbc),as.character(first_proj_yr:last_yr)] <- transport$kt_per_veh[c(mode_pt,mode_tbc),as.character(first_proj_yr-1)]
  }
  return(transport)
}
