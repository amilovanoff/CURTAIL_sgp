#' fleet_technology_market_share_proj_f
#' 
#' @import modelframework
#' @import reshape2
#' @export
fleet_technology_market_share_proj_f <- function(fleet,
                                                 last_yr=NA,
                                                 techno_ms_proj_car=NA,
                                                 techno_ms_proj_taxi=NA,
                                                 techno_ms_proj_moto=NA,
                                                 techno_ms_proj_public_bus=NA,
                                                 electrification_year=NA){
  attribute_f("fleet_technology_market_share_proj_f")
  first_proj_yr <- min(as.numeric(colnames(fleet$technology_market_share)[is.na(fleet$technology_market_share[1,])]))
  #Update prospective technology market share
  #Case: Full EVs in fleet by electrification_year
  if ((techno_ms_proj_car%in%c("ldv_ev40","optimization") & fleet$mode%in%c("Private car","Private hire car"))|(techno_ms_proj_taxi=="taxi_ev40" & fleet$mode%in%c("Taxi"))){
    #2020 market share are constant
    fleet$technology_market_share[,as.character(first_proj_yr)] <- fleet$technology_market_share[,as.character(first_proj_yr-1)]
    #Full BEV in 2030
    full_market_share <- ifelse(electrification_year-10<2021,2021,round(electrification_year)-10)
    fleet$technology_market_share["BEV",as.character((first_proj_yr):last_yr)] <- approx(x=c(first_proj_yr,full_market_share), y=c(fleet$technology_market_share["BEV",as.character(first_proj_yr)],1), xout=seq(first_proj_yr,ifelse(full_market_share>last_yr,full_market_share,last_yr),1), method = "linear",yright=1)$y[1:(last_yr-first_proj_yr+1)]
    #Reduce proportionally all other technologies
    fleet$technology_market_share[!rownames(fleet$technology_market_share)%in%"BEV",as.character((first_proj_yr+1):last_yr)] <- (fleet$technology_market_share[!rownames(fleet$technology_market_share)%in%"BEV",as.character(first_proj_yr),drop=FALSE] / sum(fleet$technology_market_share[!rownames(fleet$technology_market_share)%in%"BEV",as.character(first_proj_yr)])) %*% matrix(round(1-colSums(fleet$technology_market_share["BEV",as.character((first_proj_yr+1):last_yr),drop=FALSE]),digits=7),nrow=1,ncol=last_yr-first_proj_yr)
  
  #Case: Full EMs in fleet by 2040 for motorcyles
  } else if (techno_ms_proj_moto=="moto_em40" & fleet$mode=="Motorcycle"){
      #2020 market share are constant
      fleet$technology_market_share[,as.character(first_proj_yr)] <- fleet$technology_market_share[,as.character(first_proj_yr-1)]
      #Full BEV in 2030
      full_market_share <- ifelse(electrification_year-10<2021,2021,round(electrification_year)-10)
      fleet$technology_market_share["EM",as.character((first_proj_yr):last_yr)] <- approx(x=c(first_proj_yr,full_market_share), y=c(fleet$technology_market_share["EM",as.character(first_proj_yr)],1), xout=seq(first_proj_yr,ifelse(full_market_share>last_yr,full_market_share,last_yr),1), method = "linear",yright=1)$y[1:(last_yr-first_proj_yr+1)]
      #Reduce proportionally all other technologies
      fleet$technology_market_share["ICEM-G",as.character((first_proj_yr+1):last_yr)] <- 1-fleet$technology_market_share["EM",as.character((first_proj_yr+1):last_yr)]
      
  #Case: Full EBs by 2040
  } else if (techno_ms_proj_public_bus=="bus_eb40" & fleet$mode%in%c("Public bus","School bus","Private bus")){
    #2020 market share are constant
    fleet$technology_market_share[,as.character(first_proj_yr)] <- fleet$technology_market_share[,as.character(first_proj_yr-1)]
    full_market_share <- ifelse(electrification_year-10<2021,2021,round(electrification_year)-10)
    #Full EB from 2021
    fleet$technology_market_share["EB",as.character((first_proj_yr):last_yr)] <- approx(x=c(first_proj_yr,full_market_share), y=c(fleet$technology_market_share["EB",as.character(first_proj_yr)],1), xout=seq(first_proj_yr,ifelse(full_market_share>last_yr,full_market_share,last_yr),1), method = "linear",yright=1)$y[1:(last_yr-first_proj_yr+1)]
    fleet$technology_market_share[!rownames(fleet$technology_market_share)%in%"EB",as.character((first_proj_yr+1):last_yr)] <- (fleet$technology_market_share[!rownames(fleet$technology_market_share)%in%"EB",as.character(first_proj_yr),drop=FALSE] / sum(fleet$technology_market_share[!rownames(fleet$technology_market_share)%in%"EB",as.character(first_proj_yr)])) %*% matrix(round(1-colSums(fleet$technology_market_share["EB",as.character((first_proj_yr+1):last_yr),drop=FALSE]),digits=7),nrow=1,ncol=last_yr-first_proj_yr)
  
  } else {
    fleet$technology_market_share[,as.character(first_proj_yr:last_yr)] <- fleet$technology_market_share[,as.character(first_proj_yr-1)]
  }
 return(fleet)
}
