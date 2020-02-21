#' fleet_technology_market_share_proj_f
#' 
#' @import modelframework
#' @import reshape2
#' @export
fleet_technology_market_share_proj_f <- function(fleet,
                                                 last_yr=NA,
                                                 techno_ms_proj_car=NA,
                                                 techno_ms_proj_public_bus=NA,
                                                 techno_ms_proj_taxi=NA,
                                                 optimization_techno=NA,
                                                 techno_variable=NA){
  attribute_f("fleet_technology_market_share_proj_f")
  first_proj_yr <- min(as.numeric(colnames(fleet$technology_market_share)[is.na(fleet$technology_market_share[1,])]))
  #Update prospective technology market share
  #Case: EV3030 in Private cars
  if (techno_ms_proj_car=="ev3030" & fleet$mode%in%c("Private car","Private hire car")){
    #Assumption: 30@ of market share in 2030 as BEV in Private car
    fleet$technology_market_share["BEV",as.character(first_proj_yr:last_yr)] <- approx(x=c(first_proj_yr-1,last_yr), y=c(fleet$technology_market_share["BEV",as.character(first_proj_yr-1)],0.30) , xout=seq(first_proj_yr,last_yr,1), method = "linear")$y
    #Assumption: Reduction in ICEV-G
    fleet$technology_market_share["ICEV-G",as.character(first_proj_yr:last_yr)] <- fleet$technology_market_share["ICEV-G",as.character(first_proj_yr-1)]+fleet$technology_market_share["BEV",as.character(first_proj_yr-1)]-fleet$technology_market_share["BEV",as.character(first_proj_yr:last_yr)]
    #Assumption: All other technologies constant
    fleet$technology_market_share[is.na(fleet$technology_market_share[,as.character(first_proj_yr)]),as.character(first_proj_yr:last_yr)] <- fleet$technology_market_share[is.na(fleet$technology_market_share[,as.character(first_proj_yr)]),as.character(first_proj_yr-1)]
  
  #Case: No specifications, constant.
  } else if (techno_ms_proj_public_bus=="full_eb" & fleet$mode=="Public bus"){
    #2020 market share are constant
    fleet$technology_market_share[,as.character(first_proj_yr)] <- fleet$technology_market_share[,as.character(first_proj_yr-1)]
    #Full EB from 2021
    fleet$technology_market_share["EB",as.character((first_proj_yr+1):last_yr)] <- 1
    fleet$technology_market_share[rownames(fleet$technology_market_share)!="EB",as.character((first_proj_yr+1):last_yr)] <- 0
  
  } else if (techno_ms_proj_taxi=="full_ev" & fleet$mode=="Taxi"){
    #2020 market share are constant
    fleet$technology_market_share[,as.character(first_proj_yr)] <- fleet$technology_market_share[,as.character(first_proj_yr-1)]
    #full EV from 2021
    fleet$technology_market_share["BEV",as.character((first_proj_yr+1):last_yr)] <- 1
    fleet$technology_market_share[rownames(fleet$technology_market_share)!="BEV",as.character((first_proj_yr+1):last_yr)] <- 0
  
  #Case: Optimization. For now, only for private cars
  } else if (techno_ms_proj_car=="optimization" & fleet$mode=="Private car" & any(sapply(rownames(fleet$technology_market_share),function(x)grepl(x,optimization_techno)))){
    #Techno to optimize are the technology to adjust.
    techno_to_optimize <- unlist(strsplit(optimization_techno,";"))[unlist(strsplit(optimization_techno,";"))%in%rownames(fleet$technology_market_share)]
    fleet$technology_market_share[techno_to_optimize,as.character(first_proj_yr:last_yr)] <- (fleet$technology_market_share[techno_to_optimize,as.character(first_proj_yr-1),drop=FALSE] / sum(fleet$technology_market_share[techno_to_optimize,as.character(first_proj_yr-1)])) %*% (matrix(sapply(sum(fleet$technology_market_share[techno_to_optimize,as.character(first_proj_yr-1)])+techno_variable*0:(last_yr-first_proj_yr),function(x)ifelse(x<=1,x,1)),nrow=1,ncol=last_yr-first_proj_yr+1))
    #Assumption: Proportional reductions on all other technologies
    fleet$technology_market_share[!rownames(fleet$technology_market_share)%in%techno_to_optimize,as.character(first_proj_yr:last_yr)] <- (fleet$technology_market_share[!rownames(fleet$technology_market_share)%in%techno_to_optimize,as.character(first_proj_yr-1),drop=FALSE] / sum(fleet$technology_market_share[!rownames(fleet$technology_market_share)%in%techno_to_optimize,as.character(first_proj_yr-1)])) %*% (1-colSums(fleet$technology_market_share[techno_to_optimize,as.character(first_proj_yr:last_yr),drop=FALSE]))
    
  } else {
    fleet$technology_market_share[,as.character(first_proj_yr:last_yr)] <- fleet$technology_market_share[,as.character(first_proj_yr-1)]
  }
 return(fleet)
}
