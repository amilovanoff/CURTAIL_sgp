#' fleet_technology_market_share_proj_f
#' 
#' @import modelframework
#' @import reshape2
#' @export
fleet_technology_market_share_proj_f <- function(fleet,car_techno_ms_proj=NA,last_yr=NA){
  attribute_f("fleet_technology_market_share_proj_f")
  first_proj_yr <- min(as.numeric(colnames(fleet$technology_market_share)[is.na(fleet$technology_market_share[1,])]))
  if (fleet$mode!="Private car" | car_techno_ms_proj=="constant"){
    fleet$technology_market_share[,as.character(first_proj_yr:last_yr)] <- fleet$technology_market_share[,as.character(first_proj_yr-1)]
  } else if (car_techno_ms_proj=="ev3030"){
    #Assumption: 30@ of market share in 2030 as BEV
    fleet$technology_market_share["BEV",as.character(first_proj_yr:last_yr)] <- approx(x=c(first_proj_yr-1,last_yr), y=c(fleet$technology_market_share["BEV",as.character(first_proj_yr-1)],0.30) , xout=seq(first_proj_yr,last_yr,1), method = "linear")$y
    #Assumption: Reduction in ICEV-G
    fleet$technology_market_share["ICEV-G",as.character(first_proj_yr:last_yr)] <- fleet$technology_market_share["ICEV-G",as.character(first_proj_yr-1)]+fleet$technology_market_share["BEV",as.character(first_proj_yr-1)]-fleet$technology_market_share["BEV",as.character(first_proj_yr:last_yr)]
    #Assumption: All other technologies constant
    fleet$technology_market_share[is.na(fleet$technology_market_share[,as.character(first_proj_yr)]),as.character(first_proj_yr:last_yr)] <- fleet$technology_market_share[is.na(fleet$technology_market_share[,as.character(first_proj_yr)]),as.character(first_proj_yr-1)]
    }
 return(fleet)
}
