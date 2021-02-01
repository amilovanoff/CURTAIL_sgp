#' ef_electricity_f
#' Function: Define the GHG emission Factors of electricity
#' @export
ef_electricity_f <- function(first_yr=NA,last_yr=NA,ef_elec_scen=NA, ef_elec_variable=NA){
  attribute_f("ef_electricity_f")
  #Matrix of emission factors. Unit: kg CO2 / kWh
  mat_ef_elec <- matrix(0,nrow=1,ncol = length(first_yr:last_yr),dimnames = list("EF",first_yr:last_yr))
  #Historical emissions
  first_proj_yr <- 2020
  mat_ef_elec[,as.character(2005:(first_proj_yr-1))] <- 0.472
  #Propsective emissions are defined from scenarios
  if (ef_elec_scen=="constant"){
    mat_ef_elec[,as.character(first_proj_yr:last_yr)] <- mat_ef_elec[,as.character(first_proj_yr-1)]
  } else if(ef_elec_scen=="half"){
    mat_ef_elec[,as.character(first_proj_yr:last_yr)] <- mat_ef_elec[,as.character(first_proj_yr-1)]/2
  } else if(ef_elec_scen=="transition_renewable"){
    mat_ef_elec[,as.character(first_proj_yr:last_yr)] <- approx(x=c(first_proj_yr,2035,last_yr),y=c(mat_ef_elec[,as.character(first_proj_yr-1)],0.05,0),method="linear",xout=first_proj_yr:last_yr,rule=2)$y
  } else if(ef_elec_scen=="renewable"){
    mat_ef_elec[,as.character(first_proj_yr:last_yr)] <- 0
  } else if(ef_elec_scen=="optimization"){
    #Optimization: ef_elec_variable is an adjustement factor of the original value
    mat_ef_elec[,as.character(first_proj_yr:last_yr)] <- mat_ef_elec[,as.character(first_proj_yr-1)]*(1-ef_elec_variable)
  }
  #Format
  ef_elec_dt <- as.data.frame(mat_ef_elec) %>% 
    gather("Year","Value",convert=TRUE) %>% 
    cbind(Unit="kg CO2 eq/kWh",stringsAsFactors = FALSE)
  return(list(ef_elec_dt=ef_elec_dt))
}
