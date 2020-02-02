#' vehicle_on_road_emissions_f
#' Function: Calculates the on-road emisisons from a life-cycle perspective of the vehicle
#' @import reshape2
#' @import stringr
#' @export
vehicle_on_road_emissions_f <- function (){
  attribute_f("vehicle_on_road_emissions_f")
  #Functions' outputs
  vehicle_module_f_res <- do.call(fun_res_f,list(fun_name="vehicle_module_f"))
  fleet_ef_dt <- vehicle_module_f_res[["fleet_ef_dt"]]
  transport_activity_f_res <- do.call(fun_res_f,list(fun_name="transport_activity_f"))
  load_factors <- transport_activity_f_res[["transport_load_factors"]]
  mat_load_factors <- acast(subset(load_factors), Mode ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)
  
  #Marix of emission factors
  mat_ef <- acast(subset(fleet_ef_dt,Model_year%in%unique(load_factors$Year)), Mode + Technology + Pollutant ~ Model_year , value.var='Value',fun.aggregate=sum, margins=FALSE)
  #Conversion matrix
  mat_conv <- matrix(vapply(rownames(mat_load_factors),function(x)as.numeric(grepl(x,rownames(mat_ef))),FUN.VALUE=numeric(nrow(mat_ef))),
                             nrow=nrow(mat_ef),
                             ncol=nrow(mat_load_factors),
                             dimnames=list(rownames(mat_ef),rownames(mat_load_factors)))
  #Matrix of emission per PKT
  mat_emission_pkt <- mat_ef / (mat_conv[rownames(mat_ef),rownames(mat_load_factors)] %*% mat_load_factors)
  #Output
  vehicle_emission_pkt <- as.data.frame(mat_emission_pkt,stringsAsFactors = FALSE) %>%
    cbind(Data=rownames(mat_emission_pkt),stringsAsFactors = FALSE) %>% 
    gather("Year","Value",-Data,convert=TRUE) %>%
    cbind(Unit="g/pkt")
  vehicle_emission_pkt[,"Mode"] <- sapply(vehicle_emission_pkt$Data,function(x)str_sub(x,0,str_locate_all(x,"_")[[1]][1,"start"]-1),USE.NAMES = FALSE)
  vehicle_emission_pkt[,"Technology"] <- sapply(vehicle_emission_pkt$Data,function(x)str_sub(x,str_locate_all(x,"_")[[1]][1,"start"]+1,str_locate_all(x,"_")[[1]][2,"start"]-1),USE.NAMES = FALSE)
  vehicle_emission_pkt[,"Pollutant"] <- sapply(vehicle_emission_pkt$Data,function(x)str_sub(x,str_locate_all(x,"_")[[1]][2,"start"]+1,-1),USE.NAMES = FALSE)
  vehicle_emission_pkt$Data <- NULL
  return(list(vehicle_emission_pkt=vehicle_emission_pkt))
}
