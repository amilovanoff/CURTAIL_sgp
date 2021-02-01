#' phev_uf_f
#' Function: Calculate the utility factor of PHEV technologies from their electric driving range
#'@export
phev_uf_f <- function(range_km){
  #Range needs to be converted in miles
  range_miles<-range_km*0.6213712
  #Equation extracted from the GREET model documentation
  uf=-7.73E-09*range_miles^4+2.63E-06*range_miles^3-3.7E-04*range_miles^2+2.66E-02*range_miles
  return(uf)
}
