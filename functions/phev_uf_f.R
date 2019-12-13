#'phev_uf_f
#'
#'Calculate the utility factor of PHEV from range
#'@export
phev_uf_f <- function(range_km){
  #Range should be in miles
  range_miles<-range_km*0.6213712
  uf=-7.73E-09*range_miles^4+2.63E-06*range_miles^3-3.7E-04*range_miles^2+2.66E-02*range_miles
  return(uf)
}
