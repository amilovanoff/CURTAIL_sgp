#Input files
lca_process <- get_input_f(input_name = 'lca_process')
conv <- get_input_f(input_name = 'conversion_units')
fuel_specs <- get_input_f(input_name = 'greet_fuel_specs')
EF_fuel_GREET <- get_input_f(input_name = 'EF_fuel_greet')
#Other parameters
greet_cat <- "GHGs"
#Output file
lca_process <- lca_process[lca_process$Source=="GREET",]
ef_greet <- lca_process[lca_process$Source=="GREET",c("Unit","Phase","Process","Source")]
ef_greet[,"Value"] <- 0
#Fill the emission factors from GREET
#Fill env for Fuel production
for (fuel in which(ef_greet$Phase=="Fuel Production" & ef_greet$Source=="GREET")){
    if (ef_greet$Process[fuel]!="Electricity"){
    ef_greet[fuel,"Value"] <- subset(EF_fuel_GREET, Data=="WTP" & LCI==greet_cat)[,lca_process$GREET[fuel]]/10^3*conv["mmBTU","1 BTU"]*as.numeric(fuel_specs[ef_greet$Process[fuel],"LHV"])*conv["gal","1 L"]
  } else if (ef_greet$Process[fuel]=="Electricity") {
    ef_greet[fuel,"Value"] <- subset(EF_fuel_GREET, Data=="WTP" & LCI==greet_cat)[,lca_process$GREET[fuel]]/10^3*conv["mmBTU","1 kWh"]
  }
}
write.csv(ef_greet,"inputs/model/ef_greet.csv",row.names = FALSE)
