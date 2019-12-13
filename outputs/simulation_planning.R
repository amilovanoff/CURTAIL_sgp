#Script to write outputs
source("outputs/functions/write_f.R")

# Scenarios -----------------------------------

#Default output in default scenario. Do for all except functions with implicit attributes
function_attributes <- read.csv("architecture/function_attributes.csv",stringsAsFactors = FALSE)
function_tbc <- setdiff(unique(function_attributes$Function),c(unique(subset(function_attributes,Type=="Implicit")$Function),"fleet_initialize_f"))
do.call(write_def_outputs_f,list(function_tbc=function_tbc,scen_tbc="def"))

# do.call(write_def_outputs_f,list(function_tbc="fleet_emission_target_f",scen_tbc="ssp"))
do.call(write_def_outputs_f,list(function_tbc="fleet_emission_target_f",scen_tbc="ssp"))
do.call(write_def_outputs_f,list(function_tbc="fleet_vint_stock_f",scen_tbc="ssp"))
do.call(write_def_outputs_f,list(function_tbc="fleet_vint_stock_f",scen_tbc="aeo"))
do.call(write_def_outputs_f,list(function_tbc="lca_ef_elec_f",scen_tbc="ssp"))

# do.call(write_def_outputs_f,list(function_tbc="fleet_lca_f",scen_tbc="rebound"))
# do.call(write_def_outputs_f,list(function_tbc="fleet_lca_f",scen_tbc="ev"))
do.call(write_def_outputs_f,list(function_tbc="fleet_lca_f",scen_tbc="seek_bev"))
do.call(write_def_outputs_f,list(function_tbc="fleet_lca_f",scen_tbc="ssp"))

do.call(write_def_outputs_f,list(function_tbc="model_climate_target_f",scen_tbc="seek_bev"))

# Simulations -------------------------------------------------------------

do.call(write_simulation_f,list(function_tbc="vehicle_module_f",scen_tbc="def",sim_tbc="sim5",sim_type="discrete"))
do.call(write_simulation_f,list(function_tbc="vehicle_module_f",scen_tbc="def",sim_tbc="sim6",sim_type="discrete"))
# do.call(write_simulation_f,list(function_tbc="fleet_vint_stock_f",scen_tbc="def",sim_tbc="sim4",sim_type="discrete"))
# 
# do.call(write_simulation_f,list(function_tbc="fleet_fuel_u_f",scen_tbc="def",sim_tbc="sim4",sim_type="discrete"))
# 
# do.call(write_simulation_f,list(function_tbc="fleet_lca_f",scen_tbc="def",sim_tbc="sim4",sim_type="discrete"))
do.call(write_simulation_f,list(function_tbc="fleet_lca_f",scen_tbc="ssp",sim_tbc="sim5",sim_type="discrete"))
do.call(write_simulation_f,list(function_tbc="fleet_lca_f",scen_tbc="ssp",sim_tbc="sim6",sim_type="discrete"))
# do.call(write_simulation_f,list(function_tbc="fleet_lca_f",scen_tbc="ev",sim_tbc="sim1"))
# do.call(write_simulation_f,list(function_tbc="fleet_lca_f",scen_tbc="seek_bev",sim_tbc="sim2",sim_type="continuous"))

sim_tbc <- paste0("sim",c(3))
##Discrete simulations
for (sim in sim_tbc){
  do.call(write_simulation_f,list(function_tbc="model_climate_target_f",scen_tbc="seek_bev",sim_tbc=sim,sim_type="discrete"))
}

do.call(write_simulation_f,list(function_tbc="fleet_lca_f",scen_tbc="ssp",sim_tbc="sim4",sim_type="discrete"))

do.call(write_simulation_f,list(function_tbc="lca_ef_elec_f",scen_tbc="def",sim_tbc="sim1",sim_type="continuous"))



sim_tbc <- paste0("sim",c(1))
##Continuous simulations
for (sim in sim_tbc){
  do.call(write_simulation_f,list(function_tbc="model_climate_target_f",scen_tbc="seek_bev",sim_tbc=sim,sim_type="continuous"))
}

# Single factor sensivity analysis ------------------------------------------------------

sens_a_tbc <- paste0("sens",c(2:5))
for (sens in sens_a_tbc){
  do.call(write_sens_analysis_f,list(sens_tbc=sens,function_tbc="model_climate_target_f",scen_tbc="seek_bev",get_default_attr_val="n"))
}

do.call(write_sens_analysis_f,list(sens_tbc="uncer1",
                                   function_tbc="model_climate_target_f",
                                   scen_tbc="seek_bev",
                                   get_default_attr_val="n"))

do.call(write_sens_analysis_f,list(sens_tbc="uncer2",
                                   function_tbc="model_climate_target_f",
                                   scen_tbc="seek_bev",
                                   get_default_attr_val="n"))


scen_tbc <- paste0("scen",c(1,2,6))
for (scen in scen_tbc){
  do.call(write_sens_analysis_f,list(sens_tbc=scen,
                                     function_tbc="model_climate_target_f",
                                     scen_tbc="seek_bev",
                                     get_default_attr_val="n"))
}

# Multi-factor sensitivity analysis ---------------------------------------

do.call(write_multi_factor_sens_a_f,list(function_to_write="model_climate_target_f",
                                         mf_sens_tbc="mfsens1",
                                         scen_tbc="seek_bev",
                                         sens_tbc="uncer2",
                                         base_folder = "functions"))

do.call(write_multi_factor_sens_a_f,list(function_to_write="model_climate_target_f",
                                         mf_sens_tbc="mfsens2",
                                         scen_tbc="seek_bev",
                                         sens_tbc="uncer2",
                                         base_folder = "functions"))


scen_tbc <- paste0("scen",c(1,2,6))
for (scen in scen_tbc){
  do.call(write_multi_factor_sens_a_f,list(function_to_write="model_climate_target_f",
                                           mf_sens_tbc="mfsens1",
                                           scen_tbc="seek_bev",
                                           sens_tbc=scen,
                                           base_folder = "functions"))
  
  do.call(write_multi_factor_sens_a_f,list(function_to_write="model_climate_target_f",
                                           mf_sens_tbc="mfsens1",
                                           scen_tbc="seek_bev",
                                           sens_tbc=c(scen,"uncer2"),
                                           base_folder = "functions"))
}
