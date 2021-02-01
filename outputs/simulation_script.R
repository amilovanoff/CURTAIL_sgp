# Scenarios -----------------------------------

#Default output in default scenario. Do for all except functions with implicit attributes
function_attributes <- read.csv("architecture/function_attributes.csv",stringsAsFactors = FALSE)
function_tbc <- setdiff(unique(function_attributes$Function),c(unique(subset(function_attributes,Type=="Implicit")$Function),"rapid_transit_module_f","transport_activity_f","transport_activity_initialize_f","optimization_pathway_f"))
function_tbc <- "transport_veh_pop_f"
do.call(write_def_outputs_f,list(function_tbc=function_tbc,scen_tbc="def"))
do.call(write_def_outputs_f,list(function_tbc="transport_veh_pop_f",scen_tbc="def"))
do.call(write_def_outputs_f,list(function_tbc="transport_activity_f",scen_tbc="def"))

do.call(write_def_outputs_f,list(function_tbc="transport_activity_f",scen_tbc="scen1"))
do.call(write_def_outputs_f,list(function_tbc="transport_veh_pop_f",scen_tbc="scen1"))
do.call(write_def_outputs_f,list(function_tbc="transport_lca_ghg_f",scen_tbc="scen1"))


do.call(write_def_outputs_f,list(function_tbc="transport_fuel_use_f",scen_tbc="scen1"))

do.call(write_def_outputs_f,list(function_tbc="transport_lca_ghg_f",scen_tbc="scen2"))
do.call(write_def_outputs_f,list(function_tbc="transport_lca_ghg_f",scen_tbc="def"))

do.call(write_def_outputs_f,list(function_tbc="optimization_pathway_f",scen_tbc="optimization_travel_demand_2c"))
do.call(write_def_outputs_f,list(function_tbc="optimization_pathway_f",scen_tbc="optimization_travel_demand_15c"))
do.call(write_def_outputs_f,list(function_tbc="optimization_pathway_f",scen_tbc="optimization_modal_share_2c"))
do.call(write_def_outputs_f,list(function_tbc="optimization_pathway_f",scen_tbc="optimization_modal_share_15c"))
do.call(write_def_outputs_f,list(function_tbc="optimization_pathway_f",scen_tbc="optimization_technology_2c"))
do.call(write_def_outputs_f,list(function_tbc="optimization_pathway_f",scen_tbc="optimization_electricity_2c"))

do.call(write_def_outputs_f,list(function_tbc="transport_lca_ghg_budget_f",scen_tbc="budget"))

do.call(write_simulation_f,list(function_tbc="transport_lca_ghg_f",scen_tbc="optimization_electricity_2c",sim_tbc="sim3",sim_type="continuous"))
do.call(write_simulation_f,list(function_tbc="transport_lca_ghg_f",scen_tbc="electricity",sim_tbc="sim2",sim_type="continuous"))
do.call(write_simulation_f,list(function_tbc="transport_lca_ghg_f",scen_tbc="electricity",sim_tbc="sim4",sim_type="continuous"))


