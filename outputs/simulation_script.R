#Run the model
source("model_setup.R")

#Run functions without implicit attributes in default settings

do.call(write_def_outputs_f,list(function_tbc="transport_veh_pop_f",scen_tbc="def"))
do.call(write_def_outputs_f,list(function_tbc="transport_activity_f",scen_tbc="def"))
do.call(write_def_outputs_f,list(function_tbc="vehicle_module_f",scen_tbc="def"))
do.call(write_def_outputs_f,list(function_tbc="transport_fuel_use_f",scen_tbc="def"))
do.call(write_def_outputs_f,list(function_tbc="transport_lca_ghg_f",scen_tbc="def"))
do.call(write_def_outputs_f,list(function_tbc="vehicle_lca_ghg_f",scen_tbc="def"))
do.call(write_def_outputs_f,list(function_tbc="transport_lca_ghg_budget_f",scen_tbc="def"))

#Run functions without implicit attributes in other settings

do.call(write_def_outputs_f,list(function_tbc="transport_activity_f",scen_tbc="scen1"))
do.call(write_def_outputs_f,list(function_tbc="transport_veh_pop_f",scen_tbc="scen1"))
do.call(write_def_outputs_f,list(function_tbc="transport_lca_ghg_f",scen_tbc="scen1"))
do.call(write_def_outputs_f,list(function_tbc="transport_fuel_use_f",scen_tbc="scen1"))

do.call(write_def_outputs_f,list(function_tbc="transport_lca_ghg_f",scen_tbc="scen2"))

do.call(write_def_outputs_f,list(function_tbc="transport_lca_ghg_budget_f",scen_tbc="budget"))

do.call(write_def_outputs_f,list(function_tbc="optimization_pathway_f",scen_tbc="optimization_travel_demand_2c"))
do.call(write_def_outputs_f,list(function_tbc="optimization_pathway_f",scen_tbc="optimization_travel_demand_15c"))
do.call(write_def_outputs_f,list(function_tbc="optimization_pathway_f",scen_tbc="optimization_modal_share_2c"))
do.call(write_def_outputs_f,list(function_tbc="optimization_pathway_f",scen_tbc="optimization_modal_share_15c"))

do.call(write_simulation_f,list(function_tbc="transport_lca_ghg_f",scen_tbc="optimization_electricity_2c",sim_tbc="sim3",sim_type="continuous"))
do.call(write_simulation_f,list(function_tbc="transport_lca_ghg_f",scen_tbc="electricity",sim_tbc="sim2",sim_type="continuous"))
do.call(write_simulation_f,list(function_tbc="transport_lca_ghg_f",scen_tbc="electricity",sim_tbc="sim4",sim_type="continuous"))


