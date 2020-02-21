# -*- coding: utf-8 -*-
"""
Created on Wed Jan 29 12:16:03 2020

@author: Alex Milovanoff
"""
#Set up
import brightway2 as bw
import os
import pandas as pd 
wd_path="C:/Users/Alex Milovanoff/GitHub/flame_singapore"
os.chdir(wd_path)
bw.projects.set_current('singapore_fleet_model')
#Search relevant processes in ecoinvent database
db = bw.Database('ecoinvent 3.6 cutoff')
#

act_list = ["bus production",
            "motor scooter production",
            "passenger car production, electric, without battery",
            "passenger car production, petrol/natural gas",
            "passenger car production, diesel"]
#LCIA method
gwp_method =('IPCC 2013', 'climate change', 'GTP 100a')

dtf = pd.DataFrame()

for act_name in act_list:
    act = [act for act in db if act_name in act['name']][0]
    #Calculate LCA
    lca_calc = bw.LCA({act:1},gwp_method)    
    lca_calc.lci()
    lca_calc.lcia()
    #
    tmp_dict = {'Name':act['name'],
                'FU':act['unit'],
                'Score':lca_calc.score,
                'Unit':'kg CO2 eq'}
    tmp_dt = pd.DataFrame(tmp_dict,index=[0])
    dtf = dtf.append(tmp_dt,ignore_index=True)




