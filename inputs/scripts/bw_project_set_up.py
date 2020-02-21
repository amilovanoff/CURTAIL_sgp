# -*- coding: utf-8 -*-
"""
Project set-up
Created on Mon Dec 30 14:34:24 2019

@author: Alex Milovanoff
"""
#Library management
#Install new libraries
pip install docplex
conda install astropy


#Installation of ecoinvent database
import brightway2 as bw
#Create or setup project directory
bw.projects.set_current('singapore_fleet_model')
#Setup the project with brightway component (flow and LCIA methods)
bw.bw2setup()
#Install ecoInvent database. First step, export .7z file of ecoinvent into folder of ecospold files.
#fpei is the path to the datasets inside the folder
fpei = "C:/Users/Alex Milovanoff/OneDrive - University of Toronto/Data/ecoinvent/ecoinvent 3.6_cut-off_ecoSpold02/datasets"
ei = bw.SingleOutputEcospold2Importer(fpei, 'ecoinvent 3.6 cutoff')
ei.apply_strategies()
ei.statistics()
ei.write_database()
bw.databases
