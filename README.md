# WallThickness-VesselDiameter

This repo contains the analysis of ....

The workflow of the analysis is divided in n following parts:

1. Preparation phase.
    1.1 Downloading data from ... and loading the database. Script DataBasePrep.R renames columns and create useful variables. 
    1.2 General exploration of the variables, number of species and other plots are checked in the OverviewDataBase.R
    1.3 Scientific names are checked using the taxonstand package with the taxonstand.R script.
2. Statistical analysis.
    2.1 General relationship between vessel diameter and vessel wall and identification of the threshold in which the vessel diameter-cell wall scaling changes are analyzed with the script ... 
    2.2 Linear models predicting simple and double vessel wall thickness bellow the threshold with vesselwallthicknessbelow.R and vessedoublewallthicknessbelow.R scripts, respectively  
    2.3 Preferential associtation betewen vessels with different perofration plate types and ITE types checked with the contingency_table.R script. 
    2.4 After the threshold
    2.5 Extracting climatic and ecologic information from the species to support the functional patterns identified using the ecological_analysis.R script. 