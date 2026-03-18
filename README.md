# Firm-level production networks: what do we (really) know?

by Andrea Bacilieri, András Borsos, Pablo Astudillo-Estevez, Mads Hoefer and François Lafond

------------------------
## OVERVIEW

This repository contains the code to replicate the empirical results of the article _Firm-level production networks: what do we (really) know?_

This README file provides all the information to replicate the results, i.e., the different datasets used, computing environment, data and coding pipeline, and a brief explanation of the various scripts.

----------------------
## DATA AVAILABILITY

The empirical analysis uses confidential firm-level datasets that are subject to usage licences. Therefore, we cannot disclose or redistribute these datasets. We provide synthetic data to ensure that the code runs properly (see Section Synthetic data), but the results will deviate from the real data.

A detailed description of the data sources and construction is given in Appendix A.2 of the article, available [here](https://www.inet.ox.ac.uk/publications/no-2025-14-firm-level-production-networks-what-do-we-really-know).

------------------------------
## REQUIREMENTS

We use both Python and R. The reproduction package is structured using an RStudio project.

This code is tested on
- Python 3.9.16
- RStudio 2026.01.1+403 (2026.01.1+403)
- R 4.4.1 (2024-06-14)

A range of R libraries needs to be installed, which is done automatically, if needed, when running code/main.R (see Section Code).

--------------------------
## DATA

The project uses the following data sources.
- VAT network data for Hungary and Ecuador, which cannot be shared for confidentiality reasons, but synthetic data are provided.
- Commercial network data from FactSet, which cannot be shared for commercial licensing reasons, but synthetic data are provided.
- In data/literature, we provide the following datasets that we collect from the literature.  
    - Dataset collecting the reporting thresholds by country: thresholds.csv;
    - Dataset of the number of firms and trade relationship for various countries: literature_network_properties.csv; and
    - Random graph statistics generated (with this package) using figures reported in the literature: literature_path_length_random_graphs.csv.
    
- National statistics data for Hungary, which can be found at [KSHStatinfo v40](https://statinfo.ksh.hu/Statinfo/themeSelector.jsp?&lang=en), select "Supply and use tables, IOT". It is provided in folder data/national_statistics/hungary.
- GDP data from the World Bank Development Indicatiors, with series code "NY.GDP.MKTP.CD". It is provided in folder data/national_statistics/P_Data_Extract_From_World_Development_Indicators.

### Synthetic data
The synthetic data and all the other data (that can be made publicly available) neccesary to run the code are available on [OneDrive](https://unioxfordnexus-my.sharepoint.com/:f:/g/personal/sant4949_ox_ac_uk/IgBfssQqClViSLVW7kXgLdFgAbZXMoOijCOxpZzHEHNjUn0?email=francois.lafond%40inet.ox.ac.uk&e=HZBDJB). Once downloaded, replace the data folder in the Git project with the data folder downloaded from OneDrive.

The synthetic datasets have exactly the same column headers as our raw datasets. We generate synthetic network data and industrial codes.

--------------------------
## CODE

The code for reproducing the analyses presented in the article is self-contained and can be executed by running code/main.R. **There is one input that the user needs to specify (see below).** The code/main.R script takes a while to run (< 24 hours), depending on the machine. 

It is useful to distinguish the steps in code/main.R:

0. Install required packages

1. Data cleaning and analysis

	1.a Clean raw proprietary/synthetic data and creates the edge lists

	1.b Calculate global and local properties, shortest paths, input and output shares, influence vectors.

2. Data simulation

	2.a Simulate configuration models for local clustering

	2.b Simulate random graphs for path length

	2.c Calculate power-law exponents

3. Data visualisation

	3.a Create figures and tables that are in the article

All sections are clearly marked in code/main.R, which calls separate scripts from folder code/main_scripts.

Folder code/main_scripts contains functions for 
1. cleaning and analysing each dataset separately (Ecuador, Hungary, FactSet);  
2. simulating random graphs and fitting power-laws; and  
3. visualising the empirical results (the truncation and industrial analyses can be found here).  

All the results in code/main.R are exported to the results folder, with figures going to results/figures and tables to results/tables. 
The code/main_scripts/main_visualisation.R lists which scripts reproduce which figure and table in the article. 
Tables and figures are numbered following the article ordering (e.g., "1_fig_weight_dist.pdf" is Figure 1 in the article). We also provide a results/main.tex file which can be compiled to visualise all the tables and figures produced.

To fit power laws, we use the method of [Voitalov et al. (2019)](https://journals.aps.org/prresearch/abstract/10.1103/PhysRevResearch.1.033034), which can be found in code/code_tail_estimates/utils/tail-estimation.py. This script is provided by the authors on [GitHub](https://github.com/ivanvoitalov/tail-estimation?tab=readme-ov-file). We modified their code by setting a seed for reproducibility, i.e., we added `np.random.seed(42)`.

For further detail, please investigate the scripts called in code/main.R and those within code/main_scripts.


### Setup

The **user needs to specify the path to the python environment** for running the Voitalov at al. (2019) power-law estimation package. If the path is not specified, an error will be raised.
- _What to do_: in code/main_scripts/main_sim.R, change the `python_path` in line 34.

The data analysis and simulation depend on the setting of some hyperparameters. The following are the settings used in the article.

- Path length, nPairs = 10^4: we sample 10^4 node pairs for which to calculate the shortest path lengths in the analysis of the empirical data as well as the simulated networks.
- Path length, nSim_pl = 10: we simulate 10 random graphs, for which we then calculate shortest paths. 
- Global and local clustering, nSim = 100:  we simulate 100 random graphs, for which we then calculate the global and local clustering coefficients.
- Influence vector, labour_share = 0.5.

These may be changed in code/main_scripts/main_sim.R and code/main_scripts/main_prepare_[country_name].R. Please refer to Figure 5 and Table 6 for notes on the choice of pairs and simulations.

The data visualisations are also dependent on setting some hyperparameters. The following are the settings used in the article.
- Figure B.1, NBINS = 100: bins for the binned scatter plot. 
    - This can be changed in line 115 in code/main_scripts/main_visualisations.R.

- Truncation analysis, `dict_ <- lwcc_dict`: after deleting the edges with weight below the threshold, it keeps firms in the largest weakly connected component.
    - This can be changed to include all firms in line 96 in code/appendix/truncation_effects.R.


-------------------------
## MAINTAINERS

Andrea Bacilieri, Mads Hoefer, François Lafond

andrea.bacilieri@oxfordmartin.ox.ac.uk   
mads.hoefer@oriel.ox.ac.uk  
francois.lafond@inet.ox.ac.uk 
