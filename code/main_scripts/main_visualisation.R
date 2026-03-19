#################################################################################################################
# Produce the paper's figures and tables
#################################################################################################################
mainrootfolder <- strsplit(rstudioapi::getSourceEditorContext()$path,'/code')[[1]][1]
setwd(mainrootfolder)

## MAIN TEXT ##

#--------------------------------------------------------------------------------------------------
# Table 1: Taxonomy of production network datasets, with examples
#--------------------------------------------------------------------------------------------------
# Hard coded

#--------------------------------------------------------------------------------------------------
# Table 2: Reporting threshold by country
#--------------------------------------------------------------------------------------------------
source(file.path('code', 'datasets', 'thresholds_table.R'))

#--------------------------------------------------------------------------------------------------
# Figure 1: Distribution of the weights for Ecuador and for Hungary over time
#--------------------------------------------------------------------------------------------------
source(file.path('code', 'weighted_network', 'weight_distributions_plot.R'))

#--------------------------------------------------------------------------------------------------
# Figure 2: Number of nodes and average degree over time
#--------------------------------------------------------------------------------------------------
source(file.path('code', 'binary_network', 'average_degree_number_nodes_plot.R'))

#--------------------------------------------------------------------------------------------------
# Table 3: Mean degree and network size (regression results)
#--------------------------------------------------------------------------------------------------
source(file.path('code', 'binary_network', 'average_degree_number_nodes_regression_table.R'))

#--------------------------------------------------------------------------------------------------
# Table 4: Power-law fit of the degree distributions. 
#--------------------------------------------------------------------------------------------------
source(file.path('code', 'binary_network', 'deg_dist_exponents_table.R'))

#--------------------------------------------------------------------------------------------------
# Figure 3: Empirical CCDF of the number of suppliers and customers over time
#--------------------------------------------------------------------------------------------------
source(file.path('code', 'binary_network', 'degree_distributions_plot.R'))

#--------------------------------------------------------------------------------------------------
# Figure 4: 2D histogram for the number of customers and suppliers
#--------------------------------------------------------------------------------------------------
source(file.path('code', 'binary_network', 'in_out_degree_plot.R'))
  
#--------------------------------------------------------------------------------------------------
# Table 5: Assortativity coefficients
#--------------------------------------------------------------------------------------------------
source(file.path('code', 'binary_network', 'assortativity_table.R'))

#--------------------------------------------------------------------------------------------------
# Table 6: Reciprocity, path lengths and clustering
#--------------------------------------------------------------------------------------------------
source(file.path('code', 'binary_network', 'reciprocity_clustering_path_length_table.R'))

#--------------------------------------------------------------------------------------------------
# Figure 5: Distribution of the length of the shortest paths over time
#--------------------------------------------------------------------------------------------------
source(file.path('code', 'binary_network', 'path_length_plot.R'))

#--------------------------------------------------------------------------------------------------
# Table 7: Tail exponents for weighted network quantities
#--------------------------------------------------------------------------------------------------
source(file.path('code', 'weighted_network', 'weighted_network_quantities_tail_exponent_table.R'))

#--------------------------------------------------------------------------------------------------
# Figure 6: 2D histograms for the number of customers (suppliers) and network sales (expenses)
#--------------------------------------------------------------------------------------------------
source(file.path('code', 'weighted_network', 'strength_degree_plot.R'))

#--------------------------------------------------------------------------------------------------
# Table 8: Strength-degree elasticities
#--------------------------------------------------------------------------------------------------
source(file.path('code', 'weighted_network', 'strength_degree_elasticity_table.R'))

#--------------------------------------------------------------------------------------------------
# Figure 7: Distribution of the influence vector over time
#--------------------------------------------------------------------------------------------------
source(file.path('code', 'weighted_network', 'influence_vector_distribution_plot.R'))

#--------------------------------------------------------------------------------------------------
# Table 9: Summary of results
#--------------------------------------------------------------------------------------------------
# Hard coded


## APPENDIX ##

#--------------------------------------------------------------------------------------------------
# Figure A.1: Sectoral composition in the national I-O table at the sector level and 
# in our firm-level dataset (Hungary's network)
#--------------------------------------------------------------------------------------------------
source(file.path('code', 'appendix', 'national_statistics_network_sales_share.R'))

#--------------------------------------------------------------------------------------------------
# Appendix A.2.3, Data: Hungary, Sectoral composition.
#--------------------------------------------------------------------------------------------------
source(file.path('code', 'industrial_analysis', 'number_firms_no_indu_code.R'))

#--------------------------------------------------------------------------------------------------
# Table B.1: Covariance matrix keeping only nodes with pairwise positive values
# Table B.2: Covariance matrix keeping only nodes which have positive values for all four metrics
# Example 1 and 2 in Section B.3.3
#--------------------------------------------------------------------------------------------------
source(file.path('code', 'appendix', 'covariance_matrix_tables.R'))

#--------------------------------------------------------------------------------------------------
# Figure B.2: Empirical CCDF of the number of suppliers and the number of customers
#--------------------------------------------------------------------------------------------------
source(file.path('code', 'appendix', 'degree_distributions_crossing.R'))

#--------------------------------------------------------------------------------------------------
# Figure B.3: Binned scatter plots of the conditional relationships among size and in- and out-degrees
#--------------------------------------------------------------------------------------------------
source(file.path('code', 'appendix', 'bin_scatter_degree_size.R'))

#--------------------------------------------------------------------------------------------------
# Figure B.1: Binned scatter plots for the conditional relations
# Function separated out to allow for different number of bins
#--------------------------------------------------------------------------------------------------
source(file.path('code', 'appendix', 'heteroskedasticity_conditional_relations_plot.R'))
make_plot_binscatter_conditional_rel(NBINS = 100)

#--------------------------------------------------------------------------------------------------
# Table C.1: Binary network statistics
#--------------------------------------------------------------------------------------------------
source(file.path('code', 'appendix', 'binary_network_statistics_table.R'))

#--------------------------------------------------------------------------------------------------
# Table C.2: Share of customer-only or supplier-only firms
#--------------------------------------------------------------------------------------------------
source(file.path('code', 'appendix', 'share_customer_supplier_only_table.R'))
 
#--------------------------------------------------------------------------------------------------
# Figure C.1: Histogram of the in- and out-strengths
#--------------------------------------------------------------------------------------------------
source(file.path('code', 'appendix', 'strengths_distributions_plot.R'))

#--------------------------------------------------------------------------------------------------
# Figure C.2: 2D histogram for network expenses and network sales 
#--------------------------------------------------------------------------------------------------
source(file.path('code', 'appendix', 'in_out_strengths_plot.R'))

#--------------------------------------------------------------------------------------------------
# Figure C.3: Empirical pdf of the input and the output shares for Ecuador and Hungary over time
#--------------------------------------------------------------------------------------------------
source(file.path('code', 'appendix', 'input_output_shares_distribution_plot.R'))

#----------------------------------------------------------------------------------------------------------
# Tables of tails exponents: C.3 & C.4 (degrees), C.5 & C.6 (strength), 
# C.7 (weights), C.8 (influence vec)
#---------------------------------------------------------------------------------------------------------
source(file.path('code', 'appendix', 'tails_tables.R'))

#--------------------------------------------------------------------------------------------------
# Figures of truncation effects: D.1 (mean degree), D.2 (assortativity and clustering), 
# D.3 (power-law exponents), D.4 (strength-degree elasticities)
#--------------------------------------------------------------------------------------------------
source(file.path('code', 'appendix', 'truncation_effects.R')) 

#-----------------------------------------------------------------------------------------------------
# Table E.1, E.2: Variance decomposition of key network metrics for Ecuador in 2015, Hungary in 2021
#-----------------------------------------------------------------------------------------------------
source(file.path('code', 'industrial_analysis', 'variance_decomposition.R'))

#-----------------------------------------------------------------------------------------------------
# Table E.3, E.4: Strength–degree elasticities under alternative industry and year fixed-effects 
# specifications for Ecuador and Hungary
#-----------------------------------------------------------------------------------------------------
source(file.path('code', 'industrial_analysis', 'strength_degree_elasticity_table_industry.R'))

#-----------------------------------------------------------------------------------------------------
# Figure E.1: Mean in- and out-strength by industry for Ecuador 2015 and Hungary 2021
#-----------------------------------------------------------------------------------------------------
source(file.path('code', 'industrial_analysis', 'mean_in_out_strength_industry_plot.R'))

#-----------------------------------------------------------------------------------------------------
# E.2: Mean in- and out-degree by industry for Ecuador in 2015 and Hungary in 2021
#-----------------------------------------------------------------------------------------------------
source(file.path('code', 'industrial_analysis', 'mean_in_out_degree_industry_plot.R'))

#-----------------------------------------------------------------------------------------------------
# Figure E.3: TLS estimates for the regression of in-degree on out-degree by industry for
# Ecuador in 2015 and Hungary in 2021
#-----------------------------------------------------------------------------------------------------
source(file.path('code', 'industrial_analysis', 'in_out_degree_tls_industry_plot.R'))

