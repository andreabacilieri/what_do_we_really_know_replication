################################################################################
# This script produces the figures in the truncation appendix.

# INPUTS:
# Data:
# from data/analysis/ecuador/truncation
#   - ecuador_0.*_global_properties_**.csv
#   - ecuador_0.*_influence_vector_**.csv
#   - ecuador_0.*_local_properties_**.csv
#   - ecuador_0.*_shortest_paths_**.csv
#   - ecuador_0.*_weights_input_output_shares_**.csv

# OUTPUT:
# to results/figures/
# (1) D4_truncation_strength_degree_relation_**.pdf
# (2) D2_truncation_pagerank_relation_**.pdf
# (3) D3_truncation_degree_distribution_relation_**.pdf
# (4) D1_mean_degree_vs_threshold_by_country_year_**.pdf

# * in {0.00573952194072274, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95}
# ** in {all, lwcc}; line 96 in this script defines (1) the nodes to keep (lwcc or all nodes), and (2) the suffix in the file name

################################################################################

# ------------------------------------------------------------------------------
#  Set environment/parameters
# ------------------------------------------------------------------------------

rm(list = ls()) 
library(data.table)
library(igraph)
library(binsreg)
library(Matrix)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(patchwork)
library(viridis)

rootfolder <- strsplit(rstudioapi::getSourceEditorContext()$path, 'code')[[1]][1]

# Set the directories
dirdata <- file.path(rootfolder, 'data', 'analysis')
trunc_dirdata <- file.path(rootfolder, 'data', 'analysis', 'ecuador', 'truncation')
dirOutput <- file.path(rootfolder, 'results', 'figures', 'truncation')

# For plots
width_LaTeX = 418.25368  # in pt
function_name <- paste(rootfolder, 'code', 'utils_plots', sep = .Platform$file.sep)
function_name <- paste(function_name, 'set_size.R', sep = .Platform$file.sep)
source(function_name)

# ----------------------------------------------------------------
# Load data
# ----------------------------------------------------------------

# list files matching the pattern
files <- list.files(path = trunc_dirdata, pattern = "ecuador_.*\\.csv", full.names = TRUE)
# define file types
file_types <- c("influence_vector", "weights_input_output_shares", "global_properties", "local_properties", "shortest_paths")
# initialize dictionaries
all_dict <- list()
lwcc_dict <- list()

# initialize dictionaries for each file type
for (file_type in file_types) {
  all_dict[[file_type]] <- list()
  lwcc_dict[[file_type]] <- list()
}

# process each file
for (file in files) {
  # extract the file name without the path
  file_name <- basename(file)
  
  # extract the truncation value, file type and suffix
  parts <- strsplit(file_name, "_")[[1]]
  trunc_value <- as.numeric(parts[2])
  file_type <- paste(parts[3:(length(parts) - 1)], collapse = "_")
  suffix <- sub("\\.csv$", "", parts[length(parts)])
  
  # Load the data
  df <- fread(file)
  
  # add the data to the appropriate dictionary
  if (suffix == "all") {
    all_dict[[file_type]][[as.character(trunc_value)]] <- df
  } else if (suffix == "lwcc") {
    lwcc_dict[[file_type]][[as.character(trunc_value)]] <- df
  }
}

# choosing file input
# if dict <- lwcc_dict, then data input is the lwcc
# if dict <- all_dict, then data input is the whole network
dict_ <- lwcc_dict

# query thresholds
lit_thresholds <- fread(file.path(rootfolder, 'data', 'literature', 'thresholds_cleaned.csv'))
thresh_eq <- lit_thresholds[Country == 'Ecuador', weight_thres_pp]
thresh_h_2021 <- lit_thresholds[Country == 'Hungary' & Year =='2021', weight_thres_pp]
thresh_h_2019 <- lit_thresholds[Country == 'Hungary' & Year =='2018--2020', weight_thres_pp]
thresh_h_2015 <- lit_thresholds[Country == 'Hungary' & Year =='2015--2018', weight_thres_pp]
thresh_bel <- lit_thresholds[Country == 'Belgium', weight_thres_pp]
thresh_cr <- lit_thresholds[Country == 'Costa Rica', weight_thres_pp]
thresh_dr <- lit_thresholds[Country == 'Domin.  Rep. ', weight_thres_pp]
thresh_ch <- lit_thresholds[Country == 'Chile', weight_thres_pp]
thresh_ken <- lit_thresholds[Country == 'Kenya', weight_thres_pp]
thresh_sp <- lit_thresholds[Country == 'Spain', weight_thres_pp]
thresh_tur <- lit_thresholds[Country == 'Turkey', weight_thres_pp]
thresh_es <- lit_thresholds[Country == 'Estonia', weight_thres_pp]

# query truncation values for Ecuador
eq_trunc_vals <- fread(file.path(rootfolder, "data", "analysis", "ecuador", "truncation", "ecuador_truncation_prop_gdppp_conversion.csv"))
eq_trunc_vals[, trunc_gdppp_vals := trunc_gdppp_vals]

eq_trunc_vals$trunc_gdppp_vals

# create data table with thresholds per country and year
thresholds <- data.table(
  country = c('Ecuador', 'Hungary', 'Hungary', 'Hungary', 'Belgium', 'Costa Rica', 'Dominican Republic', 'Chile', 'Kenya', 'Spain', 'Turkey', 'Estonia'),
  year = c('2008-2015', '2021', '2019', '2015', '2002-2014', '2008-2015', '2012-2017', '2003-2011', '2019', '2008-2009', '2010-2014', '2015-2021'),
  threshold = c(thresh_eq, thresh_h_2021, thresh_h_2019, thresh_h_2015, thresh_bel, thresh_cr, thresh_dr, thresh_ch, thresh_ken, thresh_sp, thresh_tur, thresh_es)
)

# define truncation values
trunc_list <- fread(file.path(rootfolder, 'data', 'analysis', 'ecuador', 'truncation', 'ecuador_truncation_prop_gdppp_conversion.csv'))
trunc_list <- trunc_list$trunc_prop_vals

# Iterate over truncation levels and calculate the difference in Nnodes and Nedges
cat("Truncation Level | Nnodes (all_dict) | Nnodes (lwcc_dict) | Nnodes Lost | Nedges (all_dict) | Nedges (lwcc_dict) | Nedges Lost\n")
cat("---------------------------------------------------------------------------------------------------------------\n")
for (trunc_value in trunc_list) {
  trunc_key <- as.character(trunc_value)
  
  # Get Nnodes and Nedges from all_dict and lwcc_dict
  nnodes_all <- all_dict[['global_properties']][[trunc_key]]$Nnodes
  nnodes_lwcc <- lwcc_dict[['global_properties']][[trunc_key]]$Nnodes
  nedges_all <- all_dict[['global_properties']][[trunc_key]]$Nedges
  nedges_lwcc <- lwcc_dict[['global_properties']][[trunc_key]]$Nedges
  
  # Calculate the differences
  nnodes_lost <- nnodes_all - nnodes_lwcc
  nedges_lost <- nedges_all - nedges_lwcc
  
  # Print the results
  cat(sprintf("%15s | %15d | %15d | %12d | %15d | %15d | %12d\n", 
              trunc_key, nnodes_all, nnodes_lwcc, nnodes_lost, nedges_all, nedges_lwcc, nedges_lost))
}


# ------------------------------------------------------------------------------
# Truncation: Strength-degree relation
# ------------------------------------------------------------------------------

b_out_ks <- b_out_sk <- b_in_ks <- b_in_sk <- rep(NA, length(trunc_list))

for(i in 1:length(trunc_list)){
  df_ <- dict_[['local_properties']][[as.character(trunc_list[i])]]

  kin  <- df_$inD
  kout <- df_$outD
  sin  <- df_$inS
  sout <- df_$outS

  ww <- which(kout > 0)
  b_out_sk[i] <- coef(lm(log(sout[ww]) ~ log(kout[ww])))[2]
  b_out_ks[i] <- coef(lm(log(kout[ww]) ~ log(sout[ww])))[2]
  ww <- which(kin > 0)
  b_in_sk[i] <- coef(lm(log(sin[ww]) ~ log(kin[ww])))[2]
  b_in_ks[i] <- coef(lm(log(kin[ww]) ~ log(sin[ww])))[2]
}

# create data frames
df_eq_ks <- data.frame(trunc_list = eq_trunc_vals$trunc_gdppp_vals,
                     b_in_ks = b_in_ks,
                     b_out_ks = b_out_ks,
                     country = 'Ecuador')

df_eq_sk <- data.frame(trunc_list = eq_trunc_vals$trunc_gdppp_vals,
                    b_in_sk = b_in_sk,
                    b_out_sk = b_out_sk,
                    country = 'Ecuador')

# create a data frame for Hungary's values
df_hungary <- fread(file.path(dirdata, 'hungary', 'hungary_local_properties.csv'))
yrs <- c(2021, 2019, 2015)
b_out_ks_hu <- b_out_sk_hu <- b_in_ks_hu <- b_in_sk_hu <- rep(NA, length(yrs))
for(i in 1:length(yrs)){
  df_ <- df_hungary[year == yrs[i]]
  kin  <- df_$inD
  kout <- df_$outD
  sin  <- df_$inS
  sout <- df_$outS

  ww <- which(kout > 0)
  b_out_sk_hu[i] <- coef(lm(log(sout[ww]) ~ log(kout[ww])))[2]
  b_out_ks_hu[i] <- coef(lm(log(kout[ww]) ~ log(sout[ww])))[2]
  ww <- which(kin > 0)
  b_in_sk_hu[i] <- coef(lm(log(sin[ww]) ~ log(kin[ww])))[2]
  b_in_ks_hu[i] <- coef(lm(log(kin[ww]) ~ log(sin[ww])))[2]
}

df_hu_ks <- data.frame(trunc_list = c(thresh_h_2021, thresh_h_2019, thresh_h_2015),
                       b_in_ks = b_in_ks_hu,
                       b_out_ks = b_out_ks_hu,
                       country = 'Hungary')

df_hu_sk <- data.frame(trunc_list = c(thresh_h_2021, thresh_h_2019, thresh_h_2015),
                        b_in_sk = b_in_sk_hu,
                        b_out_sk = b_out_sk_hu,
                        country = 'Hungary')

# create dataframe for literature values
df_lit_sk_ks <- data.frame(
                          country = c('Belgium', 'Chile', 'Costa Rica', 'Turkey', 'Estonia'),
                          year = c('2014', '2018-2019', '2008-2015', '2015', '2021'),
                          trunc_list = c(thresh_bel, thresh_ch, thresh_cr, thresh_tur, thresh_es),
                          b_in_sk = c(NA, NA, 0.89, NA, 1.47),
                          b_out_sk = c(0.77, NA, 1.20, NA, 1.10),
                          b_in_ks = c(NA, 0.45, NA, 0.58, 0.50),
                          b_out_ks = c(NA, 0.42, NA, 0.44, 0.46)
                        )
df_lit_sk <- df_lit_sk_ks[, c('country', 'trunc_list', 'b_in_sk', 'b_out_sk')]
df_lit_ks <- df_lit_sk_ks[, c('country', 'trunc_list', 'b_in_ks', 'b_out_ks')]

df_sk <- rbind(df_eq_sk, df_hu_sk, df_lit_sk)
df_ks <- rbind(df_eq_ks, df_hu_ks, df_lit_ks)

# Convert the data from wide format to long format, excluding the `country` column
df_ks_long <- df_ks %>% 
  pivot_longer(cols = -c(trunc_list, country), names_to = "variable", values_to = "value") %>% 
  mutate(variable = case_when(
    variable == 'b_in_ks' ~ 'In-degree',
    variable == 'b_out_ks' ~ 'Out-degree',
    variable == 'b_in_sk' ~ 'In-degree',
    variable == 'b_out_sk' ~ 'Out-degree'
  ))
df_ks_long$trunc_list <- as.numeric(df_ks_long$trunc_list)
df_ks_long$trunc_list[df_ks_long$trunc_list < 0.02] <- 0.02
df_ks_long$value <- as.numeric(df_ks_long$value)

df_sk_long <- df_sk %>% 
  pivot_longer(cols = -c(trunc_list, country), names_to = "variable", values_to = "value") %>% 
  mutate(variable = case_when(
    variable == 'b_in_ks' ~ 'In-degree',
    variable == 'b_out_ks' ~ 'Out-degree',
    variable == 'b_in_sk' ~ 'In-degree',
    variable == 'b_out_sk' ~ 'Out-degree'
  ))
df_sk_long$trunc_list <- as.numeric(df_sk_long$trunc_list)
df_sk_long$trunc_list[df_sk_long$trunc_list < 0.02] <- 0.02
df_sk_long$value <- as.numeric(df_sk_long$value)

## PLOT ##
# Define custom colors and line types for countries
custom_colors <- c("Hungary" = "blue", "Ecuador" = "black", "Belgium" = "red", "Costa Rica" = "green", 
                   "Chile" = "purple", "Turkey" = "orange", "Estonia" = "lightblue")
custom_linetypes <- c("Ecuador" = "solid", "Hungary" = "solid", "Belgium" = "solid",
                      "Costa Rica" = "solid", "Chile" = "solid", "Turkey" = "solid", "Estonia" = "solid")
custom_shapes <- c("In-degree" = 15, "Out-degree" = 16)

# adding threshold lines for Estonia
segment_thresh_vals <- as.numeric(unlist(strsplit(df_lit_sk_ks$trunc_list[df_lit_sk_ks$country == "Estonia"], "--")))
segment_min <- as.numeric(segment_thresh_vals[1])
segment_max <- as.numeric(segment_thresh_vals[2])

# In-degree plot
p_sk <- ggplot(df_sk_long, aes(x = trunc_list, y = value, colour = country, linetype = country, shape = variable)) +
  geom_line(linewidth = 0.25) +
  geom_point(size = 1.5) +
  geom_segment(data = df_lit_sk_ks[df_lit_sk_ks$country == "Estonia", ],
               aes(x = as.numeric(unlist(strsplit(trunc_list, "--")))[1], 
                   xend = as.numeric(unlist(strsplit(trunc_list, "--")))[2], 
                   y = b_in_sk,
                   yend = b_in_sk),
               inherit.aes = FALSE, 
               color = custom_colors["Estonia"], 
               size = 0.5) +
  geom_point(data = df_lit_sk_ks[df_lit_sk_ks$country == "Estonia", ],
             aes(x = sqrt(segment_min * segment_max), 
                 y = b_in_sk),
             color = custom_colors["Estonia"], 
             size = 1.5, shape = custom_shapes["In-degree"]) +
  geom_segment(data = df_lit_sk_ks[df_lit_sk_ks$country == "Estonia", ],
               aes(x = as.numeric(unlist(strsplit(trunc_list, "--")))[1], 
                   xend = as.numeric(unlist(strsplit(trunc_list, "--")))[2], 
                   y = b_out_sk,
                   yend = b_out_sk),
                inherit.aes = FALSE,  
               color = custom_colors["Estonia"], 
               size = 0.5) +
  geom_point(data = df_lit_sk_ks[df_lit_sk_ks$country == "Estonia", ],
             aes(x = sqrt(segment_min * segment_max), 
                 y = b_out_sk),
             color = custom_colors["Estonia"], 
             size = 1.5, shape = custom_shapes["Out-degree"]) +
  labs(x = "Reporting threshold (% of GDPpp)", y = expression("Elasticity, " * hat(beta)), title = "Strength | Degree") +
  scale_color_manual(values = custom_colors) +
  scale_linetype_manual(values = custom_linetypes) +
  scale_shape_manual(values = custom_shapes) +
  scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)),
                breaks = scales::trans_breaks("log10", function(x) 10^x)) +
  annotation_logticks(sides = "b", outside = FALSE, color = "grey") +
  guides(colour = guide_legend(override.aes = list(shape = NA, linewidth = 1.5))) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        text = element_text(family = "serif"),
        panel.spacing = unit(0.5, "lines"),
        panel.border = element_rect(colour = "grey", fill = NA),
        axis.line = element_line(linewidth = 0.5, colour = "grey"),
        plot.title = element_text(hjust = 0.5),
        axis.ticks.length = unit(-0.25, "cm"),
        axis.ticks = element_line(linewidth = 0.5, colour = "grey"),
        legend.key.size = unit(1, "lines"),
        legend.text = element_text(size = 8),
        panel.grid.minor = element_blank())

# Out-degree plot
p_ks <- ggplot(df_ks_long, aes(x = trunc_list, y = value, colour = country, linetype = country, shape = variable)) +
  geom_line(linewidth = 0.25) +
  geom_point(size = 1.5) +
    geom_segment(data = df_lit_sk_ks[df_lit_sk_ks$country == "Estonia", ],
               aes(x = as.numeric(unlist(strsplit(trunc_list, "--")))[1], 
                   xend = as.numeric(unlist(strsplit(trunc_list, "--")))[2], 
                   y = b_in_ks,
                   yend = b_in_ks),
               inherit.aes = FALSE, 
               color = custom_colors["Estonia"], 
               size = 0.5) +
  geom_point(data = df_lit_sk_ks[df_lit_sk_ks$country == "Estonia", ],
             aes(x = sqrt(segment_min * segment_max), 
                 y = b_in_ks),
             color = custom_colors["Estonia"], 
             size = 1.5, shape = custom_shapes["In-degree"]) +
  geom_segment(data = df_lit_sk_ks[df_lit_sk_ks$country == "Estonia", ],
               aes(x = as.numeric(unlist(strsplit(trunc_list, "--")))[1], 
                   xend = as.numeric(unlist(strsplit(trunc_list, "--")))[2], 
                   y = b_out_ks,
                   yend = b_out_ks),
               inherit.aes = FALSE,  
               color = custom_colors["Estonia"], 
               size = 0.5) +
  geom_point(data = df_lit_sk_ks[df_lit_sk_ks$country == "Estonia", ],
             aes(x = sqrt(segment_min * segment_max), 
                 y = b_out_ks),
             color = custom_colors["Estonia"], 
             size = 1.5, shape = custom_shapes["Out-degree"]) +
  labs(x = "Reporting threshold (% of GDPpp)", y = expression("Elasticity, " * hat(beta)), title = "Degree | Strength") +
  scale_color_manual(values = custom_colors) +
  scale_linetype_manual(values = custom_linetypes) +
  scale_shape_manual(values = custom_shapes) +
  scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)),
                breaks = scales::trans_breaks("log10", function(x) 10^x)) +
  annotation_logticks(sides = "b", outside = FALSE, color = "grey") +
  guides(colour = guide_legend(override.aes = list(shape = NA, linewidth = 1.5))) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        text = element_text(family = "serif"),
        panel.spacing = unit(0.5, "lines"),
        panel.border = element_rect(colour = "grey", fill = NA),
        axis.line = element_line(linewidth = 0.5, colour = "grey"),
        plot.title = element_text(hjust = 0.5),
        axis.ticks.length = unit(-0.25, "cm"),
        axis.ticks = element_line(linewidth = 0.5, colour = "grey"),
        legend.key.size = unit(1, "lines"),
        legend.text = element_text(size = 8),
        panel.grid.minor = element_blank())

## COMBINE ##
pfinal <- p_sk + (p_ks + theme(axis.title.y = element_blank())) + plot_layout(
  guides = "collect") & theme(legend.position = 'bottom', legend.title = element_blank())

## EXPORT ##
fig_size <- set_size(width_LaTeX, fraction = 1)
width_l <- fig_size[[1]]
height_l <- fig_size[[2]]

if (identical(dict_, lwcc_dict)) {
  filename <- paste(dirOutput, 'D4_truncation_strength_degree_relation_lwcc.png', sep = .Platform$file.sep)
} else {
  filename <- paste(dirOutput, 'D4_truncation_strength_degree_relation_all.png', sep = .Platform$file.sep)
}

ggsave(filename, plot = pfinal, width = width_l, height = height_l, dpi = 600)


# ------------------------------------------------------------------------------
# Truncation: PageRank and degree distributions
# ------------------------------------------------------------------------------

pr <- list()
alpha <- rep(NA, length(trunc_list))
for(i in 1:length(trunc_list)){
  df_ <- dict_[['influence_vector']][[as.character(trunc_list[i])]]
  alpha[i] <- fit_power_law(x = df_$pageRank)$alpha - 1
}

# create a data frame
df_plot <- data.frame(
  trunc_list = eq_trunc_vals$trunc_gdppp_vals,
  alpha = alpha,
  country = 'Ecuador')

# create a data frame for Hungary's values
df_hungary <- fread(file.path(dirdata, 'hungary/hungary_influence_vector.csv'))
yrs <- c(2021, 2019, 2015)
alpha_hungary <- rep(NA, length(yrs))
for(i in 1:length(yrs)){
  df_ <- df_hungary[year == yrs[i]]
  alpha_hungary[i] <- fit_power_law(x = df_$pageRank)$alpha - 1
}

df_hungary <- data.frame(
  trunc_list = c(thresh_h_2021, thresh_h_2019, thresh_h_2015),
  alpha = alpha_hungary,
  country = 'Hungary'
  )

# Combine the data frames
df_combined_pr <- rbind(df_plot, df_hungary)
df_combined_pr$trunc_list <- as.numeric(df_combined_pr$trunc_list)
df_combined_pr$alpha <- as.numeric(df_combined_pr$alpha)

# Define custom colors and line types
custom_colors <- c("Hungary" = "blue", "Ecuador" = "black")
custom_shapes <- c("Hungary" = 17, "Ecuador" = 17)
custom_linetypes <- c("Ecuador" = "solid", "Hungary" = "solid")

## PLOT ##
p_pagerank <- ggplot(df_combined_pr, aes(x = trunc_list, y = alpha, color = country, linetype = country, shape = country)) +
  geom_line(linewidth = 0.25) +
  geom_point(size = 1.5) +
  labs(x = "Reporting threshold (% of GDPpp)", y = expression("Power-law exponent, " * hat(gamma)), title = "Influence vector") +
  scale_color_manual(values = custom_colors) +
  scale_shape_manual(values = custom_shapes) +
  scale_linetype_manual(values = custom_linetypes) +
  scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)),
                breaks = scales::trans_breaks("log10", function(x) 10^x)) +
  annotation_logticks(sides = "b", outside = FALSE, color = "grey") +
  guides(colour = guide_legend(override.aes = list(shape = NA, linewidth = 2.5), nrow = 1), 
        linetype = guide_legend(nrow = 1)) +
  theme_bw() +
  theme(text = element_text(family = "serif"),
        panel.border = element_rect(colour = "grey", fill=NA),
        axis.line = element_line(linewidth = 0.5, colour = "grey"),
        plot.title = element_text(hjust = 0.5),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = "transparent"),
        legend.key.height = unit(0.5, "cm"),
        legend.key.size = unit(0.5, "cm"),
        legend.position = 'bottom',
        legend.title = element_blank(),
        legend.text = element_text(size = rel(0.75)),
        axis.ticks.length = unit(-0.25, "cm"),
        axis.ticks = element_line(linewidth = 0.5, colour = "grey"),
        panel.grid.minor = element_blank())

pr <- list()
alpha_kin <- alpha_kout <- rep(NA, length(trunc_list))
for(i in 1:length(trunc_list)){
  df_ <- dict_[['local_properties']][[as.character(trunc_list[i])]]
  alpha_kin[i]   <- fit_power_law(df_$inD)$alpha - 1
  alpha_kout[i]  <- fit_power_law(df_$outD)$alpha - 1
}

# create two data frames: one for in-degree and one for out-degree
df_in <- data.frame(trunc_list = eq_trunc_vals$trunc_gdppp_vals, alpha = alpha_kin, type = "In-degree")
df_out <- data.frame(trunc_list = eq_trunc_vals$trunc_gdppp_vals, alpha = alpha_kout, type = "Out-degree")

# combine the two data frames into one
df_plot <- rbind(df_in, df_out)

df_hungary <- data.frame(
  year = c(2021, 2019, 2015, 2021, 2019, 2015),
  trunc_list = c(thresh_h_2021, thresh_h_2019, thresh_h_2015, thresh_h_2021, thresh_h_2019, thresh_h_2015),
  alpha = c(2.69, 1.83, 1.62, 1.42, 1.62, 1.46),
  type = c(rep("In-degree", 3), rep("Out-degree", 3)))

df_hungary$trunc_list <- df_hungary$trunc_list

# add a new variable to df_hungary
df_hungary$source <- "Hungary"
df_plot$source <- "Ecuador"
df_plot$year <- 2015

# add a new variable to df_hungary and df_plot
df_hungary$group <- paste(df_hungary$type, df_hungary$source)
df_plot$group <- paste(df_plot$type, df_plot$source)

# create dataframe for literature
df_lit <- data.frame(
                    source = c('Chile', 'Costa Rica', 'Dominican Republic'),
                    year = c('2018-2019', '2008-2015', '2012-2017'),
                    trunc_list = c(thresh_ch, thresh_cr, thresh_dr),
                    alpha = c(0.28, 0.58, 0.30, 0.40, 0.73, 0.43),
                    type = c(rep("In-degree", 3), rep("Out-degree", 3))
                  )
df_lit$group <- paste(df_lit$type, df_lit$source)

# combine df_plot and df_hungary
df_combined <- rbind(df_plot, df_hungary)#, df_lit)
df_combined$trunc_list <- as.numeric(df_combined$trunc_list)
df_combined$alpha <- as.numeric(df_combined$alpha)
df_combined$trunc_list[df_combined$trunc_list < 0.02] <- 0.02

# create styles
custom_colors <- c("Hungary" = "blue", "Ecuador" = "black")
custom_shapes <- c("In-degree" = 15, "Out-degree" = 16, "Influence vector" = 17)
custom_linetypes <- c("Ecuador" = "solid", "Hungary" = "solid")

## PLOT ##
p_degreedist <- ggplot(df_combined, aes(x = trunc_list, y = alpha, colour = source, shape = type, linetype = source)) +
        geom_line(linewidth = 0.25) +
        geom_point(size = 1.5) +
        labs(x = "Reporting threshold (% of GDPpp)", y = expression("Power-law exponent, " * hat(gamma)), title = "In- and out-degree") +
        scale_color_manual(values = custom_colors) +
        scale_shape_manual(values = custom_shapes) +
        scale_linetype_manual(values = custom_linetypes) +
        scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)),
                      breaks = scales::trans_breaks("log10", function(x) 10^x)) +
        annotation_logticks(sides = "b", outside = FALSE, color = "grey") +
        guides(colour = guide_legend(override.aes = list(shape = NA, linewidth = 2.5), nrow = 1), 
              linetype = guide_legend(nrow = 1)) +
        theme_bw() +
        theme(text = element_text(family = "serif"),
              panel.border = element_rect(colour = "grey", fill=NA),
              axis.line = element_line(linewidth = 0.5, colour = "grey"),
              plot.title = element_text(hjust = 0.5),
              legend.background = element_rect(fill = "transparent"),
              legend.key = element_rect(fill = "transparent"),
              legend.key.height = unit(0.5, "cm"),
              legend.key.size = unit(0.5, "cm"),
              legend.position = 'bottom',
              legend.title = element_blank(),
              legend.text = element_text(size = rel(0.75)),
              axis.ticks.length = unit(-0.25, "cm"),
              axis.ticks = element_line(linewidth = 0.5, colour = "grey"),
              panel.grid.minor = element_blank())

## COMBINE ##
p_final <- (p_pagerank + theme(legend.position = "bottom")) + 
           (p_degreedist + theme(axis.title.y = element_blank(), legend.position = "bottom")) + 
           plot_layout(guides = "collect") & 
           theme(legend.position = 'bottom', 
                 legend.title = element_blank(),
                 legend.key.size = unit(0.5, "cm"),
                 legend.text = element_text(size = rel(0.75)))

## EXPORT ##
fig_size <- set_size(width_LaTeX, fraction = 1)
width_l <- fig_size[[1]]
height_l <- fig_size[[2]]

if (identical(dict_, lwcc_dict)) {
  filename <- paste(dirOutput, 'D3_truncation_degree_distribution_relation_lwcc.png', sep = .Platform$file.sep)
} else {
  filename <- paste(dirOutput, 'D3_truncation_degree_distribution_relation_all.png', sep = .Platform$file.sep)
}

ggsave(filename, plot = p_final, width = width_l, height = height_l, dpi = 600)
    

# ------------------------------------------------------------------------------
# Truncation: Assortativity and clustering
# ------------------------------------------------------------------------------

pr <- list()
assort <- glob_clust <- loc_clust <- rep(NA, length(trunc_list))
for(i in 1:length(trunc_list)){
  df_glob <- dict_[['global_properties']][[as.character(trunc_list[i])]]
  df_loc <- dict_[['local_properties']][[as.character(trunc_list[i])]]
  assort[i]   <- df_glob$undirected_assort*100
  glob_clust[i]   <- df_glob$clustering_global_emp*100
  loc_clust[i]   <- mean(df_loc$clustering * 100, na.rm = TRUE)
}

# create a new data frame with 'type' column
df_plot <- data.frame(trunc_list = eq_trunc_vals$trunc_gdppp_vals, Assortativity = assort, 
                      glob_Clustering = glob_clust, loc_Clustering = loc_clust, Source = "Ecuador")

# create a data frame for Hungary's values
df_hungary <- data.frame(
  trunc_list = c(thresh_h_2021, thresh_h_2019, thresh_h_2015),
  Assortativity = c(-7.6, -4.4, -5.5),
  glob_Clustering = c(0.5, 1.2, 1.1),
  loc_Clustering = c(19.6, 11.4, 12.9),
  Source = "Hungary")

# merge the two data frames
df_combined <- rbind(df_plot, df_hungary)
df_combined$trunc_list <- as.numeric(df_combined$trunc_list)
df_combined$Assortativity <- as.numeric(df_combined$Assortativity)
df_combined$glob_Clustering <- as.numeric(df_combined$glob_Clustering)
df_combined$loc_Clustering <- as.numeric(df_combined$loc_Clustering)
df_combined$trunc_list[df_combined$trunc_list < 0.02] <- 0.02

# Define custom colors and line types
custom_colors <- c("Ecuador" = "black", "Hungary" = "slateblue1")
custom_linetypes <- c("Ecuador" = "solid", "Hungary" = "solid")
custom_shapes <- c("Ecuador" = 15, "Hungary" = 16)

# create the plots
p_assort <- ggplot(df_combined, aes(x = trunc_list, y = Assortativity, color = Source, linetype = Source, shape = Source)) +
  geom_line(linewidth = 0.25) +
  geom_point(size = 1.5) +
  scale_color_manual(values = custom_colors) +
  scale_linetype_manual(values = custom_linetypes) +
  scale_shape_manual(values = custom_shapes) +
  scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)),
              breaks = scales::trans_breaks("log10", function(x) 10^x)) +
  annotation_logticks(sides = "b", outside = FALSE, color = "grey") +
  labs(x = "Reporting threshold\n(% of GDPpp)", y = "Undirected assortativity") +
  theme_bw() +
  theme(text = element_text(family = "serif"),
        panel.border = element_rect(colour = "grey", fill = NA),
        axis.line = element_line(size = 0.5, colour = "grey"),
        axis.text = element_text(size = rel(0.9)),
        axis.title = element_text(size = 10),
        plot.title = element_text(hjust = 0.5),
        legend.background = element_rect(fill = "transparent"), 
        legend.key = element_rect(fill = "transparent"),
        legend.position = c(0.85, 0.1),
        legend.title = element_blank(),
        axis.ticks.length = unit(-0.25, "cm"),
        axis.ticks = element_line(size = 0.5, colour = "grey"),
        panel.grid.minor = element_blank())

p_glob_cluster <- ggplot(df_combined, aes(x = trunc_list, y = glob_Clustering, color = Source, linetype = Source, shape = Source)) +
  geom_line(linewidth = 0.25) +
  geom_point(size = 1.5) +
  scale_color_manual(values = custom_colors) +
  scale_linetype_manual(values = custom_linetypes) +
  scale_shape_manual(values = custom_shapes) +
  scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)),
              breaks = scales::trans_breaks("log10", function(x) 10^x)) +
  annotation_logticks(sides = "b", outside = FALSE, color = "grey") +
  labs(x = "Reporting threshold\n(% of GDPpp)", y = "Global clustering coef.") +
  theme_bw() +
  theme(text = element_text(family = "serif"),
        panel.border = element_rect(colour = "grey", fill = NA),
        axis.line = element_line(size = 0.5, colour = "grey"),
        axis.text = element_text(size = rel(0.9)),
        axis.title = element_text(size = 10),
        plot.title = element_text(hjust = 0.5),
        legend.background = element_rect(fill = "transparent"), 
        legend.key = element_rect(fill = "transparent"),
        legend.position = c(0.85, 0.1),
        legend.title = element_blank(),
        axis.ticks.length = unit(-0.25, "cm"),
        axis.ticks = element_line(size = 0.5, colour = "grey"),
        panel.grid.minor = element_blank())

p_loc_cluster <- ggplot(df_combined, aes(x = trunc_list, y = loc_Clustering, color = Source, linetype = Source, shape = Source)) +
  geom_line(linewidth = 0.25) +
  geom_point(size = 1.5) +
  scale_color_manual(values = custom_colors) +
  scale_linetype_manual(values = custom_linetypes) +
  scale_shape_manual(values = custom_shapes) +
  scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)),
              breaks = scales::trans_breaks("log10", function(x) 10^x)) +
  annotation_logticks(sides = "b", outside = FALSE, color = "grey") +
  labs(x = "Reporting threshold\n(% of GDPpp)", y = "Average local clustering coef.") +
  theme_bw() +
  theme(text = element_text(family = "serif"),
        panel.border = element_rect(colour = "grey", fill = NA),
        axis.line = element_line(size = 0.5, colour = "grey"),
        axis.text = element_text(size = rel(0.9)),
        axis.title = element_text(size = 10),
        plot.title = element_text(hjust = 0.5),
        legend.background = element_rect(fill = "transparent"), 
        legend.key = element_rect(fill = "transparent"),
        legend.position = c(0.85, 0.1),
        legend.title = element_blank(),
        axis.ticks.length = unit(-0.25, "cm"),
        axis.ticks = element_line(size = 0.5, colour = "grey"),
        panel.grid.minor = element_blank())

## COMBINE ##
p_final <- p_assort + p_glob_cluster + p_loc_cluster + plot_layout(guides = "collect") & theme(legend.position = 'bottom', legend.title = element_blank())

## EXPORT ##
fig_size <- set_size(width_LaTeX, fraction = 1)
width_l <- fig_size[[1]]
height_l <- fig_size[[2]] - 0.5

if (identical(dict_, lwcc_dict)) {
  filename <- paste(dirOutput, 'D2_truncation_assortativity_clustering_relation_lwcc.png', sep = .Platform$file.sep)
} else {
  filename <- paste(dirOutput, 'D2_truncation_assortativity_clustering_relation_all.png', sep = .Platform$file.sep)
}

ggsave(filename, plot = p_final, width = width_l, height = height_l, dpi = 600)


# ------------------------------------------------------------------------------
# Truncation: Mean degree
# ------------------------------------------------------------------------------

# defining input data location
dirData_lit <- file.path(rootfolder, 'data')

## LITERATURE ##
filename <- file.path(dirData_lit, 'literature/literature_network_properties.csv')
df_lit <- fread(filename)
# applying exclusion criteria of no pooled years
df_lit <- df_lit[pooled_yrs == 0, list(Years, N, E, Dataset, fig2_yesorno, data_collection_method)] 
colnames(df_lit) <- c('year', 'Nnodes', 'Nedges', 'country', 'acceleration', 'data_collection_method')
# remove dots from cells
df_lit[, c("Nnodes", "Nedges")] <- data.frame(lapply(df_lit[, c("Nnodes", "Nedges")], function(x) gsub("\\.", "", x))) 
# converting column cells to integers
df_lit[, c("Nnodes", "Nedges")] <- lapply(df_lit[, c("Nnodes", "Nedges")], as.integer)
# create average degree variable
df_lit$k_av <- df_lit$Nedges/df_lit$Nnodes 
# only keep rows with VAT as data_collection_method and existing average degree
df_lit <- df_lit %>% 
  filter(data_collection_method == "VAT" & !is.na(k_av)) %>%
  group_by(country) %>%
  filter(year == max(year)) %>%
  ungroup()
# Drop the 'acceleration' column
setDT(df_lit)
df_lit[, acceleration := NULL]
df_lit <- df_lit[country != "West Bengal"]
df_lit <- merge(df_lit, thresholds[, .(country, threshold)], by = "country", all.x = TRUE)
df_lit <- setcolorder(data.table(df_lit), c('year', 'Nnodes', 'Nedges', 'k_av', 'country', 'data_collection_method'))
df_lit[df_lit$Country != "Estonia", threshold := as.numeric(threshold)]

## HUNGARY ##
filename <- file.path(dirdata, 'hungary/hungary_global_properties.csv')
df_h <- fread(filename)
df_h <- df_h[order(year)]
df_h <- df_h[year %in% c(2021, 2019, 2015), list(year, Nnodes, Nedges)]

df_h$threshold <- c(thresh_h_2015, thresh_h_2019, thresh_h_2021)
df_h$country <- 'Hungary'
df_h$data_collection_method <- 'VAT'
df_h$k_av <- df_h$Nedges / df_h$Nnodes

## ECUADOR ##
nodes <- edges <- k_av <- threshold <- rep(NA, length(trunc_list))
threshold_eq_list <- fread(file.path(dirdata, 'ecuador', 'truncation', 'ecuador_truncation_prop_gdppp_conversion.csv'))
threshold_conversion <- threshold_eq_list$trunc_gdppp_vals
for(i in 1:length(trunc_list)){
  df_ <- dict_[['global_properties']][[as.character(trunc_list[i])]]
  nodes[i] <- df_$Nnodes
  edges[i] <- df_$Nedges
  k_av[i] <- edges[i]/nodes[i]
  threshold[i] <- trunc_list[i]
}

# Create a data table from the lists
df_ec <- data.table(
  Nnodes = nodes,
  Nedges = edges,
  k_av = k_av,
  threshold = threshold_conversion,
  data_collection_method = 'VAT',
  country = 'Ecuador',
  year = 2015
)

df <- rbind(df_h, df_ec, df_lit)
# This puts the countries with a threshold of zero at 0.02%, so they can be shown on the log scale
df$threshold[df$country != "Estonia" & df$threshold < 0.02] <- 0.02
segment_thresh_vals <- as.numeric(unlist(strsplit(df$threshold[df$country == "Estonia"], "--")))
segment_min <- as.numeric(segment_thresh_vals[1])
segment_max <- as.numeric(segment_thresh_vals[2])

df$threshold[df$country != "Estonia"] <- as.numeric(as.character(df$threshold[df$country != "Estonia"]))

# Define a vector of shapes for each country
country_shapes <- c("Hungary" = 15, "Ecuador" = 16, "Belgium" = 17, "Costa Rica" = 3, 
                    "Dominican Republic" = 4, "Kenya" = 18, "Spain" = 6, "Estonia" = 5)
custom_colors <- c("Hungary" = "blue", "Ecuador" = "black", "Belgium" = "red", "Costa Rica" = "green", 
                   "Dominican Republic" = "purple", "Kenya" = "orange", "Spain" = "brown", "Estonia" = "lightblue")

p <- ggplot(df,
            aes(x = as.numeric(threshold), y = k_av, 
                color = country, shape = country, group = country)) +
  geom_line(linewidth = 0.25, show.legend = FALSE) +
  geom_point(size = 2.5) +
  geom_segment(data = df[df$country == "Estonia", ],
               aes(x    = as.numeric(unlist(strsplit(threshold, "--")))[1], 
                   xend = as.numeric(unlist(strsplit(threshold, "--")))[2], 
                   y    = k_av, 
                   yend = k_av),
               color = custom_colors["Estonia"], 
               size = 1) +
  geom_point(data = df[df$country == "Estonia", ],
             aes(x = sqrt(segment_min * segment_max), 
                 y = k_av),
             color = custom_colors["Estonia"], 
             size = 3) +
  scale_color_manual(values = custom_colors) +
  scale_shape_manual(values = country_shapes) +
  scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)),
                breaks = scales::trans_breaks("log10", function(x) 10^x)) +
  scale_y_log10(breaks = c(5, 10, 20, 50),
                labels = scales::label_number(accuracy = 1, scale = 1, big.mark = "", trim = TRUE)) +
  annotation_logticks(sides = "b", outside = FALSE, color = "grey") +
  labs(x = "Reporting threshold (% of GDPpp)", 
       y = "Mean degree") +
  theme_bw() +
  theme(text = element_text(family = "serif"),
        panel.border = element_rect(colour = "grey", fill = NA),
        axis.line = element_line(linewidth = 0.5, colour = "grey"),
        axis.text = element_text(size = rel(1)),
        plot.title = element_text(hjust = 0.5),
        legend.background = element_rect(fill = "transparent"), 
        legend.key = element_rect(fill = "transparent"),
        legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = rel(0.75)),
        axis.ticks.length = unit(-0.25, "cm"),
        axis.ticks = element_line(linewidth = 0.5, colour = "grey"),
        panel.grid.minor = element_blank())

## EXPORT ##
fig_size <- set_size(width_LaTeX, fraction = 1)
width_l <- fig_size[[1]]
height_l <- fig_size[[2]]-0.2

if (identical(dict_, lwcc_dict)) {
  filename <- paste(dirOutput, 'D1_mean_degree_vs_threshold_by_country_year_lwcc.png', sep = .Platform$file.sep)
} else {
  filename <- paste(dirOutput, 'D1_mean_degree_vs_threshold_by_country_year_all.png', sep = .Platform$file.sep)
}

# Save the plot
ggsave(filename, plot = p, width = width_l, height = height_l, dpi = 600)

print('Finished truncation analysis.')

