##########################################################################################################################
# This script produces Figure 4: 2-D histogram for the number of customers and suppliers

# INPUTS:
# Data:
# from data/analysis/
#     - ecuador_local_properties.csv
#     - factset_local_properties.csv
#     - hungary_local_properties.csv
# from code/utils/
#     - fun_ccdf_tls_network.R
# from code/utils_plots/
#     - set_size.R
#     - 2D_hist.R

# OUTPUT:
# to results/figures/
# (1) 4_fig_in_out_degree_2Dhist.pdf

#######################################################################################################################

# ------------------------------------------------
#  Set environment/parameters
# ------------------------------------------------

rm(list = ls())
library(ggplot2)
library(data.table)
library(patchwork)
library(tls)
library(latex2exp)
library(ggpubr)


# rootfolder
rootfolder <- strsplit(rstudioapi::getSourceEditorContext()$path,'/code')[[1]][1]
# Folder to Store output
dirOutput <- file.path(rootfolder, 'results', 'figures')
# Folder to the Input data
dirdata <- file.path(rootfolder, 'data', 'analysis')

# Get function for the total least square regression
source(file.path(rootfolder, 'code', 'utils', 'fun_ccdf_tls_network.R'))

# For plots
width_LaTeX = 418.25368  # in pt
source(file.path(rootfolder, 'code', 'utils_plots', 'set_size.R'))
source(file.path(rootfolder, 'code', 'utils_plots', '2D_hist.R'))


# ------------------------------------------------
# Load data
# ------------------------------------------------

## FACTET ##
filename <- file.path(dirdata, 'factset', 'factset_local_properties.csv')
df_f <- fread(filename)
df_f$country <- 'FactSet'
df_f <- df_f[, c('outD', 'inD', 'country', 'year')]

## HUNGARY ##
filename <- file.path(dirdata, 'hungary', 'hungary_local_properties.csv')
df_H <- fread(filename)
df_H$country <- 'Hungary'
df_H <- df_H[, c('outD', 'inD', 'country', 'year')]

## ECUADOR ##
filename <- file.path(dirdata, 'ecuador', 'ecuador_local_properties.csv')
df <- fread(filename)
df$country <- 'Ecuador'
df <- df[, c('outD', 'inD', 'country', 'year')]

df <- data.table(rbind(df, df_H, df_f))
rm('df_H', 'df_f')

df[, inD := as.numeric(inD)]
df[, outD := as.numeric(outD)]
df[, country := as.character(country)]
df[, year := as.integer(year)]


# ------------------------------------------------
# Plot
# ------------------------------------------------

# min and max for each dataset to unify bins across max and min for each dataset
ec_min <- min(df[which(df$country == 'Ecuador' & df$year == 2015 & df$outD > 0 & df$inD > 0), c("inD", "outD")])
ec_max <- max(df[which(df$country == 'Ecuador' & df$year == 2015 & df$outD > 0 & df$inD > 0), c("inD", "outD")])
hu_min <- min(df[which(df$country == 'Hungary' & df$year == 2021 & df$outD > 0 & df$inD > 0), c("inD", "outD")])
hu_max <- max(df[which(df$country == 'Hungary' & df$year == 2021 & df$outD > 0 & df$inD > 0), c("inD", "outD")])
fs_min <- min(df[which(df$country == 'FactSet' & df$year == 2021 & df$outD > 0 & df$inD > 0), c("inD", "outD")])
fs_max <- max(df[which(df$country == 'FactSet' & df$year == 2021 & df$outD > 0 & df$inD > 0), c("inD", "outD")])
min_val <- min(c(ec_min, hu_min, fs_min))
max_val <- max(c(ec_max, hu_max, fs_max))

axis_val <- function(min_val, max_val, pos){
  log_min <- log10(min_val)
  log_max <- log10(max_val)
  log_range <- log_max - log_min
  two_thirds_val_log <- log_min + (pos) * log_range
  two_thirds_val <- 10^two_thirds_val_log
  return(two_thirds_val)
}

x_pos <- 1/3
y1_pos <- 5.5/6
y2_pos <- 5/6

## ECUADOR ##
h0 <- plot_2D_hist_loglog(mydata = df[which(df$country == 'Ecuador' & df$year == 2015),],
                          var_y = "inD", var_x = "outD",
                          ylabel = 'In-degree (n. suppliers)',
                          xlabel = 'Out-degree (n. customers)',
                          plot_label = 'Ecuador',
                          nbreaks = 60, mylabel = "Ecuador, 2015", min_ = min_val, max_= max_val, tlsline = T,
                          plot_x1 = axis_val(min_val, max_val, x_pos), plot_y1 = axis_val(min_val, max_val, y1_pos),
                          plot_x2 = axis_val(min_val, max_val, x_pos), plot_y2 = axis_val(min_val, max_val, y2_pos)) +
  theme(plot.title = element_text(hjust = 0.5))
  

## HUNGARY ##
h1 <- plot_2D_hist_loglog(mydata = df[which(df$country == 'Hungary' & df$year == 2021),],
                          var_y = "inD", var_x = "outD",
                          ylabel = 'In-degree (n. suppliers)',
                          xlabel = 'Out-degree (n. customers)',
                          plot_label = 'Hungary',
                          nbreaks = 60, mylabel = "Hungary, 2021", min_= min_val, max_= max_val, tlsline = T,
                          plot_x1 = axis_val(min_val, max_val, x_pos), plot_y1 = axis_val(min_val, max_val, y1_pos), 
                          plot_x2 = axis_val(min_val, max_val, x_pos), plot_y2 = axis_val(min_val, max_val, y2_pos)) +
  theme(plot.title = element_text(hjust = 0.5))


## FACTSET ##
h2 <- plot_2D_hist_loglog(mydata = df[which(df$country == 'FactSet' & df$year == 2021),],
                          var_y = "inD", var_x = "outD",
                          ylabel = 'In-degree (n. suppliers)',
                          xlabel = 'Out-degree (n. customers)',
                          plot_label = 'FactSet',
                          nbreaks = 60, mylabel = "FactSet, 2021", min_ = min_val , max_ = max_val, tlsline = T,
                          plot_x1 = axis_val(min_val, max_val, x_pos), plot_y1 = axis_val(min_val, max_val, y1_pos), 
                          plot_x2 = axis_val(min_val, max_val, x_pos), plot_y2 = axis_val(min_val, max_val, y2_pos)) +
  theme(plot.title = element_text(hjust = 0.5))


## COMBINE ##
pfinal <- h0 + 
  (h1 + rremove('ylab') + theme(axis.text.y = element_blank())) + 
  (h2 + rremove('ylab') + theme(axis.text.y = element_blank())) +
  plot_layout(guides = "collect") & 
  theme(legend.position = 'bottom', plot.margin = margin(0.1, 0.1, 0, 0.1, "cm"))


# ------------------------------------------------------------------------------
#  Export
# ------------------------------------------------------------------------------

fig_size <- set_size(width_LaTeX, fraction = 1)
width_l <- fig_size[[1]]
height_l <- fig_size[[2]] - 0.35

filename <- paste(dirOutput, '4_fig_in_out_degree_2Dhist.png', sep = .Platform$file.sep)

ggsave(plot = pfinal, filename = filename, device = "png", width = width_l, height = height_l, dpi = 300)

print('Figure 4: 2-D histogram for the number of customers and suppliers exported.')
