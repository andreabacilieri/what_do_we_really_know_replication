################################################################################
# This script produces Figure C.2: 2-D histogram for network expenses and network sales for Ecuador in 2015 and Hungary in 2021.

# INPUTS:
# Data:
# from data/analysis/
#     - ecuador_local_properties.csv
#     - hungary_local_properties.csv
# from code/utils/
#     - fun_ccdf_tls_network.R
# from code/utils_plots/
#     - 2D_hist.R
#     - set_size.R

# OUTPUT:
# to results/figures/appendix/
# (1) C2_fig_in_out_strength_2Dhist_TLSline.pdf

################################################################################


# ------------------------------------------------------------------------------
#  Setup
# ------------------------------------------------------------------------------

rm(list = ls())
library(ggplot2)
library(ggpubr)
library(data.table)
library(tls)
library(latex2exp)

# Rootfolder
rootfolder <- strsplit(rstudioapi::getSourceEditorContext()$path, 'code')[[1]][1]
# Folder to store output
dirOutput <- file.path(rootfolder, 'results', 'figures')
# Folder for the input data
dirdata <- file.path(rootfolder, 'data', 'analysis')

# Function for the total least square regression
source(file.path(rootfolder, 'code', 'utils','fun_ccdf_tls_network.R'))

# For plots
width_LaTeX <- 418.25368  # in pt
source(file.path(rootfolder, 'code', 'utils_plots', 'set_size.R'))
source(file.path(rootfolder, 'code', 'utils_plots', '2D_hist.R'))


# ------------------------------------------------------------------------------
#  Load data
# ------------------------------------------------------------------------------

# Hungary
filename <- file.path(dirdata, 'hungary', 'hungary_local_properties.csv')
df_H <- fread(filename)
df_H$country <- 'Hungary'
df_H <- df_H[, c('outS', 'inS', 'country', 'year')]

# Ecuador
filename <- file.path(dirdata, 'ecuador', 'ecuador_local_properties.csv')
df <- fread(filename)
df$country <- 'Ecuador'
df <- df[, c('outS', 'inS', 'country', 'year')]

df <- data.table(rbind(df, df_H))
rm('df_H')


# ------------------------------------------------------------------------------
#  Plots
# ------------------------------------------------------------------------------

# Get min and max so that all plots have the same x- and y-axes ranges
ww <- which(df$outS > 0 & df$inS > 0)
min_ <- min(df[ww, c("inS", "outS")])
max_ <- max(df[ww, c("inS", "outS")])

## ECUADOR ##
h0 <- plot_2D_hist_loglog(mydata = df[which(df$country == 'Ecuador' & df$year == 2015),],
                          var_y = "inS", var_x = "outS",
                          ylabel = 'In-strength (interm. expenses)',
                          xlabel = 'Out-strength (interm. sales)',
                          plot_label = 'Ecuador',
                          nbreaks = 60, mylabel = "Ecuador, 2015", min_ = min_ , max_ = max_, tlsline = T,
                          plot_x1 = 5*10^(0), plot_y1 = 10^10, # adjust to position dataset
                          plot_x2 = 2*10^2, plot_y2 = 10^(-1)) + # adjust to position TLS and beta coefficient
                          labs(title = 'Ecuador') + 
                          theme(plot.title = element_blank())

## HUNGARY ##
h1 <- plot_2D_hist_loglog(mydata = df[which(df$country == 'Hungary' & df$year == 2021),],
                          var_y = "inS", var_x = "outS",
                          ylabel = 'In-strength (interm. expenses)',
                          xlabel = 'Out-strength (interm. sales)',
                          plot_label = 'Hungary',
                          nbreaks = 60, mylabel = 'Hungary, 2021', min_ = min_ , max_ = max_, tlsline = T,
                          plot_x1 = 5*10^(0), plot_y1 = 10^10, # adjust to position dataset
                          plot_x2 = 1.25*10^1, plot_y2 = 10^(-1)) + # adjust to position TLS and beta coefficient
                          theme(plot.title = element_text(hjust = 0.5))

## COMBINE ##
pfinal <- h0 + (h1 + rremove('ylab') + rremove('y.text')) + plot_layout(guides = "collect") & theme(legend.position = 'bottom')


# ------------------------------------------------------------------------------
#  Export
# ------------------------------------------------------------------------------

fig_size <- set_size(width_LaTeX, fraction = 1)
width_l <- fig_size[[1]]
height_l <- fig_size[[2]]

filename <- file.path(dirOutput, 'C2_fig_in_out_strength_2Dhist.png')

ggsave(plot = pfinal, filename = filename, width = width_l, height = height_l, dpi = 600)

print('Figure C.2: 2-D histogram for network expenses and network sales for Ecuador in 2015 and Hungary in 2021 exported.')


