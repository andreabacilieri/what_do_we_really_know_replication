################################################################################
# This script produces Figure 7: Distribution of the influence vector for Ecuador and Hungary over time

# INPUTS:
# Data:
# from data/analysis/
#     - ecuador/ecuador_influence_vector.csv
#     - hungary/hungary_influence_vector.csv
# from code/utils/
#     - fun_ccdf_tls_network.R
# from code/utils_plots/
#     - set_size.R

# OUTPUT:
# to results/figures/
#     - 7_fig_influence_vector_ccdf.pdf

################################################################################

# ------------------------------------------------------------------------------
#  Set environment/parameters
# ------------------------------------------------------------------------------

rm(list = ls())
library(data.table)
library(ggplot2)
library(latex2exp)
library(patchwork)

# rootfolder
rootfolder <- strsplit(rstudioapi::getSourceEditorContext()$path, 'code')[[1]][1]
# Folder to Store output
dirOutput <- file.path(rootfolder, 'results', 'figures')
# Folder to the Input data
dirdata <- file.path(rootfolder, 'data', 'analysis')

# Get function for calculating empirical CCDF
function_name <- paste(rootfolder, 'code', 'utils', sep = .Platform$file.sep)
function_name <- paste(function_name, 'fun_ccdf_tls_network.R', sep = .Platform$file.sep)
source(function_name)

# For plots
width_LaTeX = 418.25368  # in pt
function_name <- paste(rootfolder, 'code', 'utils_plots', sep = .Platform$file.sep)
function_name <- paste(function_name, 'set_size.R', sep = .Platform$file.sep)
source(function_name)


# ------------------------------------------------------------------------------
#  Load and prepare data
# ------------------------------------------------------------------------------

## INFLUENCE VECTOR ##

# Hungary
filename <- file.path(dirdata, 'hungary', 'hungary_influence_vector.csv')
df_h <- data.table(read.csv(filename))

# Ecuador
filename <- file.path(dirdata, 'ecuador', 'ecuador_influence_vector.csv')
df_e = data.table(read.csv(filename))


## GET CCDF ##
df_dens <- data.table(pagerank = NA,
                      dens = NA,
                      year = NA,
                      country = NA)

# Ecuador
years <- sort(unique(df_e$year))
for(yr in years){

  data <- df_e[year == yr]$pageRank
  data_ = empirical_ccdf(data)
  data = data_$degree
  density_data = data_$ccdf
  df_ <- data.frame(pagerank = data,
                    dens = density_data,
                    year = yr,
                    country = 'Ecuador')

  df_dens <- rbind(df_dens, df_)
}

# Hungary
years <- sort(unique(df_h$year))
for(yr in years){

  data <- df_h[year == yr]$pageRank
  data_ <- empirical_ccdf(data)
  data <- data_$degree
  density_data <- data_$ccdf
  df_ <- data.frame(pagerank = data,
                    dens = density_data,
                    year = yr,
                    country = 'Hungary')

  df_dens <- rbind(df_dens, df_)
}

df_dens <- na.omit(df_dens)
rm('df_')


# ------------------------------------------------------------------------------
#  Plot
# ------------------------------------------------------------------------------

# color palette
ncol = length(2007:2021)
pal <- rainbow(n = ncol, start = 0, end = 5/6)
pal[9] <- "#009966"
par(mar = rep(0, 4))
pie(rep(1, length(pal)), col = pal)


p1 <- ggplot(data = df_dens,  aes(x = pagerank, y = dens, shape = country, color = factor(year))) +

  geom_point(size = 0.8, alpha = 0.3) +

  labs(x = TeX(r'(Influence vector)'), y = TeX(r'($P(X \, \geq \, x)$)')) +

  scale_shape_manual(values = c(3, 6)) +
  scale_colour_manual(values = pal) +

  scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)),
                minor_breaks = 10^(-10:10)) +
  scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)),
                minor_breaks = 10^(-10:10)) +
  annotation_logticks(outside = F, colour = 'gray') +

  theme_bw() +

  theme(axis.line = element_line(colour = "gray"),
        text = element_text(family = "serif"),
        panel.border = element_rect(colour = "gray"),
        plot.title = element_text(size = rel(1), vjust = 0),
        axis.title = element_text(size = rel(1)),
        axis.title.y = element_text(size = rel(1), vjust = 0),
        axis.title.x = element_text(size = rel(1), vjust = 0.5),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = 'right',
        legend.title = element_blank(),
        legend.text = element_text(size = rel(1)),
        legend.key.size = unit(0.2, "in"),
        legend.background = element_rect(fill = F),
        plot.margin = unit(c(t = 0.1, r = 0.1, b = 0.1, l = 0.1 ), "cm")) +

  guides(shape = guide_legend(ncol = 1, override.aes = list(size = 1.5, alpha = 1)),
         colour = guide_legend(ncol = 2, override.aes = list(size = 1, alpha = 1)))


# ------------------------------------------------------------------------------
#  Export
# ------------------------------------------------------------------------------

fig_size <- set_size(width_LaTeX, fraction = 0.8)
width_l <- fig_size[[1]]
height_l <- fig_size[[2]]

filename <- paste(dirOutput, '7_fig_influence_vector.png', sep = .Platform$file.sep)
ggsave(file = filename, plot = p1, width = width_l, height = height_l, unit = 'in', dpi = 600)

print('Figure 7: Distribution of the influence vector exported.')

