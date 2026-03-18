################################################################################
# This script produces Figure 6: 2D histograms for the number of customers (or suppliers) and network sales (or expenses)
# for Ecuador in 2015 and Hungary in 2021

# INPUTS:
# Data:
# from data/analysis/
#     - ecuador/ecuador_local_properties.csv
#     - hungary/hungary_local_properties.csv
# from code/utils_plots/
#     - set_size.R
#     - 2D_hist.R

# OUTPUT:
# to results/figures/
#     - 6_fig_strength_deg_corr_2Dhist.png

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
rootfolder <- strsplit(rstudioapi::getSourceEditorContext()$path, '/code')[[1]][1]
# Folder to Store output
dirOutput <- file.path(rootfolder, 'results', 'figures')
# Folder to the Input data
dirdata <- file.path(rootfolder, 'data', 'analysis')

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
df_H <- df_H[, c('outD', 'outS', 'inD', 'inS', 'year')]
df_H$country <- 'Hungary'

# Ecuador
filename <- file.path(dirdata, 'ecuador', 'ecuador_local_properties.csv')
df = fread(filename)
df <- df[, c('outD', 'outS', 'inD', 'inS', 'year')]
df$country <- 'Ecuador'
df <- data.table(rbind(df, df_H))
rm('df_H')


# ------------------------------------------------------------------------------
#  Plot
# ------------------------------------------------------------------------------

min_x_left <- min(df[outD > 0]$outD)*0.8
max_x_left <- max(df[outD > 0]$outD)
min_y_left <- min(df[outS > 0]$outS)
max_y_left <- max(df[outS > 0]$outS)

min_x_right <- min(df[inD > 0]$inD)*0.8
max_x_right <- max(df[inD > 0]$inD)
min_y_right <- min(df[inS > 0]$inS)
max_y_right <- max(df[inS > 0]$inS)

nbins <- 60

h0 <- ggplot() +

  stat_bin2d(data = df[country == 'Ecuador' & outS >0 & outD > 0 & year == 2015, ], 
             aes(x = outD, y = outS, fill = after_stat(count)), 
             bins = nbins) +
  scale_fill_distiller(palette = 'PuBuGn', direction = 1, 
                       name = 'Counts Ecuador out', 
                       trans = 'log10', limits = c(10, NA),
                       labels = scales::label_number(),
                       na.value = NA) +

  scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)),
                breaks = 10^(-1:5),
                minor_breaks = NULL) +
  scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)),
                breaks = 10^(0:9)[c(T, F, F)],
                minor_breaks = 10^(-10:10)) +
  coord_cartesian(xlim = c(min_x_left, max_x_left),
                  ylim = c(min_y_left, max_y_left)) +
  
  annotation_logticks(outside = F, colour = 'gray') +

  labs(x = 'Out-degree (n. customers)', y = 'Out-strength (network sales)', fill = "Counts", tag = 'A') +
  annotate(geom = "text", x = 2*10^3, y = 2*10^(-1), label = 'Ecuador, 2015', color = "black", size = 3) +

  theme_bw() +

  theme(axis.line = element_line(colour = "gray"),
        text = element_text(family = "serif"),
        panel.background = element_rect(fill = NA),
        panel.border = element_rect(colour = "gray"),
        plot.title = element_text(size = rel(0.9), vjust = 0, hjust = 0.5),
        axis.title = element_text(size = rel(0.9)),
        axis.title.y = element_text(size = rel(0.9), vjust = 2),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = rel(0.9), vjust =0.5),
        axis.ticks.x = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text(size = rel(0.9)),
        legend.text = element_text(size = rel(0.9)),
        legend.key.size = unit(0.2, "in"),
        legend.background = element_rect(fill = F),
        strip.placement = "bottom",
        legend.direction = "horizontal",
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        plot.margin = unit(c(0, 10, 0, 0), "pt"),
        plot.tag = element_text(size = rel(1), face = 'bold', vjust = 0)) +

  guides(fill=guide_colourbar(direction = "horizontal",
                              title.position = 'top',
                              label.theme = element_text(angle=45, size = 9),
                              label.hjust = 1,
                              title.theme = element_text(size = 9)))


h1 <- ggplot() +

  stat_bin2d(data = df[country == 'Ecuador' & inS > 0 & inD > 0 & year == 2015, ], 
             aes(x = inD, y = inS, fill = after_stat(count)), bins = nbins) +
  scale_fill_distiller(palette = 'PuBuGn', direction = 1, 
                       name = 'Counts Ecuador in', trans = 'log10', 
                       limits = c(10, NA), na.value = NA, 
                       labels = scales::label_number()) +

  scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)),
                breaks = 10^(-1:5),
                minor_breaks = NULL) +
  scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)),
                breaks = 10^(0:9)[c(T, F, F)],
                minor_breaks = 10^(-10:10)) +
  coord_cartesian(xlim = c(min_x_right, max_x_right),
                  ylim = c(min_y_right, max_y_right)) +
  annotation_logticks(outside = F, colour = 'gray') +

  labs(x = 'In-degree (n. suppliers)', y = 'In-strength (network expenses)', fill = "Counts", tag = 'B') +
  annotate(geom = "text", x = 3*10^2, y = 2*10^(-1), label = 'Ecuador, 2015', color = "black", size = 3) +

  theme_bw() +

  theme(axis.line = element_line(colour = "gray"),
        text = element_text(family = "serif"),
        panel.background = element_rect(fill = NA),
        panel.border = element_rect(colour = "gray"),
        plot.title = element_text(size = rel(0.9), vjust = 0, hjust = 0.5),
        axis.title = element_text(size = rel(0.9)),
        axis.title.y = element_text(size = rel(0.9), vjust = 2),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = rel(0.9), vjust = 0.5),
        axis.ticks.x = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text(size = rel(0.9)),
        legend.text = element_text(size = rel(0.9)),
        legend.key.size = unit(0.2, "in"),
        legend.background = element_rect(fill = F),
        strip.placement = "bottom",
        legend.direction = "horizontal",
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "pt"),
        plot.tag = element_text(size = rel(1), face = 'bold', vjust = 0)) +

  guides(fill = guide_colourbar(direction = "horizontal",
                              title.position = 'top',
                              label.theme = element_text(angle = 45, size = 9),
                              label.hjust = 1,
                              title.theme = element_text(size = 9)))


h2 <- ggplot() +

  stat_bin2d(data = df[country == 'Hungary' & outD >0 & outS >0 & year == 2021, ], 
             aes(x = outD, y = outS, fill = after_stat(count)), bins = nbins) +
  scale_fill_distiller(palette = 'PuBuGn', 
                       direction = 1, 
                       name = "Counts Hungary out", trans = 'log10', 
                       limits = c(10, NA),
                       na.value = NA,
                       labels = scales::label_number()) +

  scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)),
                breaks = 10^(-1:5),
                minor_breaks = NULL) +
  scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)),
                breaks = 10^(0:9)[c(T, F, F)],
                minor_breaks = 10^(-10:10)) +
  coord_cartesian(xlim = c(min_x_left, max_x_left),
                  ylim = c(min_y_left, max_y_left)) +
  annotation_logticks(outside = F, colour = 'gray') +

  labs(x = 'Out-degree (n. customers)', y = 'Out-strength (network sales)', fill = "Counts", tag = 'C') +
  annotate(geom = "text", x = 2*10^3, y = 2*10^(-1), label = 'Hungary, 2021', color = "black", size = 3) +

  theme_bw() +

  theme(axis.line = element_line(colour = "gray"),
        text = element_text(family = "serif"),
        panel.background = element_rect(fill = NA),
        panel.border = element_rect(colour = "gray"),
        plot.title = element_text(size = rel(0.9), vjust = 0, hjust = 0.5),
        axis.title = element_text(size = rel(0.9)),
        axis.title.y = element_text(size = rel(0.9), vjust = 2),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = rel(0.9), vjust = 0.5),
        axis.ticks.x = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text(size = rel(0.9)),
        legend.text = element_text(size = rel(0.9)),
        legend.key.size = unit(0.2, "in"),
        legend.background = element_rect(fill = F),
        strip.placement = "bottom",
        legend.direction = "horizontal",
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        plot.margin = unit(c(-50, 10, 0, 0), "pt"),
        plot.tag = element_text(size = rel(1), face = 'bold', vjust = 0)) +

  guides(fill=guide_colourbar(direction = "horizontal",
                              title.position = 'top',
                              label.theme = element_text(angle = 45, size = 9),
                              label.hjust = 1,
                              title.theme = element_text(size = 9)))


h3 <- ggplot() +

  stat_bin2d(data = df[country == 'Hungary' & inD > 0 & inS > 0 & year == 2021, ], 
             aes(x = inD, y = inS, fill = after_stat(count)), bins = nbins) +
  scale_fill_distiller(palette = 'PuBuGn', direction = 1, 
                       name = "Counts Hungary in", trans = 'log10', 
                       limits = c(10, NA),
                       na.value = NA,
                       labels = scales::label_number()) +

  scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)),
                breaks = 10^(-1:5),
                minor_breaks = NULL) +
  scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)),
                breaks = 10^(0:9)[c(T, F, F)],
                minor_breaks = 10^(-10:10)) +
  coord_cartesian(xlim = c(min_x_right, max_x_right),
                  ylim = c(min_y_right, max_y_right)) +

  annotation_logticks(outside = F, colour = 'gray') +

  labs(x = 'In-degree (n. suppliers)', y = 'In-strength (network expenses)', fill = "Counts", tag = 'D') +
  annotate(geom = "text", x = 3*10^2, y = 2*10^(-1), label = 'Hungary, 2021', color = "black", size = 3) +

  theme_bw() +

  theme(axis.line = element_line(colour = "gray"),
        text = element_text(family = "serif"),
        panel.background = element_rect(fill = NA),
        panel.border = element_rect(colour = "gray"),
        plot.title = element_text(size = rel(0.9), vjust = 0, hjust = 0.5),
        axis.title = element_text(size = rel(0.9)),
        axis.title.y = element_text(size = rel(0.9), vjust = 2),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = rel(0.9), vjust = 0.5),
        axis.ticks.x = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text(size = rel(0.9)),
        legend.text = element_text(size = rel(0.9)),
        legend.key.size = unit(0.2, "in"),
        legend.background = element_rect(fill = F),
        strip.placement = "bottom",
        legend.direction = "horizontal",
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        plot.margin = unit(c(-50, 0, 0, 0), "pt"),
        plot.tag = element_text(size = rel(1), face = 'bold', vjust = 0)) +

  guides(fill=guide_colourbar(direction = "horizontal",
                              title.position = 'top',
                              label.theme = element_text(angle = 45, size = 9),
                              label.hjust = 1,
                              title.theme = element_text(size = 9)))


# Combine
pfinal <- (h0 + h1 + plot_layout(guides = "collect") & theme(legend.position = 'bottom')) / (h2 + h3 + plot_layout(guides = "collect") & theme(legend.position = 'bottom'))


# ------------------------------------------------------------------------------
#  Export
# ------------------------------------------------------------------------------

fig_size <- set_size(width_LaTeX, fraction = 1)
width_l <- fig_size[[1]]
height_l <- fig_size[[2]] + 3

filename <- paste(dirOutput, '/6_fig_strength_deg_corr_2Dhist.png', sep = .Platform$file.sep)

ggsave(plot = pfinal, filename = filename, width = width_l, height = height_l)

print('Figure 6: 2D histograms for the number of customers/supper and network sales/expenses exported.')