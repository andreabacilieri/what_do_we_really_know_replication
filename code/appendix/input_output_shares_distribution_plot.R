################################################################################
# This script produces Figure C.3: Empirical pdf of the input and the output shares for Ecuador and Hungary over time

# INPUTS:
# Data:
# from data/analysis/
#     - ecuador_weights_input_output_shares.csv
#     - hungary_weights_input_output_shares.csv
# from code/utils_plots/
#     - set_size.R

# OUTPUT:
# to results/figures/
# (1) C3_fig_input_output_shares.pdf

################################################################################

# ------------------------------------------------------------------------------
#  Set environment/parameters
# ------------------------------------------------------------------------------

rm(list = ls())
library(data.table)
library(ggplot2)
library(ggpubr)
library(latex2exp)
library(patchwork)
library(scales) # for tick labels format


# Rootfolder
rootfolder <- strsplit(rstudioapi::getSourceEditorContext()$path, '/code')[[1]][1]
# Folder to store output
dirOutput <- file.path(rootfolder, 'results', 'figures')
# Folder for the input data
dirdata <- file.path(rootfolder, 'data', 'analysis')

# For plots
width_LaTeX <- 418.25368  # in pt
function_name <- paste(rootfolder, 'code', 'utils_plots', sep = .Platform$file.sep)
function_name <- paste(function_name, 'set_size.R', sep = .Platform$file.sep)
source(function_name)


# ------------------------------------------------------------------------------
#  Load data
# ------------------------------------------------------------------------------

# Hungary
filename <- file.path(dirdata, 'hungary', 'hungary_weights_input_output_shares.csv')
df_h <- fread(filename)
df_h <- df_h[, list(input_share, output_share, year)]

# Ecuador
filename <- file.path(dirdata, 'ecuador', 'ecuador_weights_input_output_shares.csv')
df_ec <- fread(filename)
df_ec <- df_ec[, list(input_share, output_share, year)]


# ------------------------------------------------------------------------------
#  Get PDF for log spaced bins
# ------------------------------------------------------------------------------

nbins <- 200

## HUNGARY INPUT SHARES ##
years <- sort(unique(df_h$year))
BREAKS <- hist(log10(df_h[input_share > 0]$input_share), plot = F, breaks = nbins)$breaks
df_dens_input_shares <- data.table(country = NA,
                                   year = NA,
                                   mids = NA,
                                   counts = NA)

for(yr in years){

  y2 <- log10(df_h[year == yr & input_share > 0]$input_share)
  hist_ <- hist(y2, breaks = BREAKS, plot = F)

  df_ <- data.table(country = 'Hungary',
                    year = yr,
                    mids = 10^(hist_$mids),
                    counts = hist_$counts)

  df_dens_input_shares <- rbind(df_dens_input_shares, df_)
}

df_dens_input_shares <- na.omit(df_dens_input_shares)

## HUNGARY OUTPUT SHARES ##
BREAKS <- hist(log10(df_h[output_share > 0]$output_share), plot = F, breaks = nbins)$breaks
df_dens_output_shares <- data.table(country = NA,
                                    year = NA,
                                    mids = NA,
                                    counts = NA)

for(yr in years){

  y2 <- log10(df_h[year == yr & output_share > 0]$output_share)
  hist_ <- hist(y2, breaks = BREAKS, plot = F)

  df_ <- data.table(country = 'Hungary',
                    year = yr,
                    mids = 10^(hist_$mids),
                    counts = hist_$counts)

  df_dens_output_shares <- rbind(df_dens_output_shares, df_)
}

## ECUADOR INPUT SHARES ##
years = sort(unique(df_ec$year))

BREAKS <- hist(log10(df_ec[input_share > 0]$input_share), plot = F, breaks = nbins)$breaks
for(yr in years){

  y2 <- log10(df_ec[year == yr & input_share > 0]$input_share)
  hist_ <- hist(y2, breaks = BREAKS, plot = F)

  df_ <- data.table(country = 'Ecuador',
                    year = yr,
                    mids = 10^(hist_$mids),
                    counts = hist_$counts)

  df_dens_input_shares <- rbind(df_dens_input_shares, df_)
}

df_dens_input_shares <- na.omit(df_dens_input_shares)

## ECUADOR OUTPUT SHARES ##
BREAKS <- hist(log10(df_ec[output_share > 0]$output_share), plot = F, breaks = nbins)$breaks

for(yr in years){

  y2 <- log10(df_ec[year == yr & output_share > 0]$output_share)
  hist_ <- hist(y2, breaks = BREAKS, plot = F)

  df_ <- data.table(country = 'Ecuador',
                    year = yr,
                    mids = 10^(hist_$mids),
                    counts = hist_$counts)

  df_dens_output_shares <- rbind(df_dens_output_shares, df_)
}

df_dens_output_shares <- na.omit(df_dens_output_shares)


# ------------------------------------------------------------------------------
#  Plot
# ------------------------------------------------------------------------------

# color palette
ncol = length(2007:2021)
pal <- rainbow(n=ncol, start=0, end=5/6)
pal[9] <- "#009966"
par(mar = rep(0, 4))
pie(rep(1, length(pal)), col = pal)

# get y-axes limits
min_y_e <- min(c(min(df_dens_input_shares[country == 'Ecuador',]$counts), min(df_dens_output_shares[country == 'Ecuador',]$counts)))
max_y_e <- max(c(max(df_dens_input_shares[country == 'Ecuador',]$counts), max(df_dens_output_shares[country == 'Ecuador',]$counts)))

min_y_h <- min(c(min(df_dens_input_shares[country == 'Hungary',]$counts), min(df_dens_output_shares[country == 'Hungary',]$counts)))
max_y_h <- max(c(max(df_dens_input_shares[country == 'Hungary',]$counts), max(df_dens_output_shares[country == 'Hungary',]$counts)))

# get x-axes limits
min_x_i <- min(c(min(df_dens_input_shares$mids), min(df_dens_input_shares$mids)))
max_x_i <- max(c(max(df_dens_input_shares$mids), max(df_dens_input_shares$mids)))

min_x_o <- min(c(min(df_dens_output_shares$mids), min(df_dens_output_shares$mids)))
max_x_o <- max(c(max(df_dens_output_shares$mids), max(df_dens_output_shares$mids)))

# Plot
p1 <- ggplot(data = df_dens_input_shares[country == 'Ecuador'],  aes(x = mids, y = counts, color = factor(year))) +

  geom_point(size = 0.8, alpha = 0.3) +

  scale_colour_manual(values = pal) +

  labs(x = TeX(r'(Input share)'), y = TeX(r'(Counts)')) +

  scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)),
                limits = c(min_x_i, max_x_i),
                minor_breaks = 10^(-20:20)) +
  annotation_logticks(color = 'gray', outside = F, sides = "b") +
  scale_y_continuous(labels = label_number(suffix = " K", scale = 1e-3), limits = c(min_y_e, max_y_e)) +

  annotate(geom = "text", x = 10^-9, y = 45000, label = "Ecuador", color = "black") + 

  theme_bw() +

  theme(axis.line = element_line(colour = "gray"),
        text = element_text(family = "serif"),
        panel.border = element_rect(colour = "gray"),
        plot.title = element_text(size = rel(1), vjust = 0, hjust = 0.5),
        axis.title = element_text(size = rel(1)),
        axis.title.y = element_text(size = rel(1), vjust = 0.5),
        axis.text.y = element_text(margin = margin(r = 0)),
        axis.title.x = element_text(size = rel(1), vjust = 0.5),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = 'none',
        legend.title = element_blank(),
        legend.text = element_text(size = 7),
        legend.key = element_rect(fill = 'transparent'),
        legend.key.size = unit(0.2, "in"),
        legend.background = element_rect(fill = F),
        strip.background = element_blank(),
        plot.margin = unit(c(t = 0.02, r = 0.02, b = 0.02, l = 0.02), "cm")) +

  guides(col = guide_legend(ncol = 1, override.aes = list(size = 1, alpha = 1)),
         shape = guide_legend(override.aes = list(size = 1, alpha = 1)))


p2 <- ggplot(data = df_dens_output_shares[country == 'Ecuador'],  aes(x = mids, y = counts, color = factor(year))) +

  geom_point(size = 0.8, alpha = 0.3) +

  scale_colour_manual(values = pal) +

  labs(x = TeX(r'(Output share)'), y = TeX(r'(Counts)')) +

  scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)),
                limits = c(min_x_o, max_x_o),
                minor_breaks = 10^(-20:10)) +
  annotation_logticks(color = 'gray', outside = F, sides = "b") +
  scale_y_continuous(labels = label_comma(), limits = c(min_y_e, max_y_e)) +

  annotate(geom = "text", x = 10^-9, y = 45000, label = "Ecuador", color = "black") + 

  theme_bw() +

  theme(axis.line = element_line(colour = "gray"),
        text = element_text(family = "serif"),
        panel.border = element_rect(colour = "gray"),
        plot.title = element_text(size = rel(1), vjust = 0, hjust = 0.5),
        axis.title = element_text(size = rel(1)),
        axis.title.y = element_text(size = rel(1), vjust = 0.5),
        axis.title.x = element_text(size = rel(1), vjust = 0.5),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = c(0.25, 0.5),
        legend.title = element_blank(),
        legend.text = element_text(size = rel(0.75)),
        legend.key.size = unit(0.2, "in"),
        legend.key.height = unit(0.2, 'in'),
        legend.key.width = unit(0.1, 'in'),
        legend.key = element_rect(fill = 'transparent'),
        legend.background = element_rect(fill = 'transparent'),
        strip.background = element_blank(),
        plot.margin = unit(c(t = 0.02, r = 0.02, b = 0.02, l = 0.02), "cm")) +

  guides(col = guide_legend(ncol = 2, override.aes = list(size = 1, alpha = 1)),
         shape = guide_legend(override.aes = list(size = 1, alpha = 1)))


p3 <- ggplot(data = df_dens_input_shares[country == 'Hungary'],  aes(x = mids, y = counts, color = factor(year))) +

  geom_point(size = 0.8, alpha = 0.3) +

  scale_colour_manual(values = pal[9:15]) +

  scale_shape_manual(values = c(25)) +

  labs(x = TeX(r'(Input share)'), y = TeX(r'(Counts)')) +

  scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)),
                limits = c(min_x_i, max_x_i),
                minor_breaks = 10^(-20:20)) +
  annotation_logticks(color = 'gray', outside = F, sides = "b") +
  scale_y_continuous(labels = label_number(suffix = " K", scale = 1e-3), limits = c(min_y_h, max_y_h)) +

  annotate(geom = "text", x = 10^-9, y = 250000, label = "Hungary", color = "black") +

  theme_bw() +

  theme(axis.line = element_line(colour = "gray"),
        text = element_text(family = "serif"),
        panel.border = element_rect(colour = "gray"),
        plot.title = element_text(size = rel(1), vjust = 0, hjust = 0.5),
        axis.title = element_text(size = rel(1)),
        axis.title.y = element_text(size = rel(1), vjust = 0.5),
        axis.title.x = element_text(size = rel(1), vjust = 0.5),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = 'none',
        legend.title = element_blank(),
        legend.text = element_text(size = 7),
        legend.key = element_rect(fill = 'transparent'),
        legend.key.size = unit(0.2, "in"),
        legend.background = element_rect(fill = F),
        strip.background = element_blank(),
        plot.margin = unit(c(t = 0.02, r = 0.02, b = 0.02, l = 0.02), "cm")) +

  guides(col = guide_legend(ncol = 1, override.aes = list(size = 1, alpha = 1)),
         shape = guide_legend(override.aes = list(size = 1, alpha = 1)))


p4 <- ggplot(data = df_dens_output_shares[country == 'Hungary'],  aes(x = mids, y = counts, color = factor(year))) +

  geom_point(size = 0.8, alpha = 0.3) +

  scale_colour_manual(values = pal[9:15]) +

  scale_shape_manual(values = c(25)) +

  labs(x = TeX(r'(Output share)'), y = TeX(r'(Counts)')) +

  scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)),
                limits = c(min_x_o, max_x_o),
                minor_breaks = 10^(-20:10)) +
  annotation_logticks(color = 'gray', outside = F, sides = "b") +
  scale_y_continuous(labels = label_comma(), limits = c(min_y_h, max_y_h)) +

  annotate(geom = "text", x = 10^-9, y = 250000, label = "Hungary", color = "black") + 

  theme_bw() +

  theme(axis.line = element_line(colour = "gray"),
        text = element_text(family = "serif"),
        panel.border = element_rect(colour = "gray"),
        plot.title = element_text(size = rel(1), vjust = 0, hjust = 0.5),
        axis.title = element_text(size = rel(1)),
        axis.title.y = element_text(size = rel(1), vjust = 2),
        axis.title.x = element_text(size = rel(1), vjust = 0.5),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = c(0.25, 0.5),
        legend.title = element_blank(),
        legend.text = element_text(size = rel(0.75)),
        legend.key.size = unit(0.2, "in"),
        legend.key.height = unit(0.2, 'in'),
        legend.key.width = unit(0.1, 'in'),
        legend.key = element_rect(fill = 'transparent'),
        legend.background = element_rect(fill = 'transparent'),
        strip.background = element_blank(),
        plot.margin = unit(c(t = 0.02, r = 0.02, b = 0.02, l = 0.02), "cm")) +

  guides(col = guide_legend(ncol = 2, override.aes = list(size = 1, alpha = 1)),
         shape = guide_legend(override.aes = list(size = 1, alpha = 1)))


# combine plots
g <- ggarrange(p1 + rremove("xlab") + rremove('x.ticks'),
               p2 + rremove("xlab") + rremove('x.ticks') + rremove("ylab") + rremove('y.ticks') + rremove('y.text'),
               p3,
               p4 + rremove("ylab") + rremove('y.ticks') + rremove('y.text'),
               nrow=2, ncol=2,
               heights = c(1, 1.1), widths = c(1.2, 1)) +

  theme(plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"))


# ------------------------------------------------------------------------------
#  Export
# ------------------------------------------------------------------------------

fig_size <- set_size(width_LaTeX, fraction = 1)
width_l <- fig_size[[1]]
height_l <- fig_size[[2]] + 1.5

filename <- paste(dirOutput, '/C3_fig_input_output_shares.png', sep = '')
ggsave(file = filename, plot = g, width = width_l, height = height_l, unit = 'in', dpi = 600)

print('Figure C.3: Empirical pdf of the input and the output shares for Ecuador and Hungary over time exported.')