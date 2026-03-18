#################################################################################################################################################
# This script produces Figure 1: Distribution of the weights for Ecuador (left) and for Hungary (right) over time

# INPUTS:
# Data:
# from data/analysis/
#     - ecuador/ecuador_weights_input_output_shares.csv
#     - hungary/hungary_weights_input_output_shares.csv
# from code/utils_plots/
#     - set_size.R

# OUTPUT:
# to results/figures/
#     - 1_fig_weight_dist.pdf

################################################################################################################################################


# ------------------------------------------------
#  Set environment/parameters and Get Data
# ------------------------------------------------

rm(list = ls())
library(data.table)
library(ggplot2)
library(ggpubr)
library(lemon)
library(latex2exp)

# rootfolder
rootfolder <- strsplit(rstudioapi::getSourceEditorContext()$path, '/code')[[1]][1]
# Folder to Store output
dirOutput <- file.path(rootfolder, 'results', 'figures')
# Folder to the Input data
dirdata <- file.path(rootfolder, 'data', 'analysis')

# For plots
width_LaTeX <- 418.25368  # in pt
function_name <- paste(rootfolder, 'code', 'utils_plots', sep = .Platform$file.sep)
function_name <- paste(function_name, 'set_size.R', sep = .Platform$file.sep)
source(function_name)


# ------------------------------------------------------------------------------
#  Load data
# ------------------------------------------------------------------------------

## WEIGHTS ##
keep_cols <- c('weight', 'year')

# Ecuador
filename <- file.path(dirdata, 'ecuador', 'ecuador_weights_input_output_shares.csv')
df_e <- fread(filename)
df_e <- df_e[, ..keep_cols]
df_e$year <- as.character((df_e$year))
df_e$country <- 'Ecuador'

# Hungary
filename <- file.path(dirdata, 'hungary', 'hungary_weights_input_output_shares.csv')
df_h <- fread(filename)
df_h <- df_h[, ..keep_cols]
df_h$year <- as.character((df_h$year))
df_h$country <- 'Hungary'


# ------------------------------------------------------------------------------
#  Total weight of the network
# ------------------------------------------------------------------------------
# Ecuador
sum(df_e[year == '2015']$weight)
summary(df_e[year == '2015']$weight)
# Hungary
sum(df_h[year == '2021']$weight)
summary(df_h[year == '2021']$weight)

# ------------------------------------------------------------------------------
#  Weight distribution
# ------------------------------------------------------------------------------

## GET COUNTS AND MIDS ##

# Hungary
# set break to use for all years
BREAKS <- hist(log10(df_h[weight > 0]$weight), plot = F, breaks = 200)$breaks
years <- sort(unique(df_h$year))
df_plot_h <- data.table(country = 'Hungary',
                        year = NA,
                        mids = NA,
                        counts = NA)

for(yr in years){

  y <- log10(df_h[year == yr & weight > 0]$weight)
  hist_ <- hist(y, breaks = BREAKS, plot = F)

  df_ <- data.table(country = 'Hungary',
                    year = yr,
                    mids = 10^(hist_$mids),
                    counts = hist_$counts)

  df_plot_h <- rbind(df_plot_h, df_)
}

df_plot_h <- na.omit(df_plot_h)
rm(df_)


# Ecuador
# set break to use for all years
BREAKS <- hist(log10(df_e[weight > 0]$weight), plot = F, breaks = 200)$breaks
years <- sort(unique(df_e$year))
df_plot_e <- data.table(country = 'Ecuador',
                         year = NA,
                         mids = NA,
                         counts = NA)

for(yr in years){

  y <- log10(df_e[year == yr & weight > 0]$weight)
  hist_ <- hist(y, breaks = BREAKS, plot = F)

  df_ <- data.table(country = 'Ecuador',
                    year = yr,
                    mids = 10^(hist_$mids),
                    counts = hist_$counts)

  df_plot_e <- rbind(df_plot_e, df_)
}

df_plot_e <- na.omit(df_plot_e)
rm(df_)


## PLOT DISTRIBUTION ##

# color palette
ncol = length(2007:2021)
pal <- rainbow(n = ncol, start = 0, end = 5/6)
pal[9] <- "#009966"
par(mar = rep(0, 4))
pie(rep(1, length(pal)), col = pal)

# for vertical barks
threshold_2015_2018 <- 3703703
threshold_2018_2020 <- 370370
t1 <- threshold_2015_2018/1000
t2 <-  threshold_2018_2020/1000

# Ecuador
p1 <- ggplot(data = df_plot_e, aes(x = mids, y = counts, shape = factor(year), color = factor(year))) +

  geom_point(size = 0.8, alpha = 0.4) +
  geom_line(linewidth = 0.2, alpha = 0.5) +
  scale_colour_manual(values = pal) +
  scale_shape_manual(values = c(0, 1, 2, 19, 8, 5, 6, 3, 4, 13, 9, 11)) +

  labs(x = TeX(r'(Weight)'), y = TeX(r'(Counts)')) +

  scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)),
                breaks = 10^(-10:10)[c(T, F)],
                minor_breaks = 10^(-10:10)) +
  scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)),
                breaks = 10^(-10:10),
                minor_breaks = 10^(-10:10)) +
  annotation_logticks(outside = F, colour = 'gray') +

  theme_bw() +

  theme(axis.line = element_line(colour = "gray"),
        text = element_text(family = "serif"),
        panel.border = element_rect(colour = "gray"),
        plot.title = element_text(size = rel(1), vjust = 0, hjust = 0.5),
        axis.title = element_text(size = rel(1)),
        axis.title.y = element_text(size = rel(1), vjust = 0),
        axis.title.x = element_text(size = rel(1), vjust = 0.5),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = c(0.875, 0.65),
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
        legend.title = element_blank(),
        legend.text = element_text(size = rel(0.8)),
        legend.key = element_rect(fill = F),
        legend.key.height = unit(0.75,"lines"),
        legend.key.width = unit(0.5,"lines"),
        legend.key.size = unit(0.3, "in"),
        legend.background = element_rect(fill = F),
        strip.placement = "outside",
        strip.background = element_blank(),
        strip.text = element_text(size = rel(1)),
        plot.margin = unit(c(t = 0.13, r = 0.05, b = 0.05, l = 0.05), "cm")) +

  guides(colour = guide_legend(override.aes = list(alpha = 1),
                             ncol = 1)) +

  annotate(geom="text", x = 2*10^4, y = 6*10^4, label = "Ecuador", color = "black", hjust = "left", size = 3.5)

# Hungary
p2 <- ggplot(data = df_plot_h, aes(x = mids, y = counts, shape = factor(year), color = factor(year))) +

  geom_point(size = 0.8, alpha = 0.4) +
  geom_line(linewidth = 0.2, alpha = 0.5) +
  scale_colour_manual(values = pal[9:15]) +
  scale_shape_manual(values = c(0, 1, 2, 19, 8, 5, 6, 3, 4, 13, 9, 11)) +

  geom_vline(xintercept = t1, color = 'black', linetype = 'dashed', linewidth = 0.4) +
  geom_vline(xintercept = t2, color = 'black', linetype = 'dashed', linewidth = 0.4) +

  labs(x = TeX(r'(Weight)'), y = TeX(r'(Counts)')) +

  scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)),
                breaks = 10^(0:10)[c(T, F)]) +
  scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)),
                breaks = 10^(0:6)[c(T, F)],
                minor_breaks = 10^(-10:10)) +
  annotation_logticks(outside = F, colour = 'gray') +

  annotate(geom="text", x = 1.9*10^2, y = 2*10^(2), label = "Thres. 2018Q3 - 2020Q2", angle = 90, color = "black", hjust = "left", size = 2.2) + # this will need to be adjusted
  annotate(geom="text", x = 1.8*10^3, y = 7*10^(1), label = "Thres. 2015 - 2018Q2", angle = 90, color = "black", hjust = "left", size = 2.2) + # this will need to be adjusted

  theme_bw() +

  theme(axis.line = element_line(colour = "gray"),
        text = element_text(family = "serif"),
        panel.border = element_rect(colour = "gray"),
        plot.title = element_text(size = rel(1), vjust = 0, hjust = 0.5),
        axis.title = element_text(size = rel(1)),
        axis.title.y = element_text(size = rel(1), vjust = 0),
        axis.title.x = element_text(size = rel(1), vjust = 0.5),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = c(0.875, 0.7),
        legend.margin = margin(t = -5, r = 0, b = 0, l = 0),
        legend.title = element_blank(),
        legend.text = element_text(size = rel(0.8)),
        legend.key = element_rect(fill = F),
        legend.key.height = unit(0.75, "lines"),
        legend.key.width = unit(0.5, "lines"),
        legend.key.size = unit(0.3, "in"),
        legend.background = element_rect(fill = F),
        strip.placement = "outside",
        strip.background = element_blank(),
        strip.text = element_text(size = rel(1)),
        plot.margin = unit(c(t = 0.13, r = 0.25, b = 0.05, l = 0.02), "cm")) +

  guides(colour=guide_legend(override.aes = list(alpha = 1),
                             ncol = 1)) +

  annotate(geom="text", x = 2*10^4, y = 5*10^5, label = "Hungary", color = "black", hjust = "left", size = 3.5)

# combine
g <- ggarrange(p1, p2 + rremove('ylab'),
               nrow = 1, ncol = 2,
               widths = c(1, 1))


## EXPORT ##

fig_size <- set_size(width_LaTeX, fraction = 1)
width_l <- fig_size[[1]]
height_l <- fig_size[[2]] - 1

filename <- paste(dirOutput, '/1_fig_weight_dist.png', sep = '')
ggsave(filename, plot = g,  width = width_l, height = height_l, dpi = 600)

print('Figure 1: Distribution of the weights for Ecuador (left) and for Hungary (right) over time exported.')
