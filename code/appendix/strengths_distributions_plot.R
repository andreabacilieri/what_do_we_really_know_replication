##########################################################################################################################
# This script produces Figure C.1: Empirical pdf of the in- and out-strengths for Ecuador and Hungary over time

# INPUTS:
# Data:
# from data/analysis/
#     - ecuador_local_properties.csv
#     - hungary_local_properties.csv
# from code/utils_plots/
#     - set_size.R

# OUTPUT:
# to results/figures/appendix/
# (1) C1_fig_strengths_distributions.pdf

#######################################################################################################################

# ------------------------------------------------------------------------------
#  Set environment/parameters
# ------------------------------------------------------------------------------

rm(list = ls())
library(data.table)
library(ggplot2)
library(latex2exp)
library(patchwork)
library(ggpubr)

# Rootfolder
rootfolder <- strsplit(rstudioapi::getSourceEditorContext()$path, 'code')[[1]][1]
# Folder to store output
dirOutput <- file.path(rootfolder, 'results', 'figures')
# Folder for the input data
dirdata <- file.path(rootfolder, 'data', 'analysis')

# For plots
width_LaTeX = 418.25368  # in pt
function_name <- paste(rootfolder, 'code', 'utils_plots', sep=.Platform$file.sep)
function_name <- paste(function_name, 'set_size.R', sep=.Platform$file.sep)
source(function_name)


# ------------------------------------------------------------------------------
#  Get data
# ------------------------------------------------------------------------------

# Ecuador
filename <- file.path(dirdata,'ecuador', 'ecuador_local_properties.csv')
df_ec <-  fread(filename)
df_ec <- df_ec[, c('outS', 'inS', 'year')]

df_inS <- df_ec[, list(year, inS)]
colnames(df_inS) <- c('year', 'strength')
df_outS <- df_ec[, list(year, outS)]
colnames(df_outS) <- c('year', 'strength')
df_inS$country <- 'Ecuador'
df_inS$type <- 'in-strength'
df_outS$country <- 'Ecuador'
df_outS$type <- 'out-strength'
df_strenghts_ec <- data.table(rbind(df_inS, df_outS))
rm(list=c('df_inS', 'df_outS', 'df_ec'))

# Hungary
filename <- file.path(dirdata,'hungary', 'hungary_local_properties.csv')
df_strenghts_h <- fread(filename)
df_strenghts_h <- df_strenghts_h[, c('outS', 'inS', 'year')]
df_strenghts_h$country <- 'Hungary'


# ------------------------------------------------------------------------------
#  Get PDF with counts
# ------------------------------------------------------------------------------

## IN STRENGTH ##
# Hungary
years <- sort(unique(df_strenghts_h$year))
BREAKS <- hist(log10(df_strenghts_h[inS > 0]$inS), plot = F, breaks = 100)$breaks
y  <- log10(df_strenghts_h[year == 2021 & inS > 0]$inS)
HIST <- hist(y, breaks = BREAKS, plot = F)

df_plot_ins <- data.table(country = 'Hungary',
                          year = 2021,
                          mids = 10^(HIST$mids),
                          counts = HIST$counts)

for(yr in years){

  if(yr != 2021){

    y2 <- log10(df_strenghts_h[year == yr & inS > 0]$inS)
    hist_ <- hist(y2, breaks = HIST$breaks, plot = F)

    df_ <- data.table(country = 'Hungary',
                      year = yr,
                      mids = 10^(hist_$mids),
                      counts = hist_$counts)

    df_plot_ins <- rbind(df_plot_ins, df_)

  }
}

# Ecuador
years <- sort(unique(df_strenghts_ec$year))
BREAKS <- hist(log10(df_strenghts_ec[type == 'in-strength' & strength > 0]$strength), plot = F, breaks = 80)$breaks

for(yr in years){

  y2 <- log10(df_strenghts_ec[year == yr & type == 'in-strength' & strength > 0]$strength)
  hist_ <- hist(y2, breaks = BREAKS, plot = F)

  df_ <- data.table(country = 'Ecuador',
                    year = yr,
                    mids = 10^(hist_$mids),
                    counts = hist_$counts)

  df_plot_ins <- rbind(df_plot_ins, df_)
}

## OUT STRENGTH ##
# Hungary
years <- sort(unique(df_strenghts_h$year))
BREAKS <- hist(log10(df_strenghts_h[outS > 0]$outS), plot = F, breaks = 100)$breaks
y  <- log10(df_strenghts_h[year == 2021 & outS > 0]$outS)
HIST <- hist(y, breaks = BREAKS, plot = F)

df_plot_outs <- data.table(country = 'Hungary',
                           year = 2021,
                           mids = 10^(HIST$mids),
                           counts = HIST$counts)

for(yr in years){

  if(yr != 2021){

    y2 <- log10(df_strenghts_h[year == yr & outS > 0]$outS)
    hist_ <- hist(y2, breaks = HIST$breaks, plot = F)

    df_ <- data.table(country = 'Hungary',
                      year = yr,
                      mids = 10^(hist_$mids),
                      counts = hist_$counts)

    df_plot_outs <- rbind(df_plot_outs, df_)

  }
}

# Ecuador
years <- sort(unique(df_strenghts_ec$year))
BREAKS <- hist(log10(df_strenghts_ec[type == 'out-strength' & strength > 0]$strength), plot = F, breaks = 80)$breaks

for(yr in years){

  y2 <- log10(df_strenghts_ec[year == yr & type == 'out-strength' & strength > 0]$strength)
  hist_ <- hist(y2, breaks = BREAKS, plot = F)

  df_ <- data.table(country = 'Ecuador',
                    year = yr,
                    mids = 10^(hist_$mids),
                    counts = hist_$counts)

  df_plot_outs <- rbind(df_plot_outs, df_)

}


# ------------------------------------------------------------------------------
#  Plot
# ------------------------------------------------------------------------------

# color palette
ncol <- length(2007:2021)
pal <- rainbow(n = ncol, start = 0, end = 5/6)
pal[9] <- "#009966"
par(mar = rep(0, 4))
pie(rep(1, length(pal)), col = pal)

# for vertical axes, hungary
threshold_2015_2018 <- 3703703
threshold_2018_2020 <- 370370
t1 <- threshold_2015_2018/1000
t2 <-  threshold_2018_2020/1000

# share limits x axis
xmin_ins <- min(df_strenghts_ec[type == 'in-strength' & strength > 0 & country == 'Ecuador']$strength)
xmin_outs <- min(df_strenghts_ec[type == 'out-strength' & strength > 0 & country == 'Ecuador']$strength)
xmin <- min(c(xmin_ins, xmin_outs))
xmax_ins <- max(df_strenghts_ec[type == 'in-strength' & strength > 0 & country == 'Ecuador']$strength)
xmax_outs <- max(df_strenghts_ec[type == 'out-strength' & strength > 0 & country == 'Ecuador']$strength)
xmax <- max(c(xmax_ins, xmax_outs))

ymin <- min(c(df_plot_ins[country == 'Ecuador' & counts > 0]$counts, df_plot_outs[country == 'Ecuador' & counts > 0]$counts))
ymax <- max(c(df_plot_ins[country == 'Ecuador' & counts > 0]$counts, df_plot_outs[country == 'Ecuador' & counts > 0]$counts))

# plot
p1 <- ggplot(data = df_plot_ins[country == 'Ecuador'], aes(x = mids, y = counts, shape = factor(year), color = factor(year))) +

  geom_point(size = 0.8, alpha = 0.5) +
  geom_line(size = 0.2, alpha = 0.5) +

  scale_colour_manual(values = pal) +
  scale_shape_manual(values = c(0, 1, 2, 19, 8, 5, 6, 3, 4, 13, 9, 11)) +

  labs(x = TeX(r'(In-strength)'), y = TeX(r'(Counts)')) +

  scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)),
                breaks = 10^(-2:10)[c(T, F, F)],
                limits = c(xmin, xmax),
                minor_breaks = 10^(-10:10)) +
  scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)),
                breaks = 10^(-10:10),
                limits = c(ymin, ymax),
                minor_breaks = 10^(-10:10)) +
  annotation_logticks(outside = F, colour = 'gray') +

  annotate(geom = "text", x = 10^9, y = 4.5*10^3, label = "Ecuador", color = "black", size = 3) + 

  theme_bw() +

  theme(axis.line = element_line(colour = "gray"),
        text = element_text(family = "serif"),
        panel.border = element_rect(colour = "gray"),
        plot.title = element_text(size = rel(1), vjust = 0, hjust = 0.5),
        axis.title = element_text(size = rel(1)),
        axis.title.y = element_text(size = rel(1), vjust = -1),
        axis.title.x = element_text(size = rel(1), vjust = 0.5),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = rel(0.5)),
        legend.key.size = unit(0.2, "in"),
        legend.key.height = unit(0.18, 'in'),
        legend.key.width = unit(0.13, 'in'),
        legend.background = element_rect(fill = F),
        strip.placement = "outside",
        strip.background = element_blank(),
        strip.text = element_text(size = rel(1)),
        plot.margin = unit(c(t = 0.02, r = 0.02, b = 0.02, l = 0.02), "cm")) +

  guides(colour = guide_legend(override.aes = list(alpha = 1),
                             ncol = 2))


p2 <- ggplot(data = df_plot_outs[country == 'Ecuador'], aes(x = mids, y = counts, shape = factor(year), color = factor(year))) +

  geom_point(size = 0.8, alpha = 0.5) +
  geom_line(size = 0.2, alpha = 0.5) +

  scale_colour_manual(values = pal) +
  scale_shape_manual(values = c(0, 1, 2, 19, 8, 5, 6, 3, 4, 13, 9, 11)) +

  labs(x = TeX(r'(Out-strength)'), y = TeX(r'(Counts)')) +

  scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)),
                breaks = 10^(-2:10)[c(T, F, F)],
                limits = c(xmin, xmax),
                minor_breaks = 10^(-2:10)) +
  scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)),
                breaks = 10^(-10:3),
                limits = c(ymin, ymax),
                minor_breaks = 10^(-10:10)) +
  annotation_logticks(outside = F, colour = 'gray') +

  annotate(geom = "text", x = 10^9, y = 4.5*10^3, label = "Ecuador", color = "black", size = 3) +

  theme_bw() +

  theme(axis.line = element_line(colour = "gray"),
        text = element_text(family = "serif"),
        panel.border = element_rect(colour = "gray"),
        plot.title = element_text(size = rel(1), vjust = 0, hjust = 0.5),
        axis.title = element_text(size = rel(1)),
        axis.title.y = element_text(size = rel(1), vjust = -1),
        axis.title.x = element_text(size = rel(1), vjust = 0.5),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = c(0.5, 0.3),
        legend.title = element_blank(),
        legend.text = element_text(size = rel(0.5)),
        legend.key.size = unit(0.2, "in"),
        legend.key.height = unit(0.1, 'in'),
        legend.key.width = unit(0.1, 'in'),
        legend.key = element_rect(fill = 'transparent'),
        legend.background = element_rect(fill = 'transparent'),
        strip.placement = "outside",
        strip.background = element_blank(),
        strip.text = element_text(size = rel(1)),
        plot.margin = unit(c(t = 0.02, r = 0.22, b = 0.02, l = 0.02), "cm")) +

  guides(colour = guide_legend(override.aes = list(alpha = 1),
                             ncol = 2))

# share limits x axis
xmin_ins <- min(df_strenghts_h[inS > 0 & country == 'Hungary']$inS)
xmax_ins <- max(df_strenghts_h[inS > 0 & country == 'Hungary']$inS)
xmin_outs <- min(df_strenghts_h[outS > 0 & country == 'Hungary']$outS)
xmax_outs <- max(df_strenghts_h[outS > 0 & country == 'Hungary']$outS)
xmax <- max(c(xmax_ins, xmax_outs))
xmin <- min(c(xmin_ins, xmin_outs))

ymin <- min(c(df_plot_ins[country == 'Hungary' & counts > 0]$counts, df_plot_outs[country == 'Hungary' & counts > 0]$counts))
ymax <- max(c(df_plot_ins[country == 'Hungary' & counts > 0]$counts, df_plot_outs[country == 'Hungary' & counts > 0]$counts))

p3 <- ggplot(data = df_plot_ins[country == 'Hungary'], aes(x = mids, y = counts, shape = factor(year), color = factor(year))) +

  geom_point(size = 0.8, alpha = 0.5) +
  geom_line(size = 0.2, alpha = 0.5) +

  geom_vline(xintercept = t1, color = 'black', linetype = 'dashed', size = 0.4, alpha = 0.7) +
  geom_vline(xintercept = t2, color = 'black', linetype = 'dashed', size = 0.4, alpha = 0.7) +

  scale_colour_manual(values = pal[9:15]) +
  scale_shape_manual(values = c(0, 1, 2, 19, 8, 5, 6, 3, 4, 13, 9, 11)) +

  labs(x = TeX(r'(In-strength)'), y = TeX(r'(Counts)')) +

  scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)),
                breaks = 10^(-2:10)[c(T, F, F)],
                limits = c(xmin, xmax),
                minor_breaks = 10^(-10:10)) +
  scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)),
                breaks = 10^(0:4),
                limits = c(ymin, ymax),
                minor_breaks = 10^(0:10)) +

  annotation_logticks(outside = F, colour = 'gray') +

  annotate(geom = "text", x = 10^9, y = 1.5*10^4, label = "Hungary", color = "black", size = 3) +
 
  annotate(geom = "text", x = 1.9*10^2, y = 10^(1), label = "Thres. 2018Q3 - 2020Q2", angle = 90, color = "black", hjust = "left", size = 1.6) +
  annotate(geom = "text", x = 1.8*10^3, y = 7*10^(0), label = "Thres. 2015 - 2018Q2", angle = 90, color = "black", hjust = "left", size = 1.6) +

  theme_bw() +

  theme(axis.line = element_line(colour = "gray"),
        text = element_text(family = "serif"),
        panel.border = element_rect(colour = "gray"),
        plot.title = element_text(size = rel(1), vjust = 0, hjust = 0.5),
        axis.title = element_text(size = rel(1)),
        axis.title.y = element_text(size = rel(1), vjust = -1),
        axis.title.x = element_text(size = rel(1), vjust = 0.5),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = 'none',
        legend.title = element_blank(),
        legend.text = element_text(size = rel(0.8)),
        legend.key.size = unit(0.2, "in"),
        legend.background = element_rect(fill = F),
        strip.placement = "outside",
        strip.background = element_blank(),
        strip.text = element_text(size = rel(1)),
        plot.margin = unit(c(t = 0.02, r = 0.02, b = 0.02, l = 0.02), "cm")) +

  guides(colour =guide_legend(override.aes = list(alpha = 1),
                             ncol = 1))


p4 <- ggplot(data = df_plot_outs[country == 'Hungary'], aes(x = mids, y = counts, shape = factor(year), color = factor(year))) +

  geom_point(size = 0.8, alpha = 0.5) +
  geom_line(size = 0.2, alpha = 0.5) +

  geom_vline(xintercept = t1, color = 'black', linetype = 'dashed', size = 0.4, alpha = 0.7) +
  geom_vline(xintercept = t2, color = 'black', linetype = 'dashed', size = 0.4, alpha = 0.7) +

  scale_colour_manual(values = pal[9:15]) +
  scale_shape_manual(values = c(0, 1, 2, 19, 8, 5, 6, 3, 4, 13, 9, 11)) +

  labs(x = TeX(r'(Out-strength)'), y = TeX(r'(Counts)')) +

  scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)),
                breaks = 10^(-2:10)[c(T, F, F)],
                limits = c(xmin, xmax),
                minor_breaks=10^(-2:10)) +
  scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)),
                breaks = 10^(0:4),
                minor_breaks = 10^(0:10),
                limits = c(ymin, ymax)) +
  annotation_logticks(outside = F, colour = 'gray') +

  annotate(geom = "text", x = 10^9, y = 1.5*10^4, label = "Hungary", color = "black", size = 3) +

  annotate(geom = "text", x = 1.8*10^2, y = 5*10^(1), label = "Thres. 2018Q3 - 2020Q2", angle = 90, color = "black", hjust = "left", size = 1.6) +
  annotate(geom = "text", x = 1.8*10^3, y = 4*10^(1), label = "Thres. 2015 - 2018Q2", angle = 90, color = "black", hjust = "left", size = 1.6) +

  theme_bw() +

  theme(axis.line = element_line(colour = "gray"),
        text = element_text(family = "serif"),
        panel.border = element_rect(colour = "gray"),
        plot.title = element_text(size = rel(1), vjust = 0, hjust = 0.5),
        axis.title = element_text(size = rel(1)),
        axis.title.y = element_text(size = rel(1), vjust = -1),
        axis.title.x = element_text(size = rel(1), vjust = 0.5),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = c(0.85, 0.7), 
        legend.title = element_blank(),
        legend.text = element_text(size = rel(0.5)),
        legend.key.size = unit(0.2, "in"),
        legend.key.height = unit(0.1, 'in'),
        legend.key.width = unit(0.1, 'in'),
        legend.key = element_rect(fill = 'transparent'),
        legend.background = element_rect(fill = 'transparent'),
        strip.placement = "outside",
        strip.background = element_blank(),
        strip.text = element_text(size = rel(1)),
        plot.margin = unit(c(t = 0.02, r = 0.22, b = 0.02, l = 0.02), "cm")) +

  guides(colour=guide_legend(override.aes = list(alpha = 1),
                             ncol = 1))

# Combine
g <- ggarrange(p1 + rremove('x.title') , p2 + rremove('x.title') + rremove('y.text') + rremove('y.title'),
               p3, p4 + rremove('y.title') + rremove('y.text'),
               nrow=2, ncol=2,
               widths = c(1, 0.9), heights = c(0.85, 1)) + 
               theme(plot.margin = margin(0.4, 2, 0.4, 0.4, "cm"))


# ------------------------------------------------------------------------------
#  Export
# ------------------------------------------------------------------------------

fig_size <- set_size(width_LaTeX, fraction = 1)
width_l <- fig_size[[1]]
height_l <- fig_size[[2]] + 1.25

filename <- file.path(dirOutput, 'C1_fig_strengths_distributions.png')
ggsave(file = filename, plot = g, width = width_l, height = height_l, unit = 'in', dpi = 600)

print('Figure C.1: Empirical pdf of the in- and out-strengths for Ecuador and Hungary over time exported.')