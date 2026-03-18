##########################################################################################################################
# This script produces Figure B.3: Binned scatter plots of the conditional relationships 
# among size and in- and out-degrees

# INPUTS:
# Data:
# from data/analysis/
#     - ecuador/ecuador_local_properties.csv
#     - hungary/hungary_local_properties.csv
# from code/utils/
#     - fun_ccdf_tls_network.R
# from code/utils_plots/
#     - set_size.R
#     - 2D_hist.R

# OUTPUT:
# to results/figures/
#     - B3_fig_mean_degree_strengths.png

#######################################################################################################################

# ------------------------------------------------
#  Set environment/parameters
# ------------------------------------------------
rm(list = ls())
library(data.table)
library(ggplot2)
library(ggpubr)
library(binsreg)
library(igraph)
library(zoo)
library(latex2exp)
# Rootfolder
rootfolder <- strsplit(rstudioapi::getSourceEditorContext()$path,'/code')[[1]][1]
# Get directory of the input data
dirdata <- file.path(rootfolder, 'data', 'analysis')
# Folder to Store output
dirOutput <- file.path(rootfolder, 'results', 'figures')
# For plots
width_LaTeX = 418.25368  # in pt
source(file.path(rootfolder, 'code', 'utils_plots', 'set_size.R'))


# ------------------------------------------------
# Load data & keep only 2015
# ------------------------------------------------

## Ecuador ##
df_ <- fread(file.path(dirdata, 'ecuador', 'ecuador_local_properties.csv'))
data <- list(Ecuador = df_[year == 2015, c('outD', 'inD', 'outS', 'inS', 'year')])
## Hungary ##
df_ <- fread(file.path(dirdata, 'hungary', 'hungary_local_properties.csv'))
data$Hungary <- df_[year == 2021, c('outD', 'inD', 'outS', 'inS', 'year')]


# --------------------------------------------
# Create bin scatter 
# --------------------------------------------
fix_zero_log_scale <- function(kk){
  if(length(kk) > 0){
    for(k in kk){
      kk[k] <- 0.02
    }
  }
}

prepare_data_plot <- function(df_degs){
  # Define Strength as average of in- and out-
  df_degs$strengths <- (df_degs$inS + df_degs$outS)/2
  all(df_degs$strengths > 0)
  # Define bins
  range_bins <- range(log(df_degs$strengths))
  nb <- 80
  #BINS <- exp(seq(from = range_bins[1], to = range_bins[2], length = nb))
  BINS <- seq(from = range_bins[1], to = range_bins[2], length = nb)
  
  kbar_in <- kbar_out <- nn <- kmed_in <- kmed_out <- k90_in <- k90_out <- rep(NA, nb-1)
  for(i in 1:(nb-1)){
    if(i < (nb - 1)){
      #df_ <- df_degs[(strengths >= BINS[i]) & (strengths < BINS[i+1])]
      df_ <- df_degs[(log(strengths) >= BINS[i]) & (log(strengths) < BINS[i+1])]
    }else if(i == (nb - 1)){
      # Need to include last upper bound, otherwise last firm not included
      # adjust for floating-point precision differences due to exp(log(range_bins))
      #df_ <- df_degs[(strengths >= BINS[i]) & (strengths <= BINS[i+1] + 1e-4)]
      df_ <- df_degs[(log(strengths) >= BINS[i]) & (log(strengths) <= BINS[i+1])]
    }
    kbar_in[i]  <- mean(df_$inD)
    kbar_out[i] <- mean(df_$outD)
    kmed_in[i]  <- median(df_$inD)
    kmed_out[i] <- median(df_$outD)
    k90_in[i]  <- quantile(df_$inD, 0.9)
    k90_out[i] <- quantile(df_$outD, 0.9)
    nn[i] <- dim(df_)[1]
  }

  # Store data for plotting
  df_plot <- data.table(mids = rollmean(exp(BINS), 2),
                        mids_geom = exp(rollmean(BINS, 2)),
                        mean_deg = kbar_in,
                        med_deg = kmed_in,
                        pct90_deg = k90_in,
                        nn = nn)
  df_plot$type <- 'In-degree'
  df_ <- data.table(mids = rollmean(exp(BINS), 2),
                    mids_geom = exp(rollmean(BINS, 2)),
                    mean_deg = kbar_out,
                    med_deg = kmed_out,
                    pct90_deg = k90_out,
                    nn = nn)
  df_$type <- 'Out-degree'
  df_plot <- rbind(df_plot, df_)
}

k <- 0
for (nm in names(data)) {
  df_ <- prepare_data_plot(data[[nm]])
  if(k == 0){
    df_plot <- df_
    df_plot$country <- nm
    k <- 1
  }else{
    df_$country <- nm
    df_plot <- rbind(df_plot, df_)
  }
}


# ----------------------------------------------------
# Find at which size do the in- and out-degs cross
# ---------------------------------------------------
# Ecuador
df_cross <- merge(df_plot[country == 'Ecuador' & type == 'In-degree'], 
                  df_plot[country == 'Ecuador' & type == 'Out-degree'], 
                  by = 'mids_geom', suffixes = c('_in', '_out'))
df_cross <- na.omit(df_cross)
df_cross[, diff := mean_deg_in - mean_deg_out]
df_cross[diff < 0, c('mids_geom', 'diff', 'mean_deg_in', 'mean_deg_out')]
# size = 2,016,569, mean in-deg = 131.8684, mean out-deg = 152.6101

# Hungary
df_cross <- merge(df_plot[country == 'Hungary' & type == 'In-degree'], 
                  df_plot[country == 'Hungary' & type == 'Out-degree'], 
                  by = 'mids_geom', suffixes = c('_in', '_out'))
df_cross <- na.omit(df_cross)
df_cross[, diff := mean_deg_in - mean_deg_out]
df_cross[diff < 0 & mids_geom > 10^3, c('mids_geom', 'diff', 'mean_deg_in', 'mean_deg_out')]
# size = 345,288.2    mean in-deg = 114.2119     mean out-deg = 135.3619
df_cross[diff > 0 & mids_geom < 10^3, c('mids_geom', 'diff', 'mean_deg_in', 'mean_deg_out')]
# size = 140.4985, mean in-deg = 1.490262, mean out-deg = 1.120078


# ------------------------------------------------------------------------------
# Plot: Mean degree ~ strength
# ------------------------------------------------------------------------------
pal <- c('black', 'slateblue1')

x_breaks_e <- scales::log_breaks()(df_plot[country == 'Ecuador']$mids_geom)
#x_breaks_e <- sort(unique(c(1, x_breaks)))
y_breaks_e <- scales::log_breaks(n = 4)(df_plot[country == 'Ecuador' & mean_deg > 1]$mean_deg)
#y_breaks_e <- sort(unique(c(1, y_breaks)))

x_breaks_h <- scales::log_breaks(n = 6)(df_plot[country == 'Hungary']$mids_geom)
#x_breaks_h <- sort(unique(c(1, x_breaks)))
y_breaks_h <- scales::log_breaks(n = 5)(df_plot[country == 'Hungary']$mean_deg)
#y_breaks_h <- sort(unique(c(1, y_breaks)))

#df_plot <- df_plot[nn > 0]

## ECUADOR ##

p1 <- ggplot(df_plot[country == 'Ecuador'], aes(x = mids_geom, y = mean_deg, colour = type, shape = type)) +
  geom_line(linewidth = 0.4, alpha = 0.6) +
  geom_point(size = 1.2, alpha = 0.6) +
  
  scale_x_log10(breaks = x_breaks_e,
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_y_log10(breaks = y_breaks_e,
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  
  labs(x = TeX('Size $=$ (expenses + sales)/2') , y = TeX('Mean degree')) +
  scale_shape_manual(values = c(3, 2, 1, 0)) +
  scale_colour_manual(values = pal) +
  
  
  annotate("text", x = 0.01, y = 6*10^3, label = "Ecuador, 2015", 
           color = "black", hjust = "left", size = 3) +
  
  theme_bw() +
  
  theme(axis.line = element_line(colour = "gray"),
        text = element_text(family = "serif"),
        panel.border = element_rect(colour = "gray"),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(vjust = 2, size = rel(0.8)),
        axis.title.x = element_text(vjust = -0.5, size = rel(0.8)),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = c(0.32, 0.72),
        legend.title = element_blank(),
        legend.text = element_text(size = rel(0.8)),
        legend.key = element_rect(fill = "transparent"),
        legend.key.size = unit(0.8, "lines"),
        legend.key.height = unit(0.6, "lines"),
        legend.key.width = unit(0.6, "lines"),
        legend.background = element_rect(fill = F),
        strip.text = element_text(colour = "black", size = rel(0.7)),  
        strip.background = element_blank(),
        panel.spacing.y = unit(0, "lines")) +
  
  guides(colour = guide_legend(ncol = 1, override.aes = list(alpha = 1, size = 1)),
         shape  = guide_legend(ncol = 1, override.aes = list(alpha = 1, size = 1)))

p2 <- ggplot(df_plot[country == 'Ecuador'], aes(x = mids_geom, y = med_deg, colour = type, shape = type)) +
  geom_line(linewidth = 0.4, alpha = 0.6) +
  geom_point(size = 1.2, alpha = 0.6) +
  
  scale_x_log10(breaks = x_breaks_e,
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_y_log10(breaks = y_breaks_e,
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  
  labs(x = TeX('Size $=$ (expenses + sales)/2') , y = TeX('Median degree')) +
  scale_shape_manual(values = c(3, 2, 1, 0)) +
  scale_colour_manual(values = pal) +
  
  
  #annotate("text", x = 0.01, y = 10^3, label = "Ecuador, 2015", 
   #        color = "black", hjust = "left", size = 3) +
  
  theme_bw() +
  
  theme(axis.line = element_line(colour = "gray"),
        text = element_text(family = "serif"),
        panel.border = element_rect(colour = "gray"),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(vjust = 2, size = rel(0.8)),
        axis.title.x = element_text(vjust = -0.5, size = rel(0.8)),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = 'none',
        legend.title = element_blank(),
        legend.text = element_text(size = rel(0.9)),
        legend.key = element_rect(fill = "transparent"),
        legend.key.size = unit(0.8, "lines"),
        legend.key.height = unit(0.6, "lines"),
        legend.key.width = unit(0.6, "lines"),
        legend.background = element_rect(fill = F),
        strip.text = element_text(colour = "black", size = rel(0.7)),  
        strip.background = element_blank(),
        panel.spacing.y = unit(0, "lines")) +
  
  guides(colour = guide_legend(ncol = 1, override.aes = list(alpha = 1, size = 1)),
         shape  = guide_legend(ncol = 1, override.aes = list(alpha = 1, size = 1)))

p3 <- ggplot(df_plot[country == 'Ecuador'], aes(x = mids_geom, y = pct90_deg, colour = type, shape = type)) +
  geom_line(linewidth = 0.4, alpha = 0.6) +
  geom_point(size = 1.2, alpha = 0.6) +
  
  scale_x_log10(breaks = x_breaks_e,
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_y_log10(breaks = y_breaks_e,
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  
  labs(x = TeX('Size $=$ (expenses + sales)/2') , y = TeX('Degree $90^{th}$ percentile')) +
  scale_shape_manual(values = c(3, 2, 1, 0)) +
  scale_colour_manual(values = pal) +
  
  
  #annotate("text", x = 0.01, y = 9*10^3, label = "Ecuador, 2015", 
   #        color = "black", hjust = "left", size = 3) +
  
  theme_bw() +
  
  theme(axis.line = element_line(colour = "gray"),
        text = element_text(family = "serif"),
        panel.border = element_rect(colour = "gray"),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(vjust = 2, size = rel(0.8)),
        axis.title.x = element_text(vjust = -0.5, size = rel(0.8)),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = 'none',
        legend.title = element_blank(),
        legend.text = element_text(size = rel(0.9)),
        legend.key = element_rect(fill = "transparent"),
        legend.key.size = unit(0.8, "lines"),
        legend.key.height = unit(0.6, "lines"),
        legend.key.width = unit(0.6, "lines"),
        legend.background = element_rect(fill = F),
        strip.text = element_text(colour = "black", size = rel(0.7)),  
        strip.background = element_blank(),
        panel.spacing.y = unit(0, "lines")) +
  
  guides(colour = guide_legend(ncol = 1, override.aes = list(alpha = 1, size = 1)),
         shape  = guide_legend(ncol = 1, override.aes = list(alpha = 1, size = 1)))



## HUNGARY ##

p4 <- ggplot(df_plot[country == 'Hungary'], aes(x = mids_geom, y = mean_deg, colour = type, shape = type)) +
  geom_line(linewidth = 0.4, alpha = 0.6) +
  geom_point(size = 1.2, alpha = 0.6) +
  
  scale_x_log10(breaks = x_breaks_h,
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_y_log10(breaks = y_breaks_h,
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  
  labs(x = TeX('Size $=$ (expenses + sales)/2') , y = TeX('Mean degree')) +
  scale_shape_manual(values = c(3, 2, 1, 0)) +
  scale_colour_manual(values = pal) +
  
  annotate("text", x = 1, y = 4*10^4, label = "Hungary, 2021", 
           color = "black", hjust = "left", size = 3) +
  
  theme_bw() +
  
  theme(axis.line = element_line(colour = "gray"),
        text = element_text(family = "serif"),
        panel.border = element_rect(colour = "gray"),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(vjust = 2, size = rel(0.9)),
        axis.title.x = element_text(vjust = -0.5, size = rel(0.9)),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = 'none',
        legend.title = element_blank(),
        legend.text = element_text(size = rel(1)),
        legend.key = element_rect(fill = "transparent"),
        legend.key.size = unit(0.8, "lines"),
        legend.key.height = unit(0.6, "lines"),
        legend.key.width = unit(0.6, "lines"),
        legend.background = element_rect(fill = F),
        strip.text = element_text(colour = "black", size = rel(0.7)),  
        strip.background = element_blank(),
        panel.spacing.y = unit(0, "lines")) +
  
  guides(colour = guide_legend(ncol = 2, override.aes = list(alpha = 1, size = 1)),
         shape  = guide_legend(ncol = 2, override.aes = list(alpha = 1, size = 1)))


p5 <- ggplot(df_plot[country == 'Hungary'], aes(x = mids_geom, y = med_deg, colour = type, shape = type)) +
  geom_line(linewidth = 0.4, alpha = 0.6) +
  geom_point(size = 1.2, alpha = 0.6) +
  
  scale_x_log10(breaks = x_breaks_h,
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_y_log10(breaks = y_breaks_h,
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  
  labs(x = TeX('Size $=$ (expenses + sales)/2') , y = TeX('Median degree')) +
  scale_shape_manual(values = c(3, 2, 1, 0)) +
  scale_colour_manual(values = pal) +
  
  #annotate("text", x = 1, y = 2*10^4, label = "Hungary, 2021", 
   #        color = "black", hjust = "left", size = 3) +
  
  theme_bw() +
  
  theme(axis.line = element_line(colour = "gray"),
        text = element_text(family = "serif"),
        panel.border = element_rect(colour = "gray"),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(vjust = 2, size = rel(0.9)),
        axis.title.x = element_text(vjust = -0.5, size = rel(0.9)),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = 'none',
        legend.title = element_blank(),
        legend.text = element_text(size = rel(1)),
        legend.key = element_rect(fill = "transparent"),
        legend.key.size = unit(0.8, "lines"),
        legend.key.height = unit(0.6, "lines"),
        legend.key.width = unit(0.6, "lines"),
        legend.background = element_rect(fill = F),
        strip.text = element_text(colour = "black", size = rel(0.7)),  
        strip.background = element_blank(),
        panel.spacing.y = unit(0, "lines")) +
  
  guides(colour = guide_legend(ncol = 2, override.aes = list(alpha = 1, size = 1)),
         shape  = guide_legend(ncol = 2, override.aes = list(alpha = 1, size = 1)))

p6 <- ggplot(df_plot[country == 'Hungary'], aes(x = mids_geom, y = pct90_deg, colour = type, shape = type)) +
  geom_line(linewidth = 0.4, alpha = 0.6) +
  geom_point(size = 1.2, alpha = 0.6) +
  
  scale_x_log10(breaks = x_breaks_h,
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_y_log10(breaks = y_breaks_h,
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  
  labs(x = TeX('Size $=$ (expenses + sales)/2') , y = TeX('Degree $90^{th}$ percentile')) +
  scale_shape_manual(values = c(3, 2, 1, 0)) +
  scale_colour_manual(values = pal) +
  
  #annotate("text", x = 1, y = 2*10^4, label = "Hungary, 2021", 
   #        color = "black", hjust = "left", size = 3) +
  
  theme_bw() +
  
  theme(axis.line = element_line(colour = "gray"),
        text = element_text(family = "serif"),
        panel.border = element_rect(colour = "gray"),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(vjust = 2, size = rel(0.9)),
        axis.title.x = element_text(vjust = -0.5, size = rel(0.9)),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = 'none',
        legend.title = element_blank(),
        legend.text = element_text(size = rel(1)),
        legend.key = element_rect(fill = "transparent"),
        legend.key.size = unit(0.8, "lines"),
        legend.key.height = unit(0.6, "lines"),
        legend.key.width = unit(0.6, "lines"),
        legend.background = element_rect(fill = F),
        strip.text = element_text(colour = "black", size = rel(0.7)),  
        strip.background = element_blank(),
        panel.spacing.y = unit(0, "lines")) +
  
  guides(colour = guide_legend(ncol = 2, override.aes = list(alpha = 1, size = 1)),
         shape  = guide_legend(ncol = 2, override.aes = list(alpha = 1, size = 1)))


## MERGE ##
g <- ggarrange(p1 + rremove('xlab'), p2 + rremove('xlab'), p3 + rremove('xlab'),
               p4, p5, p6,
               nrow = 2, ncol = 3, 
               common.legend = F,
               widths = c(1, 1, 1), 
               heights = c(0.9, 1))



# Export 
fig_size <- set_size(width_LaTeX, fraction = 1)
width_l <- fig_size[[1]] + 0.4
height_l <- fig_size[[2]] + 0.8

filename <- paste(dirOutput, '/B3_fig_mean_degree_strengths.png', sep = .Platform$file.sep)
ggsave(plot = g, filename = filename, width = width_l, height = height_l, dpi = 600)



filename <- 'bins_degree_size.csv'
csv_name <- file.path(dirOutput, filename)
write.csv(df_plot, csv_name, row.names = F)
