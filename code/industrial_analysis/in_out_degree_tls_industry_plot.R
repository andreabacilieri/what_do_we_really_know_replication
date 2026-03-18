##########################################################################################################################
# This script produces Figure E.3: TLS estimates for the regression of in-degree on out-degree by industry

# INPUTS:
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
#   - E3_fig_tls_in_out_deg_industry.png

#######################################################################################################################

# ------------------------------------------------
#  Set environment/parameters
# ------------------------------------------------

rm(list = ls())
library(ggplot2)
library(ggpubr)
library(data.table)
library(patchwork)
library(tls)
library(igraph)
library(xtable)
library(stringr)

# rootfolder
rootfolder <- strsplit(rstudioapi::getSourceEditorContext()$path,'/code')[[1]][1]
# Get directory of the input data
dirdata <- file.path(rootfolder, 'data', 'analysis')
# Folder to Store output
dirOutput_fig <- file.path(rootfolder, 'results', 'figures')
dirOutput_tab <- file.path(rootfolder, 'results', 'tables')
# Get function for the total least square regression
source(file.path(rootfolder, 'code', 'utils', 'fun_ccdf_tls_network.R'))
# For plots
width_LaTeX = 418.25368  # in pt
source(file.path(rootfolder, 'code', 'utils_plots', 'set_size.R'))
# TLS estimate
source(file.path(rootfolder, 'code', 'utils', 'fun_ccdf_tls_network.R'))


# ------------------------------------------------
# Load data & keep only 2015, 2021
# ------------------------------------------------

# Ecuador
df_e <- fread(file.path(dirdata, 'ecuador', 'ecuador_local_properties.csv'))
df_e <- df_e[year == 2015]
# Hungary
df_h <- fread(file.path(dirdata, 'hungary', 'hungary_local_properties.csv'))
df_h <- df_h[year == 2021]


# ------------------------------------------------
# TLS estimates by industry
# ------------------------------------------------

# Ecuador
list_df <- tlsfn_industry(df_e, 2015)
df_plot_e <- list_df[["df_plot"]]
df_plot_all_e <- list_df[["df_plot_all"]]
rm(list_df)
# keep ordering for plot
df_plot_e[, indu_descr := factor(indu_descr, levels = unique(indu_descr))]

# Hungary
list_df <- tlsfn_industry(df_h, 2021)
df_plot_h <- list_df[["df_plot"]]
df_plot_all_h <- list_df[["df_plot_all"]]
rm(list_df)
# keep ordering for plot
df_plot_h[, indu_descr := factor(indu_descr, levels = unique(indu_descr))]


# ------------------------------------------------
# Figure with the TLS estimates by industry
# ------------------------------------------------
common_breaks <- c(0.4, 0.7, 1, 1.3, 1.6)

p1 <- ggplot(df_plot_e, aes(x = tls_param, y = indu_descr)) + 
        
      # All industries estimate
      # shaded CI band for all firms
      geom_rect(data = df_plot_all_e,
                aes(xmin = ci_low, xmax = ci_up, ymin = -Inf, ymax = Inf, fill = "95% CI (all firms)"),
                alpha = 0.2, inherit.aes = F) +
      
      # Line for all firms TLS
      geom_vline(data = df_plot_all_e,
                 aes(xintercept = tls_param_all, colour = "TLS estimate (all firms)"),
                 linewidth = 0.6, linetype = 'solid', alpha = .7) +
      
      scale_colour_manual(values = c("TLS estimate (all firms)" = "red")) +
      scale_fill_manual(values = c("95% CI (all firms)" = "red")) +
      
      # Line at 1
      geom_vline(data = df_plot_all_e, aes(xintercept = 1),
                 linewidth = 0.4, linetype = 'dashed',
                 colour = 'black', alpha = .8) +
  
      # Industry-level estimates
      # error bars (only for non-NA estimates)
      geom_errorbar(aes(xmin = ci_low, xmax = ci_up), colour = "gray40", na.rm = TRUE) +
      # point estimates (only for non-NA estimates)
      geom_point(size = 1, colour = "blue", na.rm = TRUE) +
      
      scale_x_continuous(breaks = common_breaks) +
      scale_y_discrete(labels = function(x) str_wrap(x, width = 34)) +
      labs(x = "TLS estimate", y = NULL, fill = NULL, colour = NULL) +
      
      theme_bw() +
      theme(
        strip.text = element_blank(),
        strip.background = element_blank(),
        axis.title.x = element_text(size = rel(0.8)),
        axis.text.y = element_text(hjust = 0, colour = "black", size = rel(.9)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(0.2, "lines"),
        plot.title = element_text(hjust = 0.5, size = rel(1)),
        legend.position = 'none',
        legend.direction = "horizontal",
        legend.text = element_text(size = rel(0.7)),
        legend.spacing.y = unit(0.02, "lines"),
        #legend.background = 'white',
        plot.margin = unit(c(t = 0.4, r = 0, b = 0.4, l = 0.4), "cm"))


p2 <- ggplot(df_plot_h, aes(x = tls_param, y = indu_descr)) + 
  
  # All industries estimate
  # shaded CI band for all firms
  geom_rect(data = df_plot_all_h,
            aes(xmin = ci_low, xmax = ci_up, ymin = -Inf, ymax = Inf, fill = "95% CI (all firms)"),
            alpha = 0.2,
            inherit.aes = F) +
  
  # Line for all firms TLS
  geom_vline(data = df_plot_all_h,
             aes(xintercept = tls_param_all, colour = "TLS estimate (all firms)"),
             linewidth = 0.6, linetype = 'solid', alpha = .7) +
  
  scale_colour_manual(values = c("TLS estimate (all firms)" = "red")) +
  scale_fill_manual(values = c("95% CI (all firms)" = "red")) +

  # Line at 1
  geom_vline(data = df_plot_all_h, aes(xintercept = 1),
             linewidth = 0.4, linetype = 'dashed',
             colour = 'black', alpha = .8) +
  
  # Industry-level estimates
  # error bars (only for non-NA estimates)
  geom_errorbar(aes(xmin = ci_low, xmax = ci_up), colour = "gray40", na.rm = TRUE) +
  # point estimates (only for non-NA estimates)
  geom_point(size = 1, colour = "blue", na.rm = TRUE) +

    scale_x_continuous(breaks = common_breaks) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 34)) +
  labs(x = "TLS estimate", y = NULL, fill = NULL, colour = NULL) +
  
  theme_bw() +
  theme(
    strip.text = element_blank(),
    strip.background = element_blank(),
    axis.title.x = element_text(size = rel(0.8)),
    axis.text.y = element_text(hjust = 0, colour = "black", size = rel(.9)),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(0.2, "lines"),
    plot.title = element_text(hjust = 0.5, size = rel(1)),
    legend.position = 'none',
    legend.direction = "horizontal",
    legend.text = element_text(size = rel(0.7)),
    legend.spacing.y = unit(0.02, "lines"),
    legend.background = element_blank(),
    plot.margin = unit(c(t = 0.4, r = 0.5, b = 0.4, l = 0), "cm"))


# Combine plots
g <- ggarrange(p1 + ggtitle("Ecuador, 2015"), 
               p2 + ggtitle("Hungary, 2021") + rremove("ylab") + rremove("y.text") + rremove("y.ticks"),
              nrow = 1,
              ncol = 2,
              common.legend = T,
              legend = "bottom",
              widths = c(1.7, 1),
              font.label = element_text(size = rel(1)))

#  Export
fig_size <- set_size(width_LaTeX, fraction = 1)
width_l <- fig_size[[1]] + 0.6
height_l <- fig_size[[2]] + 3.6

filename <- paste(dirOutput_fig, 'E3_fig_tls_in_out_deg_industry.png', sep = .Platform$file.sep)
ggsave(plot = g, filename = filename, width = width_l, height = height_l)

print('Figure E3: TLS estimates for the number of customers and suppliers by industry exported.')


# --------------------------------------------------------------
# TLS demeaned by industry for all firms pooled together
# --------------------------------------------------------------

## De-mean by industry mean over all T (like a FE model) ##
df_mean_ind <- df_e[, .(mean_inD_industry = mean(inD), mean_outD_industry = mean(outD)), by = indu_code_1]
df_degs <- merge(df_e, df_mean_ind, by = 'indu_code_1', all.x = T)
df_degs$dem_inD <- df_degs$inD - df_degs$mean_inD_industry
df_degs$dem_outD <- df_degs$outD - df_degs$mean_outD_industry

params_allT <- tls(dem_inD ~ dem_outD + 0, data = df_degs[year == 2015,]) # $coefficient = 0.085
params_allT


