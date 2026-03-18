#' Creates a 2D histogram, with a Total Least Squares fit line if specified.
#' 
#' @param mydata A data frame with the data needed for the histogram
#' @param var_y A sting with the name (in mydata) of the variable to be plotted on the y-axis
#' @param var_x A sting with the name (in mydata) of the variable to be plotted on the x-axis
#' @param nbreaks A scalar giving the number of cells for the histogram, default is 60
#' @param mylabel A string with the name of the dataset and year (e.g. "Ecuador, 2015"), shown as annotation in the plot, default is "label"
#' @param plot_label A sting with the name of the dataset to show in the label of the "counts" legend, default is NULL
#' @param ylabel A sting with the y-axis label, dafault is var_y
#' @param xlabel A sting with the x-axis label, dafault is var_x
#' @param min_ A scalar setting the lower limit of the axes, default is 1
#' @param max_ A scalar setting the upper limit of the axes, default is 1000
#' @param tlsline Either TRUE or FALSE, whether to calculate and plot the Total Least Squares fit; default is T
#' @param plot_x1 A scalar setting the x coordinate for the positioning of mylabel
#' @param plot_x2 A scalar setting the x coordinate for the positioning of the TLF coefficient annotation
#' @param plot_y1 A scalar setting the y coordinate for the positioning of mylabel
#' @param plot_y2 A scalar setting the y coordinate for the positioning of the TLF coefficient annotation
#' 
#' @return h0 A 2D histogram


plot_2D_hist_loglog <- function(mydata, var_y, var_x, nbreaks = 60, mylabel = "label", plot_label = NULL, ylabel = var_y, 
                                xlabel = var_x, min_ = 1, max_ = 1000, tlsline = T, plot_x1, plot_x2, plot_y1, plot_y2) {

  # keep only positive values
  tokeep <- which(mydata[[var_y]] > 0 & mydata[[var_x]] > 0)
  mydata <- mydata[tokeep, ]

  if (tlsline == T){
    # get parameters for TLS line
    y <- log10(mydata[[var_y]])
    x <- log10(mydata[[var_x]])
    tlsparam <- tlsfn(y, x)
    coeff_str <- TeX(sprintf(r'($TLS, \beta = %.2f$)', tlsparam[2]), output = 'character')
  }

  # get breaks
  data <- c(mydata[[var_y]], mydata[[var_x]])
  BREAKS <- hist(log(data), breaks = nbreaks, include.lowest = T, right = T, plot = F)$breaks

  mydata <- as.data.frame(mydata)
  mydata[[var_x]] <- as.numeric(mydata[[var_x]])
  mydata[[var_y]] <- as.numeric(mydata[[var_y]])

  if (is.null(plot_label)) {
    plot_label <- mylabel
  } else {
    plot_label <- plot_label
  }

  if (tlsline == T){
    
    h0 <- ggplot() +
  
      stat_bin2d(data = mydata, aes(x =!! sym(var_x), y =!! sym(var_y), fill = after_stat(count)), breaks = exp(BREAKS)) +
      scale_fill_distiller(palette = 'PuBuGn', direction = 1, name = paste("Counts", plot_label, sep=" "), trans = 'log10', limits = c(10, NA), na.value = "transparent") +
  
      scale_x_log10(breaks =  scales::trans_breaks("log10", function(x) 10^x),
                    labels = scales::trans_format("log10", scales::math_format(10^.x)),
                    limits=c(min_, max_)) +
      scale_y_log10(breaks =  scales::trans_breaks("log10", function(x) 10^x),
                    labels = scales::trans_format("log10", scales::math_format(10^.x)),
                    limits=c(min_, max_)) +
      annotation_logticks(outside = F, colour = 'gray') +
  
      labs(y = ylabel, x = xlabel, fill = "Counts" ) +
  
      annotate(geom = "text", x = plot_x1, y = plot_y1, label = mylabel, color = "black", size = 3) +
      annotate(geom = "text", x = plot_x2, y = plot_y2, label = coeff_str, parse = T, color = "black", size = 3) +
  
      theme_bw() +
  
      theme(axis.line = element_line(colour = "gray"),
            text = element_text(family = "serif"),
            panel.background = element_rect(fill = NA),
            panel.border = element_rect(colour = "gray"),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = rel(0.9), vjust = 0),
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
            plot.margin = unit(c(0, 0, 0, 0), "pt")) +
      guides(fill = guide_colourbar(direction = "horizontal",
                                  title.position = 'top',
                                  label.theme = element_text(angle = 45, size = 10),
                                  label.hjust = 1))
  
    h0 <- h0 + geom_abline(slope = tlsparam[2], intercept = tlsparam[1], size = 0.2, linetype = "solid", colour = 'black')
    
  } else {
  
    h0 <- ggplot() +
  
      stat_bin2d(data = mydata, aes(x =!! sym(var_x), y =!! sym(var_y), fill = after_stat(count)), breaks = exp(BREAKS)) +
      scale_fill_distiller(palette = 'PuBuGn', direction = 1, name = paste("Counts", plot_label, sep = " "), trans = 'log10', limits = c(10, NA), na.value = "transparent") +
  
      scale_x_log10(breaks =  scales::trans_breaks("log10", function(x) 10^x),
                    labels = scales::trans_format("log10", scales::math_format(10^.x)),
                    limits = c(min_, max_)) +
      scale_y_log10(breaks =  scales::trans_breaks("log10", function(x) 10^x),
                    labels = scales::trans_format("log10", scales::math_format(10^.x)),
                    limits = c(min_, max_)) +
      annotation_logticks(outside = F, colour = 'gray') +
  
      labs(y = ylabel, x = xlabel, fill = "Counts" ) +
  
      theme_bw() +
  
      theme(axis.line = element_line(colour = "gray"),
            text = element_text(family = "serif"),
            panel.background = element_rect(fill = NA),
            panel.border = element_rect(colour = "gray"),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = rel(0.9), vjust = 0),
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
            plot.margin = unit(c(0, 0, 0, 0), "pt")) +
      guides(fill=guide_colourbar(direction = "horizontal",
                                  title.position = 'top',
                                  label.theme = element_text(angle = 45, size = 10),
                                  label.hjust = 1))
  
    return(h0)
    }
  }