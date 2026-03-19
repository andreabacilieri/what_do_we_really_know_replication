#######################################################
# Function that prepares the data for Figure B.3
#######################################################


prepare_data_plot <- function(df_degs){
  # Define Strength as average of in- and out-
  df_degs$strengths <- (df_degs$inS + df_degs$outS)/2
  all(df_degs$strengths > 0)
  # Define bins
  range_bins <- range(log(df_degs$strengths))
  nb <- 80
  BINS <- seq(from = range_bins[1], to = range_bins[2], length = nb)
  
  kbar_in <- kbar_out <- nn <- kmed_in <- kmed_out <- k90_in <- k90_out <- rep(NA, nb-1)
  for(i in 1:(nb-1)){
    if(i < (nb - 1)){
      df_ <- df_degs[(log(strengths) >= BINS[i]) & (log(strengths) < BINS[i+1])]
    }else if(i == (nb - 1)){
      # Need to include last upper bound, otherwise last firm not included
      # adjust for floating-point precision differences due to exp(log(range_bins))
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
