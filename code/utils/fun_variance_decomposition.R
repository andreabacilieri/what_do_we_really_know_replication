# Variance decomposition function
variance_decomp_by_group <- function(dat, metric, group = "ind_code", weight_col = NULL){
  # dat: data.table
  # metric: character, column name of metric (e.g., "inD")
  # group: grouping variable name (default ind_code)
  dat <- copy(dat)
  # filter NAs
  dat <- dat[!is.na(dat[[metric]])]
  x <- dat[[metric]]
  g <- dat[[group]]
  N <- length(x)
  overall_mean <- mean(x)
  
  # group counts and means
  tmp <- dat[, .(n = .N, gmean = mean(get(metric))), by = group]
  setkeyv(tmp, group)
  
  # between
  # Var_between = (1/N) * sum_g n_g * (gmean - overall_mean)^2
  var_between <- sum(tmp$n * (tmp$gmean - overall_mean)^2) / N
  
  # within
  # efficient: merge gmean back and compute sum( (x - gmean)^2)
  dat2 <- merge(dat, tmp, by = group, all.x = TRUE)
  var_within <- sum((dat2[[metric]] - dat2$gmean)^2) / N
  
  var_total <- sum((x - overall_mean)^2) / N
  
  res <- list(
    metric = metric,
    N = N,
    var_total = var_total,
    var_between = var_between,
    var_within = var_within,
    share_between = var_between / var_total,
    share_within = var_within / var_total,
    overall_mean = overall_mean
  )
  return(res)
}

# Compute per-industry stats for a metric
industry_stats <- function(dat, metric = "inD", group = "ind_code"){
  dat <- copy(dat)
  # filter NAs
  dat <- dat[!is.na(dat[[metric]])]
  
  tmp <- dat[, .(n = .N, mean_g = mean(get(metric)), var_g = var(get(metric))), by = group]
  overall_mean <- mean(dat[[metric]])
  tmp[, contrib_between := (n * (mean_g - overall_mean)^2)]  # numerator part for between
  tmp[, contrib_within := (n * var_g)]                       # numerator part for within
  # normalize by N to get variances; compute shares of each industry's between contribution
  N <- nrow(dat)
  tmp[, var_between_share_of_total := contrib_between / (sum((dat[[metric]] - overall_mean)^2))]
  tmp[, var_within_share_of_total := contrib_within / (sum((dat[[metric]] - overall_mean)^2))]
  return(tmp[order(-ind_code)])
}

# Melt to long form for stacked bars (Between vs Within variance decomposition)
melt_component <- function(dt){
  m <- melt(
    dt,
    id.vars = c("ind_code", "metric", "metric_label"),
    measure.vars = c("var_between_share_of_total", "var_within_share_of_total"),
    variable.name = "component",
    value.name = "share"
  )
  
  # rename for cleaner legend labels
  m[component == "var_between_share_of_total", component := "Between"]
  m[component == "var_within_share_of_total",  component := "Within"]
  
  return(m)
}

# Simple LaTeX block table generator
block_table <- function(dflist,
                        titles,
                        cols,
                        label_map = NULL,
                        digits = 2,
                        caption = "",
                        nblocks = length(dflist),
                        label) {
  
  # formatting helpers
  fmt_int <- function(x) ifelse(is.na(x) | x=="", "", formatC(as.integer(x), big.mark=","))
  fmt_num <- function(x, d=digits) ifelse(is.na(x) | x=="", "", formatC(as.numeric(x), format="f", digits=d))
  fmt_pct <- function(x, d=digits) paste0(fmt_num(100*as.numeric(x), d), "\\%")
  
  # begin LaTeX
  out <- ""
  out <- paste0(out, "\\begin{table}[ht]\n\\centering\n\\small\n")
  out <- paste0(out, "\\begin{tabular}{lrrrrr}\n")
  out <- paste0(out, "\\toprule\n")
  out <- paste0(out, "Metric & N & Var$_{tot}$ & Var$_{between}$ & Share$_{between}$ & Mean \\\\\n")
  out <- paste0(out, "\\midrule\n\n")
  
  # loop through blocks
  for (b in seq_len(nblocks)) {
    df <- as.data.frame(dflist[[b]])[ , intersect(cols, names(dflist[[b]])), drop=FALSE]
    
    # apply label map
    df$metric <- as.character(df$metric)
    if (!is.null(label_map)) {
      df$metric <- ifelse(df$metric %in% names(label_map),
                          label_map[df$metric], df$metric)
    }
    # escape underscores for latex
    df$metric <- gsub("_", "\\\\_", df$metric)
    
    # formatted cols
    N_col <- if ("N" %in% names(df)) fmt_int(df$N) else rep("", nrow(df))
    vt <- if ("var_total" %in% names(df)) fmt_num(df$var_total) else rep("", nrow(df))
    vb <- if ("var_between" %in% names(df)) fmt_num(df$var_between) else rep("", nrow(df))
    sb <- if ("share_between" %in% names(df)) fmt_pct(df$share_between) else rep("", nrow(df))
    mn <- if ("overall_mean" %in% names(df)) fmt_num(df$overall_mean) else rep("", nrow(df))
    
    # block title
    out <- paste0(out, sprintf("\\multicolumn{6}{l}{\\textbf{%s}} \\\\\n", titles[b]))
    
    # table rows
    for (i in seq_len(nrow(df))) {
      out <- paste0(out,
                    sprintf("%s & %s & %s & %s & %s & %s \\\\\n",
                            df$metric[i], N_col[i], vt[i], vb[i], sb[i], mn[i]))
    }
    
    if (b < nblocks) {
      out <- paste0(out, "\\midrule\n")
    }
  }
  
  # finish table
  out <- paste0(out, "\\bottomrule\n\\end{tabular}\n")
  out <- paste0(out, sprintf("\\caption{%s}\n", caption))
  out <- paste0(out, sprintf("\\label{%s}\n", label))
  out <- paste0(out, "\\end{table}\n")
  
  return(out)
}
