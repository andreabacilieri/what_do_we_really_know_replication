
# Extract only slope, N, R2 from lm() or plm() summaries
extract_lm_stats <- function(model) {
  s <- summary(model)
  
  slope <- s$coefficients[2,1]
  N     <- length(model$residuals)
  R2    <- s$r.squared
  
  list(slope = slope, N = N, R2 = R2)
}

extract_plm_stats <- function(model) {
  s <- summary(model)
  
  slope <- s$coefficients[[1]]
  N     <- nobs(model)             
  R2    <- R2 <- s$r.squared["rsq"]
  
  list(slope = slope, N = N, R2 = R2)
}


LM_data <- function(df, year, yvarlist, xvarlist, group_col) {
  # df: data.frame or data.table
  # year: integer (e.g. 2015)
  # yvarlist, xvarlist: same length vectors of variable names
  # group_col: character vector of grouping variables (one or more)
  
  stopifnot(length(yvarlist) == length(xvarlist))
  DT <- as.data.table(df)
  
  # prepare output structure: each element is a named list keyed by group_col
  LM_year_mod         <- setNames(vector("list", length(group_col)), group_col)
  LM_year_ind_fe_mod  <- setNames(vector("list", length(group_col)), group_col)
  LM_mod              <- setNames(vector("list", length(group_col)), group_col)
  LM_ind_fe_mod       <- setNames(vector("list", length(group_col)), group_col)
  LM_ind_year_fe_mod  <- setNames(vector("list", length(group_col)), group_col)
  
  # iterate over grouping variables
  for (gcol in group_col) {
    # each of these will be a list length = length(yvarlist)
    LM_year_mod[[gcol]]        <- vector("list", length(yvarlist))
    LM_year_ind_fe_mod[[gcol]] <- vector("list", length(yvarlist))
    LM_mod[[gcol]]             <- vector("list", length(yvarlist))
    LM_ind_fe_mod[[gcol]]      <- vector("list", length(yvarlist))
    LM_ind_year_fe_mod[[gcol]] <- vector("list", length(yvarlist))
    
    names(LM_year_mod[[gcol]])        <- yvarlist
    names(LM_year_ind_fe_mod[[gcol]]) <- yvarlist
    names(LM_mod[[gcol]])             <- yvarlist
    names(LM_ind_fe_mod[[gcol]])      <- yvarlist
    names(LM_ind_year_fe_mod[[gcol]]) <- yvarlist
    
    # iterate over variable pairs
    for (u in seq_along(yvarlist)) {
      yvar <- yvarlist[[u]]
      xvar <- xvarlist[[u]]
      
      idx <- paste0(yvar, "_", xvar)
      
      # select rows for year with positive values
      tok_year <- which(DT$year == year & DT[[yvar]] > 0 & DT[[xvar]] > 0)
      
      # 1) year-only pooled OLS (filter year and positive obs)
      fmla <- as.formula(paste0("log(", yvar, ") ~ log(", xvar, ")"))
      LM_year_mod[[gcol]][[idx]] <- extract_lm_stats(lm(fmla, data = DT[tok_year, ]))
      
      
      # 2) year-only individual FE (index = gcol)
      df_yr <- pdata.frame(DT[tok_year, ], index = gcol)
      LM_year_ind_fe_mod[[gcol]][[idx]] <- extract_plm_stats(plm(as.formula(paste0("log(", yvar, ") ~ log(", xvar, ")")), data = df_yr, model = "within", effect = "individual"))
      
      
      # select rows for all years with positive values
      tok_all <- which(DT$year %in% pooled_years & DT[[yvar]] > 0 & DT[[xvar]] > 0)
      
      # 3) pooled OLS on all years but only positive obs
      fmla_all <- as.formula(paste0("log(", yvar, ") ~ log(", xvar, ")"))
      LM_mod[[gcol]][[idx]] <- extract_lm_stats(lm(fmla_all, data = DT[tok_all, ]))
      
      # 4) individual FE on all years (index = gcol)
      df_panel <- pdata.frame(DT[tok_all, ], index = gcol)
      LM_ind_fe_mod[[gcol]][[idx]] <- extract_plm_stats(plm(as.formula(paste0("log(", yvar, ") ~ log(", xvar, ")")), data = df_panel, model = "within", effect = "individual"))
      
      # 5) two-ways FE (index = c(gcol, "year"))
      df_panel_ind <- tryCatch(pdata.frame(DT[tok_all, ], index = c(gcol, "year")))
      LM_ind_year_fe_mod[[gcol]][[idx]] <- extract_plm_stats(plm(as.formula(paste0("log(", yvar, ") ~ log(", xvar, ")")), data = df_panel_ind, model = "within", effect = "twoways"))
    }
  }
  
  # return a named list of lists
  list(
    LM_year_mod = LM_year_mod,
    LM_year_ind_fe_mod = LM_year_ind_fe_mod,
    LM_mod = LM_mod,
    LM_ind_fe_mod = LM_ind_fe_mod,
    LM_ind_year_fe_mod = LM_ind_year_fe_mod)
}

make_slope_matrix <- function(res, group_col, pair_name, label_map, country) {
  
  # result object names in the same order you want columns:
  model_families <- c("LM_year_mod",
                      "LM_year_ind_fe_mod",
                      "LM_mod",
                      "LM_ind_fe_mod",
                      "LM_ind_year_fe_mod")
  
  # preallocate matrix
  mymat <- matrix(NA_real_,
                  nrow = length(group_col),
                  ncol = length(model_families)+1)
  
  # fill matrix
  for (i in seq_along(group_col)) {
    gcol <- group_col[i]
    if (country == "Hungary") {
      label_map <- label_map_hu
    } else if (country == "Ecuador") {
      label_map <- label_map_ec
    } else {
      stop("Unknown country: ", country)
    }
    mymat[i, 1] <- label_map[gcol]
    
    for (j in seq_along(model_families)) {
      fam <- model_families[j]
      
      slope_val <- res[[ fam ]][[ gcol ]][[ pair_name ]]$slope
      mymat[i, j+1] <- slope_val
    }
  }
  
  return(mymat)
}