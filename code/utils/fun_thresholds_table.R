# ------------------------------------------------
#  Function: Format raw thresholds with thousand separators
# ------------------------------------------------

fmt_thousands <- function(x) {
  vapply(x, function(xx) {
    if (is.na(xx) || xx == "") return(xx)
    
    # extract unit (assumes unit is last token, e.g. EUR)
    has_unit <- grepl("\\s+[A-Za-z]{3}$", xx)
    unit <- if (has_unit) sub("^.*\\s+([A-Za-z]{3})$", "\\1", xx) else ""
    
    core <- if (has_unit) sub("\\s+[A-Za-z]{3}$", "", xx) else xx
    
    # handle ranges like 1000--12000
    if (grepl("--", core)) {
      parts <- strsplit(core, "--", fixed = TRUE)[[1]]
      parts_fmt <- vapply(parts, function(p) {
        format(as.numeric(p), big.mark = ",", scientific = FALSE)
      }, character(1))
      out <- paste(parts_fmt, collapse = "--")
    } else {
      num <- suppressWarnings(as.numeric(core))
      if (is.na(num)) return(xx)  # leave untouched if not numeric
      out <- format(num, big.mark = ",", scientific = FALSE)
    }
    
    if (has_unit) paste(out, unit) else out
  }, character(1))
}