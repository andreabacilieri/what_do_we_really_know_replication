################################################################################

# Function to compute tail exponents using Voitalov's python code

################################################################################

#' @param datavector A numeric vector containing the data points for which to compute tail exponents.
#' @param mywd The working directory where the data will be exported and the python script will be executed.
#' @param addnoise A boolean indicating whether to add noise to the data (default is FALSE).
#' @param COMMAND String specifying the full file path to the Python interpreter (Python environment) that should be used 
#'                to run the external script tail-estimation.py. Default is 'cd': calls the system Python via the command python3.

#' @return A list containing the computed tail exponents and their corresponding k* values.

get_tail_exponents <- function(datavector, mywd, addnoise, COMMAND = 'python3'){

  # get number of data points (needed later to compute kstar)
  nn <- length(datavector)

  # ------------------------------------------------------------------------------
  # Export data so it can be read by the python function
  # ------------------------------------------------------------------------------
  dd <- table(datavector)
  dd <- dd[order(as.numeric(names(dd)))]
  mymat <- cbind(as.numeric(names(dd)), dd)
  setwd(mywd)
  write.table(mymat, file = "dd.dat", sep = " ", row.names = F, col.names = F)
  rm(dd)

  # ------------------------------------------------------------------------------
  #  Compute tail estimates using Voitalov's python code
  # ------------------------------------------------------------------------------
  # https://github.com/ivanvoitalov/tail-estimation
  # Use commands
  if(addnoise==T){
    noiseoption <- "--noise 1"
  }else{
    noiseoption <- "--noise 0"
  }
  # verbose=1 is important to get k*
  ARGS = c("tail-estimation.py", "dd.dat", "plots.pdf", noiseoption, "--diagplots 0", "--verbose 1")
  res <- system2(COMMAND, args = ARGS, stdout = TRUE)
  # print Python log to the R console
  cat(res, sep = "\n")
  
  # We correct by (-1) to have gamma, as in this paper
  AdjHill <- as.numeric(substr(res[grep("Adjusted Hill estimated gamma", res)], 32, 100))  - 1
  Moms    <- as.numeric(substr(res[grep("Moments estimated gamma", res)], 26, 100))  - 1
  Kern    <- as.numeric(substr(res[grep("Kernel-type estimated gamma", res)], 30, 100)) - 1
  # These are the k*, number of data points used in estimation
  N <- as.numeric( substr(res[2], 25, 100) )
  ww_kappas <- c(grep("Estimated optimal k", res), grep("Estimated optimal h", res))
  AdjHill_kstar <- as.numeric(substr(res[ww_kappas[1]], 22, 100))
  Mom_kstar  <- as.numeric(substr(res[ww_kappas[2]], 22, 100))
  Ker_kstar  <- floor( N * as.numeric(substr(res[ww_kappas[3]], 22, 100)) )
  rm(res)

  # ------------------------------------------------------------------------------
  #  Compute tail estimates using the Clauset-Newman-Shalizi Hill estimator
  # ------------------------------------------------------------------------------
  # don't forget the (-1), this function returns the tail exponent of the pdf (not the ccdf)
  CNS <- try(fit_power_law(datavector,
                           implementation = "plfit",
                           force.continuous = !addnoise),
              silent = T)
  if(!substr(CNS[1], 1, 5) == "Error"){
    CNS_tail <- CNS$alpha - 1
    CNS_kstar <- sum(datavector >= CNS$xmin)
  }else{
    CNS_tail <- NA
    CNS_kstar <- NA
  }

  # ------------------------------------------------------------------------------
  # Return results
  # ------------------------------------------------------------------------------
  return(list(
    "CNS"           = CNS_tail,
    "CNS_kstar"     = CNS_kstar,
    "AdjHill"       = AdjHill,
    "AdjHill_kstar" = AdjHill_kstar,
    "Moms"          = Moms,
    "Mom_kstar"     = Mom_kstar,
    "Kern"          = Kern,
    "Ker_kstar"     = Ker_kstar
    ))
}