myargs <- commandArgs(trailingOnly = TRUE)

source("function__read_ssf_file.R")

# this is where CMake BUILDS the TSPROC executable
tsproc_exe <- myargs[1]

exponent <- 1.0

tsproc_inp <- "tsproc_test_series_compare.inp"
tsproc_log <- "tsproc_test_series_compare.log"

DATE_1 <- as.POSIXlt("1990-10-02")
DATE_2 <- as.POSIXlt("2000-10-01")

my_obs_data <- read_ssf("Q_BEC_BE_6500.ssf")
my_sim_data <- read_ssf("gsflow_sumq_05406500.ssf")

my_control_file <- c(
  "START SETTINGS",
  "  CONTEXT all",
  "  DATE_FORMAT mm/dd/yyyy",
  "END SETTINGS",

  "START GET_MUL_SERIES_SSF",
  "  CONTEXT all",
  "  FILE gsflow_sumq_05406500.ssf",
  "  SITE 05406500",
  "  NEW_SERIES_NAME Sq6500i",
  "END GET_MUL_SERIES_SSF",
  
  "START GET_MUL_SERIES_SSF",
  "  CONTEXT all",
  "  FILE Q_BEC_BE_6500.ssf",
  "  SITE 05406500",
  "  NEW_SERIES_NAME Oq6500i",
  "END GET_MUL_SERIES_SSF",

  "START REDUCE_TIME_SPAN",
  "  CONTEXT all",
  "  SERIES_NAME Oq6500i",
  "  NEW_SERIES_NAME Oq6500r",
  "  DATE_1 10/01/1990",
  "  DATE_2 09/30/2000",
  "END REDUCE_TIME_SPAN",
  
  "START NEW_TIME_BASE",
  "  CONTEXT all",
  "  SERIES_NAME Sq6500i",
  "  TB_SERIES_NAME Oq6500r",
  "  NEW_SERIES_NAME Sq6500",
  "END NEW_TIME_BASE",  
  
  "START SERIES_COMPARE", 
  "  CONTEXT all", 
  "  SERIES_NAME_SIM Sq6500",
  "  SERIES_NAME_OBS Oq6500r",
  "  NEW_C_TABLE_NAME c_series_compare",
  "  BIAS yes",
  "  STANDARD_ERROR yes",
  "  RELATIVE_BIAS yes",
  "  NASH_SUTCLIFFE yes",
  "  COEFFICIENT_OF_EFFICIENCY yes",
  "  INDEX_OF_AGREEMENT yes",
  "  VOLUMETRIC_EFFICIENCY yes",
  "  EXPONENT 1",
  "END SERIES_COMPARE",
  
  "START LIST_OUTPUT", 
  "  CONTEXT all",
  "  FILE test__series_compare.txt",
  "  C_TABLE_NAME c_series_compare",
  "END LIST_OUTPUT",
  
  "START LIST_OUTPUT", 
  "  CONTEXT all",
  "  FILE test__series_compare_inputs.txt",
  "  SERIES_FORMAT long",
  "  SERIES_NAME Oq6500r",
  "  SERIES_NAME Sq6500",
  "END LIST_OUTPUT"
)

writeLines(text=my_control_file,
           con=tsproc_inp)

retval <- system2(tsproc_exe, args=paste(tsproc_inp, tsproc_log, sep=" "))

tsp_result <- read.table("test__series_compare.txt",
           skip=9, 
           as.is=TRUE,             
           header=FALSE,
           sep=":")

colnames(tsp_result) <- c("Description", "value")

tsp_values <- tsp_result$value

my_sim_subset <- subset(my_sim_data$dat, datetime >= DATE_1 & datetime <= DATE_2)
my_obs_subset <- subset(my_obs_data$dat, datetime >= DATE_1 & datetime <= DATE_2)

simvals <- my_sim_subset$value
obsvals <- my_obs_subset$value

n <- length(simvals)
mean_obs <- mean(obsvals)
mean_sim <- mean(simvals)

S0 <- sqrt( sum( ( obsvals - mean_obs )^2 ) / (n-1) )

BIAS <- sum(simvals - obsvals) / n

STANDARD_ERROR <- sqrt( sum( ( simvals - obsvals)^2 ) / (n-1) )

RELATIVE_BIAS <- BIAS / mean_obs

RELATIVE_STD_ERROR <- STANDARD_ERROR / S0

VOLUMETRIC_EFFICIENCY <- 1 - sum(abs(simvals - obsvals) ) / sum(obsvals)

numerator <- sum( ( simvals - obsvals )^2 ) 
denominator <- sum ( ( obsvals - mean_obs )^2 )

NASH_SUTCLIFFE <- 1. - numerator / denominator

COEFFICIENT_OF_EFFICIENCY <- 1 - sum( abs( simvals - obsvals ) ^ exponent ) /
                                 sum( abs( obsvals - mean_obs ) ^ exponent )

INDEX_OF_AGREEMENT <- 1 - sum( abs( simvals - obsvals ) ^ exponent ) /
                          sum( (abs( simvals - mean_obs )  + abs( obsvals - mean_obs ) ) ^ exponent )

Delta <- numeric(7)

Delta[1] <- tsp_result$value[grep("Bias", tsp_result$Description)] - BIAS
Delta[2] <- tsp_result$value[grep("Standard error", tsp_result$Description)] - STANDARD_ERROR
Delta[3] <- tsp_result$value[grep("Relative bias", tsp_result$Description)] - RELATIVE_BIAS
Delta[4] <- tsp_result$value[grep("Nash", tsp_result$Description)] - NASH_SUTCLIFFE
Delta[5] <- tsp_result$value[grep("Coefficient of efficiency", tsp_result$Description)] - COEFFICIENT_OF_EFFICIENCY
Delta[6] <- tsp_result$value[grep("Index of agreement", tsp_result$Description)] - INDEX_OF_AGREEMENT
Delta[7] <- tsp_result$value[grep("Volumetric efficiency", tsp_result$Description)] - VOLUMETRIC_EFFICIENCY


if( any( abs( Delta ) > 1e-4 ) ) {
  cat("FAIL")    
} else {
  cat("PASS")  
}
