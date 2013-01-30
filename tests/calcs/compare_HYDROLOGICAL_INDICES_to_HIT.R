myargs <- commandArgs(trailingOnly = TRUE)

setwd("d:\\SMWData\\Source_Code\\tsproc\\build\\win_x86\\gfortran\\tests\\calcs")

source("function__read_ssf_file.R")

# this is where CMake BUILDS the TSPROC executable
tsproc_exe <- myargs[1]
tsproc_exe <- "d:\\dos\\tsproc.exe"

exponent <- 1.0

tsproc_inp <- "tsproc_test_series_compare.inp"
tsproc_log <- "tsproc_test_series_compare.log"

DATE_1 <- as.POSIXlt("1990-10-02")
DATE_2 <- as.POSIXlt("2000-10-01")

my_hit_hat_output <- read.csv("hit_hat_output_BEC.csv", as.is=TRUE)

my_control_file <- c(
  "START SETTINGS",
  "  CONTEXT all",
  "  DATE_FORMAT mm/dd/yyyy",
  "END SETTINGS",
  
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

  "START HYDROLOGIC_INDICES",
  "  CONTEXT all",
  "  SERIES_NAME Oq6500r",
  "  NEW_G_TABLE_NAME BEC_hyd_index",
  "  DRAINAGE_AREA 41.6",
  "  STREAM_CLASSIFICATION all_streams",
  "  FLOW_COMPONENT average_magnitude",
  "  FLOW_COMPONENT low_flow_magnitude",
  "  FLOW_COMPONENT high_flow_magnitude",
  "  FLOW_COMPONENT low_flow_frequency",  
  "  FLOW_COMPONENT high_flow_frequency",
  "  FLOW_COMPONENT low_flow_duration",  
  "  FLOW_COMPONENT high_flow_duration",  
  "  FLOW_COMPONENT timing",
  "  FLOW_COMPONENT rate_of_change",  
"END HYDROLOGIC_INDICES  ",
  
  "START LIST_OUTPUT", 
  "  CONTEXT all",
  "  FILE test__hydraulic_indices.txt",
  "  G_TABLE_NAME BEC_hyd_index",
  "END LIST_OUTPUT"
)

writeLines(text=my_control_file,
           con=tsproc_inp)

retval <- shell(cmd=paste(tsproc_exe, tsproc_inp, tsproc_log, sep=" "),
                 intern=TRUE,
                 ignore.stdout=FALSE)

tsp_result <- read.fortran("test__hydraulic_indices.txt",
           skip=3, 
           as.is=TRUE,             
           format=c("A6","2x","A72","1F17.0")
           )                           

colnames(tsp_result) <- c("Description", "value")

tsp_values <- tsp_result$value



if( any( abs( Delta ) > 1e-4 ) ) {
  cat("FAIL")    
} else {
  cat("PASS")  
}
