myargs <- commandArgs(trailingOnly = TRUE)

source("function__read_ssf_file.R")

# this is where CMake BUILDS the TSPROC executable
tsproc_exe <- myargs[1]

exponent <- 1.0

tsproc_inp <- "tsproc_test_hydrological_indices.inp"
tsproc_log <- "tsproc_test_hydrological_indices.log"

DATE_1 <- as.POSIXlt("1990-10-01")
DATE_2 <- as.POSIXlt("2000-10-01")

# the file referenced below includes HIT/HAT output run for water years 1991-2000
# for the Black Earth Creek gage (i.e. 10-01-1990 through 09-30-2000)
my_hit_hat_output <- read.csv("BEC_HIT_ANALYSIS.csv", as.is=TRUE)

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
  "  DATE_1 09/30/1990",
  "  DATE_2 09/30/2000",
  "END REDUCE_TIME_SPAN",

  "START HYDROLOGIC_INDICES",
  "  CONTEXT all",
  "  SERIES_NAME Oq6500r",
  "  NEW_G_TABLE_NAME BEC_hyd_index",
  "  DRAINAGE_AREA 41.6",
  #"  USE_MEDIAN",
  #"  STREAM_CLASSIFICATION all_streams",
  #"  FLOW_COMPONENT average_magnitude",
  #"  FLOW_COMPONENT low_flow_magnitude",
  #"  FLOW_COMPONENT high_flow_magnitude",
  #"  FLOW_COMPONENT low_flow_frequency",  
  #"  FLOW_COMPONENT high_flow_frequency",
  #"  FLOW_COMPONENT low_flow_duration",  
  #"  FLOW_COMPONENT high_flow_duration",  
  #"  FLOW_COMPONENT timing",
  #"  FLOW_COMPONENT rate_of_change",  
"END HYDROLOGIC_INDICES  ",
  
  "START LIST_OUTPUT", 
  "  CONTEXT all",
  "  FILE test__hydraulic_indices.txt",
  "  G_TABLE_NAME BEC_hyd_index",
  "END LIST_OUTPUT"
)

writeLines(text=my_control_file,
           con=tsproc_inp)

retval <- system2(tsproc_exe, args=paste(tsproc_inp, tsproc_log, sep=" "))

tsp_result <- read.fortran("test__hydraulic_indices.txt",
           skip=3, 
           as.is=TRUE,             
           format=c("A80","1x","1F17.0")
           )                           

colnames(tsp_result) <- c("temp", "value")

tsp_values <- tsp_result$value

Delta <- tsp_result
colnames(Delta)[1] <- "description"
colnames(Delta)[2] <- "TSPROC_value"
Delta$HIT_value <- numeric(nrow(Delta))
Delta$difference <- numeric(nrow(Delta))
Delta$RPD <- numeric(nrow(Delta))

Delta$difference <- NA

for (n  in seq(1,nrow(tsp_result) ) )  {
   
  mylist <- unlist(strsplit(tsp_result[n,1], ":"))
  mystat <- gsub(" ","",mylist[1])
    
  hit_hat_value <- as.numeric(my_hit_hat_output$Value[match(mystat,my_hit_hat_output$Index)])
  tsproc_value <-  as.numeric(tsp_result[n,2])
  rounded_tsproc_value <- round(tsproc_value, digits=3)
  Delta[n, "TSPROC_value"] <- rounded_tsproc_value
  Delta[n, "HIT_value"] <- hit_hat_value
  Delta[n, "difference"] <- round(tsproc_value, digits=3) - hit_hat_value
  Delta[n, "RPD"] <- ( rounded_tsproc_value - hit_hat_value ) / (hit_hat_value + 1.e-6) * 100.
      
}

Delta <- subset(Delta, !is.na(Delta$HIT_value) )

if( any( abs( Delta$RPD ) > 10. ) ) {
  cat("FAIL")    
} else {
  cat("PASS")  
}
