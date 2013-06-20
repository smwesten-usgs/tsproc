myargs <- commandArgs(trailingOnly = TRUE)

# this is where CMake BUILDS the TSPROC executable
#bin_dir <- myargs[1]
#bin_dir <- "d:/SMWData/Source_Code/tsproc/build/win32/tests"

exponent <- 1.0

#tsproc_exe <- paste(bin_dir,"../src/tsproc.exe",sep="/")
tsproc_exe <- myargs[1]
tsproc_inp <- "tsproc_test_v_table_to_series.inp"
tsproc_log <- "tsproc_test_v_table_to_series.log"

DATE_1 <- as.POSIXlt("1990-10-02")
DATE_2 <- as.POSIXlt("2000-10-01")

read_ssf <- function(filename) {
  
  v<-list()
  
  v$info<-paste("Values imported from",filename)
  v$header<-c("Site","Date","Time","Value")
  
  dat<-read.table(filename,colClasses=c("character","character",
                                     "character","numeric"), header=F)
  colnames(dat)<-c("site","date","time","value")
  
  dat$datetime <- strptime(paste(dat$date, dat$time),format="%m/%d/%Y %H:%M:%S")
  dat$julday <- as.numeric(dat$datetime)
  v$dat<-dat
  
  return(v)
  
}

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
  
  "START VOLUME_CALCULATION",
  "  CONTEXT all", 
  "  SERIES_NAME Oq6500r",
  "  NEW_V_TABLE_NAME VOq6500", 
  "  FLOW_TIME_UNITS sec",
  "  DATE_FILE dates_6500.txt",
  "END VOLUME_CALCULATION",
  
  "START VOLUME_CALCULATION",
  "  CONTEXT all",
  "  SERIES_NAME Sq6500",
  "  NEW_V_TABLE_NAME VSq6500",
  "  FLOW_TIME_UNITS sec",
  "  DATE_FILE dates_6500.txt",
  "END VOLUME_CALCULATION",
  
  "START V_TABLE_TO_SERIES",
  "  CONTEXT all",
  "  NEW_SERIES_NAME Sq6500_vol",
  "  V_TABLE_NAME VSq6500",
  "  TIME_ABSCISSA center",
  "END V_TABLE_TO_SERIES",
  
  "START V_TABLE_TO_SERIES",
  "  CONTEXT all",
  "  NEW_SERIES_NAME Oq6500_vol",
  "  V_TABLE_NAME VOq6500",
  "  TIME_ABSCISSA center",
  "END V_TABLE_TO_SERIES",
  
  "START V_TABLE_TO_SERIES",
  "  CONTEXT all",
  "  NEW_SERIES_NAME Sq6500_cuml",
  "  V_TABLE_NAME VSq6500",
  "  TIME_ABSCISSA center",
  "  CUMULATIVE",
  "  FACTOR 2.295684e-05",
  "END V_TABLE_TO_SERIES",
  
  "START V_TABLE_TO_SERIES",
  "  CONTEXT all",
  "  NEW_SERIES_NAME Oq6500_cuml",
  "  V_TABLE_NAME VOq6500",
  "  TIME_ABSCISSA center",
  "  CUMULATIVE",
  "  FACTOR 2.295684e-05",
  "END V_TABLE_TO_SERIES",
  
  "START LIST_OUTPUT", 
  "  CONTEXT all",
  "  FILE test__volume_calculation_obs.txt",
  "  V_TABLE_NAME VOq6500",
    "END LIST_OUTPUT",

  "START LIST_OUTPUT", 
  "  CONTEXT all",
  "  FILE test__volume_calculation_sim.txt",
    "  V_TABLE_NAME VSq6500",
  "END LIST_OUTPUT",
  
  "START LIST_OUTPUT", 
  "  CONTEXT all",
  "  FILE test__volume_to_series_obs.ssf",
  "  SERIES_FORMAT ssf",
  "  SERIES_NAME Oq6500_vol",
  "END LIST_OUTPUT",

  "START LIST_OUTPUT", 
  "  CONTEXT all",
  "  FILE test__volume_to_series_sim.ssf",
  "  SERIES_FORMAT ssf",
  "  SERIES_NAME Sq6500_vol",
  "END LIST_OUTPUT",
    
  "START LIST_OUTPUT", 
  "  CONTEXT all",
  "  FILE test__volume_to_series_cumulative_obs.ssf",
  "  SERIES_FORMAT ssf",
  "  SERIES_NAME Oq6500_cuml",
  "END LIST_OUTPUT",

  "START LIST_OUTPUT", 
  "  CONTEXT all",
  "  FILE test__volume_to_series_cumulative_sim.ssf",
  "  SERIES_FORMAT ssf",
  "  SERIES_NAME Sq6500_cuml",
  "END LIST_OUTPUT"
  
)

writeLines(text=my_control_file,
           con=tsproc_inp)

retval <- shell(cmd=paste(tsproc_exe, tsproc_inp, tsproc_log, sep=" "),
                 intern=TRUE,
                 ignore.stdout=FALSE)

tsp_obs_vol <- read_ssf("test__volume_to_series_obs.ssf")
tsp_sim_vol <- read_ssf("test__volume_to_series_sim.ssf")

tsp_obs_cum_vol <- read_ssf("test__volume_to_series_cumulative_obs.ssf")
tsp_sim_cum_vol <- read_ssf("test__volume_to_series_cumulative_sim.ssf")

mydates <- read.table("dates_6500.txt", as.is=TRUE)
mydates$date_1 <- strptime(paste(mydates$V1, mydates$V2),format="%m/%d/%Y %H:%M:%S")
mydates$date_2 <- strptime(paste(mydates$V3, mydates$V4),format="%m/%d/%Y %H:%M:%S")

my_sim_subset <- subset(my_sim_data$dat, datetime >= DATE_1 & datetime <= DATE_2)
my_obs_subset <- subset(my_obs_data$dat, datetime >= DATE_1 & datetime <= DATE_2)

simvals <- my_sim_subset$value
obsvals <- my_obs_subset$value

n <- length(simvals)
mean_obs <- mean(obsvals)
mean_sim <- mean(simvals)

Dalta <- 0

if( any( abs( Delta ) > 1e-4 ) ) {
  cat("FAIL")    
} else {
  cat("PASS")  
}
