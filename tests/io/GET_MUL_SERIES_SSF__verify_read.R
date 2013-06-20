myargs <- commandArgs(trailingOnly = TRUE)

source("function__read_ssf_file.R")

tsproc_exe <- myargs[1]

tsproc_inp <- "tsproc_get_mul_series_ssf.inp"
tsproc_log <- "tsproc_get_mul_series_ssf.log"

my_ssf_file <- read_ssf("MI_stream_obs.ssf")

ssf_dat <- my_ssf_file$dat

my_control_file <- c(
  "START SETTINGS",
  "  CONTEXT all",
  "  DATE_FORMAT mm/dd/yyyy",
  "END SETTINGS",
  
  "START GET_MUL_SERIES_SSF",
  "  CONTEXT all",
  "  FILE MI_stream_obs.ssf",
  "  SITE 04122200",
  "  NEW_SERIES_NAME 04122200",
  "  SITE 04126000",
  "  NEW_SERIES_NAME 04126000",
  "  SITE 04055000", 
  "  NEW_SERIES_NAME 04055000",
  "  SITE 04059000", 
  "  NEW_SERIES_NAME 04059000",
  "  SITE 04065500", 
  "  NEW_SERIES_NAME 04065500",
  "  SITE 04097500",
  "  NEW_SERIES_NAME 04097500",
  "  SITE 04105000", 
  "  NEW_SERIES_NAME 04105000",
  "  SITE 04111000", 
  "  NEW_SERIES_NAME 04111000",
  "  SITE 04115000",
  "  NEW_SERIES_NAME 04115000",
  "END GET_MUL_SERIES_SSF",
  
  "START LIST_OUTPUT", 
  "  CONTEXT all",
  "  FILE test__get_mul_series_ssf.ssf",
  "  SERIES_FORMAT ssf",
  "  SERIES_NAME 04122200",
  "  SERIES_NAME 04126000",
  "  SERIES_NAME 04055000",
  "  SERIES_NAME 04059000",
  "  SERIES_NAME 04065500",
  "  SERIES_NAME 04097500",
  "  SERIES_NAME 04105000",
  "  SERIES_NAME 04111000",
  "  SERIES_NAME 04115000",
  "END LIST_OUTPUT"
)

writeLines(text=my_control_file,
           con=tsproc_inp)

retval <- system2(tsproc_exe, args=paste(tsproc_inp, tsproc_log, sep=" "))

tsp_result <- read_ssf("test__get_mul_series_ssf.ssf")

tsp_dat <- tsp_result$dat

mysites <- unique(tsp_dat$site)

nrows <- length(mysites)

Delta_min <- data.frame(site=character(nrows), R=numeric(nrows), tsproc=numeric(nrows), difference=numeric(nrows))
Delta_max <- Delta_min
Delta_mean <- Delta_min

for(n in seq(1,nrows) ) {
  
  mysite <- mysites[n]
  
  Delta_min[n, "R"] <- min(ssf_dat$value[ssf_dat$site == mysite])
  Delta_min[n, "tsproc"] <- min(tsp_dat$value[tsp_dat$site == mysite])
  
  Delta_mean[n, "R"] <- mean(ssf_dat$value[ssf_dat$site == mysite])
  Delta_mean[n, "tsproc"] <- mean(tsp_dat$value[tsp_dat$site == mysite])

  Delta_max[n, "R"] <- max(ssf_dat$value[ssf_dat$site == mysite])
  Delta_max[n, "tsproc"] <- max(tsp_dat$value[tsp_dat$site == mysite])
  
}

Delta_min$difference <- Delta_min$tsproc - Delta_min$R
Delta_mean$difference <- Delta_mean$tsproc - Delta_mean$R
Delta_max$difference <- Delta_max$tsproc - Delta_max$R

if( all( abs( Delta_min$difference ) < 1e-3 ) & all( abs( Delta_mean$difference ) < 1e-3 ) &
       all( abs( Delta_max$difference ) < 1e-3 ) ) {
  cat("PASS")    
} else {
  cat("FAIL")  
}
