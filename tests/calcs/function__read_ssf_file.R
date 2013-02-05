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