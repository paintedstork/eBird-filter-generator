library (googlesheets)
library (splitstackshape)
library (tidyr)

gs_auth(token = "filterhandle.rds")

getRecords <- function (state)
{
  setwd('data')
  
  files <- paste0('ebd_records_',g_states[g_states$STATE == state,]$STATE.CODE,'.rds')
  
  records <- do.call("rbind", lapply(files, FUN = function(file) {
    readRDS(file)
  }))  
  
  setwd('..')
  
  return (records)  
}

