library (plyr)
library (dplyr)
library (rgdal)
library (sp)
library (compare)
source("eBirdColumnStrip.R")

#################################################################
#                   eBird Record Stripper                       #
#                                                               #
# This script pre-processes eBird records file and india        #
# shape files to create RDS files for uploading to shiny        #
#################################################################

#######################Configurations###########################
#Name of the eBird quarterly archive WITHOUT .zip
ebd_file_name <- 'ebd_IN_relApr-2021'

#Name of the India region shape file archive WITHOUT .zip
india_shape_file <- 'indiama-editedSQ'
state <- 'IN-KL'
################################################################

#Unzip and read eBird records
#unzip(paste('..\\data\\',ebd_file_name,'.zip',sep=''))
#ebd <- read.delim(paste(ebd_file_name,'.txt',sep=''), na.strings = c("NA", "", "null"), as.is=TRUE, quote="")

ebd <- readEbdColumns (ebd_file_name, '..\\data\\')

#' Returns the number of days in a vector of month. Feb returns 29 by default
#' 
#' @param m Index of the month 1..12.
#' @return Number of days in a particular month.
#' @examples
#' daysInMonth(5)

daysInMonth <- Vectorize (function(m=30){
  
  # Return the number of days in the month
  return(switch(m,
                '01' = 31,
                '02' = 29,
                '03' = 31,
                '04' = 30,
                '05' = 31,
                '06' = 30,
                '07' = 31,
                '08' = 31,
                '09' = 30,
                '10' = 31,
                '11' = 30,
                '12' = 31))
}
)


filterRecords <- function (state, dat)
{
  print(nrow(dat))
  print(state)
  # Strip unwanted columns from eBird records
  ebd_records <- subset(dat[dat$STATE.CODE == state,], select = c("TAXONOMIC.ORDER", "OBSERVATION.COUNT", "UNIQUE_SAMPLING_ID", "COMMON.NAME"))

    #Remove entries from shared lists
  ebd_records   <- ebd_records[!duplicated(ebd_records[c("UNIQUE_SAMPLING_ID","COMMON.NAME")]),]
    
  # Strip unwanted columns from eBird records
  ebd_records <- subset(ebd_records, select = c("TAXONOMIC.ORDER", "OBSERVATION.COUNT", "UNIQUE_SAMPLING_ID"))

  # Write to RDS file with compression
  saveRDS(ebd_records,    paste0('..\\data\\ebd_records_',state,'.rds'))
}

# Remove lists with NA duration (e.g. historical)
ebd <- ebd[!is.na(ebd$DURATION.MINUTES),]

#Add unique list identifier for removing duplicates
ebd <- within (ebd, UNIQUE_SAMPLING_ID <-  ifelse(is.na(GROUP.IDENTIFIER),SAMPLING.EVENT.IDENTIFIER,GROUP.IDENTIFIER))

#If subspecies, copy subspecies common name
ebd <- within (ebd, COMMON.NAME <-  ifelse(CATEGORY=='issf',SUBSPECIES.COMMON.NAME,COMMON.NAME))

#Create state list by removing duplicate state entries
ebd_states    <- ebd[!duplicated(ebd$STATE.CODE),]

# Strip unwanted columns from eBird states
ebd_states    <- subset(ebd_states, select = c("STATE.CODE", "STATE"))

#Splitting into state based records
sapply (ebd_states$STATE.CODE,filterRecords, dat <- ebd)
dat <- NULL #Release memory
#Create species list by removing duplicate species entries
ebd_species   <- ebd[!duplicated(ebd$TAXONOMIC.ORDER),]

#Create district list by removing duplicate district entries
ebd_districts <- ebd[!duplicated(ebd$COUNTY.CODE),]

#Create unique lists by removing duplicate lists
ebd_lists     <- ebd[!duplicated(ebd$UNIQUE_SAMPLING_ID),]

# At this point, the primary ebd data is no longer needed
ebd <-NULL #Release memory

# Strip unwanted columns from eBird species
ebd_species   <- subset(ebd_species, select = c("TAXONOMIC.ORDER", "COMMON.NAME"))

# Strip unwanted columns from eBird districts
ebd_districts <- subset(ebd_districts, select = c("COUNTY.CODE", "COUNTY"))

# Add a fortnight field. There are 12 months a year, 24 fortnights. E.g. 5.5 is 11th fortnight in a year
# E.g. 14th of September = 0.5 * int (14/30 + 0.5) = 0. 15th of February = 0.5 * int (15/30 + 0.5) = 0.5

ebd_lists <- within (ebd_lists, Fortnight <-  as.numeric(format(as.Date(OBSERVATION.DATE),"%m")) +
                             0.5 * as.integer(0.5 + as.numeric(format(as.Date(OBSERVATION.DATE),"%d"))/daysInMonth (as.integer(format(as.Date(OBSERVATION.DATE),"%m")))))

# Strip unwanted columns from eBird lists
ebd_lists <- subset(ebd_lists, select = c("STATE.CODE", "COUNTY.CODE", "Fortnight", "DURATION.MINUTES", "LONGITUDE", "LATITUDE", "UNIQUE_SAMPLING_ID", "ALL.SPECIES.REPORTED"))

#Unzip and open the shape file
unzip(paste('..\\data\\',india_shape_file,'.zip',sep=''))
indiamap <- rgdal::readOGR(paste(india_shape_file,'.shp', sep=''), india_shape_file)

sp::coordinates(ebd_lists) <- ~LONGITUDE+LATITUDE

# Map the CRS
sp::proj4string(ebd_lists) <- sp::proj4string(indiamap)

ebd_filters <- data.frame(FILTER=character(),
                          stringsAsFactors=FALSE)

ebd_lists_with_filter <- NULL

for (filterindex in 1:nrow(indiamap@data))
{
  # Store filter metadata in another dataframe
  ebd_filters [filterindex, ] <- as.character(indiamap$AREA_1[filterindex])
  head(ebd_filters,20)
  
  # Filter lists according to set filter polygons 
  india_selected  <- indiamap[filterindex, ]
  rgl_ebd_lists   <- ebd_lists
  rgl_ebd_lists$FILTER  <- 0;
  rgl_ebd_lists   <- rgl_ebd_lists[india_selected, ]
  
  # For all filtered lists, assign the filter_index
  rgl_ebd_lists$FILTER  <- filterindex;
  
  if(!is.null(ebd_lists_with_filter))
  {
    ebd_lists_with_filter <- rbind (ebd_lists_with_filter, rgl_ebd_lists)
  }
  else
  {
    ebd_lists_with_filter <- rgl_ebd_lists
  }
  
  filterindex  <- filterindex + 1
}

# Strip the list before joining
ebd_lists_with_filter <- subset(as.data.frame(ebd_lists_with_filter), select = c("UNIQUE_SAMPLING_ID", "FILTER"))

ebd_lists <- as.data.frame (ebd_lists)

# Join the filter assigned lists to the full lists. Remaining expected to be filter=0
ebd_lists <- join (ebd_lists, ebd_lists_with_filter, by = 'UNIQUE_SAMPLING_ID')

ebd_lists$FILTER[is.na(ebd_lists$FILTER)] <- 0

# Bug. Why join has 2 more than actual lists

saveRDS(ebd_species,    '..\\data\\ebd_species.rds')
saveRDS(ebd_states,     '..\\data\\ebd_states.rds')
saveRDS(ebd_districts,  '..\\data\\ebd_districts.rds')
saveRDS(ebd_filters,    '..\\data\\ebd_filters.rds')
saveRDS(ebd_lists,      '..\\data\\ebd_lists.rds')

#Remove temp files
unlink ('*.txt')
unlink ('*.pdf')
unlink (paste(india_shape_file,'.*',sep=''))
