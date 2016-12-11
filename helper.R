library (plyr)
library(sp)
library(rgdal)
library(reshape2)

#' Returns duration in minutes at nth percentile based on sorted order of list duration
#' 
#' @param q 0..100 as percentile
#' @param state One of the states in the eBird records file
#' @param region One of the regions in the shape file
#' @return Duration in minutes at the nth percentile
#' @examples
#' getMinutes(75, 'Kerala', 'India--Kerala--Alappuzha')

getMinutes <- function(q, state, filterRegion) {

  m_ebd_lists <-g_lists
  
  if(state !='None')  
  {
    m_ebd_lists <- m_ebd_lists [which(m_ebd_lists$SUBNATIONAL1_CODE == 
                                        unique(g_states$SUBNATIONAL1_CODE[ g_states$STATE_PROVINCE==state ])), ]
  }
  
  if(filterRegion !='None')  
  {
    m_ebd_lists <- m_ebd_lists [which(m_ebd_lists$FILTER == which(g_filters$FILTER == filterRegion)), ]
  }
  
  # Convert back to data frame and filter only duration
  m_ebd_lists <- subset(m_ebd_lists, select = c("DURATION.MINUTES"))

  # Remove Null duration  
  m_ebd_lists[is.na(m_ebd_lists)] <- 0
  
  # Order duration ascending 
  m_ebd_lists <- m_ebd_lists [order(m_ebd_lists$DURATION.MINUTES),]
  
  # Return requested quantile
  return (quantile (m_ebd_lists, q/100))
}
  

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

#  
# 
#' Generates the filter based on region shape and eBird data using a set of configurations
#' 
#' @param state state/province code
#' @param filterRegion Attribute name in the shape file
#' @param filterPercentile Percentile of records of a species to be considered as filter limit
#' @param duration Maximum list duration to be considered or filter calculations
#' @param fortnightly Whether to generate filters per fortnightly or monthly
#' @param makeXAs1 Whether to consider records marked as X as 1 or ignore
#' @return filter dataframe with species as rows and fortnight/month as column with values in cells.
#' @examples
#' generateFilter ('IN-KL', 'India--Kerala--Alappuzha')
generateFilter <- function(state='None', filterRegion, filterPercentile=90, duration=240, fortnightly=TRUE, makeXAs1=FALSE, dataView=1)
{
  # Copy locally
  f_ebd_lists <-g_lists
  
  if(state !='None')  
  {
    f_ebd_lists <- f_ebd_lists [which(f_ebd_lists$SUBNATIONAL1_CODE == 
                                        unique(g_states$SUBNATIONAL1_CODE[ g_states$STATE_PROVINCE==state ])), ]
  }

  if(filterRegion !='None')  
  {
    f_ebd_lists <- f_ebd_lists [which(f_ebd_lists$FILTER == 
                                        which(g_filters$FILTER == filterRegion)), ]
  }

  
  #All lists > duration hours. Note, this comes from slider bar input
  f_ebd_lists <- f_ebd_lists [which(f_ebd_lists$DURATION.MINUTES < duration+1), ]

  # Add a fortnight field. There are 12 months a year, 24 fortnights. E.g. 5.5 is 11th fortnight in a year
  # E.g. 14th of September = 0.5 * int (14/30 + 0.5) = 0. 15th of February = 0.5 * int (15/30 + 0.5) = 0.5
  f_ebd_lists <- within (f_ebd_lists, Fortnight <-  as.numeric(format(as.Date(OBSERVATION.DATE),"%m")) +
                             fortnightly * 0.5 * as.integer(0.5 + as.numeric(format(as.Date(OBSERVATION.DATE),"%d"))/daysInMonth (as.integer(format(as.Date(OBSERVATION.DATE),"%m")))))

  # Filter only relevant fields
  f_ebd_lists <- subset(f_ebd_lists, select = c("UNIQUE_SAMPLING_ID","Fortnight", "ALL.SPECIES.REPORTED"))
  
  f_ebd_records <- g_records
  
  # Merge with lists
  f_ebd_records <- join(f_ebd_records, f_ebd_lists, type="inner", by = 'UNIQUE_SAMPLING_ID')
  
  if(makeXAs1)
  {
    #Replace all X with 1
    f_ebd_records[f_ebd_records=="X"]<-1
  }
  else
  {
    #Remove all "X" observations. 
    f_ebd_records <- f_ebd_records [which(f_ebd_records$OBSERVATION.COUNT != 'X'), ]
  }
  
  # Make Observation count as numeric
  f_ebd_records <- transform(f_ebd_records, OBSERVATION.COUNT = as.numeric(as.character(OBSERVATION.COUNT)))
  
  # Filter only relevant fields
  f_ebd_records <- subset(f_ebd_records, select = c("TAXONOMIC.ORDER","OBSERVATION.COUNT", "Fortnight", "ALL.SPECIES.REPORTED"))
  #f_ebd_records <- f_ebd_records [which(!is.na (f_ebd_records$Count)), ]
  
  # Rename columns
  colnames(f_ebd_records) <- c("TOrder", "Count", "Fortnight", "AllSpecies")

  # Create a pivot with AllSpecies vs Fortnight for getting the complete and incomplete lists
  all_lists <- dcast (f_ebd_lists, 'ALL.SPECIES.REPORTED' ~ Fortnight, value.var = "ALL.SPECIES.REPORTED", fun.aggregate = sum)
  # Make NA as 0
  all_lists[is.na(all_lists)] <- 0
  all_lists[,-1] <- as.numeric(all_lists[,-1])
  
  # Create a pivot with TOrder vs Fortnight and values as 90 percentile. Note, this will come from slider bar
  filter <- dcast (f_ebd_records, TOrder ~ Fortnight, value.var = "Count", fun.aggregate = quantile, filterPercentile/100)

  # Rounding filter values
  filter[,-1] <-round(filter[,-1],0)
  # Make NA as 0
  filter[is.na(filter)] <- 0
  
  # Create a pivot with TOrder vs Fortnight and number of complete lists where it was reported
  c_lists <- dcast (f_ebd_records, TOrder ~ Fortnight, value.var = "AllSpecies", fun.aggregate = sum)
  # Make NA as 0
  c_lists[is.na(c_lists)] <- 0

  if(dataView == 1) {
# Do Nothing 
  }
  else if (dataView == 2) {
    filter <- c_lists
    
  } 
  else {
    for (col in 2:ncol(filter))
    {
      filter[,col] <- paste(filter[,col],"\n(",c_lists[,col],")",sep='')
    }
  }
  
  # Merge with species names
  f_ebd_species <- g_species
  colnames(f_ebd_species) <- c("TOrder", "Species")
  
  filter <- join(f_ebd_species, filter, type = "right", by = 'TOrder')
  
  filter$TOrder <- NULL
  
  colnames(all_lists)[1] <- "Species" 

  all_lists$Species[1]="No Complete Lists"
  filter <- rbind (all_lists,filter)    

  # Make NA as 0
  filter[is.na(filter)] <- 0

  # Bugfix. Removing duplicate entries in filter
  filter <- filter [!duplicated(filter), ]  

  #Based on Fortnight or Month, assigning characters. Can this dirty logic be normalised?
  if (fortnightly)
  {
    for (col in 1:24)
    {
      if(col%%2)
      {
        colname <- as.character(as.integer((col+1)/2))
      
      }
      else
      {
        colname <- paste(as.character(as.integer((col+1)/2)),'.',as.character(as.integer(((col+1)%%2)*5)),sep='')
      }

      if (! (colname %in% colnames(filter)))
      {
        filter[colname] <- '-'         
      }
      col <-col + 1
    }
    filter <- filter[c("Species", "1", "1.5", "2", "2.5","3","3.5", "4", "4.5", "5","5.5", "6","6.5", "7","7.5", "8","8.5", "9","9.5", "10","10.5", "11","11.5", "12", "12.5")]
    colnames (filter) <- c("Species", "J","J","F","F","M","M","A","A","M","M","J","J","J","J","A","A","S","S","O","O","N","N","D","D")
  }
  else
  {
    for (col in 1:12)
    {
      colname <- as.character(col)
      if (! (colname %in% colnames(filter)))
      {
        filter[colname] <- '-'         
      }
      col <-col + 1
    }
    filter <- filter[c("Species", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")]
    colnames (filter) <- c("Species", "J","F","M","A","M","J","J","A","S","O","N","D")
  }

  return (filter)
}
