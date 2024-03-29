library(plyr)
library(sp)
library(rgdal)
library(reshape2)
library(data.table)
library(tidyr)

#' Returns lists filtered by location 
#' 
#' @param lists eBird lsits
#' @param state One of the states in the eBird records file
#' @param region One of the regions in the shape file
#' @return eBird list for the specific location
#' @examples
#' getLocationFilteredLists(ebd_lists, 'Kerala', 'India--Kerala--Alappuzha')

getLocationFilteredLists <- function (lists, state, filterRegion) {
  
  print( paste("GetLocationFilteredLists",state,filterRegion))
  
  if(state !='None')  
  {
    lists <- lists [which(lists$STATE.CODE == 
                                        unique(g_states$STATE.CODE[ g_states$STATE==state ])), ]
    print(paste("StateFiltered", nrow(lists)))  
  }
  

  if(any(g_filters$FILTER == filterRegion))  
  { # Filter only lists that belong to a particular arbitrary polygon
    lists <- lists [which(lists$FILTER == which(g_filters$FILTER == filterRegion)), ]
  }
  else
  { # Filter only lists that do not belong to any arbitrary polygon
    lists <- lists [lists$FILTER == 0, ]
  }
  print(paste("GeoFiltered", nrow(lists)))  
  
  if(any (g_admin_filters$Filter.Name == filterRegion))  
  { # Find out all matches for a filter name. It can be state or district codes
    admin_filter_match <- grep (filterRegion, g_admin_filters$Filter.Name, fixed = TRUE)
    # Bind the lists that match with the state or district code
    lists <- rbind ( 
                    lists [which(lists$STATE.CODE %in% g_admin_filters [admin_filter_match, ]$Code), ],
                    lists [which(lists$COUNTY.CODE %in% g_admin_filters [admin_filter_match, ]$Code), ]
                   )
    
  }
  print(paste("Final Filtered", nrow(lists)))  
  return (lists)
}

#' Returns duration in minutes at nth percentile based on sorted order of list duration
#' 
#' @param q 0..100 as percentile
#' @param state One of the states in the eBird records file
#' @param region One of the regions in the shape file
#' @return Duration in minutes at the nth percentile
#' @examples
#' getMinutes(75, 'Kerala', 'India--Kerala--Alappuzha')

  
getMinutes <- function(q, state, filterRegion) {
  
  print(q)
  print(state)
  print(filterRegion)
  
  m_ebd_lists <-g_lists
  
  # Filter lists by state and filter shape
  m_ebd_lists <- getLocationFilteredLists (m_ebd_lists, state, filterRegion)
  

  # Convert back to data frame and filter only duration
  m_ebd_lists <- subset(m_ebd_lists, select = c("DURATION.MINUTES")) 

  # Remove Null duration  

# m_ebd_lists[is.na(m_ebd_lists)] <- 0
# Order duration ascending 
#  m_ebd_lists <- m_ebd_lists [order(m_ebd_lists$DURATION.MINUTES),]
  
  # Bug fix 12052019. Remove null duration and sort by minutes
  m_ebd_lists <- drop_na (m_ebd_lists) %>% arrange (DURATION.MINUTES)
  
  # Return requested quantile
  return (quantile (m_ebd_lists$DURATION.MINUTES, q/100))
}
  


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
  print(paste("generateFilter", state, filterRegion, filterPercentile, duration, fortnightly, makeXAs1, dataView))

#  filter <- as.data.frame (rbind( c("Bar-headed Goose", 0, 1, 1, 2, 0, 1, 1, 2, 0, 1, 1, 2),
#                           c("Lesser Whistling Duck", 0, 1, 1, 2, 0, 1, 1, 2, 0, 1, 1, 2), 
#                           c("Comb Duck", 0, 1, 1, 2, 0, 1, 1, 2, 0, 1, 1, 2) ))
    

#  return (filter)
    
  # Copy locally
  f_ebd_lists <-g_lists
  
  # Filter lists by state and filter shape
  f_ebd_lists <- getLocationFilteredLists (f_ebd_lists, state, filterRegion)

  if(nrow(f_ebd_lists) < 1)
  {
    return (NULL)
  }
  #All lists > duration hours. Note, this comes from slider bar input
  f_ebd_lists <- f_ebd_lists [which(f_ebd_lists$DURATION.MINUTES < duration+1), ]

  # Add a fortnight field. There are 12 months a year, 24 fortnights. E.g. 5.5 is 11th fortnight in a year
  # E.g. 14th of September = 0.5 * int (14/30 + 0.5) = 0. 15th of February = 0.5 * int (15/30 + 0.5) = 0.5

#  f_ebd_lists <- within (f_ebd_lists, Fortnight <-  as.numeric(format(as.Date(OBSERVATION.DATE),"%m")) +
#                             fortnightly * 0.5 * as.integer(0.5 + as.numeric(format(as.Date(OBSERVATION.DATE),"%d"))/daysInMonth (as.integer(format(as.Date(OBSERVATION.DATE),"%m")))))
  
  
  f_ebd_lists <- within (f_ebd_lists, if (!fortnightly) Fortnight <- floor (Fortnight))
                         
  # Filter only relevant fields
  f_ebd_lists <- subset(f_ebd_lists, select = c("UNIQUE_SAMPLING_ID","Fortnight", "ALL.SPECIES.REPORTED"))
  
# State caching optimization with lazy load of state records.  
  if( (g_current_state == state) || (state == "None"))
  {
    f_ebd_records <- g_records
  }
  else
  {
    # Load state records
    f_ebd_records <- getRecords(state)
  }
  g_current_state = state

#  f_ebd_records <- join(f_ebd_records, f_ebd_lists, type="inner", by = 'UNIQUE_SAMPLING_ID')
  
  # Merge with lists
  dt_ebd_records  = as.data.table(f_ebd_records)
  dt_ebd_lists    = as.data.table(f_ebd_lists)

  setkey(dt_ebd_records, UNIQUE_SAMPLING_ID)
  setkey(dt_ebd_lists, UNIQUE_SAMPLING_ID)
  
  # inner join - use `nomatch` argument
#  f_ebd_records <- as.data.frame(dt_ebd_records[dt_ebd_lists, nomatch=0L, on = "UNIQUE_SAMPLING_ID"])
  f_ebd_records <- as.data.table(dt_ebd_records[dt_ebd_lists, nomatch=0L, on = "UNIQUE_SAMPLING_ID"])
  
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

  
  # Filter only relevant fields
  f_ebd_records <- subset(f_ebd_records, select = c("TAXONOMIC.ORDER","OBSERVATION.COUNT", "Fortnight", "ALL.SPECIES.REPORTED"))
  #f_ebd_records <- f_ebd_records [which(!is.na (f_ebd_records$Count)), ]
  
  # Rename columns
  colnames(f_ebd_records) <- c("TOrder", "Count", "Fortnight", "AllSpecies")

#  print(paste("Before dcast", names(f_ebd_lists), nrow(f_ebd_lists)))
  
  # Create a pivot with AllSpecies vs Fortnight for getting the complete and incomplete lists
#  all_lists <- reshape2::dcast (f_ebd_lists, 'ALL.SPECIES.REPORTED' ~ Fortnight, value.var = "ALL.SPECIES.REPORTED", fun.aggregate = sum)
  all_lists <- dcast.data.table (dt_ebd_lists, 'ALL.SPECIES.REPORTED' ~ Fortnight, value.var = "ALL.SPECIES.REPORTED", fun.aggregate = sum)
    print("Pivot with AllSpecies vs Fortnight Created")
    print(head(all_lists, 3))
  # Make NA as 0
  all_lists[is.na(all_lists)] <- 0
#  all_lists[,-1] <- as.numeric(all_lists[,-1])

#  print(paste("Before dcast", names(f_ebd_records), nrow(f_ebd_records)))

  # Make Observation count as numeric
  f_ebd_records <- transform(f_ebd_records, Count = as.numeric(as.character(Count)))
  
  # Create a pivot with TOrder vs Fortnight and values as 90 percentile. Note, this will come from slider bar
#  filter <- reshape2::dcast (f_ebd_records, TOrder ~ Fortnight, value.var = "Count", fun.aggregate = quantile, filterPercentile/100)
  filter <- dcast.data.table (f_ebd_records, TOrder ~ Fortnight, value.var = "Count", sep = "_", fun.aggregate = quantile, filterPercentile/100)
#  print("Pivot with TOrder vs Fortnight Created")
#  print(head(filter, 3))
  
  # Create a pivot with TOrder vs Fortnight and number of complete lists where it was reported
#  c_lists <- reshape2::dcast (f_ebd_records, TOrder ~ Fortnight, value.var = "AllSpecies", fun.aggregate = sum)
  c_lists <- dcast.data.table (f_ebd_records, TOrder ~ Fortnight, value.var = "AllSpecies", sep = "_", fun.aggregate = sum)
#  print("Pivot with TOrder vs Fortnight and number of complete lists Created")
#  print(head(c_lists, 3))
  # Make NA as 0
  c_lists[is.na(c_lists)] <- 0

  filter <- as.data.frame(filter)
  c_lists <- as.data.frame (c_lists)
  all_lists <- as.data.frame (all_lists)
  
  # Rounding filter values
  filter[,-c(1)] <-round(filter[,-c(1)],0)
  # Make NA as 0
  filter[is.na(filter)] <- 0
  
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
  
#  setkey(f_ebd_species, TOrder)
#  setkey(filter, TOrder)
  
  filter <- join(f_ebd_species, filter, type = "right", by = 'TOrder')
#  print("Joined TOrder and Species Names")
#  print(head(filter,3))
  #  write.csv2(filter, "filter.csv")
  
  filter$TOrder <- NULL
  
  colnames(all_lists)[1] <- "Species" 

  all_lists$Species[1]="No Complete Lists"
  filter <- rbind (all_lists,filter)    
#  filter <- rbindlist (list (all_lists,filter))    
#  print("Binded all_lists with filter")
#  print(head(filter,3))
  
  # Make NA as 0
  filter[is.na(filter)] <- 0
#  print("Removed NA")
#  print(head(filter,3))
  
  # Bugfix. Removing duplicate entries in filter
  filter <- filter [!duplicated(filter), ]  
#  print("Removed duplicates in filter")
#  print(head(filter,3))
  
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
#      print(colname)
#      print(colnames(filter))
      if (! (colname %in% colnames(filter)))
      {
#        print("May be going to crash here")
        filter[colname] <- '-'         
      }
      col <-col + 1
    }
#    print("Assiging filters")
    filter <- filter[c("Species", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")]
#    print("Assiging colnames")
    colnames (filter) <- c("Species", "J","F","M","A","M","J","J","A","S","O","N","D")
#    print(head(filter,3))
  }
#  print("Added Column names")
#  print(head(filter,3))
  
#  write.csv2(filter, "filter_final.csv")
  
  return (filter)
}
