library (googlesheets)
library (splitstackshape)
library (tidyr)

gs_auth(token = "filterhandle.rds")
sheetRead <- function (filename, sheetname, anchorstart, rows, columns)  {
  
  config_file <- gs_title(filename)
  gs_ws_ls(config_file)
  
  # Read without column names to avoid type casting to integer
  mysheet <- gs_read(ss=config_file, 
                         ws = sheetname,
                         range = anchored (anchorstart, dim = c(rows, columns)),
                         col_names = FALSE,
                         skip=0)
  mysheet <- as.data.frame(mysheet)
  
  # Make first row as header
  colnames(mysheet) <- as.character(mysheet[1,])
  
  # Remove redundant header row
  mysheet = mysheet[-1,]
  
  return (mysheet)
}

getAdminFilters <- function ()
{
  adminfilters <- sheetRead("eBird-India-Filter-Regions", "AdminFilters", "A1", 1000,2) %>%   
    drop_na()

  adminfilters <- adminfilters [!duplicated(adminfilters), ] 
}

