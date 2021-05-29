source("filterconfigpuller.r")
source("datapuller.r")

setwd('data')

g_states    <- readRDS('ebd_states.rds')
#g_records   <- readRDS('ebd_records.rds')
g_species   <- readRDS('ebd_species.rds')
g_lists     <- readRDS('ebd_lists.rds')
g_districts <- readRDS('ebd_districts.rds')
g_filters   <- readRDS('ebd_filters.rds')
g_admin_filters <- readRDS('admin_filters.rds')
g_admin_filters <- data.frame(lapply(g_admin_filters, as.character), stringsAsFactors=FALSE)
try(g_admin_filters <- getAdminFilters(), silent=TRUE)
g_all_filters <- sort(c(g_filters$FILTER, unique(g_admin_filters$Filter.Name)))
setwd('..')

firstState  <- g_states$STATE [1]
try(g_records <- getRecords(firstState), silent=TRUE)
g_current_state <- firstState
