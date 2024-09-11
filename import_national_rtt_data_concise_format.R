# 0. Load libraries and define functions and globals ----
# ═══════════════════════════════════════════════════════

# * 0.1. Load libraries ----
# ──────────────────────────
library(tidyverse)
library(parallel)

# * 0.2. Define functions ----
# ────────────────────────────

fnProcessFile <- function(x){
  # Read in the entire file
  df <- read.csv(x)
  # Find the last empty cell in the first column, as this should be the last row
  # before the header line
  last_empty_row <- max(match('', df[,1]))
  # Only trim the data if an empty row can be found
  if(!is.na(last_empty_row))
  {
    # Set the column names of df to be that of the next row
    names(df) <- gsub(' ', '.', df[last_empty_row+1,])
    # Trim df to remove the empty rows and the column header row
    df <- df[last_empty_row+2:NROW(df),]
  } 
  # Add the base filename to the data frame ...
  df <- df %>% mutate(Filename = basename(x), .before = 1)
  # Standardise field names
  df <- fnStandardiseFieldNames(df) %>%
  # ... and select the required fields
    select(any_of(c('Filename', 
                    'Provider_Code', 'Provider_Name',
                    'RTT_Part', 'RTT_Part_Desc',
                    'Treatment_Function_Code', 'Treatment_Function_Name', 
                    'Wk00', 'Wk01', 'Wk02', 'Wk03', 'Wk04',
                    'Wk05', 'Wk06', 'Wk07', 'Wk08', 'Wk09',
                    'Wk10', 'Wk11', 'Wk12', 'Wk13', 'Wk14',
                    'Wk15', 'Wk16', 'Wk17', 'Total'))) %>%
    # Trim any empty rows
    dplyr::filter(!is.na(Provider_Code)) %>%
    # Ensure numbers are numbers
    mutate(across(.cols = 8:26, .fns = as.integer))
  # Summarise the data
  df <- df %>% 
    group_by(Filename, Provider_Code, Provider_Name, 
             RTT_Part, RTT_Part_Desc, 
             Treatment_Function_Code, Treatment_Function_Name) %>%
    summarise(across(.cols = 1:19, .fns = function(x){sum(x, na.rm = TRUE)}),
              .groups = 'keep') %>%
    ungroup() %>%
    mutate(Wk00_18 = rowSums(across(8:25), na.rm = TRUE)) %>%
    select(1:7, 27, 26) %>%
    ungroup()
  # Return the data frame
  return(df)
}

fnStandardiseFieldNames <- function(df){
  # Standardise required field names
  field_names <- names(df)
  field_names[field_names=='Provider.Org.Code'] <- 'Provider_Code'
  field_names[field_names=='Provider.Org.Name'] <- 'Provider_Name'
  field_names[field_names=='RTT.Part.Name'|field_names=='RTT.Part.Type'] <- 'RTT_Part'
  field_names[field_names=='RTT.Part.Description'] <- 'RTT_Part_Desc'
  if(df[1, ]$Filename=='201607.csv'){
    # Special case where the Treatment Function Code is called Treatment Function Name
    field_names[field_names=='Treatment.Function.Name'] <- 'Treatment_Function_Code'
    field_names[field_names=='Treatment.Function.Description'] <- 'Treatment_Function_Name'
  } else {
    field_names[field_names=='Treatment.Function'|field_names=='Treatment.Function.Code'] <- 'Treatment_Function_Code'
    field_names[field_names=='Treatment.Function.Name'] <- 'Treatment_Function_Name'  
  }
  field_names <- gsub('^Gt.', 'Wk', field_names)
  field_names[grepl('^Wk', field_names)] <- gsub('\\.', '', str_sub(field_names[grepl('^Wk', field_names)], 1, 5))
  field_names[field_names=='Total.All'] <- 'Grand_Total'
  names(df) <- field_names
  return(df)
}

# * 0.3. RTT data directory ----
# ──────────────────────────────
data_path = 'C:\\Users\\richard.blackwell\\OneDrive - Health Innovation South West\\Data\\NHSD\\RTT\\'

# 1. Process data ----
# ════════════════════

# * 1.1. Process RTT data for concise data frame ----
# ───────────────────────────────────────────────────
# Create the clusters for parallelisation
n_cores <- detectCores()
# Leave one cluster free
clust <- makeCluster(n_cores - 1)
# Export the route creation function
clusterExport(clust, c('fnProcessFile', 'fnStandardiseFieldNames'))
# Export tidyverse
clusterEvalQ(clust, library('tidyverse'))
# Get the routes - distance and duration only using overview = FALSE
res <- parallel::parLapply(clust, X = list.files(data_path, full.names = TRUE), fun = fnProcessFile)
# Stop the clusters
stopCluster(clust)

# Combine the results into one data frame
df_data <- do.call('rbind', res)

# Save the data as an R object
saveRDS(df_data, 'rtt_data.rds')

# * 1.2. Process RTT data for last month detail ----
# ──────────────────────────────────────────────────

df_detail <- read.csv(tail(list.files(data_path, full.names = TRUE), n = 1)) %>% 
  
  select(any_of(c(
                  'Provider_Code', 'Provider_Name',
                  'RTT_Part', 'RTT_Part_Desc',
                  'Treatment_Function_Code', 'Treatment_Function_Name','Total')
                ) | any_of(matches('^Gt.'))) %>% names()
  


fnProcessFile(tail(list.files(data_path, full.names = TRUE), n = 1))