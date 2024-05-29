library(tidyverse)
library(lubridate)
source("merge.R")

# Download and import data for each year from 109 to 112
data_by_year <- lapply(109:112, function(year) download_and_import_data(year))

merged_data <- list()
# Merge data for year 112
for(i in seq_along(data_by_year)){
  merged_data[[i]] <- merge_allStudent_native(
    allStudent112 = data_by_year[[i]]$allStudent, 
    native112 = data_by_year[[i]]$native
  )
}

# Display the structure of merged data
glimpse(merged_data)
