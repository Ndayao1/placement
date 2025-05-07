# Read and process the Family Planing data -----------------------------------

## Load necessary packages ----
library(openxlsx2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(tidyverse) # can be hashed out as necessary to avoid conflicts with above packages


## Read datasets needed ----

fp <- vector("list", 10)  # create empty list

for (i in 1:10) {
  fp[[i]] <- read_xlsx("data/famplan.xlsx", sheet = i, start_row = 1)
}

## Clean and process the immunisation data ----

### Check that all 10 sheets have same column names ----

# Get column names of the first sheet as reference
reference_cols <- colnames(fp[[1]])

# Compare all other sheets to the reference
all_same <- sapply(fp, function(df) identical(colnames(df), reference_cols))

# View the result
all_same


### Add the year to each sheet and combine them: 
    #Loop through all 10 sheets,
    
    #Add a year column based on the sheet number,
    
    #Combine everything into one dataframe:      
    
    ### Set the years
    years <- 2016:2025

### Combine sheets row-wise and tag with year     

fp_data <- map2_dfr(
  .x = 1:10,
  .y = years,
  ~ read_xlsx(file = "data/famplan.xlsx", sheet = .x, startRow = 1) |>
    mutate(year = .y)
)

### Make year the first column
famplan <- fp_data |> 
  relocate(year, .before = 1)

# Pivot the data longer (assumes 'Facility' is the identifier column)
famplan_long <- famplan |>
  pivot_longer(
    cols = -c(year, facility),  # Keep 'year' and 'Facility', pivot everything else
    names_to = "indicator",
    values_to = "value"
  )

### Visualise the trend in key immunisation indicators for selected Health Facilities

#Filter and reshape data for the facilities  and select relevant columns

## Key indicators 1
sel_facilities <- c("Taraka UC", "Bitokara HC", "Kwikila HC")
