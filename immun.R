# Read and process the Immunisation data -----------------------------------

## Load necessary packages ----
library(openxlsx2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(tidyverse)
#library(tidyverse) #hashed out to avoid conflicts with above packages


## Read datasets needed ----

hssdp <- vector("list", 10)  # create empty list

for (i in 1:10) {
  hssdp[[i]] <- read_xlsx("data/immunisation.xlsx", sheet = i, start_row = 1)
}

## Clean and process the immunisation data ----

### Check that all 10 sheets have same column names ----

# Get column names of the first sheet as reference
reference_cols <- colnames(hssdp[[1]])

# Compare all other sheets to the reference
all_same <- sapply(hssdp, function(df) identical(colnames(df), reference_cols))

# View the result
all_same


### Add the year to each sheet and combine them: 
    #Loop through all 10 sheets,
    
    #Add a year column based on the sheet number,
    
    #Combine everything into one dataframe:      
    
    ### Set the years
    years <- 2016:2025

### Combine sheets row-wise and tag with year     

immunisation <- map2_dfr(
  .x = 1:10,
  .y = years,
  ~ read_xlsx(file = "data/immunisation.xlsx", sheet = .x, startRow = 1) |>
    mutate(year = .y)
)

### Make year the first column
immun <- immunisation |> 
  relocate(year, .before = 1)

# Pivot the data longer (assumes 'Facility' is the identifier column)
immun_long <- immun |>
  pivot_longer(
    cols = -c(year, facility, code),  # keep all identifier columns
    names_to = "indicator",
    values_to = "value"
  )


# Create a province mapping based on hf_code -- This can be moved around as appropriate.

immun_data <- immun_long |>
  mutate(
    province = case_when(
      code %in% c(30101:30105, 30201, 30203:30207,
                     30301:30320, 30401:30407) ~ "Central Province",
      
      code %in% c(
        120101:120107, 120201:120207, 120301:120304,
        120401, 120403:120407, 120502:120513,
        120601:120603, 120701:120705, 120801:120804,
        120901:120903, 120906, 120907
      ) ~ "Morobe Province",
      
      code %in% c(
        190101:190115, 190201:190220
      ) ~ "West New Britain Province",
      
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(province))  # This line removes NA provinces

### Visualise the trend in key immunisation indicators for selected Health Facilities

#Filter and reshape data for the facilities  and select relevant columns

## Key indicators 1
sel_facilities <- c("Taraka UC", "Bitokara HC", "Kwikila HC")

sel_bcg <- c("under_1yr_pop", "bcg_birth", "bcg_total")

bcg <- immun_long |>
  filter(
    facility %in% sel_facilities,
    indicator %in% sel_bcg
  )

# Set indicator factor levels (to control facet order and renaming)
indicator_labels <- c(
  "under_1yr_pop" = "Under-1yr population", ## under_1 population as first facet 
  "bcg_birth" = "BCG at birth",
  "bcg_total" = "Total BCG"
)  
bcg$indicator <- factor(bcg$indicator, levels = names(indicator_labels), labels = indicator_labels)

#
# Line plot 
ggplot(bcg, aes(x = factor(year), y = value, fill = facility)) +
  geom_col(position = "dodge", alpha = 0.9) +
  facet_wrap(~ indicator, scales = "free_y", ncol = 2) +
  scale_fill_manual(
    values = c(
      "Taraka UC" = "#104E8B",     # dodgerblue4
      "Bitokara HC" = "#8B0000",   # darkred
      "Kwikila HC" = "#2F4F4F"     # darkslategray
    )
  ) +
  labs(
    title = "Trends in Immunisation Indicators (BCG)",
    x = "Year",
    y = "Value",
    color = "Facility"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5),  # Center the title
        plot.title.position = "panel",
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(face = "bold", size = 12)
  )

# BCG data by facility facets

# Define selected facilities
sel_facilities <- c("Taraka UC", "Bitokara HC", "Kwikila HC")


# Subset for indicators of interest (for bars)
immun_data_facets <- immun_long |>
  filter(facility %in% sel_facilities,
         indicator %in% names(indicator_labels)) |>
  mutate(indicator = recode(indicator, !!!indicator_labels))

# Subset for reports_sent line (will overlay on top)
reports_line_data <- immun_long |>
  filter(facility %in% sel_facilities, indicator == "reports_sent")
# Select only the indicators of interest (excluding reports_sent from facets)
indicator_labels <- c(
  "under_1yr_pop" = "Under-1yr population",
  "bcg_birth"     = "BCG at birth",
  "bcg_total"     = "Total BCG"
)

# Apply readable names to the indicator variable in both datasets
immun_data_facets$indicator <- recode(immun_data_facets$indicator, !!!indicator_labels)


# Plot
ggplot() +
  # Bars for selected indicators
  geom_col(
    data = immun_data_facets,
    aes(x = factor(year), y = value, fill = indicator),
    position = "dodge",
    alpha = 0.85
  ) +
  # Dashed trend line for reports_sent
  geom_line(
    data = reports_line_data,
    aes(x = factor(year), y = value * 10, group = 1),  # scale optional
    color = "red",
    linetype = "dashed",
    size = 1
  ) +
  geom_point(
    data = reports_line_data,
    aes(x = factor(year), y = value * 10),
    color = "black",
    size = 1
  ) +
  facet_wrap(~ facility, scales = "free_y") +
  scale_fill_manual(
    values = c(
      "BCG at birth" = "#8B1A1A",     # firebrick4
      "Total BCG" = "#00008B",   # darkblue
      "Under-1yr population" = "#006400"     # darkgreen
    )
  ) +
  labs(
    title = "Trends in Immunisation Indicators (2016–2025)",
    plot.title.position = "panel",
    subtitle = "Dashed line shows number of reports (scaled ×10)",
    x = "Year",
    y = "Value",
    fill = "Indicator"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold")
  )
