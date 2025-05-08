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
    
    # Pivot the data longer (assumes Facility and 'HF Codes as the identifier columns)
        famplan_long <- famplan |>
      pivot_longer(
        cols = -c(year, facility, hf_code),  # keep all identifier columns
        names_to = "indicator",
        values_to = "value"
      )
    
    # Create a province mapping based on hf_code -- This can be moved around as appropriate.
    
    fp_long <- famplan_long |>
      mutate(
        province = case_when(
          hf_code %in% c(30101:30105, 30201, 30203:30207,
                         30301:30320, 30401:30407) ~ "Central Province",
          
          hf_code %in% c(
            120101:120107, 120201:120207, 120301:120304,
            120401, 120403:120407, 120502:120513,
            120601:120603, 120701:120705, 120801:120804,
            120901:120903, 120906, 120907
          ) ~ "Morobe Province",
          
          hf_code %in% c(
            190101:190115, 190201:190220
          ) ~ "West New Britain Province",
          
          TRUE ~ NA_character_
        )
      ) |>
      filter(!is.na(province))  # This line removes NA provinces
    
    #Now aggregate by province, year, and indicator
    
    province_summary <- fp_long |>
      group_by(province, year, indicator) |>
      summarise(
        avg_value = mean(value, na.rm = TRUE),
        .groups = "drop"
      )
    # Plot example for CYP (Modern)
    
    ggplot(
      filter(province_summary, indicator == "cyp_modern"),
      aes(x = factor(year), y = avg_value, group = province, color = province)
    ) +
      geom_line(size = 1.2) +
      geom_point(size = 2.5) +
      scale_color_manual(
        values = c(
          "Central Province" = "#FF0000",     # red
          "Morobe Province" = "#00008B",   # darkblue
          "West New Britain Province" = "#006400"   # darkgreen
        )
      ) + 
      labs(
        title = "Average CYP (Modern Methods) by Province",
        x = "Year",
        y = "Average CYP",
        color = "Province"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
 ### Visualise the trend in key immunisation indicators for selected Health Facilities

#Filter and reshape data for the facilities  and select relevant columns

## Key indicators 1
sel_facilities <- c("Taraka UC", "Bitokara HC", "Kwikila HC")
sel_fam <- c("no_of_reports", "new_acc_total", "new_acc_modern")


###Trend analysis for modern contraceptive uptake
library(tidyverse)
library(Kendall)  # for Mann-Kendall

### Trend in modern contraceptive uptake (CYP.Modern)
      # For the full dataset
      ######

      # For the selected facilities
      trend_facilities <- c("Taraka UC", "Bitokara HC", "Kwikila HC", "Central Province", "Morobe Province", 
      "West New Britain Province")
      
      # Define which are provinces
      provinces <- c("Central Province", "Morobe Province", "West New Britain Province")
      
      # Filter and create a new column to tag provinces
      trend_data <- famplan_long |>
        filter(
          facility %in% trend_facilities,
          indicator == "cyp_modern"
        ) |>
        mutate(
          type = if_else(facility %in% provinces, "Province", "Facility")
        )
      # Define custom colours
      facility_colors <- c(
        "Taraka UC" = "#8B1A1A",            # firebrick4
        "Bitokara HC" = "#2F4F4F",          # darkslategrey
        "Kwikila HC" = "#00008B",           # Dark green
        "Central Province" = "#0000FF",     # Blue
        "Morobe Province" = "#8B008B",      # Dark magenta
        "West New Britain Province" = "#00CED1"  # darkturquoise
      )
      # Line plot to show trends over time
      
      ggplot(trend_data, aes(x = factor(year), y = value, color = facility, group = facility)) +
        geom_line(aes(linetype = type), size = 1.2) +  # Dashed for provinces
        geom_point(size = 3) +
        scale_color_manual(values = facility_colors) +
        scale_linetype_manual(values = c("Facility" = "solid", "Province" = "dashed")) +
        labs(
          title = "Trend in CYP (Modern Methods) by Facility",
          subtitle = "2016–2025",
          x = "Year",
          y = "CYP (Modern)",
          color = "Facility",
          linetype = NULL
        ) +
        theme_minimal(base_size = 12) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "right"
        )
      
  ## Facility Trends vs the Province average
      
      # Filter facility-level CYP data
      facility_trend <- fp_long |>
        filter(
          facility %in% sel_facilities,
          indicator == "cyp_modern"
        )
      
      # Filter province-level averages
      province_trend <- province_summary |>
        filter(
          indicator == "cyp_modern",
          province %in% c("Central Province", "Morobe Province", "West New Britain Province")
        )
      
      # Plot
      ggplot() +
        # Facility-level lines
        geom_line(
          data = facility_trend,
          aes(x = factor(year), y = value, color = facility, group = facility),
          size = 1
        ) +
        geom_point(
          data = facility_trend,
          aes(x = factor(year), y = value, color = facility),
          size = 2.5
        ) +
        
        # Province-level average lines (dashed)
        geom_line(
          data = province_trend,
          aes(x = factor(year), y = avg_value, color = province, group = province),
          linetype = "dashed",
          size = 1.2
        ) +
        geom_point(
          data = province_trend,
          aes(x = factor(year), y = avg_value, color = province),
          shape = 18, size = 3  #Diamond shape
        ) +
        
        scale_color_manual(
          values = c(
            "Taraka UC" = "#FF0000",     # red
            "Bitokara HC" = "#00008B",    #darkblue
            "Kwikila HC" = "#006400",    # darkgreen
            "Central Province" = "#006400",    # darkgreen
            "Morobe Province" = "#FF0000",     # red
            "West New Britain Province" = "#00008B"    #darkblue
          )
        ) +
        labs(
          title = "CYP (Modern) Trends: Facilities vs Province Averages",
          subtitle = "Dashed lines show provincial average",
          x = "Year",
          y = "CYP (Modern)",
          color = "Facility / Province"
        ) +
        theme_minimal(base_size = 12) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, face = "italic"),
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
      
      