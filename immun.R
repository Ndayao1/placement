# Read and process the Immunisation data -----------------------------------

## Load necessary packages ----
library(openxlsx2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(tidyverse)
library(ggh4x)
#library(tidyverse) #hashed out to avoid conflicts with above packages


## Read datasets needed ----

hssdp <- vector("list", 9)  # create empty list

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
    years <- 2016:2024

### Combine sheets row-wise and tag with year     

immunisation <- map2_dfr(
  .x = 1:9,
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

      facilities <- c(
        "Kwikila HC" = "Kwikila HC",
        "Mumeng HC" = "Mumeng HC",
        "Gaulim SC" = "Gaulim CHP"
      )

      immun_indicators <- c("hep_bir", "bcg_birth", "bcg_total")
      
      # Filter and RENAME facilities and variables
      
      immun_facilities <- immun_long %>%
        filter(
          facility %in% names(facilities),
          indicator %in% immun_indicators
        ) %>%
        mutate(
          facility = recode(facility, !!!facilities)
        )
      
      immun_facilities$indicator <- factor(
        immun_facilities$indicator,
        levels = immun_indicators,
        labels = c(
          "Hepatitis B at birth coverage", "BCG at birth coverage", 
          "Total under 1 BCG coverage")
      )
      ##Plot for Immunisation indicators
      
      # Ensure 'year' is a factor with all relevant levels
      immun_facilities$year <- factor(immun_facilities$year)
      
      # Find the positions for the years 2020, 2022, and 2023
      year_levels <- levels(immun_facilities$year)
      x2020 <- which(year_levels == "2020")
      x2022 <- which(year_levels == "2022")
      x2023 <- which(year_levels == "2023")
      
      ## Immunisation Plot
      
      ggplot(immun_facilities, aes(
        x = year, y = value, 
        group = facility, 
        linetype = facility, 
        shape = facility
      )) +
        geom_line(size = 1) +
        geom_point(size = 2.6) +
        facet_wrap(~ indicator, scales = "free_y", ncol = 2) +
        # Add vertical lines at 2020 (black), 2022 (blue), 2023 (red)
        geom_vline(xintercept = x2020, linetype = "dashed", color = "black", size = 1) +
        geom_vline(xintercept = x2022, linetype = "dashed", color = "#0072B2", size = 1) + # blue
        geom_vline(xintercept = x2023, linetype = "dashed", color = "#D55E00", size = 1) + # red-orange
        scale_linetype_manual(
          values = c("Kwikila HC" = "solid", "Mumeng HC" = "dashed", "Gaulim CHP" = "dotted")
        ) +
        scale_shape_manual(
          values = c("Kwikila HC" = 16, "Mumeng HC" = 17, "Gaulim CHP" = 15)
        ) +
        # Use ggh4x::facetted_pos_scales to customise
        facetted_pos_scales(
          y = list(
            scale_y_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100)),
            scale_y_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100)),
            scale_y_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100))
          )
        ) +
        labs(
          title = "Yearly trends in immunisation coverage (%)",
          subtitle = "Vertical lines: 2020 = Mumeng upgrade; 2022 = Training (Kwikila & Gaulim); 2023 = Gaulim upgrade",
          x = "Year",
          y = "Value",
          linetype = "Facility",
          shape = "Facility"
        ) +
        theme_gray(base_size = 12) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, face = "plain"),
          axis.text.x = element_text(angle = 45, hjust = 1),
          strip.text = element_text(face = "bold")
        )


####-- Barplot
      
      # Define Oxford colour palette for the three facilities
      oxford_colors <- c(
        "Kwikila HC" = "#002147",  # Oxford Blue
        "Mumeng HC" = "#0072B2",  # Blue
        "Gaulim CHP" = "#008000"   # Oxford Green
      )
      
      # Ensure 'year' is a factor with all relevant levels
      immun_facilities$year <- factor(immun_facilities$year)
      
      # Find the positions for the years 2020, 2022, and 2023
      year_levels <- levels(immun_facilities$year)
      x2020 <- which(year_levels == "2020")
      x2022 <- which(year_levels == "2022")
      x2023 <- which(year_levels == "2023")
      
      ggplot(immun_facilities, aes(
        x = year, y = value, 
        fill = facility
      )) +
        geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
        facet_wrap(~ indicator, scales = "free_y", ncol = 2) +
        # Add vertical lines at 2020 (black), 2022 (blue), 2023 (red)
        geom_vline(xintercept = x2020, linetype = "dashed", color = "black", size = 1) +
        geom_vline(xintercept = x2022, linetype = "dashed", color = "#0072B2", size = 1) +
        geom_vline(xintercept = x2023, linetype = "dashed", color = "#D55E00", size = 1) +
        
        scale_fill_manual(
          values = oxford_colors,
          name = "Facility"
        ) +
        # Use ggh4x::facetted_pos_scales to customise
        facetted_pos_scales(
          y = list(
            scale_y_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100)),
            scale_y_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100)),
            scale_y_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100))
          )
        ) +
        labs(
          title = "Yearly trends in immunisation coverage (%)",
          subtitle = "Vertical lines: 2020 = Mumeng upgrade; 2022 = Training (Kwikila & Gaulim); 2023 = Gaulim upgrade",
          x = "Year",
          y = "Value",
          fill = "Facility"
        ) +
        theme_gray(base_size = 12) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, face = "plain"),
          axis.text.x = element_text(angle = 45, hjust = 1),
          strip.text = element_text(face = "bold")
        )
