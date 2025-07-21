# Read and process the Maternal data -----------------------------------

## Load necessary packages ----
library(openxlsx2)
library(dplyr)
library(tidyr)
library(ggplot2) 
library(purrr)
library(oxthema)
#library(tidyverse) #hashed out to avoid conflicts with above packages


## Read datasets needed ----

      ### Read sheet for each year separately ----
      sheet1 <- read_xlsx(file = "data/hssdp.xlsx", sheet = 1, start_row = 1)
      sheet2 <- read_xlsx(file = "data/hssdp.xlsx", sheet = 2, start_row = 1)
      sheet3 <- read_xlsx(file = "data/hssdp.xlsx", sheet = 3, start_row = 1)
      sheet4 <- read_xlsx(file = "data/hssdp.xlsx", sheet = 4, start_row = 1)
      sheet5 <- read_xlsx(file = "data/hssdp.xlsx", sheet = 5, start_row = 1)
      sheet6 <- read_xlsx(file = "data/hssdp.xlsx", sheet = 6, start_row = 1)
      sheet7 <- read_xlsx(file = "data/hssdp.xlsx", sheet = 7, start_row = 1)
      sheet8 <- read_xlsx(file = "data/hssdp.xlsx", sheet = 8, start_row = 1)
      sheet9 <- read_xlsx(file = "data/hssdp.xlsx", sheet = 9, start_row = 1)
     
      ### Alternative code to loop through the sheets to read ----
       for (i in 1:9) {
         assign(
           paste("sheet", i, sep = ""), 
           read_xlsx(file = "data/hssdp.xlsx", sheet = i, start_row = 1)
         )
       }
      
      ### Or this
      sheets <- vector("list", 9)  # create empty list
      
      for (i in 1:9) {
        sheets[[i]] <- read_xlsx("data/hssdp.xlsx", sheet = i, start_row = 1)
      }

## Clean and process the Maternal health data ----

    ### Check that all 10 sheets have same column names ----
  
        # Get column names of the first sheet as reference
        reference_cols <- colnames(sheets[[1]])
        
        # Compare all other sheets to the reference
        all_same <- sapply(sheets, function(df) identical(colnames(df), reference_cols))
        
        # View the result
        all_same


### row bind sheets and then pivot longer ----
                  
      
##Add the year to each sheet and combine them: 
  #Loop through all 10 sheets,
      
  #Add a year column based on the sheet number,
      
  #Combine everything into one dataframe:      
      
    ### Set the years
    years <- 2016:2024
      
    ### Combine sheets row-wise and tag with year     
      
    hssdp <- map2_dfr(
        .x = 1:9,
        .y = years,
        ~ read_xlsx(file = "data/hssdp.xlsx", sheet = .x, startRow = 1) |>
          mutate(year = .y)
      )
    ## Fix error from numeric conversion
    hssdp <- map2_dfr(
      .x = 1:9,
      .y = years,
      function(sheet, year) {
        read_xlsx(file = "data/hssdp.xlsx", sheet = sheet, startRow = 1) |>
          # Convert problematic column to numeric
          mutate(mmr_per_100000 = as.numeric(mmr_per_100000),
                 year = year)
      }
    )
      
    ### Make year the first column
maternal <- hssdp |> 
        relocate(year, .before = 1)

# Pivot the data longer (assumes 'Facility' is the identifier column)
# Pivot the data longer (assumes Facility and 'HF Codes as the identifier columns)
mat_long <- maternal |>
  pivot_longer(
    cols = -c(year, facility, hf_code),  # keep all identifier columns
    names_to = "indicator",
    values_to = "value"
  )

# Create a province mapping based on hf_code -- This can be moved around as appropriate.

maternal_long <- mat_long |>
  mutate(
    province = case_when(
      hf_code %in% c(30101:30407) ~ "Central Province",
      
      hf_code %in% c(
        120101:120907
      ) ~ "Morobe Province",
      
      hf_code %in% c(
        180101:180402
      ) ~ "East New Britain Province",
      
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(province))  # This line removes NA provinces

  #Now aggregate by province, year, and indicator
  
  province_summary <- maternal_long |>
    group_by(province, year, indicator) |>
    summarise(
      avg_value = mean(value, na.rm = TRUE),
      .groups = "drop"
    )


  # Filter and reshape data for selected facilities
  # Create a named vector for renaming
  facilities <- c(
    "Kwikila HC" = "Kwikila HC",
    "Mumeng HC" = "Mumeng CHP",
    "Gaulim SC" = "Gaulim SC"
  )
  
  
  delivery_indicators <- c("del_in_facility", "del_village_att", "del_still_births", "del_mat_deaths", 
                           "del_born_before_arr", "del_village_compli")
  
  # Filter and RENAME facility
  
  delivery_facilities <- maternal_long %>%
    filter(
      facility %in% names(facilities),
      indicator %in% delivery_indicators
    ) %>%
    mutate(
      facility = recode(facility, !!!facilities)
    )
  
    
  
  # Set indicator order and labels
  delivery_facilities$indicator <- factor(
    delivery_facilities$indicator,
    levels = delivery_indicators,
    labels = c(
      "Facility deliveries", "Village deliveries attended", "Stillbirths", "Maternal deaths",
      "Born before arrival*", "Village birth complications"
    )
  )
  
  ##Plot for Delivery indicators
  
  # Ensure 'year' is a factor with all relevant levels
  delivery_facilities$year <- factor(delivery_facilities$year)
  
  # Find the positions for the years 2020, 2022, and 2023
  year_levels <- levels(delivery_facilities$year)
  x2020 <- which(year_levels == "2020")
  x2022 <- which(year_levels == "2022")
  x2023 <- which(year_levels == "2023")
  
  ggplot(delivery_facilities, aes(
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
      values = c("Kwikila HC" = "solid", "Mumeng CHP" = "dashed", "Gaulim SC" = "dotted")
    ) +
    scale_shape_manual(
      values = c("Kwikila HC" = 16, "Mumeng CHP" = 17, "Gaulim SC" = 15)
    ) +
    labs(
      title = "Yearly trends in delivery indicators",
      subtitle = "Vertical lines: Black = Mumeng upgrade, Blue = Training (Kwikila & Gaulim), Orange = Gaulim upgrade",
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
  
  
  ## Remove decimals from last panel
  library(ggh4x)
  
  ggplot(delivery_facilities, aes(
    x = year, y = value, 
    group = facility, 
    linetype = facility, 
    shape = facility
  )) +
    geom_line(size = 1) +
    geom_point(size = 2.6) +
    facet_wrap(~ indicator, scales = "free_y", ncol = 2) +
    geom_vline(xintercept = x2020, linetype = "dashed", color = "black", size = 1) +
    geom_vline(xintercept = x2022, linetype = "dashed", color = "#0072B2", size = 1) +
    geom_vline(xintercept = x2023, linetype = "dashed", color = "#D55E00", size = 1) +
    scale_linetype_manual(
      values = c("Kwikila HC" = "solid", "Mumeng CHP" = "dashed", "Gaulim SC" = "dotted")
    ) +
    scale_shape_manual(
      values = c("Kwikila HC" = 16, "Mumeng CHP" = 17, "Gaulim SC" = 15)
    ) +
    # Use ggh4x::facetted_pos_scales to customise
    facetted_pos_scales(
      y = list(
        NULL,              # 1st panel, default scaling
        scale_y_continuous(limits = c(0, 500), breaks = c(0, 100, 200, 300, 400, 500)),
        scale_y_continuous(limits = c(0, 15), breaks = c(0, 5, 10, 15)),
        scale_y_continuous(limits = c(0, 10), breaks = c(0, 5, 10)),
        scale_y_continuous(limits = c(0, 40), breaks = c(0, 10, 20, 30, 40)),
        scale_y_continuous(limits = c(0, 10), breaks = c(0, 5, 10))   # last panel custom scales
      )
    ) +
    labs(
      title = "Yearly trends in delivery indicators",
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
  
  
  ##Plot for ANC indicators
  anc_indicators <- c("anc_1st_visit", "anc_4th_visit", "anc_1st_cov", "anc_4th_cov")
  
  # Filter and RENAME facility
  
  anc_facilities <- maternal_long %>%
    filter(
      facility %in% names(facilities),
      indicator %in% anc_indicators
    ) %>%
    mutate(
      facility = recode(facility, !!!facilities)
    )
  
  anc_facilities$indicator <- factor(
    anc_facilities$indicator,
    levels = anc_indicators,
    labels = c(
      "No. attending 1st ANC visit", "No. attending 4th ANC visit", "1st ANC coverage* (%)", "4th ANC coverage (%)")
  )
  ##Plot for ANC indicators
  
  # Ensure 'year' is a factor with all relevant levels
  anc_facilities$year <- factor(anc_facilities$year)
  
  # Find the positions for the years 2020, 2022, and 2023
  year_levels <- levels(delivery_facilities$year)
  x2020 <- which(year_levels == "2020")
  x2022 <- which(year_levels == "2022")
  x2023 <- which(year_levels == "2023")
  
  ## ANC Plot
  
  ggplot(anc_facilities, aes(
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
      values = c("Kwikila HC" = "solid", "Mumeng CHP" = "dashed", "Gaulim SC" = "dotted")
    ) +
    scale_shape_manual(
      values = c("Kwikila HC" = 16, "Mumeng CHP" = 17, "Gaulim SC" = 15)
    ) +
    # Use ggh4x::facetted_pos_scales to customise
    facetted_pos_scales(
      y = list(
        scale_y_continuous(limits = c(0, 750), breaks = c(0, 150, 300, 450, 600, 750)),
        scale_y_continuous(limits = c(0, 750), breaks = c(0, 150, 300, 450, 600, 750)),
        scale_y_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100)),
        scale_y_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100))   
      )
    ) +
    labs(
      title = "Yearly trends in ANC visits",
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
  
  
  ### 3. Postnatal care and immediate immunisation
  
  pnc_indicators <- c("brst_feeding_1hr", "del_lbw_2500g", "kang_mother_care")
  
  # Filter and RENAME facility
  
  pnc_facilities <- maternal_long %>%
    filter(
      facility %in% names(facilities),
      indicator %in% pnc_indicators
    ) %>%
    mutate(
      facility = recode(facility, !!!facilities)
    )
  
  pnc_facilities$indicator <- factor(
    pnc_facilities$indicator,
    levels = pnc_indicators,
    labels = c(
      "Breastfeeding within 1 hour", "No. of LBW babies", "No. initiated on KMC")
  )
  ##Plot for ANC indicators
  
  # Ensure 'year' is a factor with all relevant levels
  pnc_facilities$year <- factor(pnc_facilities$year)
  
  # Find the positions for the years 2020, 2022, and 2023
  year_levels <- levels(delivery_facilities$year)
  x2020 <- which(year_levels == "2020")
  x2022 <- which(year_levels == "2022")
  x2023 <- which(year_levels == "2023")
  
  ## PNC Plot
  
  ggplot(pnc_facilities, aes(
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
      values = c("Kwikila HC" = "solid", "Mumeng CHP" = "dashed", "Gaulim SC" = "dotted")
    ) +
    scale_shape_manual(
      values = c("Kwikila HC" = 16, "Mumeng CHP" = 17, "Gaulim SC" = 15)
    ) +
    # Use ggh4x::facetted_pos_scales to customise
    facetted_pos_scales(
      y = list(
        scale_y_continuous(limits = c(0, 450), breaks = c(0, 150, 300, 450)),
        scale_y_continuous(limits = c(0, 50), breaks = c(0, 10, 20, 30, 40, 50)),
        scale_y_continuous(limits = c(0, 450), breaks = c(0, 150, 300, 450))
        )
      ) +
    labs(
      title = "Yearly trends in immediate postnatal care indicators",
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
  
  
  
  
  
  
  
  
  