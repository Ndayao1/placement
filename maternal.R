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
      sheet10 <- read_xlsx(file = "data/hssdp.xlsx", sheet = 10, start_row = 1)
      
      ### Alternative code to loop through the sheets to read ----
       for (i in 1:10) {
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
        190101:190220
      ) ~ "West New Britain Province",
      
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


### Visualise the trend in key maternal indicators for selected Health Facilities
  
#Filter and reshape data for the facilities  and select relevant columns
  
## Key indicators 1
 
  #For Mumeng HC
  mumeng <- c("Mumeng HC")
  
  # Define indicators and facilities of interest
  delivery_indicators <- c("pop_births", "del_still_births",
    "del_mat_deaths", "del_born_before_arr", "del_in_facility",
    "del_village_compli")
  
     # Filter for Mumeng in the long dataset
  delivery_mumeng <- maternal_long |>
    filter(
      facility %in% mumeng,
      indicator %in% delivery_indicators
    )
  
  #set indicator order and labels
    #For Mumeng
  
        delivery_mumeng$indicator <- factor(
          delivery_mumeng$indicator,
          levels = delivery_indicators,
          labels = c("Births (Population)",              ##births as first facet 
                     "Stillbirths", "Maternal deaths",
                     "Born before arrival", "In-facility deliveries",
                     "Village birth complications"
          )
        )
        
        # Line plot with intervention line
        ggplot(delivery_mumeng, aes(x = factor(year), y = value, color = facility, group = facility)) +
          geom_line(size = 1.2) +
          geom_point(size = 3) +
          facet_wrap(~ indicator, scales = "free_y", ncol = 2) +
          scale_color_manual(
            values = c("Mumeng HC" = "#00008B")   # darkblue
          ) +
          # Add vertical line at year 2020 (intervention marker)
          geom_vline(xintercept = which(levels(factor(delivery_mumeng$year)) == "2020"), 
                     linetype = "dashed", color = "red", size = 1) +
          labs(
            title = "10-year Trend in Delivery Indicators for Mumeng HC",
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
        
    # Visualise the trends summaries against the province averages    
        # Define indicators and labels
        indicator_levels <- c("pop_births", "del_still_births", "del_mat_deaths", 
          "del_born_before_arr", "del_in_facility", "del_village_compli"
        )
        
        indicator_labels <- c("Births (Population)", "Stillbirths", "Maternal Deaths", 
          "Born Before Arrival", "In-Facility Deliveries", "Village Birth Complications"
        )
        
        # Selected facilities
        sel_facilities <- c("Bulolo HC", "Mumeng HC")
        
        sel_province <- c("Morobe Province")
        
        #sel_provinces <- c("Central Province", "Morobe Province", "East New Britain Province")
        
        # Clean and label: Facility-level data
        facility_trend <- maternal_long |>
          filter(
            facility %in% sel_facilities,
            indicator %in% indicator_levels
          ) |>
          mutate(
            indicator = factor(indicator, levels = indicator_levels, labels = indicator_labels)
          )
        
        # Clean and label: Province-level data
        province_trend <- province_summary |>
          filter(
            province %in% sel_province,
            indicator %in% indicator_levels
          ) |>
          mutate(
            indicator = factor(indicator, levels = indicator_levels, labels = indicator_labels)
          )
        
        # Plot: Bars (facility), dashed lines (province means)
        ggplot() +
          # Facility bars
          geom_col(
            data = facility_trend,
            aes(x = factor(year), y = value, fill = facility),
            position = "dodge",
            alpha = 0.9
          ) +
          # Province-level average lines
          geom_line(
            data = province_trend,
            aes(x = factor(year), y = avg_value, color = province, group = province),
            linetype = "dashed",
            size = 1.2
          ) +
          # Province points
          geom_point(
            data = province_trend,
            aes(x = factor(year), y = avg_value, color = province),
            shape = 16, size = 3
          ) +
          facet_wrap(~ indicator, scales = "free_y", ncol = 2) +
          scale_fill_manual(
            values = c(
              "Mumeng HC" = "#00008B",   # darkblue
              "Bulolo HC" = "brown"     # 
            )
          ) +
          scale_color_manual(
            values = c("Morobe Province" = "darkgreen"),     # brown
          ) +
          # Add vertical line at year 2020 (intervention marker)
          geom_vline(xintercept = which(levels(factor(facility_trend$year)) == "2020"), 
                     linetype = "dashed", color = "red", size = 1.2) +
          labs(
            title = "Trends in Delivery Indicators: Facilities vs Province Averages",
            subtitle = "Bars = Facility data | Dashed line = Province averages \n Red vertical line represents intervention" ,
            x = "Year",
            y = "Value",
            fill = "Facility",
            color = "Province"
          ) +
          theme_minimal(base_size = 12) +
          theme(
            plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, face = "italic"),
            axis.text.x = element_text(angle = 45, hjust = 1),
            strip.text = element_text(face = "bold")
          )
        
        
        # Plot: Line (facility), dashed lines (province means)
        ggplot() +
          # Facility lines
          geom_line(
            data = facility_trend,
            aes(x = factor(year), y = value, color = facility, group = facility),
            size = 1.2
          ) +
          geom_point(
            data = facility_trend,
            aes(x = factor(year), y = value, color = facility),
            size = 3
          ) +
          # Province-level average lines
          geom_line(
            data = province_trend,
            aes(x = factor(year), y = avg_value, color = province, group = province),
            linetype = "dashed",
            size = 1.2
          ) +
          # Province points
          geom_point(
            data = province_trend,
            aes(x = factor(year), y = avg_value, color = province),
            shape = 16, size = 3
          ) +
          facet_wrap(~ indicator, scales = "free_y", ncol = 2) +
          scale_color_manual(
            values = c("Morobe Province" = "darkgreen",  "Mumeng HC" = "#00008B", "Bulolo HC" = "brown"     # 
            ),  
          ) +
          # Add vertical line at year 2020 (intervention marker)
          geom_vline(xintercept = which(levels(factor(facility_trend$year)) == "2020"), 
                     linetype = "dashed", color = "red", size = 1.2) +
          labs(
            title = "Trends in Delivery Indicators: Facilities vs Province Averages",
            subtitle = "Continuous line = Facility data | Dashed line = Province averages \n Red vertical line represents intervention" ,
            x = "Year",
            y = "Value",
            fill = "Facility",
            color = "Province"
          ) +
          theme_minimal(base_size = 12) +
          theme(
            plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, face = "italic"),
            axis.text.x = element_text(angle = 45, hjust = 1),
            strip.text = element_text(face = "bold")
          )
        
        
    ## Key indicators 2
      
    # Filter for selected facilities and ANC indicators
        # Visualise the trends summaries against the province averages    
        # Define indicators and labels
        anc_indicator_levels <- c(
          "anc_1st_visit",
          "anc_4th_visit",
          "anc_booster_tt",
          "anc_1st_cov",
          "anc_4th_cov",
          "anc_avg_cov"
        )
        
        anc_indicator_labels <- c(
          "First ANC visit",
          "Fourth ANC visit",
          "Booster TT dose (ANC)",
          "First ANC visit coverage",
          "Fourth ANC visit coverage",
          "Average ANC coverage"
        )
        
        # Selected facilities
        sel_facilities <- c("Bulolo HC", "Mumeng HC")
        
        sel_province <- c("Morobe Province")
        
        #sel_provinces <- c("Central Province", "Morobe Province", "East New Britain Province")
        
        # Clean and label: Facility-level data
        anc_facility_trend <- maternal_long |>
          filter(
            facility %in% sel_facilities,
            indicator %in% anc_indicator_levels
          ) |>
          mutate(
            indicator = factor(indicator, levels = anc_indicator_levels, labels = anc_indicator_labels)
          )
        
        # Clean and label: Province-level data
        anc_province_trend <- province_summary |>
          filter(
            province %in% sel_province,
            indicator %in% anc_indicator_levels 
          ) |>
          mutate(
            indicator = factor(indicator, levels = anc_indicator_levels, labels = anc_indicator_labels)
          )
        
        # Plot: Line (facility), dashed lines (province means)
        ggplot() +
          # Facility lines
          geom_line(
            data = anc_facility_trend,
            aes(x = factor(year), y = value, color = facility, group = facility),
            size = 1.2
          ) +
          geom_point(
            data = anc_facility_trend,
            aes(x = factor(year), y = value, color = facility),
            size = 3
          ) +
          # Province-level average lines
          geom_line(
            data = anc_province_trend,
            aes(x = factor(year), y = avg_value, color = province, group = province),
            linetype = "dashed",
            size = 1.2
          ) +
          # Province points
          geom_point(
            data = anc_province_trend,
            aes(x = factor(year), y = avg_value, color = province),
            shape = 16, size = 3
          ) +
          facet_wrap(~ indicator, scales = "free_y", ncol = 2) +
          scale_color_manual(
            values = c("Morobe Province" = "darkgreen",  "Mumeng HC" = "#00008B", "Bulolo HC" = "brown"     # 
            ),  
          ) +
          # Add vertical line at year 2020 (intervention marker)
          geom_vline(xintercept = which(levels(factor(facility_trend$year)) == "2020"), 
                     linetype = "dashed", color = "red", size = 1.2) +
          labs(
            title = "Trends in ANC Indicators for selected facilities (2016-2024)",
            subtitle = "Solid line = Facility data | Dashed line = Province averages \n Red vertical line represents intervention" ,
            x = "Year",
            y = "Value",
            fill = "Facility",
            color = "Province"
          ) +
          theme_minimal(base_size = 12) +
          theme(
            plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, face = "italic"),
            axis.text.x = element_text(angle = 45, hjust = 1),
            strip.text = element_text(face = "bold")
          )
        
        ##Bar pplot with Province Trend
        # Plot: Bars (facility), dashed lines (province means)
        ggplot() +
          # Facility bars
          geom_col(
            data = anc_facility_trend,
            aes(x = factor(year), y = value, fill = facility),
            position = "dodge",
            alpha = 0.9
          ) +
          # Province-level average lines
          geom_line(
            data = anc_province_trend,
            aes(x = factor(year), y = avg_value, color = province, group = province),
            linetype = "dashed",
            size = 1.2
          ) +
          # Province points
          geom_point(
            data = anc_province_trend,
            aes(x = factor(year), y = avg_value, color = province),
            shape = 16, size = 3
          ) +
          facet_wrap(~ indicator, scales = "free_y", ncol = 2) +
          scale_fill_manual(
            values = c(
              "Mumeng HC" = "#00008B",   # darkblue
              "Bulolo HC" = "brown"     # 
            )
          ) +
          scale_color_manual(
            values = c("Morobe Province" = "darkgreen"),     # brown
          ) +
          # Add vertical line at year 2020 (intervention marker)
          geom_vline(xintercept = which(levels(factor(anc_facility_trend$year)) == "2020"), 
                     linetype = "dashed", color = "red", size = 1.2) +
          labs(
            title = "Trends in ANC Indicators for selected facilities (2016-2024)",
            subtitle = "Bars = Facility data | Dashed line = Province averages \n Red vertical line represents intervention" ,
            x = "Year",
            y = "Value",
            fill = "Facility",
            color = "Province"
          ) +
          theme_minimal(base_size = 12) +
          theme(
            plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, face = "italic"),
            axis.text.x = element_text(angle = 45, hjust = 1),
            strip.text = element_text(face = "bold")
          )
        
        
 ## Key indicators 3 - Newborn care indicators
      
      newborn <- maternal |>
        filter(facility %in% sel_facilities) |>
        select(
          year, facility,
          del_lbw_2500g,
          resuscitated,
          brst_feeding_1hr,
          skin_skin,
          kang_mother_care
      
        )|>
        pivot_longer(
          cols = -c(year, facility),
          names_to = "indicator",
          values_to = "value"
        ) 
      
      # Filter for selected facilities and Newborn indicators
      # Visualise the trends summaries against the province averages    
      # Define indicators and labels
      newborn_indicator_levels <- c(
        "del_lbw_2500g",
        "resuscitated",
        "brst_feeding_1hr",
        "skin_skin",
        "kang_mother_care"
      )
      
      newborn_indicator_labels <- c(
          "Low birthweight (<2500g)",
          "Resuscitated at birth",
          "Breastfeeding within 1 hour",
          "Skin-to-skin contact",
          "Kangaroo mother care"
      )
      
      # Selected facilities
      sel_facilities <- c("Bulolo HC", "Mumeng HC")
      
      sel_province <- c("Morobe Province")
      
      #sel_provinces <- c("Central Province", "Morobe Province", "East New Britain Province")
      
      # Clean and label: Facility-level data
      newborn_facility_trend <- maternal_long |>
        filter(
          facility %in% sel_facilities,
          indicator %in% newborn_indicator_levels
        ) |>
        mutate(
          indicator = factor(indicator, levels = newborn_indicator_levels, labels = newborn_indicator_labels)
        )
      
      # Clean and label: Province-level data
      newborn_province_trend <- province_summary |>
        filter(
          province %in% sel_province,
          indicator %in% newborn_indicator_levels 
        ) |>
        mutate(
          indicator = factor(indicator, levels = newborn_indicator_levels, labels = newborn_indicator_labels)
        )
      
      # Plot: Line (facility), dashed lines (province means)
      ggplot() +
        # Facility lines
        geom_line(
          data = newborn_facility_trend,
          aes(x = factor(year), y = value, color = facility, group = facility),
          size = 1.2
        ) +
        geom_point(
          data = newborn_facility_trend,
          aes(x = factor(year), y = value, color = facility),
          size = 3
        ) +
        # Province-level average lines
        geom_line(
          data = newborn_province_trend,
          aes(x = factor(year), y = avg_value, color = province, group = province),
          linetype = "dashed",
          size = 1.2
        ) +
        # Province points
        geom_point(
          data = newborn_province_trend,
          aes(x = factor(year), y = avg_value, color = province),
          shape = 16, size = 3
        ) +
        facet_wrap(~ indicator, scales = "free_y", ncol = 2) +
        scale_color_manual(
          values = c("Morobe Province" = "darkgreen",  "Mumeng HC" = "blue", "Bulolo HC" = "brown"     # 
          ),  
        ) +
        # Add vertical line at year 2020 (intervention marker)
        geom_vline(xintercept = which(levels(factor(facility_trend$year)) == "2020"), 
                   linetype = "dashed", color = "red", size = 1.2) +
        labs(
          title = "Trends in Newborn care Indicators for selected facilities (2016-2024)",
          subtitle = "Solid line = Facility data | Dashed line = Province averages \n Red vertical line represents intervention" ,
          x = "Year",
          y = "Value",
          fill = "Facility",
          color = "Province"
        ) +
        theme_minimal(base_size = 12) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, face = "italic"),
          axis.text.x = element_text(angle = 45, hjust = 1),
          strip.text = element_text(face = "bold")
        )
      
           ##Bar pplot with Province Trend
      # Plot: Bars (facility), dashed lines (province means)
      ggplot() +
        # Facility bars
        geom_col(
          data = anc_facility_trend,
          aes(x = factor(year), y = value, fill = facility),
          position = "dodge",
          alpha = 0.9
        ) +
        # Province-level average lines
        geom_line(
          data = anc_province_trend,
          aes(x = factor(year), y = avg_value, color = province, group = province),
          linetype = "dashed",
          size = 1.2
        ) +
        # Province points
        geom_point(
          data = anc_province_trend,
          aes(x = factor(year), y = avg_value, color = province),
          shape = 16, size = 3
        ) +
        facet_wrap(~ indicator, scales = "free_y", ncol = 2) +
        scale_fill_manual(
          values = c(
            "Mumeng HC" = "#00008B",   # darkblue
            "Bulolo HC" = "brown"     # 
          )
        ) +
        scale_color_manual(
          values = c("Morobe Province" = "darkgreen"),     # brown
        ) +
        # Add vertical line at year 2020 (intervention marker)
        geom_vline(xintercept = which(levels(factor(anc_facility_trend$year)) == "2020"), 
                   linetype = "dashed", color = "red", size = 1.2) +
        labs(
          title = "Trends in ANC Indicators for selected facilities (2016-2024)",
          subtitle = "Bars = Facility data | Dashed line = Province averages \n Red vertical line represents intervention" ,
          x = "Year",
          y = "Value",
          fill = "Facility",
          color = "Province"
        ) +
        theme_minimal(base_size = 12) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, face = "italic"),
          axis.text.x = element_text(angle = 45, hjust = 1),
          strip.text = element_text(face = "bold")
        )
      