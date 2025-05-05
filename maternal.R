# Read and process the maternal data -----------------------------------

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
sheets <- vector("list", 10)  # create empty list

for (i in 1:10) {
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
                    #maternal <- bind_rows(sheet1, sheet2, .id = "sex") |>
                       #mutate(sex = ifelse(sex == 1, "Male", "Female")) 
      
##Add the year to each sheet and combine them: 
  #Loop through all 10 sheets,
      
  #Add a year column based on the sheet number,
      
  #Combine everything into one dataframe:      
      
    ### Set the years
    years <- 2016:2025
      
    ### Combine sheets row-wise and tag with year     
      
    maternal <- map2_dfr(
        .x = 1:10,
        .y = years,
        ~ read_xlsx(file = "data/hssdp.xlsx", sheet = .x, startRow = 1) |>
          mutate(year = .y)
      )
      
    ### Make year the first column
maternal <- maternal |> 
        relocate(year, .before = 1)

# Pivot the data longer (assumes 'Facility' is the identifier column)
maternal_long <- maternal |>
  pivot_longer(
    cols = -c(year, facility),  # Keep 'year' and 'Facility', pivot everything else
    names_to = "indicator",
    values_to = "value"
  )

### Visualise the trend in key maternal indicators for selected Health Facilities
  
#Filter and reshape data for the facilities  and select relevant columns
  
## Key indicators 1
  selected_facilities <- c("Taraka UC", "Bitokara HC", "Kwikila HC")
        
        selected_data <- maternal |>
          filter(facility %in% selected_facilities) |>
          select(
            year, facility,
            no_of_reports,
            pop_births,
            del_still_births,
            del_mat_deaths,
            del_born_before_arr,
            del_in_facility,
            del_village_compli
          )|>
          pivot_longer(
            cols = -c(year, facility),
            names_to = "indicator",
            values_to = "value"
          )

          ##Plot
        selected_data$indicator <- factor(selected_data$indicator, 
                                    levels = c("no_of_reports","pop_births", "del_born_before_arr", 
                            "del_in_facility", "del_still_births", 
                            "del_mat_deaths", "del_village_compli")) ##no_of_reports as first facet  
        # Line plot 
        ggplot(selected_data, aes(x = factor(year), y = value, color = facility, group = facility)) +
          geom_line(size = 1.2) +
          geom_point(size = 3) +
          facet_wrap(~ indicator, scales = "free_y", ncol = 2) +
          scale_color_manual(
            values = c(
              "Taraka UC" = "#FF0000",     # red
              "Bitokara HC" = "#00008B",   # darkblue
              "Kwikila HC" = "#006400"     # darkgreen
            )
          ) +
          labs(
            title = "Trends in Delivery Indicators",
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
## Key indicators 2
selected_facilities <- c("Taraka UC", "Bitokara HC", "Kwikila HC")

      anc_mat <- maternal |>
        filter(facility %in% selected_facilities) |>
        select(
          year, facility,
          anc_1st_visit,
          anc_4th_visit,
          anc_booster_tt,
          anc_1st_cov,
          anc_4th_cov,
          anc_avg_cov
        )|>
        pivot_longer(
          cols = -c(year, facility),
          names_to = "indicator",
          values_to = "value"
        ) 

      ##Plot
      
      anc_mat$indicator <- factor(anc_mat$indicator, 
                                  levels = c("anc_1st_visit", "anc_4th_visit", "anc_booster_tt", "anc_1st_cov", 
                                             "anc_4th_cov", "anc_avg_cov")) ##pop_births as first facet
      
      ggplot(anc_mat, aes(x = factor(year), y = value, fill = facility)) +
        geom_col(position = "dodge", alpha = 0.8) +
        facet_wrap(~ indicator, scales = "free_y", ncol = 2) +
        scale_fill_manual(
          values = c(
            "Taraka UC" = "#00008B",     # darkblue
            "Bitokara HC" = "#006400",   # darkgreen
            "Kwikila HC" = "#8B4513"     # chocolate
          )
        ) + 
        labs(
          title = "ANC attendance in the study facilities (2016–2025)",
          x = "Year",
          y = "Value",
          fill = "Facility"
        ) +
        theme_minimal(base_size = 12) +
        theme(plot.title = element_text(hjust = 0.5),  # Center the title
              plot.title.position = "panel",
          axis.text.x = element_text(angle = 45, hjust = 1),
          strip.text = element_text(face = "bold", size = 12)
        )

      ## Key indicators 3 - Newborn care indicators
      selected_facilities <- c("Taraka UC", "Bitokara HC", "Kwikila HC")
      
      newborn <- maternal |>
        filter(facility %in% selected_facilities) |>
        select(
          year, facility,
          del_lbw_2500g,
          resuscitated,
          brst_feeding_1hr,
          skin_skin,
          kang_mother_care,
          bebi_kol
        )|>
        pivot_longer(
          cols = -c(year, facility),
          names_to = "indicator",
          values_to = "value"
        ) 
      
      ##Plot
      
      newborn$indicator <- factor(newborn$indicator, 
                                  levels = c("del_lbw_2500g", "resuscitated", "brst_feeding_1hr", "skin_skin", 
                                             "kang_mother_care", "bebi_kol")) ##facet order
      
      ggplot(newborn, aes(x = factor(year), y = value, color = facility, group = facility)) +
        geom_line(size = 1.2) +
        geom_point(size = 3) +
        facet_wrap(~ indicator, scales = "free_y", ncol = 2) +
        scale_color_manual(
          values = c(
            "Taraka UC" = "#FF0000",     # red
            "Bitokara HC" = "#00008B",   # darkblue
            "Kwikila HC" = "#006400"     # darkgreen
          )
        ) +
        labs(
          title = "Trends in Newborn Care Indicators",
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
  #taraka_data <- maternal |>
      #filter(facility == "Taraka UC") |>
      #select(year, facility, no_of_reports, pop_births, del_lbw_2500g, del_still_births, del_mat_deaths, del_born_before_arr, del_in_facility, brst_feeding_1hr)

   
  # Filter and reshape the data
  maternal_filtered <- maternal |>
    filter(facility == "Taraka UC") |>
    select(year, facility, no_of_reports, brst_feeding_1hr, del_born_before_arr, del_in_facility,
           del_lbw_2500g, del_mat_deaths, del_still_births) |>
    pivot_longer(
      cols = c(brst_feeding_1hr, del_born_before_arr, del_in_facility,
               del_lbw_2500g, del_mat_deaths, del_still_births),
      names_to = "Indicator",
      values_to = "Value"
    )

  # Separate data for line plot
  line_data <- maternal |>
    filter(facility == "Taraka UC") |>
    select(year, no_of_reports)

# Plot
  ggplot(maternal_filtered, aes(x = factor(year), y = Value, fill = Indicator)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ Indicator, scales = "free_y") +
    geom_line(data = line_data, aes(x = factor(year), y = no_of_reports, group = 1),
              inherit.aes = FALSE, color = "black", linetype = "dashed", size = 1) +
    geom_point(data = line_data, aes(x = factor(year), y = no_of_reports),
               inherit.aes = FALSE, color = "black", size = 2) +
    labs(
      title = "Trends in Key Maternal Indicators for Taraka UC (2016–2025)",
      x = "Year", y = "Value",
      caption = "Dashed line = Number of reports"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold")
    )
  