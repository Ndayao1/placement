library(readxl)
# Load libraries
library(gtsummary)
library(huxtable)
library(openxlsx)
library(dplyr)
library(glmmTMB)
library(ggplot2)

mat_data <- read_excel("data/mat_data.xlsx")

mat_data$Hospital<-as.factor(mat_data$Hospital)

mat_data$Intervention_phase<-as.factor(mat_data$Intervention_phase)

# Package names
packages <- c("gtsummary", "huxtable", "openxlsx","glmmTMB","dplyr", "DHARMa")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}


##----------------------------------------------------------------------------------------

-----------------------------------------------------------------------

tbl <- mat_data %>%
  mutate(Hospital_Phase = paste(Hospital, Intervention_phase, sep = "_")) %>%
  select(Hospital_Phase, anc_1st_visit, anc_4th_visit, del_still_births, del_mat_deaths,
         anc_booster_tt, del_in_facility, del_lbw_2500g, lbw_2500_per1000, stillbirth_rate_per1000) %>%
  tbl_summary(
    by = Hospital_Phase,
    statistic = list(all_continuous() ~ "{median} ({p25}, {p75})"),
    digits = all_continuous() ~ 1,
    missing = "no",
    label = list(
      anc_1st_visit ~ "ANC 1st visit",
      anc_4th_visit ~ "ANC 4th visit",
      del_still_births ~ "No. of stillbirths",
      del_mat_deaths ~ "No. of maternal deaths",
      anc_booster_tt ~ "ANC booster TT",
      del_in_facility ~ "Facility deliveries",
      del_lbw_2500g ~ "No. of LBW (<2500g)",
      lbw_2500_per1000 ~ "LBW rate per 1000",
      stillbirth_rate_per1000 ~ "Stillbirth rate per 1000")
  ) %>%
  add_overall() %>%
  add_n() %>%
  bold_labels()
  print(tbl)
  

# Export to Excel
as_hux_xlsx(tbl, file = "descriptive_summary_table(Median,IQR).xlsx")

##---------------------------------------------------------------------------------------
tbl2 <- Mat_data %>%
  mutate(Hospital_Phase = paste(Hospital, Intervention_phase, sep = "_")) %>%
  select(Hospital_Phase, anc_1st_visit, anc_4th_visit, del_still_births, del_mat_deaths,
         anc_booster_tt, del_lbw_2500g, lbw_2500_per1000, stillbirth_rate_per1000) %>%
  tbl_summary(
    by = Hospital_Phase,
    statistic = list(all_continuous() ~ "{mean} ({sd})"),
    digits = all_continuous() ~ 1,
    missing = "no"
  ) %>%
  add_overall() %>%
  add_n() %>%
  bold_labels()

# Export to Excel
as_hux_xlsx(tbl2, file = "descriptive_summary_table(Mean,sd).xlsx")


## Initial Approach
tbl <- mat_data %>%
  select(Hospital, anc_1st_visit, anc_4th_visit, del_still_births, del_mat_deaths,
         anc_booster_tt, del_lbw_2500g,lbw_2500_per1000,stillbirth_rate_per1000 ) %>%
  tbl_summary(
    by = Hospital,
    statistic = list(all_continuous() ~ "{median} ({p25}, {p75})"),
    digits = all_continuous() ~ 1,
    missing = "no",
    label = list(
      anc_1st_visit ~ "ANC 1st Visit",
      anc_4th_visit ~ "ANC 4th Visit",
      del_still_births ~ "Delivery Stillbirths",
      del_mat_deaths ~ "Delivery Maternal Deaths",
      anc_booster_tt ~ "ANC Booster TT",
      del_lbw_2500g ~ "Delivery LBW <2500g",
      lbw_2500_per1000 ~ "LBW Rate per 1000",
      stillbirth_rate_per1000 ~ "Stillbirth Rate per 1000"
    )
  ) %>%
  add_overall() %>%
  add_n() %>%
  bold_labels()
  print(tbl)


# Export to Excel
as_hux_xlsx(tbl, file = "descriptive_summary_table.xlsx")

###-----------------------------------------------------------------------

library(gtsummary)

# Define labels for variables
var_labels <- c(
  anc_1st_visit = "1st ANC visit",
  anc_4th_visit = "4th ANC visit",
  del_still_births = "Stillbirths",
  del_mat_deaths = "Maternal Deaths",
  anc_booster_tt = "ANC Booster TT",
  del_lbw_2500g = "LBW <2500g",
  lbw_2500_per1000 = "LBW rate per 1000",
  stillbirth_rate_per1000 = "Stillbirth rate per 1000"
)

# Create the summary table with renamed variables
library(gtsummary)

tbl2 <- mat_data %>%
  select(
    Hospital, anc_1st_visit, anc_4th_visit, del_still_births, del_mat_deaths,
    anc_booster_tt, del_lbw_2500g, lbw_2500_per1000, stillbirth_rate_per1000
  ) %>%
  tbl_summary(
    by = Hospital,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})"
    ),
    digits = all_continuous() ~ 1,
    missing = "no",
    label = list(
      anc_1st_visit ~ "ANC 1st Visit",
      anc_4th_visit ~ "ANC 4th Visit",
      del_still_births ~ "Delivery Stillbirths",
      del_mat_deaths ~ "Delivery Maternal Deaths",
      anc_booster_tt ~ "ANC Booster TT",
      del_lbw_2500g ~ "Delivery LBW <2500g",
      lbw_2500_per1000 ~ "LBW Rate per 1000",
      stillbirth_rate_per1000 ~ "Stillbirth Rate per 1000"
    )
  ) %>%
  add_overall() %>%
  add_n() %>%
  bold_labels()
  print(tbl2)

# Export to Excel
as_hux_xlsx(tbl2, file = "descriptive_summary_table2.xlsx")

####----------infrastructure upgrade-----------------------------

##--Facility2 received at infrastructure upgrade at month 26 and facility3 at month 25

##anc_1st_visit = "First ANC visit count",
##anc_4th_visit = "Fourth ANC visit count", 
##del_still_births = "Stillbirths count",
##del_mat_deaths = "Maternal deaths count"


# Filter data for Facility2 and Facility3
plot_data_23 <- mat_data %>% filter(Hospital %in% c("Facility2", "Facility3"))

model_glmmTMB <- glmmTMB(
  anc_1st_visit ~ time + infra + post_infra_time + (1 | Hospital),
  data = plot_data_23,
  family = nbinom2()
)
summary(model_glmmTMB)

#Test assumptions
install.packages("DHARMa")
library(DHARMa)

sim_res <- simulateResiduals(model_glmmTMB)
plot(sim_res)
testDispersion(sim_res)        # Over/underdispersion
testZeroInflation(sim_res)     # Zero inflation
testUniformity(sim_res)        # Uniformity of residuals

plotResiduals(sim_res, plot_data_23$time)
plotResiduals(sim_res, plot_data_23$infra)
plotResiduals(sim_res, plot_data_23$post_infra_time)


# Add fitted values from the glmmTMB model
plot_data_23$fitted <- predict(model_glmmTMB, type = "response")
# Plot observed vs. fitted for both hospitals
ggplot(plot_data_23, aes(x = time, color = Hospital)) +
  geom_line(aes(y = anc_1st_visit), size = 1) +
  geom_line(aes(y = fitted), linetype = "dashed", size = 1) +
  geom_vline(data = data.frame(Hospital = c("Facility2", "Facility3"), 
                               intervention = c(26, 25)),
             aes(xintercept = intervention, color = Hospital),
             linetype = "dotted",size = 1, show.legend = FALSE) +
  labs(title = "1st ANC Visits Pre and Post Infrastructure Upgrades",
       subtitle = "Dashed lines represent fitted slope \n Vertical lines represent intervention",
       x = "Month", y = "No. of ANC visits") +
  scale_color_brewer(palette = "Dark2") + # Colourblind-friendly palette
  theme_gray(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, face = "plain"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold")
  )


 ##----Fourth ANC visit count----------------------------------------------------

model_4th_visit <- glmmTMB(
  anc_4th_visit ~ time + infra + post_infra_time + (1 | Hospital),
  data = plot_data_23,
  family = nbinom2()
)
summary(model_4th_visit)


# Add fitted values from the glmmTMB model
plot_data_23$fitted <- predict(model_4th_visit, type = "response")

# Plot observed vs. fitted for both hospitals
ggplot(plot_data_23, aes(x = time, color = Hospital)) +
  geom_line(aes(y = anc_4th_visit), size = 1) +
  geom_line(aes(y = fitted), linetype = "dashed", size = 1) +
  geom_vline(data = data.frame(Hospital = c("Facility2", "Facility3"),
                               intervention = c(26, 25)),
             aes(xintercept = intervention, color = Hospital),
             linetype = "dotted",size = 1, show.legend = FALSE) +
  labs(title = "4th ANC Visits Pre and Post Infrastructure Upgrades",
       subtitle = "Dashed lines represent fitted slope \n Vertical lines represent intervention",
       x = "Month", y = "No. of ANC visits") +
  scale_color_brewer(palette = "Dark2") + # Colourblind-friendly palette
  theme_gray(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, face = "plain"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold")
  )


###-------Booster TT Dose

model_tt <- glmmTMB(
  anc_booster_tt ~ time + training + post_training_time + (1 | Hospital),
  data = plot_data_23,
  family = nbinom2()
)

summary(model_tt)

# Add fitted values to your dataset
plot_data_23$fitted_tt <- predict(model_tt, type = "response")

# Plot observed and fitted TT doses by facility
ggplot(plot_data_23, aes(x = time, color = Hospital)) +
  geom_line(aes(y = anc_booster_tt), size = 1) +
  geom_line(aes(y = fitted_tt), linetype = "dashed", size = 1) +
  geom_vline(data = data.frame(Hospital = c("Facility2", "Facility3"),
                               intervention = c(26, 25)),
             aes(xintercept = intervention, color = Hospital),
             linetype = "dotted",size = 1, show.legend = FALSE) +
       labs(title = "Booster TT Dose Pre and Post Infrastructure Upgrades",
            subtitle = "Vertical lines represent intervention",
            x = "Month", y = "Stillbirth Count") +
         theme_minimal(base_size = 12) +
         theme(
           plot.title = element_text(hjust = 0.5, face = "bold"),
           plot.subtitle = element_text(hjust = 0.5, face = "italic"))


### Facility Deliveries
model_deliveries <- glmmTMB(
  del_in_facility ~ time + infra + post_infra_time + (1 | Hospital),
  data = plot_data_23,
  family = nbinom2()
)
summary(model_deliveries)

# Add fitted values from the glmmTMB model
plot_data_23$fitted <- predict(model_deliveries, type = "response")

# Plot observed vs. fitted for both hospitals
ggplot(plot_data_23, aes(x = time, color = Hospital)) +
  geom_line(aes(y = del_in_facility), size = 0.8) +
  geom_line(aes(y = fitted), linetype = "dashed", size = 0.8) +
  geom_vline(data = data.frame(Hospital = c("Facility2", "Facility3"),
                               intervention = c(26, 25)),
             aes(xintercept = intervention, color = Hospital),
             linetype = "dotted",size = 1, show.legend = FALSE) +
  labs(title = "Facility Deliveries Pre and Post Infrastructure Upgrades",
       subtitle = "Dashed lines represent fitted slope \n Vertical lines represent intervention",
       x = "Month", y = "No. of deliveries") +
  scale_color_brewer(palette = "Dark2") + # Colourblind-friendly palette
  theme_gray(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, face = "plain"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold")
  )

##Model for stillbirths-------------------------------------------------------

model_stillbirth <- glmmTMB(
  del_still_births ~ time + infra + post_infra_time + (1 | Hospital),
  data = plot_data_23,
  family = nbinom2()
)

summary(model_stillbirth)

# Add fitted values to your dataset
plot_data_23$fitted_stillbirths <- predict(model_stillbirth, type = "response")

# Plot observed and fitted stillbirths by facility
ggplot(plot_data_23, aes(x = time, color = Hospital)) +
  geom_line(aes(y = del_still_births), size = 0.8) +
  geom_line(aes(y = fitted_stillbirths), linetype = "dashed", size = 0.8) +
  geom_vline(data = data.frame(Hospital = c("Facility2", "Facility3"),
                               intervention = c(26, 25)),
             aes(xintercept = intervention, color = Hospital),
             linetype = "dotted",size = 1, show.legend = FALSE) +
  labs(title = "Trends in stillbirths pre and post infrastructure Upgrades",
       subtitle = "Dashed lines represent fitted slope \n Vertical lines represent intervention",
       x = "Month", y = "No. of stillbirths") +
  scale_color_brewer(palette = "Dark2") + # Colourblind-friendly palette
  theme_gray(base_size = 10) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, face = "plain"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold")
  )



######-----------healthcare training
##--Facility1 received at Staff healthcare training at month 33 and facility3 at month 19
# Filter data for Facility2 and Facility3
plot_data_13 <- mat_data %>% filter(Hospital %in% c("Facility1", "Facility3"))

###---Deliveries after training
model_del_training <- glmmTMB(
  del_in_facility ~ time + training + post_training_time + (1 | Hospital),
  data = plot_data_13,
  family = nbinom2()
)

summary(model_del_training)

# Add fitted values to your dataset
plot_data_13$deliveries <- predict(model_del_training, type = "response")

# Plot observed and fitted TT doses by facility
# Define custom colours for each facility
facility_colors <- c("Facility1" = "#1f78b4",  # Blue
                     "Facility3" = "#ff7f00")  # Orange

# Create your plot
ggplot(plot_data_13, aes(x = time, color = Hospital)) +
  geom_line(aes(y = del_in_facility), size = 0.8) +
  geom_line(aes(y = deliveries), linetype = "dashed", size = 0.8) +
  geom_vline(data = data.frame(Hospital = c("Facility1", "Facility3"),
                               intervention = c(33, 19)),
             aes(xintercept = intervention, color = Hospital),
             linetype = "dotted", size = 1, show.legend = FALSE) +
  scale_color_manual(values = facility_colors) +  # Use manual colour mapping
  labs(title = "Facility Deliveries Pre and Post Training",
       subtitle = "Dashed lines represent fitted slope \n Vertical lines represent intervention",
       x = "Month", y = "No. of deliveries") +
  theme_gray(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, face = "plain"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold")
  )

###---LBW  after training
model_lbw <- glmmTMB(
  del_lbw_2500g ~ time + training + post_training_time + (1 | Hospital),
  data = plot_data_13,
  family = nbinom2()
)

summary(model_lbw)

# Add fitted values to your dataset
plot_data_13$fitted_lbw <- predict(model_lbw, type = "response")

# Plot observed and fitted TT doses by facility
ggplot(plot_data_13, aes(x = time, color = Hospital)) +
  geom_line(aes(y = del_lbw_2500g), size = 0.8) +
  geom_line(aes(y = fitted_lbw), linetype = "dashed", size = 0.8) +
  geom_vline(data = data.frame(Hospital = c("Facility1", "Facility3"),
                               intervention = c(33, 19)),
             aes(xintercept = intervention, color = Hospital),
             linetype = "dotted", size = 1, show.legend = FALSE) +
  scale_color_manual(values = facility_colors) +
  labs(title = "Low birth weight Pre and Post Healthcare training",
       subtitle = "Dashed lines represent fitted slope \n Vertical lines represent intervention",
       x = "Month", y = "No. of LBW (<2500g)") +
  theme_gray(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, face = "plain"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold")
  )
