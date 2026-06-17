# Author: Kennedy Lushasi
# Comparing cases and exposures 3 months before September 2025 when free PEP provision began  and 
# during free pep provision  for Sept, Oct and November, and 3 months after the free PEP PERIOD ended
# in Arusha and Kilosa


rm(list = ls())

library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)  
library(knitr)
library(broom)

# Load data
patients <- read.csv("data/clean/patients_clean.csv", stringsAsFactors = FALSE)# health facility data for human bites
cases <- read.csv("data/clean/animals_clean.csv", stringsAsFactors = FALSE)# veterinary/field workers for animal bites

# Clean + prepare data
patients <- patients %>%
  mutate(
    VISIT_STATUS = tolower(trimws(VISIT_STATUS)),
    DATE_VISIT = as.Date(DATE_VISIT), # Use the correct date column explicitly
    # Standardize district names
    DISTRICT = ifelse(DISTRICT == "Arusha",
                      "Arusha City Council",
                      DISTRICT)
  )


# Create period based on DATE_VISIT
patients_p<- patients %>%
  mutate(period = case_when(
    DATE_VISIT >= as.Date("2025-06-01") & DATE_VISIT <= as.Date("2025-08-31") ~ "Before",
    DATE_VISIT >= as.Date("2025-09-01") & DATE_VISIT <= as.Date("2025-11-30") ~ "During",
    DATE_VISIT >= as.Date("2025-12-01") & DATE_VISIT <= as.Date("2026-02-28") ~ "After",
    TRUE ~ NA_character_
  ))


# Filter ONLY Kilosa and Arusha + valid periods
patients_filtered <- patients_p %>%
  filter(
    DISTRICT %in% c("Kilosa", "Arusha City Council"),
    !is.na(period)
  )

patients_filtered$FACILITY



# Summary table
summary_table <- patients_filtered %>%
  group_by(DISTRICT, period) %>%
  summarise(
    total_bites = n(),
    first_dose  = sum(VISIT_STATUS == "first"),
    second_dose = sum(VISIT_STATUS == "second"),
    third_dose  = sum(VISIT_STATUS == "third"),
    
    # Treat 4th and any higher as IM completion
    fourth_dose = sum(VISIT_STATUS %in% c("forth","fifth")),
    completed = third_dose + fourth_dose,
    completion_rate = ifelse(first_dose > 0, completed / first_dose, NA),
    positive_clinical = sum(VISIT_STATUS == "positive_clinical_signs"),
    .groups = "drop"
  )

print(summary_table)
write_csv(summary_table, "outputs/summary_table.csv")

# Validation check
patients_filtered %>%
  count(VISIT_STATUS, period)


#-----------------------------------
# Percentage change in reporting of bites during and after the pilot; 
# Reshape data to wide format (one row per district)
bite_change <- summary_table %>%
  select(DISTRICT, period, total_bites) %>%
  pivot_wider(names_from = period, values_from = total_bites) %>%
  mutate(
    pct_increase_during = ((During - Before) / Before) * 100, # % increase from Before → During
    pct_change_after = ((After - During) / During) * 100,   # % change from During → After (negative = decrease)
    pct_decrease_after = ((During - After) / During) * 100  # make decrease explicit as positive value
  )
print(bite_change)


bite_change_clean <- bite_change %>%
  mutate(
    pct_increase_during = round(pct_increase_during, 1),
    pct_decrease_after = round(pct_decrease_after, 1)
  ) %>%
  select(DISTRICT, pct_increase_during, pct_decrease_after)

print(bite_change_clean)


# Add proportions for reporting
summary_table_prop <- summary_table %>%
  mutate(
    prop_started = first_dose / total_bites,
    prop_completed_total = completed / total_bites,
    prop_completed_started = completion_rate
  )
# order the period
summary_table_prop <- summary_table_prop %>%
mutate(period = factor(period, levels = c("Before", "During", "After")))


# PEP Completion rate
# add the percentage at the top of the bars
fig_completion <- ggplot(summary_table_prop, 
                         aes(x = period, y = completion_rate, fill = DISTRICT)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  #  add labels on top of bars
  geom_text(aes(label = percent(completion_rate, accuracy = 1)),
            position = position_dodge(width = 0.8),
            vjust = -0.5, size = 4) +
  labs(title = "",
       x = "Study period",
       y = "Completion rate") +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, 1)) +   # ensures space for labels
  theme_classic() +
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA)
  )
ggsave("Figures/figure_completion_rates.pdf",
       plot = fig_completion,
       width = 8, height = 6,
       bg = "white")


# Total bite cases, reported bite cases before, during and after between the study settings
fig_cases <- ggplot(summary_table_prop, 
                    aes(x = period, y = total_bites, fill = DISTRICT)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  labs(title = "",
       x = "Study period",
       y = "Number of cases") +
  theme_classic() +
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA)
  )

ggsave("Figures/figure_bite_cases.pdf", plot = fig_cases, width = 8, height = 6,bg = "white")


#----------------------------------------------
# Trend of bite cases and pep seeking behaviours, create cascade data



cascade_data <- patients_filtered %>%
  filter(DISTRICT %in% c("Kilosa", "Arusha City Council"),
         !is.na(period)) %>%
  mutate(
    VISIT_STATUS = tolower(trimws(VISIT_STATUS)),
    dose_stage = case_when(
      VISIT_STATUS == "first"  ~ "1st",
      VISIT_STATUS == "second" ~ "2nd",
      VISIT_STATUS == "third"  ~ "3rd",
      VISIT_STATUS == "forth"  ~ "4th",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(dose_stage)) %>%
  count(DISTRICT, period, dose_stage)

# Apply ordering AFTER creating the dataset
cascade_data <- cascade_data %>%
  mutate(
    period = factor(period, levels = c("Before", "During", "After")),
    dose_stage = factor(dose_stage, levels = c("1st","2nd","3rd","4th"))
  )

# Plot
fig <- ggplot(cascade_data,
              aes(x = dose_stage, y = n, fill = DISTRICT)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~period, nrow = 1) +
  
  labs(title = "",
       x = "Doses",
       y = "Number of victims") +
  scale_y_continuous(
    breaks = seq(0, max(cascade_data$n, na.rm = TRUE), by = 50)
  ) +
  theme_classic() +
  theme(
    strip.background = element_blank(), # remove boxes around facet labels (Before/During/After)
    # make labels bold and clean
    strip.text = element_text(face = "bold"),
    # keep axis lines, panel.border = element_blank(),
    # clean grid
    #panel.grid.major.x = element_blank(),
    #panel.grid.minor = element_blank(),
    #panel.grid.major.y = element_line(color = "grey80")
  )
ggsave("Figures/figure_cascade.pdf", plot = fig, width = 8, height = 6)


##---------------------
# Among patients treated in facilities located in Kilosa or Arusha City Council 
# during the pilot period, identify those whose home district was outside the 
# study district.

# Create/standardize the origin variable first
patients <- patients_filtered %>%
  mutate(
    patient_origin = case_when(
      !is.na(COUNCIL) & trimws(COUNCIL) != "" &
        COUNCIL == "Arusha CC" ~ "Arusha City Council",
      !is.na(COUNCIL) & trimws(COUNCIL) != "" &
        COUNCIL == "Kilosa DC" ~ "Kilosa",
      !is.na(COUNCIL) & trimws(COUNCIL) != "" ~ COUNCIL,
      TRUE ~ DISTRICT
    )
  )


# Define outsiders
external_analysis <- patients %>%
  filter(period == "During",
    DISTRICT %in% c("Kilosa", "Arusha City Council")
  ) %>% mutate(
    external = patient_origin != DISTRICT)

# Summary table
external_summary <- external_analysis %>%
  mutate(external = patient_origin != DISTRICT) %>%
  group_by(DISTRICT) %>%
  summarise(
    total_patients = n(),
    external_patients = sum(external),
    prop_external = external_patients / total_patients
  )
print(external_summary)


#------------------

# Explore to see how many people of these that got the 1st dose of human rabies 
# vaccines during the pilot , and were bitten by high risk animals


# identify first-dose recipients during the pilot period, classify them according 
# to high-risk animal exposure based on rabies_status, and summarize the 
# number and proportion of exposed patients overall and by study district.

# Define high-risk exposures
high_risk_cases <- patients %>%
  filter(
    period == "During",
    tolower(trimws(VISIT_STATUS)) == "first"
  ) %>%
  mutate(
    high_risk = tolower(trimws(rabies_status)) %in% c(
      "suspicious_for_rabies",
      "suspicious_for_of_rabies",
      "unknown",
      "treatment"))

# Overall summary
high_risk_summary <- high_risk_cases %>%
  summarise(
    first_dose_patients = n(),
    high_risk_patients = sum(high_risk, na.rm = TRUE),
    prop_high_risk = high_risk_patients / first_dose_patients
  )
high_risk_summary

# by district
high_risk_by_district <- high_risk_cases %>%
  filter(DISTRICT %in% c("Kilosa", "Arusha City Council")) %>%
  group_by(DISTRICT) %>%
  summarise(
    first_dose_patients = n(),
    high_risk_patients = sum(high_risk, na.rm = TRUE),
    prop_high_risk = high_risk_patients / first_dose_patients,
    .groups = "drop"
  )

high_risk_by_district

#---------------------------------------------------------------------------
# Bite patient presentations and rabies exposure risk per 100,000 population 
# Before, During, and After  the pilot
#---------------------------------------------------------------------------

# Population table for Arusha and Kilosa Dc
populations <- tibble(
  DISTRICT = c("Kilosa", "Arusha City Council"),
  population = c(670000, 723101))


# High-risk exposures
# patients_analysis <- patients %>%
#   mutate(
#     high_risk = tolower(trimws(rabies_status)) %in% c(
#       "suspicious_for_rabies",
#       "suspicious_for_of_rabies",
#       "unknown",
#       "treatment"))


# Keep only study districts and study periods
patients_analysis <- patients %>% 
  filter( DISTRICT %in% c("Kilosa", "Arusha City Council"),
          !is.na(period),
          period %in% c("Before", "During", "After")) %>%
  mutate(
    high_risk = tolower(trimws(rabies_status)) %in% c
    ( "suspicious_for_rabies","suspicious_for_of_rabies","unknown","treatment"))

#Check
patients_analysis %>%
  count(period)


# Create summary table of bite changes /100,000 persons
risk_table <- patients_analysis %>%
  filter(!is.na(period),
         period %in% c("Before", "During", "After"),
         DISTRICT %in% c("Kilosa", "Arusha City Council"),
         tolower(trimws(VISIT_STATUS)) == "first") %>%
  group_by(DISTRICT, period) %>%
  summarise(bite_patients = n(),
            high_risk_bites = sum(high_risk, na.rm = TRUE),.groups = "drop") %>%
  left_join(populations, by = "DISTRICT") %>%
  mutate(pct_high_risk =round(100 * high_risk_bites / bite_patients, 1),
    bite_incidence_100k =round((bite_patients * 4 / population) * 100000, 1),
    high_risk_incidence_100k =round((high_risk_bites * 4 / population) * 100000, 1)) %>%
  select(DISTRICT,period,bite_patients,high_risk_bites,pct_high_risk,bite_incidence_100k, high_risk_incidence_100k) %>% 
  mutate( period = factor(period,
                          levels = c("Before", "During", "After"))) %>%
  arrange(DISTRICT, period)
risk_table

write_csv(risk_table, "outputs/risk_table.csv")



# calculate the change in high-risk exposure incidence during the pilot
risk_changes <- risk_table %>%
  select(DISTRICT, 
         period,
         high_risk_incidence_100k) %>%
  pivot_wider(names_from = period,
              values_from = high_risk_incidence_100k) %>%
  mutate(pct_change_during =
           round(100 * (During - Before) / Before, 1),
         pct_change_after =
           round(100 * (After - During) / During, 1))
risk_changes

write_csv(risk_changes, "outputs/risk_changes.csv")

# Percentage table of high risk bites
risk_summary <- patients_analysis %>%
  filter(tolower(trimws(VISIT_STATUS)) == "first") %>%
  group_by(period) %>%
  summarise(total_patients = n(),
            high_risk_patients = sum(high_risk),
            non_high_risk = total_patients - high_risk_patients, 
            percent_high_risk =round(100 * high_risk_patients / total_patients, 1),
            .groups = "drop")
risk_summary
write_csv(risk_summary, "outputs/risk_summary.csv")



# Compare proportion of high risk bites across periods
risk_period <- patients_analysis %>%
  filter(VISIT_STATUS == "first",
         DISTRICT %in% c("Kilosa","Arusha City Council")) %>% 
  count(period, high_risk)
chisq.test(xtabs(n ~ period + high_risk,data = risk_period))


# Logistic regression of the high risk exposures
risk_model <- glm(high_risk ~ period + DISTRICT,family = binomial(), 
                  data = patients_analysis %>%filter(VISIT_STATUS == "first"))
summary(risk_model)
exp(cbind(OR = coef(risk_model), confint(risk_model)))


# Examine the actual percentage changes
patients_analysis %>%
  filter(
    VISIT_STATUS == "first",
    DISTRICT %in% c("Kilosa","Arusha City Council")) %>%
  group_by(period) %>%
  summarise(
    total = n(),
    high_risk = sum(high_risk),
    percent_high_risk = round(100 * high_risk / total, 1))


#--------------------------
# PEP compeletion rate
completion_table <- patients_analysis %>%
  group_by(DISTRICT, period) %>%
  summarise(
    first_dose = sum(VISIT_STATUS == "first"),
    completed = sum(VISIT_STATUS %in% c("third","forth","fifth")),
    completion_rate = completed / first_dose,
    .groups = "drop")
completion_table

# Test
completion_data <- patients_analysis %>% 
  mutate(completed =VISIT_STATUS %in% c("third","forth","fifth"))
chisq.test(xtabs(~ period + completed,data = completion_data))


# Logistic regression
completion_model <- glm(completed ~ period + DISTRICT,family = binomial(), 
                        data = completion_data)
summary(completion_model)
exp(cbind(OR = coef(completion_model), confint(completion_model)))

# if OR > 1 for During = improved completion during free PEP, but when OR < 1 = reduced completion.


# Incidence rate ratio. because only 6 rows exist, use pairwise incidence ratios
# IRR > 1 = increase, IRR < 1 = decrease
risk_changes <- risk_table %>%
  select(DISTRICT,period, high_risk_incidence_100k) %>% 
  tidyr::pivot_wider(names_from = period,values_from = high_risk_incidence_100k) %>%
  mutate(IRR_During_vs_Before = During / Before,
         IRR_After_vs_During = After / During)
risk_changes

# Plot, incidence of high riks exposures

ggplot(risk_table,aes( x = period,y = high_risk_incidence_100k,
                       color = DISTRICT, 
                       group = DISTRICT)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 4) +
  theme_classic() +
  labs(x = "",y = "High-risk exposures per 100,000 population")




# -------------------------------------------------
# Effect size table for incidence changes, before, during and after the pilot
# -------------------------------------------------
effect_table <- risk_table %>%
  select(DISTRICT,period,bite_incidence_100k,high_risk_incidence_100k) %>%
  pivot_wider(names_from = period,values_from = c(bite_incidence_100k,high_risk_incidence_100k)) %>%
  mutate(bite_pct_change_during =round(100 *(bite_incidence_100k_During -bite_incidence_100k_Before)/bite_incidence_100k_Before,1),
         bite_pct_change_after =round(100 *(bite_incidence_100k_After -bite_incidence_100k_During)/bite_incidence_100k_During, 1),
    risk_pct_change_during = round(100 * (high_risk_incidence_100k_During -high_risk_incidence_100k_Before) / high_risk_incidence_100k_Before,1),
    risk_pct_change_after =round(100 *(high_risk_incidence_100k_After - high_risk_incidence_100k_During)/high_risk_incidence_100k_During,1))
effect_table


# Odd ratio table of high risk exposures
risk_OR_table <- tidy(risk_model, exponentiate = TRUE,conf.int = TRUE) %>%
  select(term,estimate, conf.low, conf.high,p.value)
risk_OR_table


# Odds Ratio Table (PEP Completion)
completion_OR_table <- tidy(completion_model,exponentiate = TRUE,conf.int = TRUE) %>%
  select(term, estimate,conf.low, conf.high,p.value)
completion_OR_table

# District × Period Interaction, this is important as kilosa and arusha behave differently

interaction_model <- glm(high_risk ~ period * DISTRICT, family = binomial(), 
                         data = patients_analysis %>%
                           filter(VISIT_STATUS == "first"))
summary(interaction_model)

anova(risk_model,interaction_model,test = "Chisq")



##=======================================
##-----------------------

# Monthly bite trends
monthly_bites <- patients %>%
  filter(
    DISTRICT %in% c(
      "Kilosa",
      "Arusha City Council"),
    VISIT_STATUS == "first") %>%
  mutate(month =lubridate::floor_date(DATE_VISIT,"month")) %>%
  count(month, DISTRICT)

ggplot(monthly_bites,aes(x = month,y = n,color = DISTRICT)) +
  geom_line(linewidth = 1.2) +
  geom_point() +
  theme_classic() +
  labs(x = "",y = "First-dose bite patients")

##---------------


