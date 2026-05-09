##----------------------------------------------------------------------------##
## File name: SAEH Table One.                                                 ##  
## Programmer: Kelsey MacKenzie                                               ##
## Date: 14-SEP-2025                                                          ##
## Last modified: 12-FEB-2026                                                 ##
## Purpose: Create descriptive characteristics table one for final data set   ##
## PI: Emily Boniface                                                         ##
##----------------------------------------------------------------------------##

#### SETUP ---------------------------------------------------------------------

##### Load packages  ----
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(gt)
library(webshot2)
library(rmarkdown)
library(janitor)
library(magick)
library(stringr)
library(RColorBrewer)
library(gtsummary)
library(scales)
library(forcats)
library(gtsummary)
library(haven)



#### Import Data ---------------------------------------------------------------
SAEH_clean <- readRDS("SAEHdata_clean.rds") %>%
  rename_with(~ gsub("\\.", "_", .x)) %>%
  select(-munic_id) %>%            # drop the locality-level munic_id
  rename(munic_id = munic_code)    # promote the clean munic_code to munic_id

SAEH_clean %>%
  filter(age_unit %in% c("days", "months", "No response", "Not specified")) %>%
  select(id, year, age_unit, cveedad, edad, icd_code) %>%
  arrange(year, age_unit)

# remove records whose age wasn't documented as years
SAEH_clean <- SAEH_clean %>% filter(age_unit == "years")


###--- TABLE ONE  --------------------------------------------------------------
SAEH_clean <- SAEH_clean %>%
  mutate(
    # State region categorization sources: https://www.mdpi.com/1660-4601/16/3/407?utm_source=researchgate.net&medium=article#
    #                                      https://www.gob.mx/cms/uploads/attachment/file/209093/ENSANUT.pdf
    region = case_when(
      state %in% c("Baja California", "Baja California Sur", "Sonora", "Chihuahua", 
                   "Coahuila de Zaragoza", "Nuevo León", "Tamaulipas") ~ "North",
      state %in% c("Sinaloa", "México", "Jalisco", "Colima", "Nayarit", 
                   "Michoacán de Ocampo", "Morelos", "Aguascalientes",
                   "Zacatecas", "Guanajuato", "Durango", "San Luis Potosí", 
                   "Querétaro de Arteaga") ~ "Central",
      state == "Ciudad de México" ~ "Mexico City",
      state %in% c("Oaxaca", "Chiapas", "Tabasco", "Campeche", "Yucatán", 
                   "Quintana Roo", "Guerrero", "Puebla", "Tlaxcala", "Hidalgo", 
                   "Veracruz de Ignacio de la Llave") ~ "South",
      is.na(state) | state %in% c("Not Specified", "No response", "Not Applicable") ~ "Missing",
      state %in% c("USA", "Rest of Latin America", "Other Latin American Country", 
                   "Other Country", "Other Continent") ~ "Missing",
      TRUE ~ "Missing"),
    
    # Speaks Indigenous Language
    habla_lengua_label = case_when(
      habla_lengua == 2 ~ "No",
      habla_lengua == 1 ~ "Yes",
      habla_lengua == 3 ~ "No response",
      habla_lengua == 4 ~ "No response",
      TRUE ~ "Missing"),
    
    # Indigenous Identity
    indigena_label = case_when(
      indigena == 1 ~ "Yes",
      indigena == 2 ~ "No",
      indigena == 9 ~ "No response",
      TRUE ~ "Missing"),
    
    # Age category
    age_cat = case_when(
      edad < 15 ~ "under 15",
      edad >= 15 & edad <= 16 ~ "15-16",
      edad >= 17 & edad <= 19 ~ "17-19",
      edad >= 20 & edad <= 24 ~ "20-24",
      edad >= 25 & edad <= 29 ~ "25-29",
      edad >= 30 & edad <= 34 ~ "30-34",
      edad >= 35 & edad <= 39 ~ "35-39",
      edad >= 40 ~ "40+",
      TRUE ~ "Missing"),
    
    
    # Relationship status
    relationship_grouped = case_when(
      relationship_status == "Single" ~ "Single",
      relationship_status %in% c("Married", "Domestic partnership") ~ "Married/Partnership",
      relationship_status %in% c("Widowed", "Divorced", "Separated") ~ "Widowed/Divorced/Separated",
      is.na(relationship_status) | relationship_status %in% c("Unknown", "No response", "Not specified", "Not applicable", "Missing") ~ "Missing",
      TRUE ~ relationship_status),
    
    # Parity category
    parity_cat = case_when(
      is.na(parity) ~ "Missing",
      parity == 0 ~ "0",
      parity == 1 ~ "1",
      parity == 2 ~ "2",
      parity >= 3 ~ "3+",
      TRUE ~ "Missing"),
    
    # Insurance combination
    insurance_grouped = case_when(
      insurance %in% c("Public insurance", "INSABI", "IMSS BIENESTAR", "OPD IMSS BIENESTAR", "Prospera", "State government", "ISSFAM", "PEMEX", "SEDENA", "SEMAR", "ISSSTE", "Gratuidad") ~ "Public/State programs",
      insurance %in% c("No response", "Not specified", "Not Specified", "Missing") | is.na(insurance) ~ "No response/Not specified/Missing",
      TRUE ~ insurance),
    
    # Hospital care level
    hosp_care = case_when(
      care_level == "PRIMER NIVEL" ~ "Primary level",
      care_level == "SEGUNDO NIVEL" ~ "Secondary level",
      care_level == "TERCER NIVEL" ~ "Tertiary level",
      care_level == "NO APLICA" ~ "Missing",
      TRUE ~ NA_character_),
    
    # Municipality marginalization
    marg_quintile = case_when(
      grade_marg == "Muy bajo" ~ 1,
      grade_marg == "Bajo" ~ 2,
      grade_marg == "Medio" ~ 3,
      grade_marg == "Alto" ~ 4,
      grade_marg == "Muy alto" ~ 5,
      TRUE ~ NA),
    marg_group = case_when(
      marg_quintile %in% c(1,2) ~ "Less marginalized",
      marg_quintile %in% c(3,4,5) ~ "More marginalized",
      TRUE ~ NA_character_),
    
    # Municipality population
    municipality_pop = case_when(
      is.na(total_population) ~ "Missing",
      total_population < 15000 ~ "<15 K",
      total_population >= 15000 & total_population < 100000 ~ "15–99 K",
      total_population >= 100000 ~ "100 K+",
      TRUE ~ NA_character_),
    
    # Municipality education
    edu_group = case_when(
      pop_educ < 40 ~ "More educated",
      pop_educ >= 40 ~ "Less educated",
      TRUE ~ NA_character_),
    
    # Municipality adolescent fertility rate
    adol_fert = case_when(
      is.na(adol_fertrate) ~ "Missing",
      as.numeric(adol_fertrate) < 70.5 ~ "Low",
      as.numeric(adol_fertrate) >= 70.5 ~ "High",
      TRUE ~ NA_character_),
    
    # Municipality unit stratum
    unit_strat = case_when(
          is.na(unit_stratum) ~ "Missing",
          unit_stratum == "URBANO" ~ "Urban",
          unit_stratum == "RURAL" ~ "Rural",
          TRUE ~ NA_character_))


# Factor variables 
SAEH_clean <- SAEH_clean %>%
  mutate(
    region = fct_relevel(
      as.factor(region),
      "North",
      "Central",
      "Mexico City",
      "South",
      "Missing"),
    indigena_label = fct_relevel(
      as.factor(indigena_label),
      "Yes",
      "No",
      "No response",
      "Missing"),
    habla_lengua_label = fct_relevel(
      as.factor(habla_lengua_label),
      "Yes",
      "No",
      "No response",
      "Missing"),
    age_cat = fct_relevel(
      as.factor(age_cat),
      "under 15",
      "15-16",
      "17-19",
      "20-24",
      "25-29",
      "30-34",
      "35-39",
      "40+",
      "Missing"),
    insurance_grouped = fct_relevel(
      as.factor(insurance_grouped),
      "Uninsured",
      "IMSS",
      "Gratuidad",
      "Public/State programs",
      "Private insurance",
      "ISSFAM/PEMEX/SEDENA/SEMAR/ISSSTE",
      "Other",
      "Unknown",
      "Missing"),
    relationship_grouped= fct_relevel(
      as.factor(relationship_grouped),
      "Single",
      "Married/Partnership",
      "Widowed/Divorced/Separated",
      "Missing"),
    adol_fert = fct_relevel(
      as.factor(adol_fert),
      "Low",
      "High",
      "Missing"),
    hosp_care = fct_relevel(
      as.factor(hosp_care),
      "Primary level",
      "Secondary level",
      "Tertiary level",
      "Missing"),
    marg_group = fct_relevel(
      as.factor(marg_group),
      "Less marginalized",
      "More marginalized",
      "Missing"),
    edu_group = fct_relevel(
      as.factor(edu_group),
      "Less educated",
      "More educated",
      "Missing"),
    unit_strat = fct_relevel(
      as.factor(unit_strat),
      "Urban",
      "Rural",
      "Missing"),
    municipality_pop = fct_relevel(
      as.factor(municipality_pop),
      "<15 K",
      "15–99 K",
      "100 K+",
      "Missing"),
    hosp_care = fct_relevel(
      as.factor(hosp_care),
      "Primary level",
      "Secondary level",
      "Tertiary level",
      "Missing"))



# Create Table 1 summary with new categories
table_one <- SAEH_clean %>%
  filter(!is.na(region)) %>%
  select(
    region,
    age_cat,
    indigena_label,
    #habla_lengua_label,
    relationship_grouped,
    parity_cat,
    insurance_grouped,
    year,
    adol_fert,
    hosp_care,
    municipality_pop,
    marg_group,
    edu_group
    #admin_unit_code,
    #admin_unit_name,
    #unit_strat
    ) %>%
  tbl_summary(
    type = list(
      age_cat ~ "categorical",
      parity_cat ~ "categorical",
      hosp_care ~ "categorical",
      municipality_pop ~ "categorical",
      marg_group ~ "categorical",
      edu_group ~ "categorical",
      adol_fert ~ "categorical"
      #admin_unit_code ~ "categorical",
      #admin_unit_name ~ "categorical",
      #unit_strat ~ "categorical"
      ),
    statistic = list(
      all_categorical() ~ "{n} ({p}%)"),
    missing = "ifany",
    label = list(
      region ~ "State of Residence (Region)",
      age_cat ~ "Age",
      indigena_label ~ "Indigenous Identity",
      #habla_lengua_label ~ "Speaks Indigenous Language",
      relationship_grouped ~ "Relationship Status",
      parity_cat ~ "Parity",
      insurance_grouped ~ "Insurance",
      year ~ "Year",
      adol_fert ~ "Municipality Adolescent Fertility Rate \u00B9",
      hosp_care ~ "Hospital Care Level",
      municipality_pop ~ "Municipality Population",
      marg_group ~ "Municipality Marginalization \u00B2",
      edu_group ~ "Municipality Education \u00B3"
      #admin_unit_code ~ "Administrative Unit Code",
      #admin_unit_name ~ "Administrative Unit Name",
      #unit_strat ~ "Unit Stratum"
      )) %>%
  modify_caption("Table 1. Patient Characteristics for Abortive Events based out of Public Hospitals in Mexico (2018-2024)") %>%
  bold_labels()


# Convert to gt
table_one_gt <- as_gt(table_one)

# Add footnotes
gt_table <- table_one_gt %>%
  tab_source_note(source_note = "1. Low adolescent fertility rate is below national median (70.5 per 1000 adolescents aged 15–19).") %>%
  tab_source_note(source_note = "2. Less marginalization defined as bottom two quintiles of wealth index; more marginalized are top 3 quintiles.") %>%
  tab_source_note(source_note = "3. More educated is defined as municipalities where < 40% lack basic education; less educated is ≥ 40% lacking basic education.")

# Print table with footnotes
gt_table

# Save as PNG
Sys.setenv(CHROMOTE_HEADLESS = "new")
gtsave(gt_table, "saeh_tableone_12.11.png")





# Add Table 1 Variables to Emily's Additional Data Set -------------------------
SAEH_final_2 <- readRDS("SAEH_final_2.rds")

# remove records whose age wasn't documented as years
SAEH_final_2 <- SAEH_final_2 %>% filter(age_unit == "years")


SAEH_final_2 <- SAEH_final_2 %>%
  mutate(
    # State region categorization sources: https://www.mdpi.com/1660-4601/16/3/407?utm_source=researchgate.net&medium=article#
    #                                      https://www.gob.mx/cms/uploads/attachment/file/209093/ENSANUT.pdf
    region = case_when(
      state %in% c("Baja California", "Baja California Sur", "Sonora", "Chihuahua", 
                   "Coahuila de Zaragoza", "Nuevo León", "Tamaulipas") ~ "North",
      state %in% c("Sinaloa", "México", "Jalisco", "Colima", "Nayarit", 
                   "Michoacán de Ocampo", "Morelos", "Aguascalientes",
                   "Zacatecas", "Guanajuato", "Durango", "San Luis Potosí", 
                   "Querétaro de Arteaga") ~ "Central",
      state == "Ciudad de México" ~ "Mexico City",
      state %in% c("Oaxaca", "Chiapas", "Tabasco", "Campeche", "Yucatán", 
                   "Quintana Roo", "Guerrero", "Puebla", "Tlaxcala", "Hidalgo", 
                   "Veracruz de Ignacio de la Llave") ~ "South",
      is.na(state) | state %in% c("Not Specified", "No response", "Not Applicable") ~ "Missing",
      state %in% c("USA", "Rest of Latin America", "Other Latin American Country", 
                   "Other Country", "Other Continent") ~ "Missing",
      TRUE ~ "Missing"),
    
    # Speaks Indigenous Language
    habla_lengua_label = case_when(
      habla_lengua == 2 ~ "No",
      habla_lengua == 1 ~ "Yes",
      habla_lengua == 3 ~ "No response",
      habla_lengua == 4 ~ "No response",
      TRUE ~ "Missing"),
    
    # Indigenous Identity
    indigena_label = case_when(
      indigena == 1 ~ "Yes",
      indigena == 2 ~ "No",
      indigena == 9 ~ "No response",
      TRUE ~ "Missing"),
    
    # Age category
    age_cat = case_when(
      edad < 15 ~ "under 15",
      edad >= 15 & edad <= 16 ~ "15-16",
      edad >= 17 & edad <= 19 ~ "17-19",
      edad >= 20 & edad <= 24 ~ "20-24",
      edad >= 25 & edad <= 29 ~ "25-29",
      edad >= 30 & edad <= 34 ~ "30-34",
      edad >= 35 & edad <= 39 ~ "35-39",
      edad >= 40 ~ "40+",
      TRUE ~ "Missing"),
    
    
    # Relationship status
    relationship_grouped = case_when(
      relationship_status == "Single" ~ "Single",
      relationship_status %in% c("Married", "Domestic partnership") ~ "Married/Partnership",
      relationship_status %in% c("Widowed", "Divorced", "Separated") ~ "Widowed/Divorced/Separated",
      is.na(relationship_status) | relationship_status %in% c("Unknown", "No response", "Not specified", "Not applicable", "Missing") ~ "Missing",
      TRUE ~ relationship_status),
    
    # Parity category
    parity_cat = case_when(
      is.na(parity) ~ "Missing",
      parity == 0 ~ "0",
      parity == 1 ~ "1",
      parity == 2 ~ "2",
      parity >= 3 ~ "3+",
      TRUE ~ "Missing"),
    
    # Insurance combination
    insurance_grouped = case_when(
      insurance %in% c("Public insurance", "INSABI", "IMSS BIENESTAR", "OPD IMSS BIENESTAR", "Prospera", "State government", "ISSFAM", "PEMEX", "SEDENA", "SEMAR", "ISSSTE", "Gratuidad") ~ "Public/State programs",
      insurance %in% c("No response", "Not specified", "Not Specified", "Missing") | is.na(insurance) ~ "No response/Not specified/Missing",
      TRUE ~ insurance),
    
    # Hospital care level
    hosp_care = case_when(
      care_level == "PRIMER NIVEL" ~ "Primary level",
      care_level == "SEGUNDO NIVEL" ~ "Secondary level",
      care_level == "TERCER NIVEL" ~ "Tertiary level",
      care_level == "NO APLICA" ~ "Missing",
      TRUE ~ NA_character_),
    
    # Municipality marginalization
    marg_quintile = case_when(
      grade_marg == "Muy bajo" ~ 1,
      grade_marg == "Bajo" ~ 2,
      grade_marg == "Medio" ~ 3,
      grade_marg == "Alto" ~ 4,
      grade_marg == "Muy alto" ~ 5,
      TRUE ~ NA),
    marg_group = case_when(
      marg_quintile %in% c(1,2) ~ "Less marginalized",
      marg_quintile %in% c(3,4,5) ~ "More marginalized",
      TRUE ~ NA_character_),
    
    # Municipality population
    municipality_pop = case_when(
      is.na(total_population) ~ "Missing",
      total_population < 15000 ~ "<15 K",
      total_population >= 15000 & total_population < 100000 ~ "15–99 K",
      total_population >= 100000 ~ "100 K+",
      TRUE ~ NA_character_),
    
    # Municipality education
    edu_group = case_when(
      pop_educ < 40 ~ "More educated",
      pop_educ >= 40 ~ "Less educated",
      TRUE ~ NA_character_),
    
    # Municipality adolescent fertility rate
    adol_fert = case_when(
      is.na(adol_fertrate) ~ "Missing",
      as.numeric(adol_fertrate) < 70.5 ~ "Low",
      as.numeric(adol_fertrate) >= 70.5 ~ "High",
      TRUE ~ NA_character_),
    
    # Municipality unit stratum
    unit_strat = case_when(
      is.na(unit_stratum) ~ "Missing",
      unit_stratum == "URBANO" ~ "Urban",
      unit_stratum == "RURAL" ~ "Rural",
      TRUE ~ NA_character_))



# Factor variables 
SAEH_final_2 <- SAEH_final_2 %>%
  mutate(
    region = fct_relevel(
      as.factor(region),
      "North",
      "Central",
      "Mexico City",
      "South",
      "Missing"),
    indigena_label = fct_relevel(
      as.factor(indigena_label),
      "Yes",
      "No",
      "No response",
      "Missing"),
    habla_lengua_label = fct_relevel(
      as.factor(habla_lengua_label),
      "Yes",
      "No",
      "No response",
      "Missing"),
    age_cat = fct_relevel(
      as.factor(age_cat),
      "under 15",
      "15-16",
      "17-19",
      "20-24",
      "25-29",
      "30-34",
      "35-39",
      "40+",
      "Missing"),
    insurance_grouped = fct_relevel(
      as.factor(insurance_grouped),
      "Uninsured",
      "IMSS",
      "Gratuidad",
      "Public/State programs",
      "Private insurance",
      "ISSFAM/PEMEX/SEDENA/SEMAR/ISSSTE",
      "Other",
      "Unknown",
      "Missing"),
    relationship_grouped= fct_relevel(
      as.factor(relationship_grouped),
      "Single",
      "Married/Partnership",
      "Widowed/Divorced/Separated",
      "Missing"),
    adol_fert = fct_relevel(
      as.factor(adol_fert),
      "Low",
      "High",
      "Missing"),
    hosp_care = fct_relevel(
      as.factor(hosp_care),
      "Primary level",
      "Secondary level",
      "Tertiary level",
      "Missing"),
    marg_group = fct_relevel(
      as.factor(marg_group),
      "Less marginalized",
      "More marginalized",
      "Missing"),
    edu_group = fct_relevel(
      as.factor(edu_group),
      "Less educated",
      "More educated",
      "Missing"),
    unit_strat = fct_relevel(
      as.factor(unit_strat),
      "Urban",
      "Rural",
      "Missing"),
    municipality_pop = fct_relevel(
      as.factor(municipality_pop),
      "<15 K",
      "15–99 K",
      "100 K+",
      "Missing"),
    hosp_care = fct_relevel(
      as.factor(hosp_care),
      "Primary level",
      "Secondary level",
      "Tertiary level",
      "Missing"))

# Add Table 1 Variables to Emily's Additional Data Set #2 ----------------------
SAEH_final_3 <- readRDS("SAEH_final_3.rds")

# remove records whose age wasn't documented as years
SAEH_final_3 <- SAEH_final_3 %>% filter(age_unit == "years")

SAEH_final_3 <- SAEH_final_3 %>%
  mutate(
    # State region categorization sources: https://www.mdpi.com/1660-4601/16/3/407?utm_source=researchgate.net&medium=article#
    #                                      https://www.gob.mx/cms/uploads/attachment/file/209093/ENSANUT.pdf
    region = case_when(
      state %in% c("Baja California", "Baja California Sur", "Sonora", "Chihuahua", 
                   "Coahuila de Zaragoza", "Nuevo León", "Tamaulipas") ~ "North",
      state %in% c("Sinaloa", "México", "Jalisco", "Colima", "Nayarit", 
                   "Michoacán de Ocampo", "Morelos", "Aguascalientes",
                   "Zacatecas", "Guanajuato", "Durango", "San Luis Potosí", 
                   "Querétaro de Arteaga") ~ "Central",
      state == "Ciudad de México" ~ "Mexico City",
      state %in% c("Oaxaca", "Chiapas", "Tabasco", "Campeche", "Yucatán", 
                   "Quintana Roo", "Guerrero", "Puebla", "Tlaxcala", "Hidalgo", 
                   "Veracruz de Ignacio de la Llave") ~ "South",
      is.na(state) | state %in% c("Not Specified", "No response", "Not Applicable") ~ "Missing",
      state %in% c("USA", "Rest of Latin America", "Other Latin American Country", 
                   "Other Country", "Other Continent") ~ "Missing",
      TRUE ~ "Missing"),
    
    # Speaks Indigenous Language
    habla_lengua_label = case_when(
      habla_lengua == 2 ~ "No",
      habla_lengua == 1 ~ "Yes",
      habla_lengua == 3 ~ "No response",
      habla_lengua == 4 ~ "No response",
      TRUE ~ "Missing"),
    
    # Indigenous Identity
    indigena_label = case_when(
      indigena == 1 ~ "Yes",
      indigena == 2 ~ "No",
      indigena == 9 ~ "No response",
      TRUE ~ "Missing"),
    
    # Age category
    age_cat = case_when(
      edad < 15 ~ "under 15",
      edad >= 15 & edad <= 16 ~ "15-16",
      edad >= 17 & edad <= 19 ~ "17-19",
      edad >= 20 & edad <= 24 ~ "20-24",
      edad >= 25 & edad <= 29 ~ "25-29",
      edad >= 30 & edad <= 34 ~ "30-34",
      edad >= 35 & edad <= 39 ~ "35-39",
      edad >= 40 ~ "40+",
      TRUE ~ "Missing"),
    
    
    # Relationship status
    relationship_grouped = case_when(
      relationship_status == "Single" ~ "Single",
      relationship_status %in% c("Married", "Domestic partnership") ~ "Married/Partnership",
      relationship_status %in% c("Widowed", "Divorced", "Separated") ~ "Widowed/Divorced/Separated",
      is.na(relationship_status) | relationship_status %in% c("Unknown", "No response", "Not specified", "Not applicable", "Missing") ~ "Missing",
      TRUE ~ relationship_status),
    
    # Parity category
    parity_cat = case_when(
      is.na(parity) ~ "Missing",
      parity == 0 ~ "0",
      parity == 1 ~ "1",
      parity == 2 ~ "2",
      parity >= 3 ~ "3+",
      TRUE ~ "Missing"),
    
    # Insurance combination
    insurance_grouped = case_when(
      insurance %in% c("Public insurance", "INSABI", "IMSS BIENESTAR", "OPD IMSS BIENESTAR", "Prospera", "State government", "ISSFAM", "PEMEX", "SEDENA", "SEMAR", "ISSSTE", "Gratuidad") ~ "Public/State programs",
      insurance %in% c("No response", "Not specified", "Not Specified", "Missing") | is.na(insurance) ~ "No response/Not specified/Missing",
      TRUE ~ insurance),
    
    # Hospital care level
    hosp_care = case_when(
      care_level == "PRIMER NIVEL" ~ "Primary level",
      care_level == "SEGUNDO NIVEL" ~ "Secondary level",
      care_level == "TERCER NIVEL" ~ "Tertiary level",
      care_level == "NO APLICA" ~ "Missing",
      TRUE ~ NA_character_),
    
    # Municipality marginalization
    marg_quintile = case_when(
      grade_marg == "Muy bajo" ~ 1,
      grade_marg == "Bajo" ~ 2,
      grade_marg == "Medio" ~ 3,
      grade_marg == "Alto" ~ 4,
      grade_marg == "Muy alto" ~ 5,
      TRUE ~ NA),
    marg_group = case_when(
      marg_quintile %in% c(1,2) ~ "Less marginalized",
      marg_quintile %in% c(3,4,5) ~ "More marginalized",
      TRUE ~ NA_character_),
    
    # Municipality population
    municipality_pop = case_when(
      is.na(total_population) ~ "Missing",
      total_population < 15000 ~ "<15 K",
      total_population >= 15000 & total_population < 100000 ~ "15–99 K",
      total_population >= 100000 ~ "100 K+",
      TRUE ~ NA_character_),
    
    # Municipality education
    edu_group = case_when(
      pop_educ < 40 ~ "More educated",
      pop_educ >= 40 ~ "Less educated",
      TRUE ~ NA_character_),
    
    # Municipality adolescent fertility rate
    adol_fert = case_when(
      is.na(adol_fertrate) ~ "Missing",
      as.numeric(adol_fertrate) < 70.5 ~ "Low",
      as.numeric(adol_fertrate) >= 70.5 ~ "High",
      TRUE ~ NA_character_),
    
    # Municipality unit stratum
    unit_strat = case_when(
      is.na(unit_stratum) ~ "Missing",
      unit_stratum == "URBANO" ~ "Urban",
      unit_stratum == "RURAL" ~ "Rural",
      TRUE ~ NA_character_))


# Factor variables 
SAEH_final_3 <- SAEH_final_3 %>%
  mutate(
    region = fct_relevel(
      as.factor(region),
      "North",
      "Central",
      "Mexico City",
      "South",
      "Missing"),
    indigena_label = fct_relevel(
      as.factor(indigena_label),
      "Yes",
      "No",
      "No response",
      "Missing"),
    habla_lengua_label = fct_relevel(
      as.factor(habla_lengua_label),
      "Yes",
      "No",
      "No response",
      "Missing"),
    age_cat = fct_relevel(
      as.factor(age_cat),
      "under 15",
      "15-16",
      "17-19",
      "20-24",
      "25-29",
      "30-34",
      "35-39",
      "40+",
      "Missing"),
    insurance_grouped = fct_relevel(
      as.factor(insurance_grouped),
      "Uninsured",
      "IMSS",
      "Gratuidad",
      "Public/State programs",
      "Private insurance",
      "ISSFAM/PEMEX/SEDENA/SEMAR/ISSSTE",
      "Other",
      "Unknown",
      "Missing"),
    relationship_grouped= fct_relevel(
      as.factor(relationship_grouped),
      "Single",
      "Married/Partnership",
      "Widowed/Divorced/Separated",
      "Missing"),
    adol_fert = fct_relevel(
      as.factor(adol_fert),
      "Low",
      "High",
      "Missing"),
    hosp_care = fct_relevel(
      as.factor(hosp_care),
      "Primary level",
      "Secondary level",
      "Tertiary level",
      "Missing"),
    marg_group = fct_relevel(
      as.factor(marg_group),
      "Less marginalized",
      "More marginalized",
      "Missing"),
    edu_group = fct_relevel(
      as.factor(edu_group),
      "Less educated",
      "More educated",
      "Missing"),
    unit_strat = fct_relevel(
      as.factor(unit_strat),
      "Urban",
      "Rural",
      "Missing"),
    municipality_pop = fct_relevel(
      as.factor(municipality_pop),
      "<15 K",
      "15–99 K",
      "100 K+",
      "Missing"),
    hosp_care = fct_relevel(
      as.factor(hosp_care),
      "Primary level",
      "Secondary level",
      "Tertiary level",
      "Missing"))


# SAVE FINAL DATA SETS ---------------------------------------------------------

#### Save as rds
saveRDS(SAEH_clean, "SAEH_dataset.rds")
saveRDS(SAEH_final_2, "SAEH_dataset2.rds")

### Save this data set for Emily to use
write_dta(SAEH_clean, "SAEH_dataset.dta")
names(SAEH_final_2) <- gsub("\\.", "_", names(SAEH_final_2))
write_dta(SAEH_final_2, "SAEH_dataset2.dta")

### Save this data set for Emily to use (only dropping other countries)
saveRDS(SAEH_final_3, "SAEH_dataset3.rds")
names(SAEH_final_3) <- gsub("\\.", "_", names(SAEH_final_3))
write_dta(SAEH_final_3, "SAEH_dataset3.dta")
