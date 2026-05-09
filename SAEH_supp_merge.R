##---------------------------------------------------------------------------------------------##
## File name: SAEH Merge of Supplemental Data Sets                                             ##  
## Programmer: Kelsey MacKenzie                                                                ##
## Date: 30-NOV-2025                                                                           ##
## Last modified: 30-NOV-2025                                                                  ##
## Purpose: Add supplemental variables for facility, municipality, and state level data sets   ##
## PI: Emily Boniface                                                                          ##
##---------------------------------------------------------------------------------------------##

#### SETUP ---------------------------------------------------------------------

##### Load packages  ----
library(writexl)
library(dplyr)
library(ggplot2)
library(janitor)
library(stringr)
library(tidyverse)
library(haven)
library(here)
library(readxl)
library(openxlsx)

#### Import Data ---------------------------------------------------------------
final_saeh <- readRDS("SAEHdata_2018_2024.rds")

# Merge 1 ----------------------------------------------------------------------

#### Import facility level data
facility_db <- read.xlsx("ESTABLECIMIENTO_SALUD_202509.xlsx", sheet = 1)

# Select and rename columns in English
municio <- facility_db %>%
  select(
    clues = "CLUES",
    state_code = "CLAVE.DE.LA.ENTIDAD",
    mun_code = "CLAVE.DEL.MUNICIPIO",
    admin_unit_code = "CLAVE.DE.LA.INS.ADM",
    admin_unit_name = "NOMBRE.DE.LA.INS.ADM",
    unit_stratum = "ESTRATO.UNIDAD",
    care_level = "NIVEL.ATENCION",
    latitude = "LATITUD",
    longitude = "LONGITUD")

#### Create new variable ----
municio <- municio %>%
  mutate(
    munic_id = paste0(state_code, mun_code))

# Clues in municio not in final_saeh 
clues_only_in_municio <- setdiff(municio$clues, final_saeh$clues)
length(clues_only_in_municio)

# Clues in final_saeh not in municio
clues_only_in_final_saeh <- setdiff(final_saeh$clues, municio$clues)
length(clues_only_in_final_saeh)


#### Merge using CLUES variable
merged_data <- inner_join(final_saeh, municio, by = "clues")


# Merge 2 ----------------------------------------------------------------------

#### Import municipality level data
municio_pop <- read_csv("inafed_bd_1760674084.csv",
                        skip = 4,
                        quote = "\"",
                        col_types = cols())

### Filter necessary variables
municipality_pop <- municio_pop %>%
  select(
    munic_id = cve_inegi,
    total_population = total)

# Are any of them empty or not 5 number codes?
municipality_pop %>%
  filter(str_length(munic_id) != 5) %>%
  distinct(munic_id)

# Drop that one
municipality_pop <- municipality_pop %>%
  filter(munic_id != "Fuente:")


# Unmatched IDs in municipality_pop but not in final_saeh
munic_id_only_in_municipality_pop <- setdiff(municipality_pop$munic_id, final_saeh$munic_code)
length(munic_id_only_in_municipality_pop) # 53 not in final_saeh
unique(munic_id_only_in_municipality_pop)

# Unmatched IDs in final_saeh but not in municipality_pop
munic_id_only_in_final_saeh <- setdiff(final_saeh$munic_code, municipality_pop$munic_id)
length(munic_id_only_in_final_saeh) #48 not in municipality_pop
unique(munic_id_only_in_final_saeh)


# At the top of Merge 2, after creating merged_data_A, add:
merged_data_A <- merged_data %>% mutate(munic_merge = ifelse(munic_code == "04013", "04001", munic_code))

#### Merge using munic_id variable
merged_data_A <- merged_data_A %>% left_join(municipality_pop, by = c("munic_merge" = "munic_id"))


# Merge 3 - CONAPO marginalization + Calkini fix -------------------------------
municio_marg <- read_excel("IMM_2020.xlsx", sheet = "IMM_2020") %>% clean_names()

### Filter necessary variables
municio_margin <- municio_marg %>%
  select(
    munic_id    = cve_mun,
    grade_marg     = gm_2020,
    grade_marg_cont = im_2020)

# Merge marginalization
merged_data_B <- merged_data_A %>%
  left_join(municio_margin, by = c("munic_code" = "munic_id"))

# Verify 04013 is the only unmatched municipality
# Define exclusion codes (same as your cleaning script)
missing_state_codes <- c("88", "99", "00")
other_country_codes <- c("33", "34", "35", "37", "38", "39")

merged_data_B %>%
  filter(is.na(grade_marg)) %>%
  filter(!state_code %in% missing_state_codes) %>%
  filter(!state_code %in% other_country_codes) %>%
  filter(!grepl("997$|998$|999$", munic_code)) %>%
  count(munic_code, state_code) %>%
  arrange(desc(n))

# Fix: recode 04013 → 04001 (Calkini) for ALL municipality-level merges
# This ensures ALL downstream merges (pop, indigenous, education, fertility)
# also pick up Calkini values for these 25 records
merged_data_B <- merged_data_B %>%
  mutate(
    munic_merge = ifelse(munic_code == "04013", "04001", munic_code))

# Get Calkini (04001) values from CONAPO
calkini_marg <- municio_margin %>% filter(munic_id == "04001")

# Add munic_merge column — use 04001 for 04013, keep original for everything else
merged_data_B <- merged_data_B %>%
  mutate(munic_merge = ifelse(munic_code == "04013", "04001", munic_code)) %>%
  mutate(
    grade_marg      = ifelse(munic_code == "04013", calkini_marg$grade_marg,      grade_marg),
    grade_marg_cont = ifelse(munic_code == "04013", calkini_marg$grade_marg_cont, grade_marg_cont))

# Verify — should be 0
cat("Missing grade_marg after fix:", sum(is.na(merged_data_B$grade_marg)), "\n")




# Merge 4 ----------------------------------------------------------------------
# Source: https://www.inpi.gob.mx/indicadores2020/

municio_indig <- read_excel("2-poblacion-indigena-autoadscrita-por-municipio-muestra-censal-2020-2-1-.xlsx",
                            skip = 3) %>% clean_names()

# Pad with zero's
municio_indig <- municio_indig %>%
  mutate(
    munic_code = str_pad(as.character(clave_de_municipio), 
                                 width = 5, 
                                 pad = "0"))
table(municio_indig$munic_code, useNA = "ifany")

### Filter necessary variables
municio_ind <- municio_indig %>%
  filter(
    tipo == "Porcentaje",
    estimador == "Estimación") %>%
  mutate(
    tipo = case_when(
      tipo == "Porcentaje" ~ "Percentage",
      TRUE ~ NA),
    estimador = case_when(
      estimador == "Estimación" ~ "Estimate",
      TRUE ~ NA)) %>%
  rename(
    indigenous = se_considera_indigena,
    not_indigenous = no_se_considera_indigena,
    missing_indig = no_especificado) %>%
  select(-no_indicador, -entidad, -cobertura, -poblacion_de_3_anos_y_mas)



#### Merge using munic_id variable
merged_data_C <- merged_data_B %>% left_join(municio_ind, by = c("munic_merge" = "munic_code"))

# Merge 5 ----------------------------------------------------------------------
municio_edu <- read_excel("IRS_ent_mun_2000_2020/IRS_entidades_mpios_2020.xlsx",
                          sheet = "Municipios",
                          skip = 2) %>% clean_names()

### Filter necessary variables
municio_educ <- municio_edu %>%
  slice(-c(1,2)) %>%
  select(
    munic_id = clave_municipio,
    pop_educ = x8)



# Extract unmatched munic_id in municio_educ but not in merged_data$munic_code
munic_id_only_in_educ <- setdiff(municio_educ$munic_id, merged_data$munic_code)
length(munic_id_only_in_educ) # 45
unique(munic_id_only_in_educ) 

# Extract unmatched munic_code in merged_data but not in municio_educ$munic_id
munic_id_only_in_merged_data <- setdiff(merged_data$munic_code, municio_educ$munic_id)
length(munic_id_only_in_merged_data) # 48
unique(munic_id_only_in_merged_data)




#### Merge using munic_id variable
merged_data_D <- merged_data_C %>% left_join(municio_educ, by = c("munic_merge" = "munic_id"))

# Merge 6 ----------------------------------------------------------------------
municio_fert <- read_csv("tf_adolescente_municipal_2020.csv")

### Filter necessary variables
municio_fertil <- municio_fert %>%
  select(
    munic_id      = Clave_del_municipio,
    adol_fertrate = Tasa_de_fecundidad_adolescente_TFA) %>%
  mutate(munic_id = str_pad(as.character(munic_id), width = 5, pad = "0"))


# IDs in municio_fertil but not in merged_data
munic_id_only_in_fertil <- setdiff(municio_fertil$munic_id, merged_data$munic_code)
length(munic_id_only_in_fertil) # 366
unique(munic_id_only_in_fertil)

# IDs in merged_data but not in municio_fertil
munic_id_only_in_merged <- setdiff(merged_data$munic_code, municio_fertil$munic_id)
length(munic_id_only_in_merged) # 337
unique(munic_id_only_in_merged)

municio_fertil %>%
  filter(munic_id == "0") # can't drop because it has a rate

#### Merge using munic_id variable
merged_data_E <- merged_data_D %>% left_join(municio_fertil, by = c("munic_merge" = "munic_id"))

# Merge 7 ----------------------------------------------------------------------
state_repo <- read_excel("ConDem50a19_ProyPob20a70/0_Pob_Mitad_1950_2070.xlsx") %>%
   clean_names()
  
### Filter necessary variables
state_reprod <- state_repo %>%
  select(
    state = entidad,
    age = edad,
    sex = sexo,
    pop = poblacion)

### Sum all the populations for females, ages 15-49, by state
state_female_15_49 <- state_reprod %>%
  filter(sex == "Mujeres") %>%                         # filter only females
  filter(age >= 15 & age <= 49) %>%                    # filter ages 15 to 49
  group_by(state) %>%                                  # group by state variable
  summarise(total_female_pop_15_49 = sum(pop, na.rm = TRUE)) %>%
  ungroup()

### Sum all the populations for females, ages 15-45, by state
state_female_15_45 <- state_reprod %>%
  filter(sex == "Mujeres") %>%                         # filter only females
  filter(age >= 15 & age <= 45) %>%                    # filter ages 15 to 45
  group_by(state) %>%                                  # group by state variable
  summarise(total_female_pop_15_45 = sum(pop, na.rm = TRUE)) %>%
  ungroup()


# Combine the two population summaries into one dataframe by state
pob_mitad <- state_female_15_49 %>%
  left_join(state_female_15_45, by = "state")


#### Merge using state variable
merged_data_F <- merged_data_E %>% left_join(pob_mitad, by = "state")

  
# SAVE FINAL DATA SET ----------------------------------------------------------

#### Save as rds
saveRDS(merged_data_F, "SAEHdata_merged.rds")

#### Save as .dta for Emily
#names(merged_data_F) <- gsub("\\.", "_", names(merged_data_F))
#write_dta(merged_data_F, "SAEHdata_merged.dta")
