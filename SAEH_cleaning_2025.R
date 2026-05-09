##----------------------------------------------------------------------------##
## File name: SAEH Preliminary Data Cleaning                                  ##  
## Programmer: Kelsey MacKenzie                                               ##
## Date: 11-JUL-2025                                                          ##
## Last modified: 15-AUG-2025                                                 ##
## Purpose: Adapt Laura's/Biani's/Emily's code for creating SAEH data set     ##
## PI: Emily Boniface                                                         ##
##----------------------------------------------------------------------------##

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

# --- 2018 ---------------------------------------------------------------------

#### EGRESO --------------------------------------------------------------------

###### Import the expenses data  ----
expenses_2018 <- read.csv("DATA/ssa_egresos_2018/EGRESO_2018.csv")

###### Make variables lowercase  ----
expenses_2018 <- expenses_2018 %>%
  clean_names()

#### 1: Drop everything except ICD-10 codes starting w/ O02-O08 and code Z303  ----
expenses_2018 <- expenses_2018 %>%
  rename(icd_code = afecprin) %>%
  filter(substr(icd_code, 1, 3) %in% c("O02", "O03", "O04", "O05", "O06", "O07", "O08") |
           icd_code == "Z303")

##### A. Table of frequencies of the primary ICD-10 codes  ----
table(expenses_2018$icd_code, useNA = "ifany")

###### B. ICD-10 sub code details: * indicates additional sub-level of coding #####

# O02: Other abnormal products of conception: O020, O021, O028*, O029
# O03: Spontaneous abortion: O030, O031, O032, O033*, O034, O035, O036, O037, O038*, O039
# O04: Complications following induced termination of pregnancy: O045, O046. O047, O048*
# O05: Other abortion: O050, O051, O052, O053, O054, O055, O056, O057, O058, O059
# O06: Unspecified abortion: O060, O061, O062, O063, O066, O067, O068, O069
# O07: Failed attempted termination of pregnancy: O070, O071, O072, O073*, O074
# O08: Complications following ectopic and molar pregnancy: O080, O081, O082, O083, O084, O085, O086, O087, O088*, O089
# Z303: Menstrual extraction

##### C. Quality check: Does n = 88962  ----
nrow(expenses_2018)

#### 2: Drop unnecessary variables - tuhpsiq, servhc, servhp, nacioen, servicioingre, servicio02, servicio03, servicioegre, infec, mp  ----
expenses_2018 <- expenses_2018 %>%
  select(-tuhpsiq, -servhc, -servhp, -nacioen, -servicioingre,
         -servicio02, -servicio03, -servicioegre, -infec, -mp)

colnames(expenses_2018)

#### 3: egreso, ingre - convert admission and discharge date variables ----
expenses_2018$egreso <- as.Date(expenses_2018$egreso)
expenses_2018$ingre  <- as.Date(expenses_2018$ingre)

table(expenses_2018$egreso, useNA = "ifany") %>% head(10)
table(expenses_2018$ingre, useNA = "ifany") %>% head(10)

#### 4: dias_esta - check outliers & negative values ----
summary(expenses_2018$dias_esta)

##### A. View negative values  ----
expenses_2018 %>%
  filter(dias_esta < 0) %>%
  select(id, ingre, egreso, dias_esta)

##### B. View extreme high values (> 30 days)  ----
expenses_2018 %>%
  filter(dias_esta > 30) %>%
  arrange(desc(dias_esta)) %>%
  select(id, ingre, egreso, dias_esta) %>%
  head(90)

##### C. Check the distribution of dias_esta ----
ggplot(expenses_2018, aes(x = dias_esta)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  scale_y_continuous(trans = "log10", labels = scales::comma) +
  labs(
    title = "Length of Stay (Including Outliers)",
    x = "dias_esta (Days)",
    y = "Log10 Frequency"
  ) +
  theme_minimal()

# Remove extreme outliers to zoom in
expenses_2018 %>%
  filter(dias_esta >= 0 & dias_esta <= 30) %>%
  ggplot(aes(x = dias_esta)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(title = "Length of Stay (0–30 Days)", x = "dias_esta", y = "Frequency") +
  theme_minimal()

#### 5: cveedad - check if anything is != 3 ----

##### A. Check frequency of all cveedad values  ----
table(expenses_2018$cveedad, useNA = "ifany")

##### B. Identify rows where cveedad != 3  ----
expenses_2018 %>%
  filter(cveedad != 3) %>%
  select(id, cveedad, edad) %>%
  head() # 1st row is a 2 days old, 2nd row is a 7 months old

##### C. Convert cveedad to factor with labeled levels and name age_unit
expenses_2018 <- expenses_2018 %>%
  mutate(age_unit = factor(cveedad,
                           levels = c(0, 1, 2, 3, 9),
                           labels = c("hours", "days", "months", "years", "No response")))

table(expenses_2018$age_unit, useNA = "ifany")

##### D. Create age groups for edad and rename to age
expenses_2018 <- expenses_2018 %>%
  mutate(
    age_group = case_when(
      edad < 10 ~ 1,
      edad >= 10 & edad <= 14 ~ 2,
      edad >= 15 & edad <= 19 ~ 3,
      edad >= 20 & edad <= 24 ~ 4,
      edad >= 25 & edad <= 29 ~ 5,
      edad >= 30 & edad <= 34 ~ 6,
      edad >= 35 & edad <= 39 ~ 7,
      edad >= 40 & edad <= 44 ~ 8,
      edad >= 45 & edad <= 49 ~ 9,
      edad >= 50 & edad <= 54 ~ 10,
      edad >= 55 ~ 11,
      TRUE ~ NA),
    age_group = factor(age_group, levels = 1:11,
                     labels = c("<10", "10–14", "15–19", "20–24", "25–29",
                       "30–34", "35–39", "40–44", "45–49", "50–54", "55+")))

table(expenses_2018$age_group, useNA = "ifany")

#### 6: sex - check if anything != 2 ----

##### A. Check frequency of all sex values  ----
table(expenses_2018$sexo, useNA = "ifany")

##### B. sex - convert and relabel ----
expenses_2018 <- expenses_2018 %>%
  mutate(
    sex = case_when(
      sexo == "1" ~ "Male",
      sexo == "2" ~ "Female",
      sexo == "9" ~ "Not specified",
      TRUE ~ NA),
    sex = factor(sex, 
                 levels = c("Male", "Female", "Not specified")))

table(expenses_2018$sex, useNA = "ifany")

#### 7: weight and height - replace 999 values with NA ----
expenses_2018 <- expenses_2018 %>%
  mutate(
    weight = na_if(peso, 999),
    height = na_if(talla, 999))

#### 8: insurance - rename and recode insurance types ----
expenses_2018 <- expenses_2018 %>%
  mutate(
    insurance = case_when(
      derhab == "0"  ~ "Uninsured",
      derhab == "1"  ~ "IMSS",
      derhab == "2"  ~ "ISSSTE",
      derhab == "3"  ~ "PEMEX",
      derhab == "4"  ~ "SEDENA",
      derhab == "5"  ~ "SEMAR",
      derhab == "6"  ~ "State government",
      derhab == "7"  ~ "Private insurance",
      derhab == "8"  ~ "Public insurance",
      derhab == "9"  ~ "Unknown",
      derhab == "G"  ~ "Gratuidad",
      derhab == "P"  ~ "Prospera",
      TRUE ~ NA),
    insurance = factor(insurance,
                       levels = c("Uninsured", "IMSS", "ISSSTE", "PEMEX", "SEDENA",
                                  "SEMAR", "State government", "Private insurance", 
                                  "Public insurance", "Unknown", "Gratuidad", "Prospera")))

table(expenses_2018$insurance, useNA = "ifany")
table(expenses_2018$derhab, useNA = "ifany")

#### 9: state - recode variable values and rename  ----

# Step A: Check for non-Mexico residences
expenses_2018 %>%
  filter(entidad %in% c("33", "34", "35")) %>%
  select(id, entidad) %>%
  head() 

# Step B: Recode state values to full names
expenses_2018 <- expenses_2018 %>%
  mutate(
    entidad_char = as.character(entidad),
    state = case_when(
      entidad_char == "1" ~ "Aguascalientes",
      entidad_char == "2" ~ "Baja California",
      entidad_char == "3" ~ "Baja California Sur",
      entidad_char == "4" ~ "Campeche",
      entidad_char == "5" ~ "Coahuila de Zaragoza",
      entidad_char == "6" ~ "Colima",
      entidad_char == "7" ~ "Chiapas",
      entidad_char == "8" ~ "Chihuahua",
      entidad_char == "9" ~ "Ciudad de México",
      entidad_char == "10" ~ "Durango",
      entidad_char == "11" ~ "Guanajuato",
      entidad_char == "12" ~ "Guerrero",
      entidad_char == "13" ~ "Hidalgo",
      entidad_char == "14" ~ "Jalisco",
      entidad_char == "15" ~ "México",
      entidad_char == "16" ~ "Michoacán de Ocampo",
      entidad_char == "17" ~ "Morelos",
      entidad_char == "18" ~ "Nayarit",
      entidad_char == "19" ~ "Nuevo León",
      entidad_char == "20" ~ "Oaxaca",
      entidad_char == "21" ~ "Puebla",
      entidad_char == "22" ~ "Querétaro de Arteaga",
      entidad_char == "23" ~ "Quintana Roo",
      entidad_char == "24" ~ "San Luis Potosí",
      entidad_char == "25" ~ "Sinaloa",
      entidad_char == "26" ~ "Sonora",
      entidad_char == "27" ~ "Tabasco",
      entidad_char == "28" ~ "Tamaulipas",
      entidad_char == "29" ~ "Tlaxcala",
      entidad_char == "30" ~ "Veracruz de Ignacio de la Llave",
      entidad_char == "31" ~ "Yucatán",
      entidad_char == "32" ~ "Zacatecas",
      entidad_char == "33" ~ "USA",
      entidad_char == "34" ~ "Other Latin American Country",
      entidad_char == "35" ~ "Other Country",
      entidad_char == "99" ~ "Not Specified",
      TRUE ~ NA),
    state = factor(state,
                   levels = c(
                     "Aguascalientes", "Baja California", "Baja California Sur", "Campeche",
                     "Coahuila de Zaragoza", "Colima", "Chiapas", "Chihuahua", "Ciudad de México",
                     "Durango", "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", "México",
                     "Michoacán de Ocampo", "Morelos", "Nayarit", "Nuevo León", "Oaxaca", "Puebla",
                     "Querétaro de Arteaga", "Quintana Roo", "San Luis Potosí", "Sinaloa", "Sonora",
                     "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz de Ignacio de la Llave",
                     "Yucatán", "Zacatecas", "USA", "Other Latin American Country", "Other Country",
                     "Not Specified"))) %>%
  select(-entidad_char)  # Remove helper variable

# check that code ran correctly
table(expenses_2018$state, useNA = "ifany")
table(expenses_2018$entidad, useNA = "ifany")

#### 10: munic - convert to factor and pad with 0's ----
expenses_2018 <- expenses_2018 %>%
  mutate(
    munic = str_pad(as.character(munic), width = 3, pad = "0"),
    munic = factor(munic))

table(expenses_2018$munic, useNA = "ifany")

#### 11: entidad - convert to factor and pad with 0's ----
expenses_2018 <- expenses_2018 %>%
  mutate(
    entidad = str_pad(as.character(entidad), width = 2, pad = "0"),
    entidad = factor(entidad))

table(expenses_2018$entidad, useNA = "ifany")

#### 12: munic_code - making the variable ----
expenses_2018 <- expenses_2018 %>%
  mutate(
    munic_code = paste0(entidad, munic))

table(expenses_2018$munic_code, useNA = "ifany")

#### 13: loc - convert to factor and pad with 0's ----
expenses_2018 <- expenses_2018 %>%
  mutate(
    loc = str_pad(as.character(loc), width = 4, pad = "0"),
    loc = factor(loc))

table(expenses_2018$loc, useNA = "ifany")

#### 14: indigena - make a new binary variable ----
table(expenses_2018$indigena, useNA = "ifany")

# Step 1: Create the binary variable
expenses_2018 <- expenses_2018 %>%
  mutate(
    indigena.num = case_when(
      indigena == 1 ~ 1,
      indigena == 2 ~ 0,
      indigena %in% c(3, 4, 9) ~ 2))

## Check it's converted correctly
table(expenses_2018$indigena.num, useNA = "ifany")

# Step 2: Convert binary to factor with labels
expenses_2018 <- expenses_2018 %>%
  mutate(
    indigena.bin = factor(indigena.num, 
                          levels = c(0, 1, 2),
                          labels = c("No", "Yes", "No response")))

table(expenses_2018$indigena.bin, useNA = "ifany")

#### 15: speaks_indigen - create and relabel ----
table(expenses_2018$habla_lengua, useNA = "ifany")

# Step 1: Create numeric binary variable
expenses_2018 <- expenses_2018 %>%
  mutate(
    habla_lengua.num = case_when(
      habla_lengua == 1 ~ 1,
      habla_lengua == 2 ~ 0,
      habla_lengua %in% c(3, 4) ~ 2))

## Check it's converted correctly
table(expenses_2018$habla_lengua.num, useNA = "ifany")

# Step 2: Convert binary to factor with labels
expenses_2018 <- expenses_2018 %>%
  mutate(
    speaks_indigen = factor(habla_lengua.num,
                                levels = c(0, 1, 2),
                                labels = c("No", "Yes", "No response")))

table(expenses_2018$speaks_indigen, useNA = "ifany")

#### 16: lengua_indigena - convert and relabel ----
unique(expenses_2018$lengua_indigena) # values not in our range - "91", "92", "93", "96", and "NULL" are being lumped into NA in the following code

#### Drop lengua_indigena
expenses_2018 <- expenses_2018 %>%
  select(-lengua_indigena)

# expenses_2018 <- expenses_2018 %>%
#  mutate(
#    indigen_spoken = case_when(
#      is.na(lengua_indigena) ~ "Not applicable",
#      TRUE ~ as.character(lengua_indigena)),
#    indigen_spoken = factor(indigen_spoken,  # NOTE: using the just-created variable
#                            levels = c(as.character(1:67), "99", "Not applicable"),
#                            labels = c(
#                              "Aguacateco", "Amuzgo", "Amuzgo de Guerrero", 
#                              "Amuzgo de Oaxaca", "Cakchiquel", "Chatino", 
#                              "Chichimeca Jonaz", "Chocho", "Chol",
#                              "Chontal", "Chontal de Oaxaca", "Chontal de Tabasco", 
#                              "Chuj", "Cochimi", "Cora", "Cucapa", "Cuicateco", 
#                              "Guarijio", "Huasteco o Teenek", "Huave", "Huichol", 
#                              "Ixcateco", "Ixil", "Jacalteco", "Kanjobal", "Kekchi", 
#                              "Kikapu", "Kiliwa", "Kumiai", "Lacandon", 
#                              "Lenguas Chinantecas", "Lenguas Mixtecas", 
#                              "Lenguas Zapotecas", "Mame", "Matlalzinca", "Maya", 
#                              "Mayo", "Mazahua", "Mazateco", "Mixe", 
#                              "Motocintleco", "Nahuatl", "Ocuilteco", 
#                              "Otomi o Ñañu", "Paipai", "Pame", "Papabuco", 
#                              "Papago", "Pima", "Popoloca", "Popoluca", 
#                              "Purepecha o Tarasco", "Quiche", "Seri", 
#                              "Tarahumara o Raramuri", "Tepehua", "Tepehuano", 
#                              "Tepehuano de Chihuahua", "Tepehuano de Durango", 
#                              "Tlapaneco", "Tojolabal", "Totonaca", "Triqui", 
#                              "Tzeltal", "Tzptzil", "Yaqui", "Zoque", 
#                              "Not specified", "Not applicable")))

#### 17: habla_esp - convert and relabel ----
table(expenses_2018$habla_esp, useNA = "ifany")

# Step 1: Create numeric binary variable
expenses_2018 <- expenses_2018 %>%
  mutate(
    habla_esp.num = case_when(
      habla_esp == 1 ~ 1,
      habla_esp == 2 ~ 0,
      habla_esp %in% c(3, 4, 9) ~ 2))

## Check it's converted correctly
table(expenses_2018$habla_esp.num, useNA = "ifany")

# Step 2: Convert binary to factor with labels
expenses_2018 <- expenses_2018 %>%
  mutate(
    habla_esp.bin = factor(habla_esp.num,
                            levels = c(0, 1, 2),
                            labels = c("No", "Yes", "No response")))

table(expenses_2018$habla_esp.bin, useNA = "ifany")

#### 18: admiss_type - convert and relabel ----
expenses_2018 <- expenses_2018 %>%
  mutate(
    admiss_type = factor(tipserv,
                       levels = c("1", "2"),
                       labels = c("Normal", "Short stay")))

table(expenses_2018$admiss_type, useNA = "ifany")
table(expenses_2018$tipserv, useNA = "ifany")

#### 19: origin - convert and relabel ----
expenses_2018 <- expenses_2018 %>%
  mutate(
    origin = factor(proced,
                    levels = c("1", "2", "3", "4", "6", "9"),
                    labels = c("Outpatient", "Emergency", "Referred", "Other", "Pathological nursery", "Not specified")))

table(expenses_2018$origin, useNA = "ifany")
table(expenses_2018$proced, useNA = "ifany")

#### 20: discharge_res - convert and relabel ----
expenses_2018 <- expenses_2018 %>%
  mutate(
    discharge_res = factor(motegre,
                           levels = c("1", "2", "3", "4", "5", "6", "7", "9"),
                           labels = c(
                             "Recovery",
                             "Improvement",
                             "Voluntary discharge",
                             "Transfer",
                             "Death",
                             "Escape",
                             "Other",
                             "Not specified")))

table(expenses_2018$discharge_res, useNA = "ifany")
table(expenses_2018$motegre, useNA = "ifany")

#### 21: month_discharge - convert and relabel ----
expenses_2018 <- expenses_2018 %>%
  mutate(
    month_discharge = factor(mes_estadistico,
                             levels = as.character(1:12),
                             labels = c(
                               "January", "February", "March", "April", "May", "June",
                               "July", "August", "September", "October", "November", "December")))

table(expenses_2018$month_discharge, useNA = "ifany")
table(expenses_2018$mes_estadistico, useNA = "ifany")

#### 22: relationship_status - convert and relabel ----
expenses_2018 <- expenses_2018 %>%
  mutate(
    relationship_status = factor(estado_conyugal_key,
                                 levels = c("1", "5", "3", "2", "4", "6", "8", "9", "0"),
                                 labels = c(
                                   "Single",
                                   "Married",
                                   "Divorced",
                                   "Widowed",
                                   "Domestic partnership",
                                   "Separated",
                                   "Not applicable",
                                   "Unknown",
                                   "Not specified")))

table(expenses_2018$relationship_status, useNA = "ifany")
table(expenses_2018$estado_conyugal_key, useNA = "ifany")

#### 23: admiss_readmit - convert and relabel ----
expenses_2018 <- expenses_2018 %>%
  mutate(
    admiss_readmit = factor(vez,
                            levels = c("1", "2", "9"),
                            labels = c(
                              "First time",
                              "Subsequent",
                              "Not specified")))

table(expenses_2018$admiss_readmit, useNA = "ifany")
table(expenses_2018$vez, useNA = "ifany")

#### 24: Check how many rows have mismatched diag_ini and icd_code
mismatches <- expenses_2018 %>%
  filter(diag_ini != icd_code)

(n_mismatches <- nrow(mismatches)) # 1831 so we are keeping diag_ini



#### AFECCIONES ----------------------------------------------------------------

###### Import the conditions data  ----
conditions_2018 <- read.csv("DATA/ssa_egresos_2018/AFECCIONES_2018.csv")

###### Make variables lowercase  ----
conditions_2018 <- conditions_2018 %>%
  clean_names()

###### Reshape to wide format based on numafec  ----
conditions_2018_wide <- conditions_2018 %>%
  select(id, afec) %>%
  group_by(id) %>%
  mutate(condition_n = row_number()) %>%       # Count afec per id
  ungroup() %>%
  pivot_wider(
    names_from = condition_n,
    names_prefix = "afec",
    values_from = afec)

##### Verify pivot ran correctly ----
sum(duplicated(conditions_2018_wide$id))



#### PROCEDIMIENTOS ------------------------------------------------------------
    
###### Import the procedures data  ----
procedures_2018 <- read.csv("DATA/ssa_egresos_2018/PROCEDIMIENTOS_2018.csv")

###### Make variables lowercase  ----
procedures_2018 <- procedures_2018 %>%
  clean_names()

##### Drop unnecessary variables - tipo, anest, quirof, qh, qm  ----
procedures_2018 <- procedures_2018  %>%
  select(-tipo, -anest, -quirof, -qh, -qm)

##### Keep only first 10 procedures per id and pivot to wide format ----
procedures_2018_wide <- procedures_2018 %>%
  group_by(id) %>%
  arrange(id) %>%
  slice_head(n = 10) %>%
  mutate(procedure_n = row_number()) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = id,
    names_from = procedure_n,
    names_prefix = "promed",
    values_from = promed)


##### Create procedure variables ----

# Step 1: Collapse all promed variables into a single string column per row
procedures_2018_wide <- procedures_2018_wide %>%
  mutate(promed_concat = paste(promed1, promed2, promed3, promed4, promed5,
                               promed6, promed7, promed8, promed9, promed10, sep = " "))

# Step 2: Use str_detect() to flag presence of each procedure code
procedures_2018_wide <- procedures_2018_wide %>%
  mutate(
    proc_dc_ab     = as.integer(str_detect(promed_concat, "\\b6901\\b")),
    proc_dc_post   = as.integer(str_detect(promed_concat, "\\b6902\\b")),
    proc_asp_ab    = as.integer(str_detect(promed_concat, "\\b6951\\b")),
    proc_asp_post  = as.integer(str_detect(promed_concat, "\\b6952\\b")),
    proc_miso_ab   = as.integer(str_detect(promed_concat, "\\b75A1\\b")),
    proc_miso_post = as.integer(str_detect(promed_concat, "\\b75A2\\b")),
    proc_oth_ab    = as.integer(str_detect(promed_concat, "\\b75A3\\b")),
    proc_oth_post  = as.integer(str_detect(promed_concat, "\\b75A4\\b")))


#### OBSTET --------------------------------------------------------------------

###### Import the obstet data  ----
obstet_2018 <- read.csv("DATA/ssa_egresos_2018/OBSTET_2018.csv")

###### Make variables lowercase  ----
obstet_2018 <- obstet_2018 %>%
  clean_names()

##### Drop unnecessary variables - ttipaten, producto, tipnaci, cesareas  ----
obstet_2018 <- obstet_2018  %>%
  select(-tipaten, -producto, -tipnaci, -cesareas, -clues)

###### Rename variables  ----
table(obstet_2018$gestas, useNA = "ifany")

obstet_2018 <- obstet_2018  %>%
  rename(
    gravida = gestas,
    parity = partos) 

table(obstet_2018$gravida, useNA = "ifany")

##### Convert and relabel - hayprod ----
table(obstet_2018$hayprod, useNA = "ifany")

# Step 1: Create numeric binary variable
obstet_2018 <- obstet_2018  %>%
  mutate(
    hayprod.num = case_when(
      hayprod == 1 ~ 1,
      hayprod == 2 ~ 0,
      hayprod %in% c(9) ~ 2))

## Check it's converted correctly
table(obstet_2018$hayprod.num, useNA = "ifany")

# Step 2: Convert binary to factor with labels
obstet_2018 <- obstet_2018  %>%
  mutate(
    hayprod.bin = factor(hayprod.num,
                           levels = c(0, 1, 2),
                           labels = c("No", "Yes", "No response")))

table(obstet_2018$hayprod.bin, useNA = "ifany")

###### Factor and label planfam  ----
obstet_2018 <- obstet_2018 %>%
  mutate(    
    contracep = case_when(
    planfam %in% c(2, 3) ~ 2,  # Injectable
    TRUE ~ planfam) %>%
      factor(levels = c(0, 1, 2, 4, 5, 6, 7, 8, 9, 10, 11, 99, 88),
             labels = c("None", 
                        "Pills", 
                        "Injectable", 
                        "Implant", 
                        "Copper IUD",
                        "Female condom", 
                        "Male condom", 
                        "Medicated IUD", 
                        "Patch",
                        "Permanent", 
                        "Other", 
                        "Unspecified", 
                        "Not applicable")))

table(obstet_2018$contracep, useNA = "ifany")
table(obstet_2018$planfam, useNA = "ifany")

###### Change 99's to NA in gestac  ----
table(obstet_2018$gestac, useNA = "ifany")

obstet_2018 <- obstet_2018 %>%
  mutate(gestat = na_if(gestac, 99))

table(obstet_2018$gestat, useNA = "ifany")


#### PRODUCTOS -----------------------------------------------------------------

###### Import the products data  ----
products_2018 <- read.csv("DATA/ssa_egresos_2018/PRODUCTOS_2018.csv")

###### Make variables lowercase  ----
products_2018 <- products_2018 %>%
  clean_names()

##### Drop unnecessary variables - condegre, naviapag, navirean, lactancia_exclusiva  ----
products_2018 <- products_2018 %>%
  select(-condegre, -naviapag, -navirean, -lactancia_exclusiva)

##### Pivot wider so each product becomes a column  ----
products_2018_wide <- products_2018 %>%
  filter(!is.na(condnac)) %>%
  mutate(numproducto = paste0("product_", numproducto)) %>%
  pivot_wider(
    id_cols = id,
    names_from = numproducto,
    values_from = condnac)

##### Convert to factors with labels (Fetal Death/Live Birth/Not Specified)  ----
products_2018_wide <- products_2018_wide %>%
  mutate(across(starts_with("product_"),
                ~ factor(.x,
                         levels = c(1, 2, 9),
                         labels = c("Fetal Death", "Live Birth", "Not Specified"))))




#### Merge with EGRESOS and save the cleaned data ------------------------------

# Convert id to character in all data sets
expenses_2018 <- expenses_2018 %>% mutate(id = as.character(id))
conditions_2018_wide <- conditions_2018_wide %>% mutate(id = as.character(id))
procedures_2018_wide <- procedures_2018_wide %>% mutate(id = as.character(id))
obstet_2018 <- obstet_2018 %>% mutate(id = as.character(id))
products_2018_wide <- products_2018_wide %>% mutate(id = as.character(id))

# Merge with EGRESOS 
final_2018 <- expenses_2018 %>%
  left_join(conditions_2018_wide, by = "id") %>%
  left_join(procedures_2018_wide, by = "id") %>%
  left_join(obstet_2018, by = "id") %>%
  left_join(products_2018_wide, by = "id")

# Create year variable to differentiate years for final join
final_2018 <- final_2018 %>%
  mutate(year = 2018)


# Save the final data set
#saveRDS(final_2018, "SAEH_FinalData_2018.rds")

# Save as .dta for Emily
#names(final_2018) <- gsub("\\.", "_", names(final_2018))
#write_dta(final_2018, "final_2018.dta")




# --- 2019 ---------------------------------------------------------------------



#### EGRESO --------------------------------------------------------------------

###### Import the expenses data  ----
expenses_2019 <- read.csv("DATA/ssa_egresos_2019/EGRESOS_2019.txt")

###### Make variables lowercase  ----
expenses_2019 <- expenses_2019 %>%
  clean_names()

#### 1: Drop everything except ICD-10 codes starting w/ O02-O08 and code Z303  ----
expenses_2019 <- expenses_2019 %>%
  rename(icd_code = afecprin) %>%
  filter(substr(icd_code, 1, 3) %in% c("O02", "O03", "O04", "O05", "O06", "O07", "O08") |
           icd_code == "Z303")

##### A. Table of frequencies of the primary ICD-10 codes  ----
table(expenses_2019$icd_code, useNA = "ifany")

nrow(expenses_2019)

#### 2: Drop unnecessary variables - tuhpsiq, servhc, servhp, nacioen, servicioingre, servicio02, servicio03, servicioegre, infec, mp, ttipaten, producto, tipnaci, cesareas  ----
expenses_2019 <- expenses_2019 %>%
  select(-tuhpsiq, -servhc, -servhp, -nacioen, -servicioingre,
         -servicio02, -servicio03, -servicioegre, -infec, -mp,
         -tipaten, -producto, -tipnaci, -cesareas)

#### 3: egreso, ingre - convert admission and discharge date variables ----
expenses_2019$egreso <- as.Date(expenses_2019$egreso)
expenses_2019$ingre  <- as.Date(expenses_2019$ingre)

#### 4: dias_esta - check outliers & negative values ----
summary(expenses_2019$dias_esta)

##### A. View negative values  ----
expenses_2019 %>%
  filter(dias_esta < 0) %>%
  select(id, ingre, egreso, dias_esta)

##### B. View extreme high values (> 30 days)  ----
expenses_2019 %>%
  filter(dias_esta > 30) %>%
  arrange(desc(dias_esta)) %>%
  select(id, ingre, egreso, dias_esta) %>%
  head(90)

#### 5: cveedad - check if anything is != 3 ----

##### A. Check frequency of all cveedad values  ----
table(expenses_2019$cveedad, useNA = "ifany")

##### B. Identify rows where cveedad != 3  ----
expenses_2019 %>%
  filter(cveedad != 3) %>%
  select(id, cveedad, edad) %>%
  head() # 4 individuals with ages unknown?

##### C. Convert cveedad to factor with labeled levels and name age_unit
expenses_2019 <- expenses_2019 %>%
  mutate(age_unit = factor(cveedad,
                           levels = c(0, 1, 2, 3, 9),
                           labels = c("hours", "days", "months", "years", "No response")))

table(expenses_2019$age_unit, useNA = "ifany")

##### D. Create age groups for edad and rename to age
expenses_2019 <- expenses_2019 %>%
  mutate(
    age_group = case_when(
      edad < 10 ~ 1,
      edad >= 10 & edad <= 14 ~ 2,
      edad >= 15 & edad <= 19 ~ 3,
      edad >= 20 & edad <= 24 ~ 4,
      edad >= 25 & edad <= 29 ~ 5,
      edad >= 30 & edad <= 34 ~ 6,
      edad >= 35 & edad <= 39 ~ 7,
      edad >= 40 & edad <= 44 ~ 8,
      edad >= 45 & edad <= 49 ~ 9,
      edad >= 50 & edad <= 54 ~ 10,
      edad >= 55 ~ 11,
      TRUE ~ NA),
    age_group = factor(age_group, levels = 1:11,
                       labels = c("<10", "10–14", "15–19", "20–24", "25–29",
                                  "30–34", "35–39", "40–44", "45–49", "50–54", "55+")))

table(expenses_2019$age_group, useNA = "ifany")

#### 6: sex - check if anything != 2 ----

##### A. Check frequency of all sex values  ----
table(expenses_2019$sexo, useNA = "ifany")

##### B. sex - convert and relabel ----
expenses_2019 <- expenses_2019 %>%
  mutate(
    sex = case_when(
      sexo == "1" ~ "Male",
      sexo == "2" ~ "Female",
      sexo == "9" ~ "Not specified",
      TRUE ~ NA),
    sex = factor(sex, 
                 levels = c("Male", "Female", "Not specified")))

table(expenses_2019$sex, useNA = "ifany")

#### 7: weight and height - replace 999 values with NA ----
expenses_2019 <- expenses_2019 %>%
  mutate(
    weight = na_if(peso, 999),
    height = na_if(talla, 999))

# weight: ensure numeric
expenses_2019 <- expenses_2019 %>%
  mutate(weight = as.numeric(weight))

#### 8: insurance - rename and recode insurance types ----
expenses_2019 <- expenses_2019 %>%
  mutate(
    insurance = case_when(
      derhab == "0"  ~ "Uninsured",
      derhab == "1"  ~ "IMSS",
      derhab == "2"  ~ "ISSSTE",
      derhab == "3"  ~ "PEMEX",
      derhab == "4"  ~ "SEDENA",
      derhab == "5"  ~ "SEMAR",
      derhab == "6"  ~ "State government",
      derhab == "7"  ~ "Private insurance",
      derhab == "8"  ~ "Public insurance",
      derhab == "9"  ~ "Unknown",
      derhab == "10"  ~ "Other",
      derhab == "G"  ~ "Gratuidad",
      derhab == "P"  ~ "Prospera",
      TRUE ~ NA),
    insurance = factor(insurance,
                       levels = c("Uninsured", "IMSS", "ISSSTE", "PEMEX", "SEDENA",
                                  "SEMAR", "State government", "Private insurance", 
                                  "Public insurance", "Unknown", "Other", "Gratuidad", "Prospera")))

table(expenses_2019$insurance, useNA = "ifany")
table(expenses_2019$derhab, useNA = "ifany")

#### 9: state - recode variable values and rename  ----

# Step A: Check for non-Mexico residences
expenses_2019 %>%
  filter(entidad %in% c("33", "34", "35")) %>%
  select(id, entidad) %>%
  head() 

# Step B: Recode state values to full names
expenses_2019 <- expenses_2019 %>%
  mutate(
    entidad_char = as.character(entidad),
    state = case_when(
      entidad_char == "1" ~ "Aguascalientes",
      entidad_char == "2" ~ "Baja California",
      entidad_char == "3" ~ "Baja California Sur",
      entidad_char == "4" ~ "Campeche",
      entidad_char == "5" ~ "Coahuila de Zaragoza",
      entidad_char == "6" ~ "Colima",
      entidad_char == "7" ~ "Chiapas",
      entidad_char == "8" ~ "Chihuahua",
      entidad_char == "9" ~ "Ciudad de México",
      entidad_char == "10" ~ "Durango",
      entidad_char == "11" ~ "Guanajuato",
      entidad_char == "12" ~ "Guerrero",
      entidad_char == "13" ~ "Hidalgo",
      entidad_char == "14" ~ "Jalisco",
      entidad_char == "15" ~ "México",
      entidad_char == "16" ~ "Michoacán de Ocampo",
      entidad_char == "17" ~ "Morelos",
      entidad_char == "18" ~ "Nayarit",
      entidad_char == "19" ~ "Nuevo León",
      entidad_char == "20" ~ "Oaxaca",
      entidad_char == "21" ~ "Puebla",
      entidad_char == "22" ~ "Querétaro de Arteaga",
      entidad_char == "23" ~ "Quintana Roo",
      entidad_char == "24" ~ "San Luis Potosí",
      entidad_char == "25" ~ "Sinaloa",
      entidad_char == "26" ~ "Sonora",
      entidad_char == "27" ~ "Tabasco",
      entidad_char == "28" ~ "Tamaulipas",
      entidad_char == "29" ~ "Tlaxcala",
      entidad_char == "30" ~ "Veracruz de Ignacio de la Llave",
      entidad_char == "31" ~ "Yucatán",
      entidad_char == "32" ~ "Zacatecas",
      entidad_char == "33" ~ "USA",
      entidad_char == "34" ~ "Other Latin American Country",
      entidad_char == "35" ~ "Other Country",
      entidad_char == "99" ~ "Not Specified",
      TRUE ~ NA),
    state = factor(state,
                   levels = c(
                     "Aguascalientes", "Baja California", "Baja California Sur", "Campeche",
                     "Coahuila de Zaragoza", "Colima", "Chiapas", "Chihuahua", "Ciudad de México",
                     "Durango", "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", "México",
                     "Michoacán de Ocampo", "Morelos", "Nayarit", "Nuevo León", "Oaxaca", "Puebla",
                     "Querétaro de Arteaga", "Quintana Roo", "San Luis Potosí", "Sinaloa", "Sonora",
                     "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz de Ignacio de la Llave",
                     "Yucatán", "Zacatecas", "USA", "Other Latin American Country", "Other Country",
                     "Not Specified"))) %>%
  select(-entidad_char)


# check that code ran correctly
table(expenses_2019$state, useNA = "ifany")
table(expenses_2019$entidad, useNA = "ifany")


#### 10: munic - convert to factor and pad with 0's ----
expenses_2019 <- expenses_2019 %>%
  mutate(
    munic = str_pad(as.character(munic), width = 3, pad = "0"),
    munic = factor(munic))

table(expenses_2019$munic, useNA = "ifany")

#### 11: entidad - convert to factor and pad with 0's ----
expenses_2019 <- expenses_2019 %>%
  mutate(
    entidad = str_pad(as.character(entidad), width = 2, pad = "0"),
    entidad = factor(entidad))

table(expenses_2019$entidad, useNA = "ifany")

#### 12: munic_code - making the variable ----
expenses_2019 <- expenses_2019 %>%
  mutate(
    munic_code = paste0(entidad, munic))

#### 13: loc - convert to factor and pad with 0's ----
expenses_2019 <- expenses_2019 %>%
  mutate(
    loc = str_pad(as.character(loc), width = 4, pad = "0"),
    loc = factor(loc))

table(expenses_2019$loc, useNA = "ifany")

#### 14: indigena - make a new binary variable ----
table(expenses_2019$indigena, useNA = "ifany")

# Step 1: Create the binary variable
expenses_2019 <- expenses_2019 %>%
  mutate(
    indigena.num = case_when(
      indigena == 1 ~ 1,
      indigena == 2 ~ 0,
      indigena %in% c(3, 4, 9) ~ 2))

## Check it's converted correctly
table(expenses_2019$indigena.num, useNA = "ifany")

# Step 2: Convert binary to factor with labels
expenses_2019 <- expenses_2019 %>%
  mutate(
    indigena.bin = factor(indigena.num, 
                          levels = c(0, 1, 2),
                          labels = c("No", "Yes", "No response")))
table(expenses_2019$indigena.bin, useNA = "ifany")

#### 15: speaks_indigen - create and relabel ----
table(expenses_2019$habla_lengua, useNA = "ifany")

# Step 1: Create numeric binary variable
expenses_2019 <- expenses_2019 %>%
  mutate(
    habla_lengua.num = case_when(
      habla_lengua == 1 ~ 1,
      habla_lengua == 2 ~ 0,
      habla_lengua %in% c(3, 4) ~ 2))

## Check it's converted correctly
table(expenses_2019$habla_lengua.num, useNA = "ifany")

# Step 2: Convert binary to factor with labels
expenses_2019 <- expenses_2019 %>%
  mutate(
    speaks_indigen = factor(habla_lengua.num,
                            levels = c(0, 1, 2),
                            labels = c("No", "Yes", "No response")))


#### 16: lengua_indigena - convert and relabel ----
unique(expenses_2019$lengua_indigena) # values not in our range - "99", "450", "451", "450", and "NULL" are being lumped into NA in the following code

#### Drop lengua_indigena
expenses_2019 <- expenses_2019 %>%
  select(-lengua_indigena)


#### 17: habla_esp - convert and relabel ----
table(expenses_2019$habla_esp, useNA = "ifany")

# Step 1: Create numeric binary variable
expenses_2019 <- expenses_2019 %>%
  mutate(
    habla_esp.num = case_when(
      habla_esp == 1 ~ 1,
      habla_esp == 2 ~ 0,
      habla_esp %in% c(3, 4, 9) ~ 2))

## Check it's converted correctly
table(expenses_2019$habla_esp.num, useNA = "ifany")

# Step 2: Convert binary to factor with labels
expenses_2019 <- expenses_2019 %>%
  mutate(
    habla_esp.bin = factor(habla_esp.num,
                           levels = c(0, 1, 2),
                           labels = c("No", "Yes", "No response")))

table(expenses_2019$habla_esp.bin, useNA = "ifany")

#### 18: admiss_type - convert and relabel ----
expenses_2019 <- expenses_2019 %>%
  mutate(
    admiss_type = factor(tipserv,
                         levels = c("1", "2"),
                         labels = c("Normal", "Short stay")))

table(expenses_2019$tipserv, useNA = "ifany")
table(expenses_2019$admiss_type, useNA = "ifany")

#### 19: origin - convert and relabel ----
expenses_2019 <- expenses_2019 %>%
  mutate(
    origin = factor(proced,
                    levels = c("1", "2", "3", "4", "6", "9"),
                    labels = c("Outpatient", "Emergency", "Referred", "Other", "Pathological nursery", "Not specified")))

table(expenses_2019$origin, useNA = "ifany")
table(expenses_2019$proced, useNA = "ifany")

#### 20: discharge_res - convert and relabel ----
expenses_2019 <- expenses_2019 %>%
  mutate(
    discharge_res = factor(motegre,
                           levels = c("1", "2", "3", "4", "5", "6", "7", "9"),
                           labels = c(
                             "Recovery",
                             "Improvement",
                             "Voluntary discharge",
                             "Transfer",
                             "Death",
                             "Escape",
                             "Other",
                             "Not specified")))

table(expenses_2019$discharge_res, useNA = "ifany")
table(expenses_2019$motegre, useNA = "ifany")

#### 21: month_discharge - convert and relabel ----
expenses_2019 <- expenses_2019 %>%
  mutate(
    month_discharge = factor(mes_estadistico,
                             levels = as.character(1:12),
                             labels = c(
                               "January", "February", "March", "April", "May", "June",
                               "July", "August", "September", "October", "November", "December")))


#### 22: relationship_status - convert and relabel ----
expenses_2019 <- expenses_2019 %>%
  mutate(
    relationship_status = factor(estado_conyugal_key,
                                 levels = c("1", "5", "3", "2", "4", "6", "8", "9", "0"),
                                 labels = c(
                                   "Single",
                                   "Married",
                                   "Divorced",
                                   "Widowed",
                                   "Domestic partnership",
                                   "Separated",
                                   "Not applicable",
                                   "Unknown",
                                   "Not specified")))

#### 23: admiss_readmit - convert and relabel ----
expenses_2019 <- expenses_2019 %>%
  mutate(
    admiss_readmit = factor(vez,
                            levels = c("1", "2", "9"),
                            labels = c(
                              "First time",
                              "Subsequent",
                              "Not specified")))



###### Rename variables some old obstet variables ----
expenses_2019 <- expenses_2019 %>%
  rename(
    gravida = gestas,
    parity = partos)


##### Convert and relabel - hayprod ----
table(expenses_2019$hayprod, useNA = "ifany")


# Step 1: Create numeric binary variable
expenses_2019 <- expenses_2019 %>%
  mutate(
    hayprod.num = case_when(
      hayprod == 1 ~ 1,
      hayprod == 2 ~ 0,
      hayprod %in% c(8, 0) ~ 2))

## Check it's converted correctly
table(expenses_2019$hayprod.num, useNA = "ifany")

# Step 2: Convert binary to factor with labels
expenses_2019 <- expenses_2019 %>%
  mutate(
    hayprod.bin = factor(hayprod.num,
                         levels = c(0, 1, 2),
                         labels = c("No", "Yes", "No response")))

## Check it's converted correctly
table(expenses_2019$hayprod.bin, useNA = "ifany")


###### Factor and label planfam  ----
expenses_2019 <- expenses_2019 %>%
  mutate(    
    contracep = case_when(
      planfam %in% c(2, 3) ~ 2,  # Injectable
      TRUE ~ planfam) %>%
      factor(
        levels = c(0, 1, 2, 4, 5, 6, 7, 8, 9, 10, 11, 99, 88),
        labels = c(
          "None",
          "Pills",
          "Injectable",
          "Implant",
          "Copper IUD",
          "Female condom",
          "Male condom",
          "Medicated IUD",
          "Patch",
          "Permanent",
          "Other",
          "Unspecified",
          "Not applicable")))

table(expenses_2019$contracep, useNA = "ifany")
table(expenses_2019$planfam, useNA = "ifany")


###### Change 99's to NA in gestac  ----
expenses_2019 <- expenses_2019 %>%
  mutate(gestat = na_if(gestac, 99))



#### AFECCIONES ----------------------------------------------------------------

###### Import the conditions data  ----
conditions_2019 <- read.csv("DATA/ssa_egresos_2019/AFECCIONES_2019.txt")

###### Make variables lowercase  ----
conditions_2019 <- conditions_2019 %>%
  clean_names()

###### Reshape to wide format based on numafec  ----
conditions_2019_wide <- conditions_2019 %>%
  select(id, afec) %>%
  group_by(id) %>%
  mutate(condition_n = row_number()) %>%       # Count afec per id
  ungroup() %>%
  pivot_wider(
    names_from = condition_n,
    names_prefix = "afec",
    values_from = afec)

##### Verify pivot ran correctly ----
sum(duplicated(conditions_2019_wide$id))



#### PROCEDIMIENTOS ------------------------------------------------------------

###### Import the procedures data  ----
procedures_2019 <- read.csv("DATA/ssa_egresos_2019/PROCEDIMIENTOS_2019.txt")

###### Make variables lowercase  ----
procedures_2019 <- procedures_2019 %>%
  clean_names()

##### Drop unnecessary variables - tipo, anest, quirof, qh, qm  ----
procedures_2019 <- procedures_2019  %>%
  select(-tipo, -anest, -quirof, -qh, -qm)

##### Keep only first 10 procedures per id and pivot to wide format ----
procedures_2019_wide <- procedures_2019 %>%
  group_by(id) %>%
  arrange(id) %>%
  slice_head(n = 10) %>%
  mutate(procedure_n = row_number()) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = id,
    names_from = procedure_n,
    names_prefix = "promed",
    values_from = promed)


##### Create procedure variables ----

# Step 1: Collapse all promed variables into a single string column per row
procedures_2019_wide <- procedures_2019_wide %>%
  mutate(promed_concat = paste(promed1, promed2, promed3, promed4, promed5,
                               promed6, promed7, promed8, promed9, promed10, sep = " "))

# Step 2: Use str_detect() to flag presence of each procedure code
procedures_2019_wide <- procedures_2019_wide %>%
  mutate(
    proc_dc_ab     = as.integer(str_detect(promed_concat, "\\b6901\\b")),
    proc_dc_post   = as.integer(str_detect(promed_concat, "\\b6902\\b")),
    proc_asp_ab    = as.integer(str_detect(promed_concat, "\\b6951\\b")),
    proc_asp_post  = as.integer(str_detect(promed_concat, "\\b6952\\b")),
    proc_miso_ab   = as.integer(str_detect(promed_concat, "\\b75A1\\b")),
    proc_miso_post = as.integer(str_detect(promed_concat, "\\b75A2\\b")),
    proc_oth_ab    = as.integer(str_detect(promed_concat, "\\b75A3\\b")),
    proc_oth_post  = as.integer(str_detect(promed_concat, "\\b75A4\\b")))






#### PRODUCTOS -----------------------------------------------------------------

###### Import the products data  ----
products_2019 <- read.csv("DATA/ssa_egresos_2019/PRODUCTOS_2019.txt")

###### Make variables lowercase  ----
products_2019 <- products_2019 %>%
  clean_names()

##### Drop unnecessary variables - condegre, naviapag, navirean, lactancia_exclusiva  ----
products_2019 <- products_2019 %>%
  select(-condegre, -naviapag, -navirean, -lactancia_exclusiva) %>%
  distinct()

##### Pivot wider so each product becomes a column  ----
products_2019_wide <- products_2019 %>%
  filter(!is.na(condnac)) %>%
  mutate(numproducto = paste0("product_", numproducto)) %>%
  pivot_wider(
    id_cols = id,
    names_from = numproducto,
    values_from = condnac)


##### Convert to factors with labels (Fetal Death/Live Birth/Not Specified)  ----
products_2019_wide <- products_2019_wide %>%
  mutate(across(starts_with("product_"),
                ~ factor(.x,
                         levels = c(1, 2, 9),
                         labels = c("Fetal Death", "Live Birth", "Not Specified"))))



#### Merge with EGRESOS and save the cleaned data ------------------------------

# Convert id to character in all data sets
expenses_2019 <- expenses_2019 %>% mutate(id = as.character(id))
conditions_2019_wide <- conditions_2019_wide %>% mutate(id = as.character(id))
procedures_2019_wide <- procedures_2019_wide %>% mutate(id = as.character(id))
products_2019_wide <- products_2019_wide %>% mutate(id = as.character(id))

# Merge with EGRESOS 
final_2019 <- expenses_2019 %>%
  left_join(conditions_2019_wide, by = "id") %>%
  left_join(procedures_2019_wide, by = "id") %>%
  left_join(products_2019_wide, by = "id")

# Create year variable to differentiate years for final join
final_2019 <- final_2019 %>%
  mutate(year = 2019)




# --- 2020 ---------------------------------------------------------------------



#### EGRESO --------------------------------------------------------------------

###### Import the expenses data  ----
expenses_2020 <- read.delim("DATA/ssa_egresos_2020/EGRESOS.txt",
  sep = "|",
  header = T,
  stringsAsFactors = F)

###### Make variables lowercase  ----
expenses_2020 <- expenses_2020 %>%
  clean_names()

#### 1: Drop everything except ICD-10 codes starting w/ O02-O08 and code Z303  ----
expenses_2020 <- expenses_2020 %>%
  rename(icd_code = afecprin) %>%
  filter(substr(icd_code, 1, 3) %in% c("O02", "O03", "O04", "O05", "O06", "O07", "O08") |
           icd_code == "Z303")

##### A. Table of frequencies of the primary ICD-10 codes  ----
table(expenses_2020$icd_code, useNA = "ifany")
nrow(expenses_2020)

#### 2: Drop unnecessary variables - tuhpsiq, servhc, servhp, nacioen, servicioingre, servicio02, servicio03, servicioegre, infec, mp, ttipaten, producto, tipnaci, cesareas  ----
expenses_2020 <- expenses_2020 %>%
  select(-tuhpsiq, -servhc, -servhp, -nacioen, -servicioingre,
         -servicio02, -servicio03, -servicioegre, -infec, -mp,
         -tipaten, -producto, -tipnaci, -cesareas)

#### 3: egreso, ingre - convert admission and discharge date variables ----
expenses_2020$egreso <- as.Date(expenses_2020$egreso)
expenses_2020$ingre  <- as.Date(expenses_2020$ingre)

#### 4: dias_esta - check outliers & negative values ----
summary(expenses_2020$dias_esta)

##### A. View negative values  ----
expenses_2020 %>%
  filter(dias_esta < 0) %>%
  select(id, ingre, egreso, dias_esta)

##### B. View extreme high values (> 30 days)  ----
expenses_2020 %>%
  filter(dias_esta > 30) %>%
  arrange(desc(dias_esta)) %>%
  select(id, ingre, egreso, dias_esta) %>%
  head(90)


#### 5: cveedad - check if anything is != 5 (not the age unit ~ years) ----

##### A. Check frequency of all cveedad values  ----
table(expenses_2020$cveedad, useNA = "ifany")

##### B. Identify rows where cveedad != 5  ----
expenses_2020 %>%
  filter(cveedad != 5) %>%
  select(id, cveedad, edad) %>%
  head() # none!

##### C. Convert cveedad to factor with labeled levels and name age_unit
expenses_2020 <- expenses_2020 %>%
  mutate(age_unit = factor(cveedad,
                           levels = c(2, 3, 4, 5, 9),
                           labels = c("hours", "days", "months", "years", "Not specified")))

##### D. Create age groups for edad and rename to age
expenses_2020 <- expenses_2020 %>%
  mutate(
    age_group = case_when(
      edad < 10 ~ 1,
      edad >= 10 & edad <= 14 ~ 2,
      edad >= 15 & edad <= 19 ~ 3,
      edad >= 20 & edad <= 24 ~ 4,
      edad >= 25 & edad <= 29 ~ 5,
      edad >= 30 & edad <= 34 ~ 6,
      edad >= 35 & edad <= 39 ~ 7,
      edad >= 40 & edad <= 44 ~ 8,
      edad >= 45 & edad <= 49 ~ 9,
      edad >= 50 & edad <= 54 ~ 10,
      edad >= 55 ~ 11,
      TRUE ~ NA),
    age_group = factor(age_group, levels = 1:11,
                       labels = c("<10", "10–14", "15–19", "20–24", "25–29",
                                  "30–34", "35–39", "40–44", "45–49", "50–54", "55+")))


#### 6: sex - check if anything != 2 ----

##### A. Check frequency of all sex values  ----
table(expenses_2020$sexo, useNA = "ifany")

##### B. sex - convert and relabel ----
expenses_2020 <- expenses_2020 %>%
  mutate(
    sex = case_when(
      sexo == "1" ~ "Male",
      sexo == "2" ~ "Female",
      sexo == "0" ~ "No response",
      sexo == "9" ~ "No response",
      TRUE ~ NA),
    sex = factor(sex, 
                 levels = c("Male", "Female", "No response")))


#### 7: weight and height - replace 999 values with NA ----
expenses_2020 <- expenses_2020 %>%
  mutate(
    weight = na_if(peso, 999),
    height = na_if(talla, 999))

# weight: ensure numeric
expenses_2020 <- expenses_2020 %>%
  mutate(weight = as.numeric(weight))

#### 8: insurance - rename and recode insurance types ----

# remove leading and trailing spaces
expenses_2020 <- expenses_2020 %>%
  mutate(
    derhab = str_trim(derhab))

expenses_2020 <- expenses_2020 %>%
  mutate(
    insurance = case_when(
      derhab == "0"  ~ "Uninsured",
      derhab == "1"  ~ "IMSS",
      derhab == "2"  ~ "ISSSTE",
      derhab == "3"  ~ "PEMEX",
      derhab == "4"  ~ "SEDENA",
      derhab == "5"  ~ "SEMAR",
      derhab == "6"  ~ "State government",
      derhab == "7"  ~ "Private insurance",
      derhab == "8"  ~ "Public insurance",
      derhab == "9"  ~ "Unknown",
      derhab == "10"  ~ "Other",
      derhab == "11"  ~ "INSABI",
      derhab == "G"  ~ "Gratuidad",
      derhab == "B"  ~ "IMSS BIENESTAR",
      derhab == "99"  ~ "Not Specified",
      TRUE ~ NA),
    insurance = factor(insurance,
                       levels = c("Uninsured", "IMSS", "ISSSTE", "PEMEX", "SEDENA",
                                  "SEMAR", "State government", "Private insurance", 
                                  "Public insurance", "Unknown", "Other", "INSABI", "Gratuidad", 
                                  "IMSS BIENESTAR", "Not Specified")))

table(expenses_2020$insurance, useNA = "ifany")
table(expenses_2020$derhab, useNA = "ifany")

#### 9: state - recode variable values and rename  ----

# Step A: Check for non-Mexico residences
expenses_2020 %>%
  filter(entidad %in% c("37", "38")) %>%
  select(id, entidad) %>%
  head() 

# Step B: Recode state values to full names
expenses_2020 <- expenses_2020 %>%
  mutate(
    state = case_when(
      entidad == 1  ~ "Aguascalientes",
      entidad == 2  ~ "Baja California",
      entidad == 3  ~ "Baja California Sur",
      entidad == 4  ~ "Campeche",
      entidad == 5  ~ "Coahuila de Zaragoza",
      entidad == 6  ~ "Colima",
      entidad == 7  ~ "Chiapas",
      entidad == 8  ~ "Chihuahua",
      entidad == 9  ~ "Ciudad de México",
      entidad == 10 ~ "Durango",
      entidad == 11 ~ "Guanajuato",
      entidad == 12 ~ "Guerrero",
      entidad == 13 ~ "Hidalgo",
      entidad == 14 ~ "Jalisco",
      entidad == 15 ~ "México",
      entidad == 16 ~ "Michoacán de Ocampo",
      entidad == 17 ~ "Morelos",
      entidad == 18 ~ "Nayarit",
      entidad == 19 ~ "Nuevo León",
      entidad == 20 ~ "Oaxaca",
      entidad == 21 ~ "Puebla",
      entidad == 22 ~ "Querétaro de Arteaga",
      entidad == 23 ~ "Quintana Roo",
      entidad == 24 ~ "San Luis Potosí",
      entidad == 25 ~ "Sinaloa",
      entidad == 26 ~ "Sonora",
      entidad == 27 ~ "Tabasco",
      entidad == 28 ~ "Tamaulipas",
      entidad == 29 ~ "Tlaxcala",
      entidad == 30 ~ "Veracruz de Ignacio de la Llave",
      entidad == 31 ~ "Yucatán",
      entidad == 32 ~ "Zacatecas",
      entidad == 37 ~ "USA",
      entidad == 38 ~ "Rest of Latin America",
      entidad == 39 ~ "Other Continent",
      entidad == 88 ~ "Not Applicable",
      entidad == 99 ~ "No response",
      entidad == 0  ~ "No response",
      TRUE ~ NA),
    state = factor(state,
                   levels = c(
                     "Aguascalientes", "Baja California", "Baja California Sur", "Campeche",
                     "Coahuila de Zaragoza", "Colima", "Chiapas", "Chihuahua", "Ciudad de México",
                     "Durango", "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", "México",
                     "Michoacán de Ocampo", "Morelos", "Nayarit", "Nuevo León", "Oaxaca", "Puebla",
                     "Querétaro de Arteaga", "Quintana Roo", "San Luis Potosí", "Sinaloa", "Sonora",
                     "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz de Ignacio de la Llave",
                     "Yucatán", "Zacatecas", "USA", "Rest of Latin America", "Other Continent",
                     "Not Applicable", "No response")))

table(expenses_2020$state, useNA = "ifany")


#### 10: munic - convert to factor and pad with 0's ----
expenses_2020 <- expenses_2020 %>%
  mutate(
    munic = str_pad(as.character(munic), width = 3, pad = "0"),
    munic = factor(munic))

#### 11: entidad - convert to factor and pad with 0's ----
expenses_2020 <- expenses_2020 %>%
  mutate(
    entidad = str_pad(as.character(entidad), width = 2, pad = "0"),
    entidad = factor(entidad))

#### 12: munic_code - making the variable ----
expenses_2020 <- expenses_2020 %>%
  mutate(
    munic_code = paste0(entidad, munic))

#### 13: loc - convert to factor and pad with 0's ----
expenses_2020 <- expenses_2020 %>%
  mutate(
    loc = str_pad(as.character(loc), width = 4, pad = "0"),
    loc = factor(loc))

#### 14: indigena - make a new binary variable ----
table(expenses_2020$indigena, useNA = "ifany")

# Step 1: Create the binary variable
expenses_2020 <- expenses_2020 %>%
  mutate(
    indigena.num = case_when(
      indigena == 1 ~ 1,
      indigena == 2 ~ 0,
      indigena %in% c(3, 4, 9) ~ 2))

## Check it's converted correctly
table(expenses_2020$indigena.num, useNA = "ifany")

# Step 2: Convert binary to factor with labels
expenses_2020 <- expenses_2020 %>%
  mutate(
    indigena.bin = factor(indigena.num, 
                          levels = c(0, 1, 2),
                          labels = c("No", "Yes", "No response")))


#### 15: speaks_indigen - create and relabel ----
table(expenses_2020$habla_lengua, useNA = "ifany")

# Step 1: Create numeric binary variable
expenses_2020 <- expenses_2020 %>%
  mutate(
    habla_lengua.num = case_when(
      habla_lengua == 1 ~ 1,
      habla_lengua == 2 ~ 0,
      habla_lengua %in% c(3, 4) ~ 2))

## Check it's converted correctly
table(expenses_2020$habla_lengua.num, useNA = "ifany")

# Step 2: Convert binary to factor with labels
expenses_2020 <- expenses_2020 %>%
  mutate(
    speaks_indigen = factor(habla_lengua.num,
                            levels = c(0, 1, 2),
                            labels = c("No", "Yes", "No response")))


#### 16: lengua_indigena - convert and relabel ----
unique(expenses_2020$lengua_indigena) # values not in our range - "8888",  "450", "471", "1041",  "922", "1021", "9999",  "400",  "611",  "935", "933", "934",  "931",  "823", "491", "1013", "1032", "481", "451", "482", "1031", "1111", "311", "911", and "494" are being lumped into NA in the following code

#### Drop lengua_indigena
expenses_2020 <- expenses_2020 %>%
  select(-lengua_indigena)


#### 17: habla_esp no longer collected


#### 18: admiss_type - convert and relabel ----
expenses_2020 <- expenses_2020 %>%
  mutate(
    admiss_type = factor(tipserv,
                         levels = c("1", "2"),
                         labels = c("Normal", "Short stay")))

table(expenses_2020$tipserv, useNA = "ifany")
table(expenses_2020$admiss_type, useNA = "ifany")

#### 19: origin - convert and relabel ----
expenses_2020 <- expenses_2020 %>%
  mutate(
    origin = factor(proced,
                    levels = c("1", "2", "3", "4", "5", "9"),
                    labels = c("Outpatient", "Emergency", "Referred", "Pathological nursery", "Other", "Not specified")))

table(expenses_2020$proced, useNA = "ifany")
table(expenses_2020$origin, useNA = "ifany")

#### 20: discharge_res - convert and relabel ----
expenses_2020 <- expenses_2020 %>%
  mutate(
    discharge_res = factor(motegre,
                           levels = c("1", "2", "3", "4", "5", "6", "7", "9"),
                           labels = c(
                             "Recovery",
                             "Improvement",
                             "Voluntary discharge",
                             "Transfer",
                             "Death",
                             "Escape",
                             "Other",
                             "Not specified")))

table(expenses_2020$motegre, useNA = "ifany")
table(expenses_2020$discharge_res, useNA = "ifany")

#### 21: month_discharge - convert and relabel ----
expenses_2020 <- expenses_2020 %>%
  mutate(
    month_discharge = factor(mes_estadistico,
                             levels = as.character(1:12),
                             labels = c(
                               "January", "February", "March", "April", "May", "June",
                               "July", "August", "September", "October", "November", "December")))

table(expenses_2020$mes_estadistico, useNA = "ifany")
table(expenses_2020$month_discharge, useNA = "ifany")

#### 22: relationship_status - convert and relabel ----
expenses_2020 <- expenses_2020 %>%
  mutate(
    relationship_status = factor(estado_conyugal_key,
                                 levels = c("1", "5", "3", "2", "4", "6", "8", "9", "0"),
                                 labels = c(
                                   "Single",
                                   "Married",
                                   "Divorced",
                                   "Widowed",
                                   "Domestic partnership",
                                   "Separated",
                                   "Not applicable",
                                   "Unknown",
                                   "Not specified")))

table(expenses_2020$estado_conyugal_key, useNA = "ifany")
table(expenses_2020$relationship_status, useNA = "ifany")

#### 23: admiss_readmit - convert and relabel ----
expenses_2020 <- expenses_2020 %>%
  mutate(
    admiss_readmit = factor(vez,
                            levels = c("1", "2", "9"),
                            labels = c(
                              "First time",
                              "Subsequent",
                              "Not specified")))

table(expenses_2020$vez, useNA = "ifany")
table(expenses_2020$admiss_readmit, useNA = "ifany")



###### Rename variables some old obstet variables ----
expenses_2020 <- expenses_2020 %>%
  rename(
    gravida = gestas,
    parity = partos)


##### Convert and relabel - hayprod ----
table(expenses_2020$hayprod, useNA = "ifany")

# Step 1: Create numeric binary variable
expenses_2020 <- expenses_2020 %>%
  mutate(
    hayprod.num = case_when(
      hayprod == 1 ~ 1,
      hayprod == 2 ~ 0,
      hayprod %in% c(8, 9) ~ 2))

## Check it's converted correctly
table(expenses_2020$hayprod.num, useNA = "ifany")

# Step 2: Convert binary to factor with labels
expenses_2020 <- expenses_2020 %>%
  mutate(
    hayprod.bin = factor(hayprod.num,
                         levels = c(0, 1, 2),
                         labels = c("No", "Yes", "No response")))

## Check it's converted correctly
table(expenses_2020$hayprod.bin, useNA = "ifany")


###### Factor and label planfam  ----
expenses_2020 <- expenses_2020 %>%
  mutate(    
    contracep = case_when(
      planfam %in% c(2, 3) ~ 2,  # Injectable
      TRUE ~ planfam) %>%
      factor(
        levels = c(0, 1, 2, 4, 5, 6, 7, 8, 9, 10, 11, 99, 88),
        labels = c(
          "None",
          "Pills",
          "Injectable",
          "Implant",
          "Copper IUD",
          "Female condom",
          "Male condom",
          "Medicated IUD",
          "Patch",
          "Permanent",
          "Other",
          "Unspecified",
          "Not applicable")))

table(expenses_2020$planfam, useNA = "ifany")
table(expenses_2020$contracep, useNA = "ifany")

###### Change 99's to NA in gestac  ----
expenses_2020 <- expenses_2020 %>%
  mutate(gestat = na_if(gestac, 99))



#### AFECCIONES ----------------------------------------------------------------

###### Import the conditions data  ----
conditions_2020 <- read.delim("DATA/ssa_egresos_2020/AFECCIONES.txt",
                            sep = "|",
                            header = T,
                            stringsAsFactors = F)

###### Make variables lowercase  ----
conditions_2020 <- conditions_2020 %>%
  clean_names()

###### Change names for easier cleaning  ----
conditions_2020 <- conditions_2020 %>%
  rename(
    afec = cod_cie_comorbilidad,
    numafec = numero_comorbilidad)


###### Reshape to wide format based on numafec  ----
conditions_2020_wide <- conditions_2020 %>%
  select(id, afec) %>%
  group_by(id) %>%
  mutate(condition_n = row_number()) %>%       # Count afec per id
  ungroup() %>%
  pivot_wider(
    names_from = condition_n,
    names_prefix = "afec",
    values_from = afec)

##### Verify pivot ran correctly ----
sum(duplicated(conditions_2020_wide$id))



#### PROCEDIMIENTOS ------------------------------------------------------------

###### Import the procedures data  ----
procedures_2020 <- read.delim("DATA/ssa_egresos_2020/PROCEDIMIENTOS.txt",
                              sep = "|",
                              header = T,
                              stringsAsFactors = F)

###### Make variables lowercase  ----
procedures_2020 <- procedures_2020 %>%
  clean_names()

##### Drop unnecessary variables - tipo, anest, quirof  ----
procedures_2020 <- procedures_2020 %>%
  select(-tipo, -anest, -quirof)

##### Keep only first 10 procedures per id and pivot to wide format ----
procedures_2020_wide <- procedures_2020 %>%
  group_by(id) %>%
  arrange(id) %>%
  slice_head(n = 10) %>%
  mutate(procedure_n = row_number()) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = id,
    names_from = procedure_n,
    names_prefix = "promed",
    values_from = promed)


##### Create procedure variables ----

# Step 1: Collapse all promed variables into a single string column per row
procedures_2020_wide <- procedures_2020_wide %>%
  mutate(promed_concat = paste(promed1, promed2, promed3, promed4, promed5,
                               promed6, promed7, promed8, promed9, promed10, sep = " "))

# Step 2: Use str_detect() to flag presence of each procedure code
procedures_2020_wide <- procedures_2020_wide %>%
  mutate(
    proc_dc_ab     = as.integer(str_detect(promed_concat, "\\b6901\\b")),
    proc_dc_post   = as.integer(str_detect(promed_concat, "\\b6902\\b")),
    proc_asp_ab    = as.integer(str_detect(promed_concat, "\\b6951\\b")),
    proc_asp_post  = as.integer(str_detect(promed_concat, "\\b6952\\b")),
    proc_miso_ab   = as.integer(str_detect(promed_concat, "\\b75A1\\b")),
    proc_miso_post = as.integer(str_detect(promed_concat, "\\b75A2\\b")),
    proc_oth_ab    = as.integer(str_detect(promed_concat, "\\b75A3\\b")),
    proc_oth_post  = as.integer(str_detect(promed_concat, "\\b75A4\\b")))






#### PRODUCTOS -----------------------------------------------------------------

###### Import the products data  ----
products_2020 <- read.delim("DATA/ssa_egresos_2020/PRODUCTOS.txt",
                            sep = "|",
                            header = T,
                            stringsAsFactors = F)

###### Make variables lowercase  ----
products_2020 <- products_2020 %>%
  clean_names()

##### Drop unnecessary variables - condegre, naviapag, navirean, lactancia_exclusiva  ----
products_2020 <- products_2020 %>%
  select(-condegre, -naviapag, -navirean, -lactancia_exclusiva, -alojamiento_conjunto) %>%
  distinct()

##### Pivot wider so each product becomes a column  ----
products_2020_wide <- products_2020 %>%
  filter(!is.na(condnac)) %>%
  mutate(numproducto = paste0("product_", numproducto)) %>%
  pivot_wider(
    id_cols = id,
    names_from = numproducto,
    values_from = condnac)


##### Convert to factors with labels (Fetal Death/Live Birth/Not Specified)  ----
products_2020_wide <- products_2020_wide %>%
  mutate(across(starts_with("product_"),
                ~ factor(.x,
                         levels = c(1, 2, 9),
                         labels = c("Fetal Death", "Live Birth", "Not Specified"))))



#### Merge with EGRESOS and save the cleaned data ------------------------------

# Convert id to character in all data sets
expenses_2020 <- expenses_2020 %>% mutate(id = as.character(id))
conditions_2020_wide <- conditions_2020_wide %>% mutate(id = as.character(id))
procedures_2020_wide <- procedures_2020_wide %>% mutate(id = as.character(id))
products_2020_wide <- products_2020_wide %>% mutate(id = as.character(id))

# Merge with EGRESOS 
final_2020 <- expenses_2020 %>%
  left_join(conditions_2020_wide, by = "id") %>%
  left_join(procedures_2020_wide, by = "id") %>%
  left_join(products_2020_wide, by = "id")

# Create year variable to differentiate years for final join
final_2020 <- final_2020 %>%
  mutate(year = 2020)



# --- 2021 ---------------------------------------------------------------------



#### EGRESO --------------------------------------------------------------------

###### Import the expenses data  ----
expenses_2021 <- read.delim("DATA/ssa_egresos_2021/EGRESOS.txt",
                            sep = "|",
                            header = T,
                            stringsAsFactors = F)

###### Make variables lowercase  ----
expenses_2021 <- expenses_2021 %>%
  clean_names()

#### 1: Drop everything except ICD-10 codes starting w/ O02-O08 and code Z303  ----
expenses_2021 <- expenses_2021 %>%
  rename(icd_code = afecprin) %>%
  filter(substr(icd_code, 1, 3) %in% c("O02", "O03", "O04", "O05", "O06", "O07", "O08") |
           icd_code == "Z303")

##### A. Table of frequencies of the primary ICD-10 codes  ----
table(expenses_2021$icd_code, useNA = "ifany")
nrow(expenses_2021)

#### 2: Drop unnecessary variables - tuhpsiq, servhc, servhp, nacioen, servicioingre, servicio02, servicio03, servicioegre, infec, mp, ttipaten, producto, tipnaci, cesareas  ----
expenses_2021 <- expenses_2021 %>%
  select(-tuhpsiq, -servhc, -servhp, -nacioen, -servicioingre,
         -servicio02, -servicio03, -servicioegre, -infec, -mp,
         -tipaten, -producto, -tipnaci, -cesareas)

#### 3: egreso, ingre - convert admission and discharge date variables ----
expenses_2021$egreso <- as.Date(expenses_2021$egreso)
expenses_2021$ingre  <- as.Date(expenses_2021$ingre)

#### 4: dias_esta - check outliers & negative values ----
summary(expenses_2021$dias_esta)

##### A. View negative values  ----
expenses_2021 %>%
  filter(dias_esta < 0) %>%
  select(id, ingre, egreso, dias_esta)

##### B. View extreme high values (> 30 days)  ----
expenses_2021 %>%
  filter(dias_esta > 30) %>%
  arrange(desc(dias_esta)) %>%
  select(id, ingre, egreso, dias_esta) %>%
  head(90)


#### 5: cveedad - check if anything is != 5 (not the age unit ~ years) ----

##### A. Check frequency of all cveedad values  ----
table(expenses_2021$cveedad, useNA = "ifany")

##### B. Identify rows where cveedad != 5  ----
expenses_2021 %>%
  filter(cveedad != 5) %>%
  select(id, cveedad, edad) %>%
  head() # 6 individuals with unknown ages?

##### C. Convert cveedad to factor with labeled levels and name age_unit
expenses_2021 <- expenses_2021 %>%
  mutate(age_unit = factor(cveedad,
                           levels = c(2, 3, 4, 5, 9),
                           labels = c("hours", "days", "months", "years", "Not specified")))

table(expenses_2021$age_unit, useNA = "ifany")

##### D. Create age groups for edad and rename to age
expenses_2021 <- expenses_2021 %>%
  mutate(
    age_group = case_when(
      edad < 10 ~ 1,
      edad >= 10 & edad <= 14 ~ 2,
      edad >= 15 & edad <= 19 ~ 3,
      edad >= 20 & edad <= 24 ~ 4,
      edad >= 25 & edad <= 29 ~ 5,
      edad >= 30 & edad <= 34 ~ 6,
      edad >= 35 & edad <= 39 ~ 7,
      edad >= 40 & edad <= 44 ~ 8,
      edad >= 45 & edad <= 49 ~ 9,
      edad >= 50 & edad <= 54 ~ 10,
      edad >= 55 ~ 11,
      TRUE ~ NA),
    age_group = factor(age_group, levels = 1:11,
                       labels = c("<10", "10–14", "15–19", "20–24", "25–29",
                                  "30–34", "35–39", "40–44", "45–49", "50–54", "55+")))

table(expenses_2021$age_group, useNA = "ifany")

#### 6: sex - check if anything != 2 ----

##### A. Check frequency of all sex values  ----
table(expenses_2021$sexo, useNA = "ifany")

##### B. sex - convert and relabel ----
expenses_2021 <- expenses_2021 %>%
  mutate(
    sex = case_when(
      sexo == "1" ~ "Male",
      sexo == "2" ~ "Female",
      sexo == "3" ~ "Intersex",
      sexo == "9" ~ "No response",
      TRUE ~ NA),
    sex = factor(sex, 
                 levels = c("Male", "Female", "Intersex", "No response")))


#### 7: weight and height - replace 999 values with NA ----
expenses_2021 <- expenses_2021 %>%
  mutate(
    weight = na_if(peso, 999),
    height = na_if(talla, 999))

# weight: ensure numeric
expenses_2021 <- expenses_2021 %>%
  mutate(weight = as.numeric(weight))

#### 8: insurance - rename and recode insurance types ----
expenses_2021 <- expenses_2021 %>%
  mutate(
    insurance = case_when(
      derhab == "0"  ~ "Uninsured",
      derhab == "1"  ~ "IMSS",
      derhab == "2"  ~ "ISSSTE",
      derhab == "3"  ~ "PEMEX",
      derhab == "4"  ~ "SEDENA",
      derhab == "5"  ~ "SEMAR",
      derhab == "6"  ~ "State government",
      derhab == "7"  ~ "Private insurance",
      derhab == "8"  ~ "Public insurance",
      derhab == "9"  ~ "Unknown",
      derhab == "10"  ~ "Other",
      derhab == "11"  ~ "INSABI",
      derhab == "G"  ~ "Gratuidad",
      derhab == "B"  ~ "IMSS BIENESTAR",
      derhab == "99"  ~ "Not Specified",
      TRUE ~ NA),
    insurance = factor(insurance,
                       levels = c("Uninsured", "IMSS", "ISSSTE", "PEMEX", "SEDENA",
                                  "SEMAR", "State government", "Private insurance", 
                                  "Public insurance", "Unknown", "Other", "INSABI", "Gratuidad", 
                                  "IMSS BIENESTAR", "Not Specified")))

table(expenses_2021$insurance, useNA = "ifany")
table(expenses_2021$derhab, useNA = "ifany")

#### 9: state - recode variable values and rename  ----

# Step A: Check for non-Mexico residences
expenses_2021 %>%
  filter(entidad %in% c("37", "38")) %>%
  select(id, entidad) %>%
  head() 

# Step B: Recode state values to full names
expenses_2021 <- expenses_2021 %>%
  mutate(
    state = case_when(
      entidad == 1  ~ "Aguascalientes",
      entidad == 2  ~ "Baja California",
      entidad == 3  ~ "Baja California Sur",
      entidad == 4  ~ "Campeche",
      entidad == 5  ~ "Coahuila de Zaragoza",
      entidad == 6  ~ "Colima",
      entidad == 7  ~ "Chiapas",
      entidad == 8  ~ "Chihuahua",
      entidad == 9  ~ "Ciudad de México",
      entidad == 10 ~ "Durango",
      entidad == 11 ~ "Guanajuato",
      entidad == 12 ~ "Guerrero",
      entidad == 13 ~ "Hidalgo",
      entidad == 14 ~ "Jalisco",
      entidad == 15 ~ "México",
      entidad == 16 ~ "Michoacán de Ocampo",
      entidad == 17 ~ "Morelos",
      entidad == 18 ~ "Nayarit",
      entidad == 19 ~ "Nuevo León",
      entidad == 20 ~ "Oaxaca",
      entidad == 21 ~ "Puebla",
      entidad == 22 ~ "Querétaro de Arteaga",
      entidad == 23 ~ "Quintana Roo",
      entidad == 24 ~ "San Luis Potosí",
      entidad == 25 ~ "Sinaloa",
      entidad == 26 ~ "Sonora",
      entidad == 27 ~ "Tabasco",
      entidad == 28 ~ "Tamaulipas",
      entidad == 29 ~ "Tlaxcala",
      entidad == 30 ~ "Veracruz de Ignacio de la Llave",
      entidad == 31 ~ "Yucatán",
      entidad == 32 ~ "Zacatecas",
      entidad == 37 ~ "USA",
      entidad == 38 ~ "Rest of Latin America",
      entidad == 39 ~ "Other Continent",
      entidad == 88 ~ "Not Applicable",
      entidad == 99 ~ "No response",
      entidad == 0  ~ "No response",
      TRUE ~ NA),
    state = factor(state,
                   levels = c(
                     "Aguascalientes", "Baja California", "Baja California Sur", "Campeche",
                     "Coahuila de Zaragoza", "Colima", "Chiapas", "Chihuahua", "Ciudad de México",
                     "Durango", "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", "México",
                     "Michoacán de Ocampo", "Morelos", "Nayarit", "Nuevo León", "Oaxaca", "Puebla",
                     "Querétaro de Arteaga", "Quintana Roo", "San Luis Potosí", "Sinaloa", "Sonora",
                     "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz de Ignacio de la Llave",
                     "Yucatán", "Zacatecas", "USA", "Rest of Latin America", "Other Continent", 
                     "Not Applicable", "No response")))

table(expenses_2021$state, useNA = "ifany")


#### 10: munic - convert to factor and pad with 0's ----
expenses_2021 <- expenses_2021 %>%
  mutate(
    munic = str_pad(as.character(munic), width = 3, pad = "0"),
    munic = factor(munic))

#### 11: entidad - convert to factor and pad with 0's ----
expenses_2021 <- expenses_2021 %>%
  mutate(
    entidad = str_pad(as.character(entidad), width = 2, pad = "0"),
    entidad = factor(entidad))

#### 12: munic_code - making the variable ----
expenses_2021 <- expenses_2021 %>%
  mutate(
    munic_code = paste0(entidad, munic))

#### 13: loc - convert to factor and pad with 0's ----
expenses_2021 <- expenses_2021 %>%
  mutate(
    loc = str_pad(as.character(loc), width = 4, pad = "0"),
    loc = factor(loc))

#### 14: indigena - make a new binary variable ----
table(expenses_2021$indigena, useNA = "ifany")

# Step 1: Create the binary variable
expenses_2021 <- expenses_2021 %>%
  mutate(
    indigena.num = case_when(
      indigena == 1 ~ 1,
      indigena == 2 ~ 0,
      indigena %in% c(8, 9) ~ 2))

## Check it's converted correctly
table(expenses_2021$indigena.num, useNA = "ifany")

# Step 2: Convert binary to factor with labels
expenses_2021 <- expenses_2021 %>%
  mutate(
    indigena.bin = factor(indigena.num, 
                          levels = c(0, 1, 2),
                          labels = c("No", "Yes", "No response")))


#### 15: speaks_indigen - create and relabel ----
table(expenses_2021$habla_lengua, useNA = "ifany")

# Step 1: Create numeric binary variable
expenses_2021 <- expenses_2021 %>%
  mutate(
    habla_lengua.num = case_when(
      habla_lengua == 1 ~ 1,
      habla_lengua == 2 ~ 0,
      habla_lengua %in% c(8, 9) ~ 2))

## Check it's converted correctly
table(expenses_2021$habla_lengua.num, useNA = "ifany")

# Step 2: Convert binary to factor with labels
expenses_2021 <- expenses_2021 %>%
  mutate(
    speaks_indigen = factor(habla_lengua.num,
                            levels = c(0, 1, 2),
                            labels = c("No", "Yes", "No response")))


#### 16: lengua_indigena - convert and relabel ----
unique(expenses_2021$lengua_indigena) # values not in our range - "NULL" "9999" "9998" "0450" "0811" "0922" "1021" "1041" "1013" "1014" "5000" "0934" "0941" "0933" "0935" "0931" "0823" "0611" "0491" "1032" "0481" "0824" "0451" "0331" "0712" "0332" "1111" "611"  "911"  "1031" "0200" "0400" "0461" "0943" "0441" "0422" "0511" "0456" "0471" "0433" "0711" "0911" "0311" "1022" "1311" and "0800" are being lumped into NA in the following code

#### Drop lengua_indigena
expenses_2021 <- expenses_2021 %>%
  select(-lengua_indigena)


#### 17: habla_esp no longer collected


#### 18: admiss_type - convert and relabel ----
expenses_2021 <- expenses_2021 %>%
  mutate(
    admiss_type = factor(tipserv,
                         levels = c("1", "2"),
                         labels = c("Normal", "Short stay")))

table(expenses_2021$tipserv, useNA = "ifany")
table(expenses_2021$admiss_type, useNA = "ifany")

#### 19: origin - convert and relabel ----
expenses_2021 <- expenses_2021 %>%
  mutate(
    origin = factor(proced,
                    levels = c("1", "2", "3", "4", "5", "9"),
                    labels = c("Outpatient", "Emergency", "Referred", "Pathological nursery", "Other", "Not specified")))

table(expenses_2021$proced, useNA = "ifany")
table(expenses_2021$origin, useNA = "ifany")

#### 20: discharge_res - convert and relabel ----
expenses_2021 <- expenses_2021 %>%
  mutate(
    discharge_res = factor(motegre,
                           levels = c("1", "2", "3", "4", "5", "6", "7", "9"),
                           labels = c(
                             "Recovery",
                             "Improvement",
                             "Voluntary discharge",
                             "Transfer",
                             "Death",
                             "Escape",
                             "Other",
                             "Not specified")))

table(expenses_2021$motegre, useNA = "ifany")
table(expenses_2021$discharge_res, useNA = "ifany")

#### 21: month_discharge - convert and relabel ----
expenses_2021 <- expenses_2021 %>%
  mutate(
    month_discharge = factor(mes_estadistico,
                             levels = as.character(1:12),
                             labels = c(
                               "January", "February", "March", "April", "May", "June",
                               "July", "August", "September", "October", "November", "December")))


#### 22: relationship_status - convert and relabel ----
expenses_2021 <- expenses_2021 %>%
  mutate(
    relationship_status = factor(estado_conyugal_key,
                                 levels = c("1", "5", "3", "2", "4", "6", "8", "9", "0"),
                                 labels = c(
                                   "Single",
                                   "Married",
                                   "Divorced",
                                   "Widowed",
                                   "Domestic partnership",
                                   "Separated",
                                   "Not applicable",
                                   "Unknown",
                                   "Not specified")))

table(expenses_2021$estado_conyugal_key, useNA = "ifany")
table(expenses_2021$relationship_status, useNA = "ifany")

#### 23: admiss_readmit - convert and relabel ----
expenses_2021 <- expenses_2021 %>%
  mutate(
    admiss_readmit = factor(vez,
                            levels = c("1", "2", "9"),
                            labels = c(
                              "First time",
                              "Subsequent",
                              "Not specified")))

table(expenses_2021$vez, useNA = "ifany")
table(expenses_2021$admiss_readmit, useNA = "ifany")

###### Rename variables some old obstet variables ----
expenses_2021 <- expenses_2021 %>%
  rename(
    gravida = gestas,
    parity = partos) 


##### Convert and relabel - hayprod ----
table(expenses_2021$hayprod, useNA = "ifany")

# Step 1: Create numeric binary variable
expenses_2021 <- expenses_2021 %>%
  mutate(
    hayprod.num = case_when(
      hayprod == 1 ~ 1,
      hayprod == 2 ~ 0,
      hayprod %in% c(8, 9) ~ 2))

## Check it's converted correctly
table(expenses_2021$hayprod.num, useNA = "ifany")

# Step 2: Convert binary to factor with labels
expenses_2021 <- expenses_2021 %>%
  mutate(
    hayprod.bin = factor(hayprod.num,
                         levels = c(0, 1, 2),
                         labels = c("No", "Yes", "No response")))

## Check it's converted correctly
table(expenses_2021$hayprod.bin, useNA = "ifany")


###### Factor and label planfam  ----
expenses_2021 <- expenses_2021 %>%
  mutate(    
    contracep = case_when(
      planfam %in% c(2, 3) ~ 2,  # Injectable
      TRUE ~ planfam) %>%
      factor(
        levels = c(0, 1, 2, 4, 5, 6, 7, 8, 9, 10, 11, 99, 88),
        labels = c(
          "None",
          "Pills",
          "Injectable",
          "Implant",
          "Copper IUD",
          "Female condom",
          "Male condom",
          "Medicated IUD",
          "Patch",
          "Permanent",
          "Other",
          "Unspecified",
          "Not applicable")))

table(expenses_2021$planfam, useNA = "ifany")
table(expenses_2021$contracep, useNA = "ifany")

###### Change 99's to NA in gestac  ----
expenses_2021 <- expenses_2021 %>%
  mutate(gestat = na_if(gestac, 99))



#### AFECCIONES ----------------------------------------------------------------

###### Import the conditions data  ----
conditions_2021 <- read.delim("DATA/ssa_egresos_2021/AFECCIONES.txt",
                              sep = "|",
                              header = T,
                              stringsAsFactors = F)

###### Make variables lowercase  ----
conditions_2021 <- conditions_2021 %>%
  clean_names()


###### Reshape to wide format based on numafec  ----
conditions_2021_wide <- conditions_2021 %>%
  select(id, afec) %>%
  group_by(id) %>%
  mutate(condition_n = row_number()) %>%       # Count afec per id
  ungroup() %>%
  pivot_wider(
    names_from = condition_n,
    names_prefix = "afec",
    values_from = afec)

##### Verify pivot ran correctly ----
sum(duplicated(conditions_2021_wide$id))



#### PROCEDIMIENTOS ------------------------------------------------------------

###### Import the procedures data  ----
procedures_2021 <- read.delim("DATA/ssa_egresos_2021/PROCEDIMIENTOS.txt",
                              sep = "|",
                              header = T,
                              stringsAsFactors = F)

###### Make variables lowercase  ----
procedures_2021 <- procedures_2021 %>%
  clean_names()

##### Drop unnecessary variables - tipo, anest, quirof  ----
procedures_2021 <- procedures_2021 %>%
  select(-tipo, -anest, -quirof, -tiempo_quirofano)

##### Keep only first 10 procedures per id and pivot to wide format ----
procedures_2021_wide <- procedures_2021 %>%
  group_by(id) %>%
  arrange(id) %>%
  slice_head(n = 10) %>%
  mutate(procedure_n = row_number()) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = id,
    names_from = procedure_n,
    names_prefix = "promed",
    values_from = promed)


##### Create procedure variables ----

# Step 1: Collapse all promed variables into a single string column per row
procedures_2021_wide <- procedures_2021_wide %>%
  mutate(promed_concat = paste(promed1, promed2, promed3, promed4, promed5,
                               promed6, promed7, promed8, promed9, promed10, sep = " "))

# Step 2: Use str_detect() to flag presence of each procedure code
procedures_2021_wide <- procedures_2021_wide %>%
  mutate(
    proc_dc_ab     = as.integer(str_detect(promed_concat, "\\b6901\\b")),
    proc_dc_post   = as.integer(str_detect(promed_concat, "\\b6902\\b")),
    proc_asp_ab    = as.integer(str_detect(promed_concat, "\\b6951\\b")),
    proc_asp_post  = as.integer(str_detect(promed_concat, "\\b6952\\b")),
    proc_miso_ab   = as.integer(str_detect(promed_concat, "\\b75A1\\b")),
    proc_miso_post = as.integer(str_detect(promed_concat, "\\b75A2\\b")),
    proc_oth_ab    = as.integer(str_detect(promed_concat, "\\b75A3\\b")),
    proc_oth_post  = as.integer(str_detect(promed_concat, "\\b75A4\\b")))







#### PRODUCTOS -----------------------------------------------------------------

###### Import the products data  ----
products_2021 <- read.delim("DATA/ssa_egresos_2021/PRODUCTOS.txt",
                            sep = "|",
                            header = T,
                            stringsAsFactors = F)

###### Make variables lowercase  ----
products_2021 <- products_2021 %>%
  clean_names()

##### Drop unnecessary variables - condegre, naviapag, navirean, lactancia_exclusiva  ----
products_2021 <- products_2021 %>%
  select(-condegre, -naviapag, -navirean, -lactancia_exclusiva, -alojamiento_conjunto) %>%
  distinct()

##### Pivot wider so each product becomes a column  ----
products_2021_wide <- products_2021 %>%
  filter(!is.na(condnac)) %>%
  mutate(numproducto = paste0("product_", numproducto)) %>%
  pivot_wider(
    id_cols = id,
    names_from = numproducto,
    values_from = condnac)


##### Convert to factors with labels (Fetal Death/Live Birth/Not Specified)  ----
products_2021_wide <- products_2021_wide %>%
  mutate(across(starts_with("product_"),
                ~ factor(.x,
                         levels = c(1, 2, 9),
                         labels = c("Fetal Death", "Live Birth", "Not Specified"))))



#### Merge with EGRESOS and save the cleaned data ------------------------------

# Convert id to character in all data sets
expenses_2021 <- expenses_2021 %>% mutate(id = as.character(id))
conditions_2021_wide <- conditions_2021_wide %>% mutate(id = as.character(id))
procedures_2021_wide <- procedures_2021_wide %>% mutate(id = as.character(id))
products_2021_wide <- products_2021_wide %>% mutate(id = as.character(id))

# Merge with EGRESOS 
final_2021 <- expenses_2021 %>%
  left_join(conditions_2021_wide, by = "id") %>%
  left_join(procedures_2021_wide, by = "id") %>%
  left_join(products_2021_wide, by = "id")

# Create year variable to differentiate years for final join
final_2021 <- final_2021 %>%
  mutate(year = 2021)





# --- 2022 ---------------------------------------------------------------------



#### EGRESO --------------------------------------------------------------------

###### Import the expenses data  ----
expenses_2022 <- read.delim("DATA/ssa_egresos_2022/EGRESOS.txt",
                            sep = "|",
                            header = T,
                            stringsAsFactors = F)

###### Make variables lowercase  ----
expenses_2022 <- expenses_2022 %>%
  clean_names()

#### 1: Drop everything except ICD-10 codes starting w/ O02-O08 and code Z303  ----
expenses_2022 <- expenses_2022 %>%
  rename(icd_code = afecprin) %>%
  filter(substr(icd_code, 1, 3) %in% c("O02", "O03", "O04", "O05", "O06", "O07", "O08") |
           icd_code == "Z303")

##### A. Table of frequencies of the primary ICD-10 codes  ----
table(expenses_2022$icd_code, useNA = "ifany")
nrow(expenses_2022)

#### 2: Drop unnecessary variables - tuhpsiq, servhc, servhp, nacioen, servicioingre, servicio02, servicio03, servicioegre, infec, mp, ttipaten, producto, tipnaci, cesareas  ----
expenses_2022 <- expenses_2022 %>%
  select(-tuhpsiq, -servhc, -servhp, -nacioen, -servicioingre,
         -servicio02, -servicio03, -servicioegre, -infec, -mp,
         -tipaten, -producto, -tipnaci, -cesareas)

#### 3: egreso, ingre - convert admission and discharge date variables ----
expenses_2022$egreso <- as.Date(expenses_2022$egreso)
expenses_2022$ingre  <- as.Date(expenses_2022$ingre)

#### 4: dias_esta - check outliers & negative values ----
summary(expenses_2022$dias_esta)

##### A. View negative values  ----
expenses_2022 %>%
  filter(dias_esta < 0) %>%
  select(id, ingre, egreso, dias_esta)

##### B. View extreme high values (> 30 days)  ----
expenses_2022 %>%
  filter(dias_esta > 30) %>%
  arrange(desc(dias_esta)) %>%
  select(id, ingre, egreso, dias_esta) %>%
  head(90)


#### 5: cveedad - check if anything is != 5 (not the age unit ~ years) ----

##### A. Check frequency of all cveedad values  ----
table(expenses_2022$cveedad, useNA = "ifany")

##### B. Identify rows where cveedad != 5  ----
expenses_2022 %>%
  filter(cveedad != 5) %>%
  select(id, cveedad, edad) %>%
  head() # 9 individuals with unknown ages?

##### C. Convert cveedad to factor with labeled levels and name age_unit
expenses_2022 <- expenses_2022 %>%
  mutate(age_unit = factor(cveedad,
                           levels = c(2, 3, 4, 5, 9),
                           labels = c("hours", "days", "months", "years", "Not specified")))

##### D. Create age groups for edad and rename to age
expenses_2022 <- expenses_2022 %>%
  mutate(
    age_group = case_when(
      edad < 10 ~ 1,
      edad >= 10 & edad <= 14 ~ 2,
      edad >= 15 & edad <= 19 ~ 3,
      edad >= 20 & edad <= 24 ~ 4,
      edad >= 25 & edad <= 29 ~ 5,
      edad >= 30 & edad <= 34 ~ 6,
      edad >= 35 & edad <= 39 ~ 7,
      edad >= 40 & edad <= 44 ~ 8,
      edad >= 45 & edad <= 49 ~ 9,
      edad >= 50 & edad <= 54 ~ 10,
      edad >= 55 ~ 11,
      TRUE ~ NA),
    age_group = factor(age_group, levels = 1:11,
                       labels = c("<10", "10–14", "15–19", "20–24", "25–29",
                                  "30–34", "35–39", "40–44", "45–49", "50–54", "55+")))

table(expenses_2022$age_group, useNA = "ifany")

#### 6: sex - check if anything != 2 ----

##### A. Check frequency of all sex values  ----
table(expenses_2022$sexo, useNA = "ifany")

##### B. sex - convert and relabel ----
expenses_2022 <- expenses_2022 %>%
  mutate(
    sex = case_when(
      sexo == "1" ~ "Male",
      sexo == "2" ~ "Female",
      sexo == "3" ~ "Intersex",
      sexo == "9" ~ "No response",
      TRUE ~ NA),
    sex = factor(sex, 
                 levels = c("Male", "Female", "Intersex", "No response")))


#### 7: weight and height - replace 999 values with NA ----
expenses_2022 <- expenses_2022 %>%
  mutate(
    weight = na_if(peso, 999),
    height = na_if(talla, 999))

# weight: ensure numeric
expenses_2022 <- expenses_2022 %>%
  mutate(weight = as.numeric(weight))

#### 8: insurance - rename and recode insurance types ----
expenses_2022 <- expenses_2022 %>%
  mutate(
    insurance = case_when(
      derhab == "0"  ~ "Uninsured",
      derhab == "1"  ~ "IMSS",
      derhab == "2"  ~ "ISSSTE",
      derhab == "3"  ~ "PEMEX",
      derhab == "4"  ~ "SEDENA",
      derhab == "5"  ~ "SEMAR",
      derhab == "6"  ~ "State government",
      derhab == "7"  ~ "Private insurance",
      derhab == "8"  ~ "Public insurance",
      derhab == "9"  ~ "Unknown",
      derhab == "10"  ~ "Other",
      derhab == "11"  ~ "INSABI",
      derhab == "G"  ~ "Gratuidad",
      derhab == "B"  ~ "IMSS BIENESTAR",
      derhab == "99"  ~ "Not Specified",
      TRUE ~ NA),
    insurance = factor(insurance,
                       levels = c("Uninsured", "IMSS", "ISSSTE", "PEMEX", "SEDENA",
                                  "SEMAR", "State government", "Private insurance", 
                                  "Public insurance", "Unknown", "Other", "INSABI",
                                  "Gratuidad", "IMSS BIENESTAR", "Not Specified")))

table(expenses_2022$insurance, useNA = "ifany")
table(expenses_2022$derhab, useNA = "ifany")

#### 9: state - recode variable values and rename  ----

# Step A: Check for non-Mexico residences
expenses_2022 %>%
  filter(entidad %in% c(37, 38)) %>%
  select(id, entidad) %>%
  head() 

# Step B: Recode state values to full names
expenses_2022 <- expenses_2022 %>%
  mutate(
    state = case_when(
      entidad == 1  ~ "Aguascalientes",
      entidad == 2  ~ "Baja California",
      entidad == 3  ~ "Baja California Sur",
      entidad == 4  ~ "Campeche",
      entidad == 5  ~ "Coahuila de Zaragoza",
      entidad == 6  ~ "Colima",
      entidad == 7  ~ "Chiapas",
      entidad == 8  ~ "Chihuahua",
      entidad == 9  ~ "Ciudad de México",
      entidad == 10 ~ "Durango",
      entidad == 11 ~ "Guanajuato",
      entidad == 12 ~ "Guerrero",
      entidad == 13 ~ "Hidalgo",
      entidad == 14 ~ "Jalisco",
      entidad == 15 ~ "México",
      entidad == 16 ~ "Michoacán de Ocampo",
      entidad == 17 ~ "Morelos",
      entidad == 18 ~ "Nayarit",
      entidad == 19 ~ "Nuevo León",
      entidad == 20 ~ "Oaxaca",
      entidad == 21 ~ "Puebla",
      entidad == 22 ~ "Querétaro de Arteaga",
      entidad == 23 ~ "Quintana Roo",
      entidad == 24 ~ "San Luis Potosí",
      entidad == 25 ~ "Sinaloa",
      entidad == 26 ~ "Sonora",
      entidad == 27 ~ "Tabasco",
      entidad == 28 ~ "Tamaulipas",
      entidad == 29 ~ "Tlaxcala",
      entidad == 30 ~ "Veracruz de Ignacio de la Llave",
      entidad == 31 ~ "Yucatán",
      entidad == 32 ~ "Zacatecas",
      entidad == 37 ~ "USA",
      entidad == 38 ~ "Rest of Latin America",
      entidad == 39 ~ "Other Continent",
      entidad == 88 ~ "Not Applicable",
      entidad == 99 ~ "No response",
      entidad == 0  ~ "No response",
      TRUE ~ NA),
    state = factor(state,
                   levels = c(
                     "Aguascalientes", "Baja California", "Baja California Sur", "Campeche",
                     "Coahuila de Zaragoza", "Colima", "Chiapas", "Chihuahua", "Ciudad de México",
                     "Durango", "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", "México",
                     "Michoacán de Ocampo", "Morelos", "Nayarit", "Nuevo León", "Oaxaca", "Puebla",
                     "Querétaro de Arteaga", "Quintana Roo", "San Luis Potosí", "Sinaloa", "Sonora",
                     "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz de Ignacio de la Llave",
                     "Yucatán", "Zacatecas", "USA", "Rest of Latin America", "Other Continent",
                     "Not Applicable", "No response")))

table(expenses_2022$state, useNA = "ifany")


#### 10: munic - convert to factor and pad with 0's ----
expenses_2022 <- expenses_2022 %>%
  mutate(
    munic = str_pad(as.character(munic), width = 3, pad = "0"),
    munic = factor(munic))

#### 11: entidad - convert to factor and pad with 0's ----
expenses_2022 <- expenses_2022 %>%
  mutate(
    entidad = str_pad(as.character(entidad), width = 2, pad = "0"),
    entidad = factor(entidad))

#### 12: munic_code - making the variable ----
expenses_2022 <- expenses_2022 %>%
  mutate(
    munic_code = paste0(entidad, munic))

#### 13: loc - convert to factor and pad with 0's ----
expenses_2022 <- expenses_2022 %>%
  mutate(
    loc = str_pad(as.character(loc), width = 4, pad = "0"),
    loc = factor(loc))

#### 14: indigena - make a new binary variable ----
table(expenses_2022$indigena, useNA = "ifany")

# Step 1: Create the binary variable
expenses_2022 <- expenses_2022 %>%
  mutate(
    indigena.num = case_when(
      indigena == 1 ~ 1,
      indigena == 2 ~ 0,
      indigena %in% c(8, 9) ~ 2))

## Check it's converted correctly
table(expenses_2022$indigena.num, useNA = "ifany")

# Step 2: Convert binary to factor with labels
expenses_2022 <- expenses_2022 %>%
  mutate(
    indigena.bin = factor(indigena.num, 
                          levels = c(0, 1, 2),
                          labels = c("No", "Yes", "No response")))


#### 15: speaks_indigen - create and relabel ----
table(expenses_2022$habla_lengua, useNA = "ifany")

# Step 1: Create numeric binary variable
expenses_2022 <- expenses_2022 %>%
  mutate(
    habla_lengua.num = case_when(
      habla_lengua == 1 ~ 1,
      habla_lengua == 2 ~ 0,
      habla_lengua %in% c(8, 9) ~ 2))

## Check it's converted correctly
table(expenses_2022$habla_lengua.num, useNA = "ifany")

# Step 2: Convert binary to factor with labels
expenses_2022 <- expenses_2022 %>%
  mutate(
    speaks_indigen = factor(habla_lengua.num,
                            levels = c(0, 1, 2),
                            labels = c("No", "Yes", "No response")))


#### 16: lengua_indigena - convert and relabel ----
unique(expenses_2022$lengua_indigena) # all values are not in our range??  -  8888  922 1041  471  121 1022  400  200 1023  450  453  933  611 5000  452 9999  481  934 1021 1013 1024 1014 811  491 1032 1031  441  456  461  823  422  451  511  911  311  331  712  482  931  935  981  972  332 1111  711  800  494  are being lumped into NA in the following code

#### Drop lengua_indigena
expenses_2022 <- expenses_2022 %>%
  select(-lengua_indigena)


#### 17: habla_esp no longer collected


#### 18: admiss_type - convert and relabel ----
expenses_2022 <- expenses_2022 %>%
  mutate(
    admiss_type = factor(tipserv,
                         levels = c("1", "2"),
                         labels = c("Normal", "Short stay")))

#### 19: origin - convert and relabel ----
expenses_2022 <- expenses_2022 %>%
  mutate(
    origin = factor(proced,
                    levels = c("1", "2", "3", "4", "5", "9"),
                    labels = c("Outpatient", "Emergency", "Referred", "Pathological nursery", "Other", "Not specified")))

table(expenses_2022$proced, useNA = "ifany")
table(expenses_2022$origin, useNA = "ifany")

#### 20: discharge_res - convert and relabel ----
expenses_2022 <- expenses_2022 %>%
  mutate(
    discharge_res = factor(motegre,
                           levels = c("1", "2", "3", "4", "5", "6", "7", "9"),
                           labels = c(
                             "Recovery",
                             "Improvement",
                             "Voluntary discharge",
                             "Transfer",
                             "Death",
                             "Escape",
                             "Other",
                             "Not specified")))

table(expenses_2022$motegre, useNA = "ifany")
table(expenses_2022$discharge_res, useNA = "ifany")

#### 21: month_discharge - convert and relabel ----
expenses_2022 <- expenses_2022 %>%
  mutate(
    month_discharge = factor(mes_estadistico,
                             levels = as.character(1:12),
                             labels = c(
                               "January", "February", "March", "April", "May", "June",
                               "July", "August", "September", "October", "November", "December")))


#### 22: relationship_status - convert and relabel ----
expenses_2022 <- expenses_2022 %>%
  mutate(
    relationship_status = factor(estado_conyugal_key,
                                 levels = c("1", "5", "3", "2", "4", "6", "8", "9", "0"),
                                 labels = c(
                                   "Single",
                                   "Married",
                                   "Divorced",
                                   "Widowed",
                                   "Domestic partnership",
                                   "Separated",
                                   "Not applicable",
                                   "Unknown",
                                   "Not specified")))

table(expenses_2022$estado_conyugal_key, useNA = "ifany")
table(expenses_2022$relationship_status, useNA = "ifany")

#### 23: admiss_readmit - convert and relabel ----
expenses_2022 <- expenses_2022 %>%
  mutate(
    admiss_readmit = factor(vez,
                            levels = c("1", "2", "9"),
                            labels = c(
                              "First time",
                              "Subsequent",
                              "Not specified")))




###### Rename variables some old obstet variables ----
expenses_2022 <- expenses_2022 %>%
  rename(
    gravida = gestas,
    parity = partos)


##### Convert and relabel - hayprod ----
table(expenses_2022$hayprod, useNA = "ifany")

# Step 1: Create numeric binary variable
expenses_2022 <- expenses_2022 %>%
  mutate(
    hayprod.num = case_when(
      hayprod == 1 ~ 1,
      hayprod == 2 ~ 0,
      hayprod %in% c(8, 9) ~ 2))

## Check it's converted correctly
table(expenses_2022$hayprod.num, useNA = "ifany")

# Step 2: Convert binary to factor with labels
expenses_2022 <- expenses_2022 %>%
  mutate(
    hayprod.bin = factor(hayprod.num,
                         levels = c(0, 1, 2),
                         labels = c("No", "Yes", "No response")))

## Check it's converted correctly
table(expenses_2022$hayprod.bin, useNA = "ifany")


###### Factor and label planfam  ----
expenses_2022 <- expenses_2022 %>%
  mutate(    
    contracep = case_when(
      planfam %in% c(2, 3) ~ 2,  # Injectable
      TRUE ~ planfam) %>%
      factor(
        levels = c(0, 1, 2, 4, 5, 6, 7, 8, 9, 10, 11, 99, 88),
        labels = c(
          "None",
          "Pills",
          "Injectable",
          "Implant",
          "Copper IUD",
          "Female condom",
          "Male condom",
          "Medicated IUD",
          "Patch",
          "Permanent",
          "Other",
          "Unspecified",
          "Not applicable")))

table(expenses_2022$planfam, useNA = "ifany")
table(expenses_2022$contracep, useNA = "ifany")

###### Change 99's to NA in gestac  ----
expenses_2022 <- expenses_2022 %>%
  mutate(gestat = na_if(gestac, 99))



#### AFECCIONES ----------------------------------------------------------------

###### Import the conditions data  ----
conditions_2022 <- read.delim("DATA/ssa_egresos_2022/AFECCIONES.txt",
                              sep = "|",
                              header = T,
                              stringsAsFactors = F)

###### Make variables lowercase  ----
conditions_2022 <- conditions_2022 %>%
  clean_names()


###### Reshape to wide format based on numafec  ----
conditions_2022_wide <- conditions_2022 %>%
  select(id, afec) %>%
  group_by(id) %>%
  mutate(condition_n = row_number()) %>%       # Count afec per id
  ungroup() %>%
  pivot_wider(
    names_from = condition_n,
    names_prefix = "afec",
    values_from = afec)

##### Verify pivot ran correctly ----
sum(duplicated(conditions_2022_wide$id))



#### PROCEDIMIENTOS ------------------------------------------------------------

###### Import the procedures data  ----
procedures_2022 <- read.delim("DATA/ssa_egresos_2022/PROCEDIMIENTOS.txt",
                              sep = "|",
                              header = T,
                              stringsAsFactors = F)

###### Make variables lowercase  ----
procedures_2022 <- procedures_2022 %>%
  clean_names()

##### Drop unnecessary variables - tipo, anest, quirof  ----
procedures_2022 <- procedures_2022 %>%
  select(-tipo, -anest, -quirof, -tiempo_quirofano)

##### Keep only first 10 procedures per id and pivot to wide format ----
procedures_2022_wide <- procedures_2022 %>%
  group_by(id) %>%
  arrange(id) %>%
  slice_head(n = 10) %>%
  mutate(procedure_n = row_number()) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = id,
    names_from = procedure_n,
    names_prefix = "promed",
    values_from = promed)


##### Create procedure variables ----

# Step 1: Collapse all promed variables into a single string column per row
procedures_2022_wide <- procedures_2022_wide %>%
  mutate(promed_concat = paste(promed1, promed2, promed3, promed4, promed5,
                               promed6, promed7, promed8, sep = " ")) #only 8 procedures codes found

# Step 2: Use str_detect() to flag presence of each procedure code
procedures_2022_wide <- procedures_2022_wide %>%
  mutate(
    proc_dc_ab     = as.integer(str_detect(promed_concat, "\\b6901\\b")),
    proc_dc_post   = as.integer(str_detect(promed_concat, "\\b6902\\b")),
    proc_asp_ab    = as.integer(str_detect(promed_concat, "\\b6951\\b")),
    proc_asp_post  = as.integer(str_detect(promed_concat, "\\b6952\\b")),
    proc_miso_ab   = as.integer(str_detect(promed_concat, "\\b75A1\\b")),
    proc_miso_post = as.integer(str_detect(promed_concat, "\\b75A2\\b")),
    proc_oth_ab    = as.integer(str_detect(promed_concat, "\\b75A3\\b")),
    proc_oth_post  = as.integer(str_detect(promed_concat, "\\b75A4\\b")))






#### PRODUCTOS -----------------------------------------------------------------

###### Import the products data  ----
products_2022 <- read.delim("DATA/ssa_egresos_2022/PRODUCTOS.txt",
                            sep = "|",
                            header = T,
                            stringsAsFactors = F)

###### Make variables lowercase  ----
products_2022 <- products_2022 %>%
  clean_names()

##### Drop unnecessary variables - condegre, naviapag, navirean, lactancia_exclusiva  ----
products_2022 <- products_2022 %>%
  select(-condegre, -naviapag, -navirean, -lactancia_exclusiva, -alojamiento_conjunto) %>%
  distinct()

##### Pivot wider so each product becomes a column  ----
products_2022_wide <- products_2022 %>%
  filter(!is.na(condnac)) %>%
  mutate(numproducto = paste0("product_", numproducto)) %>%
  pivot_wider(
    id_cols = id,
    names_from = numproducto,
    values_from = condnac)


##### Convert to factors with labels (Fetal Death/Live Birth/Not Specified)  ----
products_2022_wide <- products_2022_wide %>%
  mutate(across(starts_with("product_"),
                ~ factor(.x,
                         levels = c(1, 2, 9),
                         labels = c("Fetal Death", "Live Birth", "Not Specified"))))



#### Merge with EGRESOS and save the cleaned data ------------------------------

# Convert id to character in all data sets
expenses_2022 <- expenses_2022 %>% mutate(id = as.character(id))
conditions_2022_wide <- conditions_2022_wide %>% mutate(id = as.character(id))
procedures_2022_wide <- procedures_2022_wide %>% mutate(id = as.character(id))
products_2022_wide <- products_2022_wide %>% mutate(id = as.character(id))

# Merge with EGRESOS 
final_2022 <- expenses_2022 %>%
  left_join(conditions_2022_wide, by = "id") %>%
  left_join(procedures_2022_wide, by = "id") %>%
  left_join(products_2022_wide, by = "id")

# Create year variable to differentiate years for final join
final_2022 <- final_2022 %>%
  mutate(year = 2022)




# --- 2023 ---------------------------------------------------------------------



#### EGRESO --------------------------------------------------------------------

###### Import the expenses data  ----
expenses_2023 <- read.delim("DATA/ssa_egresos_2023/EGRESOS.txt",
                            sep = "|",
                            header = T,
                            stringsAsFactors = F)

###### Make variables lowercase  ----
expenses_2023 <- expenses_2023 %>%
  clean_names()

#### 1: Drop everything except ICD-10 codes starting w/ O02-O08 and code Z303  ----
expenses_2023 <- expenses_2023 %>%
  rename(icd_code = afecprin) %>%
  filter(substr(icd_code, 1, 3) %in% c("O02", "O03", "O04", "O05", "O06", "O07", "O08") |
           icd_code == "Z303")

##### A. Table of frequencies of the primary ICD-10 codes  ----
table(expenses_2023$icd_code, useNA = "ifany")
nrow(expenses_2023)

#### 2: Drop unnecessary variables - tuhpsiq, servhc, servhp, nacioen, servicioingre, servicio02, servicio03, servicioegre, infec, mp, ttipaten, producto, tipnaci, cesareas  ----
expenses_2023 <- expenses_2023 %>%
  select(-tuhpsiq, -servhc, -servhp, -nacioen, -servicioingre,
         -servicio02, -servicio03, -servicioegre, -infec, -mp,
         -tipaten, -producto, -tipnaci, -cesareas)

#### 3: egreso, ingre - convert admission and discharge date variables ----
expenses_2023$egreso <- as.Date(expenses_2023$egreso)
expenses_2023$ingre  <- as.Date(expenses_2023$ingre)

#### 4: dias_esta - check outliers & negative values ----
summary(expenses_2023$dias_esta)

##### A. View negative values  ----
expenses_2023 %>%
  filter(dias_esta < 0) %>%
  select(id, ingre, egreso, dias_esta)

##### B. View extreme high values (> 30 days)  ----
expenses_2023 %>%
  filter(dias_esta > 30) %>%
  arrange(desc(dias_esta)) %>%
  select(id, ingre, egreso, dias_esta) %>%
  head(90)


#### 5: cveedad - check if anything is != 5 (not the age unit ~ years) ----

##### A. Check frequency of all cveedad values  ----
table(expenses_2023$cveedad, useNA = "ifany")

##### B. Identify rows where cveedad != 5  ----
expenses_2023 %>%
  filter(cveedad != 5) %>%
  select(id, cveedad, edad) %>%
  head() # 1 individual with an unknown age?

##### C. Convert cveedad to factor with labeled levels and name age_unit
expenses_2023 <- expenses_2023 %>%
  mutate(age_unit = factor(cveedad,
                           levels = c(2, 3, 4, 5, 9),
                           labels = c("hours", "days", "months", "years", "Not specified")))

##### D. Create age groups for edad and rename to age
expenses_2023 <- expenses_2023 %>%
  mutate(
    age_group = case_when(
      edad < 10 ~ 1,
      edad >= 10 & edad <= 14 ~ 2,
      edad >= 15 & edad <= 19 ~ 3,
      edad >= 20 & edad <= 24 ~ 4,
      edad >= 25 & edad <= 29 ~ 5,
      edad >= 30 & edad <= 34 ~ 6,
      edad >= 35 & edad <= 39 ~ 7,
      edad >= 40 & edad <= 44 ~ 8,
      edad >= 45 & edad <= 49 ~ 9,
      edad >= 50 & edad <= 54 ~ 10,
      edad >= 55 ~ 11,
      TRUE ~ NA),
    age_group = factor(age_group, levels = 1:11,
                       labels = c("<10", "10–14", "15–19", "20–24", "25–29",
                                  "30–34", "35–39", "40–44", "45–49", "50–54", "55+")))

table(expenses_2023$age_group, useNA = "ifany")

#### 6: sex - check if anything != 2 ----

##### A. Check frequency of all sex values  ----
table(expenses_2023$sexo, useNA = "ifany")

##### B. sex - convert and relabel ----
expenses_2023 <- expenses_2023 %>%
  mutate(
    sex = case_when(
      sexo == "1" ~ "Male",
      sexo == "2" ~ "Female",
      sexo == "3" ~ "Intersex",
      sexo == "9" ~ "No response",
      TRUE ~ NA),
    sex = factor(sex, 
                 levels = c("Male", "Female", "Intersex", "No response")))


#### 7: weight and height - replace 999 values with NA ----
expenses_2023 <- expenses_2023 %>%
  mutate(
    weight = na_if(peso, 999),
    height = na_if(talla, 999))

# weight: ensure numeric
expenses_2023 <- expenses_2023 %>%
  mutate(weight = as.numeric(weight))

#### 8: insurance - rename and recode insurance types ----
expenses_2023 <- expenses_2023 %>%
  mutate(
    insurance = case_when(
      derhab == "0"  ~ "No response",
      derhab == "1"  ~ "Uninsured",
      derhab == "2"  ~ "IMSS",
      derhab == "3"  ~ "ISSSTE",
      derhab == "4"  ~ "PEMEX",
      derhab == "5"  ~ "SEDENA",
      derhab == "6"  ~ "SEMAR",
      derhab == "8"  ~ "Other",
      derhab == "10"  ~ "IMSS BIENESTAR",
      derhab == "11"  ~ "ISSFAM",
      derhab == "13"  ~ "INSABI",
      derhab == "14"  ~ "OPD IMSS BIENESTAR",
      derhab == "G"  ~ "Gratuidad",
      derhab == "99"  ~ "No response",
      TRUE ~ NA),
    insurance = factor(insurance,
                       levels = c("Uninsured", "IMSS", "ISSSTE", "PEMEX",
                                  "SEDENA", "SEMAR", "Other", "IMSS BIENESTAR", "ISSFAM",
                                  "INSABI", "OPD IMSS BIENESTAR", "Gratuidad", "No response")))

table(expenses_2023$insurance, useNA = "ifany")
table(expenses_2023$derhab, useNA = "ifany")


#### 9: state - recode variable values and rename  ----

# Step A: Check for non-Mexico residences
expenses_2023 %>%
  filter(entidad %in% c(37, 38)) %>%
  select(id, entidad) %>%
  head() 

# Step B: Recode state values to full names
expenses_2023 <- expenses_2023 %>%
  mutate(
    state = case_when(
      entidad == 1  ~ "Aguascalientes",
      entidad == 2  ~ "Baja California",
      entidad == 3  ~ "Baja California Sur",
      entidad == 4  ~ "Campeche",
      entidad == 5  ~ "Coahuila de Zaragoza",
      entidad == 6  ~ "Colima",
      entidad == 7  ~ "Chiapas",
      entidad == 8  ~ "Chihuahua",
      entidad == 9  ~ "Ciudad de México",
      entidad == 10 ~ "Durango",
      entidad == 11 ~ "Guanajuato",
      entidad == 12 ~ "Guerrero",
      entidad == 13 ~ "Hidalgo",
      entidad == 14 ~ "Jalisco",
      entidad == 15 ~ "México",
      entidad == 16 ~ "Michoacán de Ocampo",
      entidad == 17 ~ "Morelos",
      entidad == 18 ~ "Nayarit",
      entidad == 19 ~ "Nuevo León",
      entidad == 20 ~ "Oaxaca",
      entidad == 21 ~ "Puebla",
      entidad == 22 ~ "Querétaro de Arteaga",
      entidad == 23 ~ "Quintana Roo",
      entidad == 24 ~ "San Luis Potosí",
      entidad == 25 ~ "Sinaloa",
      entidad == 26 ~ "Sonora",
      entidad == 27 ~ "Tabasco",
      entidad == 28 ~ "Tamaulipas",
      entidad == 29 ~ "Tlaxcala",
      entidad == 30 ~ "Veracruz de Ignacio de la Llave",
      entidad == 31 ~ "Yucatán",
      entidad == 32 ~ "Zacatecas",
      entidad == 37 ~ "USA",
      entidad == 38 ~ "Rest of Latin America",
      entidad == 39 ~ "Other Continent",
      entidad == 88 ~ "Not Applicable",
      entidad == 99 ~ "No response",
      entidad == 0  ~ "No response",
      TRUE ~ NA),
    state = factor(state,
                   levels = c(
                     "Aguascalientes", "Baja California", "Baja California Sur", "Campeche",
                     "Coahuila de Zaragoza", "Colima", "Chiapas", "Chihuahua", "Ciudad de México",
                     "Durango", "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", "México",
                     "Michoacán de Ocampo", "Morelos", "Nayarit", "Nuevo León", "Oaxaca", "Puebla",
                     "Querétaro de Arteaga", "Quintana Roo", "San Luis Potosí", "Sinaloa", "Sonora",
                     "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz de Ignacio de la Llave",
                     "Yucatán", "Zacatecas", "USA", "Rest of Latin America", "Other Continent",
                     "Not Applicable", "No response")))

table(expenses_2023$state, useNA = "ifany")


#### 10: munic - convert to factor and pad with 0's ----
expenses_2023 <- expenses_2023 %>%
  mutate(
    munic = str_pad(as.character(munic), width = 3, pad = "0"),
    munic = factor(munic))

#### 11: entidad - convert to factor and pad with 0's ----
expenses_2023 <- expenses_2023 %>%
  mutate(
    entidad = str_pad(as.character(entidad), width = 2, pad = "0"),
    entidad = factor(entidad))

#### 12: munic_code - making the variable ----
expenses_2023 <- expenses_2023 %>%
  mutate(
    munic_code = paste0(entidad, munic))

#### 13: loc - convert to factor and pad with 0's ----
expenses_2023 <- expenses_2023 %>%
  mutate(
    loc = str_pad(as.character(loc), width = 4, pad = "0"),
    loc = factor(loc))

#### 14: indigena - make a new binary variable ----
table(expenses_2023$indigena, useNA = "ifany")

# Step 1: Create the binary variable
expenses_2023 <- expenses_2023 %>%
  mutate(
    indigena.num = case_when(
      indigena == 1 ~ 1,
      indigena == 2 ~ 0,
      indigena %in% c(8, 9) ~ 2))

## Check it's converted correctly
table(expenses_2023$indigena.num, useNA = "ifany")

# Step 2: Convert binary to factor with labels
expenses_2023 <- expenses_2023 %>%
  mutate(
    indigena.bin = factor(indigena.num, 
                          levels = c(0, 1, 2),
                          labels = c("No", "Yes", "No response")))


#### 15: speaks_indigen - create and relabel ----
table(expenses_2023$habla_lengua, useNA = "ifany")

# Step 1: Create numeric binary variable
expenses_2023 <- expenses_2023 %>%
  mutate(
    habla_lengua.num = case_when(
      habla_lengua == 1 ~ 1,
      habla_lengua == 2 ~ 0,
      habla_lengua %in% c(8, 9) ~ 2))

## Check it's converted correctly
table(expenses_2023$habla_lengua.num, useNA = "ifany")

# Step 2: Convert binary to factor with labels
expenses_2023 <- expenses_2023 %>%
  mutate(
    speaks_indigen = factor(habla_lengua.num,
                            levels = c(0, 1, 2),
                            labels = c("No", "Yes", "No response")))


#### 16: lengua_indigena - convert and relabel ----
unique(expenses_2023$lengua_indigena) # all values are not in our range??

#### Drop lengua_indigena
expenses_2023 <- expenses_2023 %>%
  select(-lengua_indigena)


#### 17: habla_esp no longer collected


#### 18: admiss_type - convert and relabel ----
expenses_2023 <- expenses_2023 %>%
  mutate(
    admiss_type = factor(tipserv,
                         levels = c("1", "2"),
                         labels = c("Normal", "Short stay")))

#### 19: origin - convert and relabel ----
expenses_2023 <- expenses_2023 %>%
  mutate(
    origin = factor(proced,
                    levels = c("1", "2", "3", "4", "5", "9"),
                    labels = c("Outpatient", "Emergency", "Referred", "Other", "Pathological nursery", "Not specified")))

table(expenses_2023$proced, useNA = "ifany")
table(expenses_2023$origin, useNA = "ifany")

#### 20: discharge_res - convert and relabel ----
expenses_2023 <- expenses_2023 %>%
  mutate(
    discharge_res = factor(motegre,
                           levels = c("1", "2", "3", "4", "5", "6", "7", "9"),
                           labels = c(
                             "Recovery",
                             "Improvement",
                             "Voluntary discharge",
                             "Transfer",
                             "Death",
                             "Escape",
                             "Other",
                             "Not specified")))

table(expenses_2023$discharge_res, useNA = "ifany")

#### 21: month_discharge - convert and relabel ----
expenses_2023 <- expenses_2023 %>%
  mutate(
    month_discharge = factor(mes_estadistico,
                             levels = as.character(1:12),
                             labels = c(
                               "January", "February", "March", "April", "May", "June",
                               "July", "August", "September", "October", "November", "December")))


#### 22: relationship_status - convert and relabel ----
expenses_2023 <- expenses_2023 %>%
  mutate(
    relationship_status = factor(estado_conyugal_key,
                                 levels = c("1", "5", "3", "2", "4", "6", "8", "9", "0"),
                                 labels = c(
                                   "Single",
                                   "Married",
                                   "Divorced",
                                   "Widowed",
                                   "Domestic partnership",
                                   "Separated",
                                   "Not applicable",
                                   "Unknown",
                                   "Not specified")))

table(expenses_2023$relationship_status, useNA = "ifany")

#### 23: admiss_readmit - convert and relabel ----
expenses_2023 <- expenses_2023 %>%
  mutate(
    admiss_readmit = factor(vez,
                            levels = c("1", "2", "9"),
                            labels = c(
                              "First time",
                              "Subsequent",
                              "Not specified")))




###### Rename variables some old obstet variables ----
expenses_2023 <- expenses_2023 %>%
  rename(
    gravida = gestas,
    parity = partos)  


##### Convert and relabel - hayprod ----
table(expenses_2023$hayprod, useNA = "ifany")

# Step 1: Create numeric binary variable
expenses_2023 <- expenses_2023 %>%
  mutate(
    hayprod.num = case_when(
      hayprod == 1 ~ 1,
      hayprod == 2 ~ 0,
      hayprod %in% c(8, 9) ~ 2))

## Check it's converted correctly
table(expenses_2023$hayprod.num, useNA = "ifany")

# Step 2: Convert binary to factor with labels
expenses_2023 <- expenses_2023 %>%
  mutate(
    hayprod.bin = factor(hayprod.num,
                         levels = c(0, 1, 2),
                         labels = c("No", "Yes", "No response")))

## Check it's converted correctly
table(expenses_2023$hayprod.bin, useNA = "ifany")


###### Factor and label planfam  ----
expenses_2023 <- expenses_2023 %>%
  mutate(
    contracep = case_when(
      planfam %in% c(2, 3, 12) ~ 2,  # Injectable
      planfam %in% c(4, 13) ~ 3,     # Implant
      TRUE ~ planfam) %>%
      factor(
        levels = c(0, 1, 2, 3, 5, 6, 7, 8, 9, 10, 11, 99, 88),
        labels = c(
          "None",
          "Pills",
          "Injectable",
          "Implant",    
          "Copper IUD",
          "Female condom",
          "Male condom",
          "Medicated IUD",
          "Patch",
          "Permanent",
          "Other",
          "Unspecified",
          "Not applicable")))

table(expenses_2023$contracep, useNA = "ifany")

###### Change 99's to NA in gestac  ----
expenses_2023 <- expenses_2023 %>%
  mutate(gestat = na_if(gestac, 99))



#### AFECCIONES ----------------------------------------------------------------

###### Import the conditions data  ----
conditions_2023 <- read.delim("DATA/ssa_egresos_2023/AFECCIONES.txt",
                              sep = "|",
                              header = T,
                              stringsAsFactors = F)

###### Make variables lowercase  ----
conditions_2023 <- conditions_2023 %>%
  clean_names()


###### Reshape to wide format based on numafec  ----
conditions_2023_wide <- conditions_2023 %>%
  select(id, afec) %>%
  group_by(id) %>%
  mutate(condition_n = row_number()) %>%       # Count afec per id
  ungroup() %>%
  pivot_wider(
    names_from = condition_n,
    names_prefix = "afec",
    values_from = afec)

##### Verify pivot ran correctly ----
sum(duplicated(conditions_2023_wide$id))



#### PROCEDIMIENTOS ------------------------------------------------------------

###### Import the procedures data  ----
procedures_2023 <- read.delim("DATA/ssa_egresos_2023/PROCEDIMIENTOS.txt",
                              sep = "|",
                              header = T,
                              stringsAsFactors = F)

###### Make variables lowercase  ----
procedures_2023 <- procedures_2023 %>%
  clean_names()

##### Drop unnecessary variables - tipo, anest, quirof  ----
procedures_2023 <- procedures_2023 %>%
  select(-tipo, -anest, -quirof, -tiempo_quirofano)

##### Keep only first 10 procedures per id and pivot to wide format ----
procedures_2023_wide <- procedures_2023 %>%
  group_by(id) %>%
  arrange(id) %>%
  slice_head(n = 10) %>%
  mutate(procedure_n = row_number()) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = id,
    names_from = procedure_n,
    names_prefix = "promed",
    values_from = promed)


##### Create procedure variables ----

# Step 1: Collapse all promed variables into a single string column per row
procedures_2023_wide <- procedures_2023_wide %>%
  mutate(promed_concat = paste(promed1, promed2, promed3, promed4, promed5,
                               promed6, promed7, promed8, sep = " ")) #only 8 procedures found

# Step 2: Use str_detect() to flag presence of each procedure code
procedures_2023_wide <- procedures_2023_wide %>%
  mutate(
    proc_dc_ab     = as.integer(str_detect(promed_concat, "\\b6901\\b")),
    proc_dc_post   = as.integer(str_detect(promed_concat, "\\b6902\\b")),
    proc_asp_ab    = as.integer(str_detect(promed_concat, "\\b6951\\b")),
    proc_asp_post  = as.integer(str_detect(promed_concat, "\\b6952\\b")),
    proc_miso_ab   = as.integer(str_detect(promed_concat, "\\b75A1\\b")),
    proc_miso_post = as.integer(str_detect(promed_concat, "\\b75A2\\b")),
    proc_oth_ab    = as.integer(str_detect(promed_concat, "\\b75A3\\b")),
    proc_oth_post  = as.integer(str_detect(promed_concat, "\\b75A4\\b")))






#### PRODUCTOS -----------------------------------------------------------------

###### Import the products data  ----
products_2023 <- read.delim("DATA/ssa_egresos_2023/PRODUCTOS.txt",
                            sep = "|",
                            header = T,
                            stringsAsFactors = F)

###### Make variables lowercase  ----
products_2023 <- products_2023 %>%
  clean_names()

##### Drop unnecessary variables - condegre, naviapag, navirean, lactancia_exclusiva  ----
products_2023 <- products_2023 %>%
  select(-condegre, -naviapag, -navirean, -lactancia_exclusiva, -alojamiento_conjunto) %>%
  distinct()

##### Pivot wider so each product becomes a column  ----
products_2023_wide <- products_2023 %>%
  filter(!is.na(condnac)) %>%
  mutate(numproducto = paste0("product_", numproducto)) %>%
  pivot_wider(
    id_cols = id,
    names_from = numproducto,
    values_from = condnac)


##### Convert to factors with labels (Fetal Death/Live Birth/Not Specified)  ----
products_2023_wide <- products_2023_wide %>%
  mutate(across(starts_with("product_"),
                ~ factor(.x,
                         levels = c(1, 2, 9),
                         labels = c("Fetal Death", "Live Birth", "Not Specified"))))



#### Merge with EGRESOS and save the cleaned data ------------------------------

# Convert id to character in all data sets
expenses_2023 <- expenses_2023 %>% mutate(id = as.character(id))
conditions_2023_wide <- conditions_2023_wide %>% mutate(id = as.character(id))
procedures_2023_wide <- procedures_2023_wide %>% mutate(id = as.character(id))
products_2023_wide <- products_2023_wide %>% mutate(id = as.character(id))

# Merge with EGRESOS 
final_2023 <- expenses_2023 %>%
  left_join(conditions_2023_wide, by = "id") %>%
  left_join(procedures_2023_wide, by = "id") %>%
  left_join(products_2023_wide, by = "id")

# Create year variable to differentiate years for final join
final_2023 <- final_2023 %>%
  mutate(year = 2023)







# --- 2024 ---------------------------------------------------------------------



#### EGRESO --------------------------------------------------------------------

###### Import the expenses data  ----
expenses_2024 <- read.delim("DATA/ssa_egresos_2024/EGRESOS.txt",
                            sep = "|",
                            header = T,
                            stringsAsFactors = F)

###### Make variables lowercase  ----
expenses_2024 <- expenses_2024 %>%
  clean_names()

#### 1: Drop everything except ICD-10 codes starting w/ O02-O08 and code Z303  ----
expenses_2024 <- expenses_2024 %>%
  rename(icd_code = afecprin) %>%
  filter(substr(icd_code, 1, 3) %in% c("O02", "O03", "O04", "O05", "O06", "O07", "O08") |
           icd_code == "Z303")

##### A. Table of frequencies of the primary ICD-10 codes  ----
table(expenses_2024$icd_code, useNA = "ifany")
nrow(expenses_2024)

#### 2: Drop unnecessary variables - tuhpsiq, servhc, servhp, nacioen, servicioingre, servicio02, servicio03, servicioegre, infec, mp, ttipaten, producto, tipnaci, cesareas  ----
expenses_2024 <- expenses_2024 %>%
  select(-tuhpsiq, -servhc, -servhp, -nacioen, -servicioingre,
         -servicio02, -servicio03, -servicioegre, -infec, -mp,
         -tipaten, -producto, -tipnaci, -cesareas)

#### 3: egreso, ingre - convert admission and discharge date variables ----
expenses_2024$egreso <- as.Date(expenses_2024$egreso)
expenses_2024$ingre  <- as.Date(expenses_2024$ingre)

#### 4: dias_esta - check outliers & negative values ----
summary(expenses_2024$dias_esta)

##### A. View negative values  ----
expenses_2024 %>%
  filter(dias_esta < 0) %>%
  select(id, ingre, egreso, dias_esta)

##### B. View extreme high values (> 30 days)  ----
expenses_2024 %>%
  filter(dias_esta > 30) %>%
  arrange(desc(dias_esta)) %>%
  select(id, ingre, egreso, dias_esta) %>%
  head(90)


#### 5: cveedad - check if anything is != 5 (not the age unit ~ years) ----

##### A. Check frequency of all cveedad values  ----
table(expenses_2024$cveedad, useNA = "ifany")

##### B. Identify rows where cveedad != 5  ----
expenses_2024 %>%
  filter(cveedad != 5) %>%
  select(id, cveedad, edad) %>%
  head() # 10 individuals with unknown ages?

##### C. Convert cveedad to factor with labeled levels and name age_unit
expenses_2024 <- expenses_2024 %>%
  mutate(age_unit = factor(cveedad,
                           levels = c(2, 3, 4, 5, 9),
                           labels = c("hours", "days", "months", "years", "Not specified")))

##### D. Create age groups for edad and rename to age
expenses_2024 <- expenses_2024 %>%
  mutate(
    age_group = case_when(
      edad < 10 ~ 1,
      edad >= 10 & edad <= 14 ~ 2,
      edad >= 15 & edad <= 19 ~ 3,
      edad >= 20 & edad <= 24 ~ 4,
      edad >= 25 & edad <= 29 ~ 5,
      edad >= 30 & edad <= 34 ~ 6,
      edad >= 35 & edad <= 39 ~ 7,
      edad >= 40 & edad <= 44 ~ 8,
      edad >= 45 & edad <= 49 ~ 9,
      edad >= 50 & edad <= 54 ~ 10,
      edad >= 55 ~ 11,
      TRUE ~ NA),
    age_group = factor(age_group, levels = 1:11,
                       labels = c("<10", "10–14", "15–19", "20–24", "25–29",
                                  "30–34", "35–39", "40–44", "45–49", "50–54", "55+")))

table(expenses_2024$age_group, useNA = "ifany")

#### 6: sex - check if anything != 2 ----

##### A. Check frequency of all sex values  ----
table(expenses_2024$sexo, useNA = "ifany")

##### B. sex - convert and relabel ----
expenses_2024 <- expenses_2024 %>%
  mutate(
    sex = case_when(
      sexo == "1" ~ "Male",
      sexo == "2" ~ "Female",
      sexo == "3" ~ "Intersex",
      sexo == "9" ~ "No response",
      TRUE ~ NA),
    sex = factor(sex, 
                 levels = c("Male", "Female", "Intersex", "No response")))


#### 7: weight and height - replace 999 values with NA ----
expenses_2024 <- expenses_2024 %>%
  mutate(
    weight = na_if(peso, 999),
    height = na_if(talla, 999))

# weight: ensure numeric
expenses_2024 <- expenses_2024 %>%
  mutate(weight = as.numeric(weight))

#### 8: insurance - rename and recode insurance types ----
expenses_2024 <- expenses_2024 %>%
  mutate(
    insurance = case_when(
      derhab == "0"  ~ "No response",
      derhab == "1"  ~ "Uninsured",
      derhab == "2"  ~ "IMSS",
      derhab == "3"  ~ "ISSSTE",
      derhab == "4"  ~ "PEMEX",
      derhab == "5"  ~ "SEDENA",
      derhab == "6"  ~ "SEMAR",
      derhab == "8"  ~ "Other",
      derhab == "10"  ~ "IMSS BIENESTAR",
      derhab == "11"  ~ "ISSFAM",
      derhab == "13"  ~ "INSABI",
      derhab == "14"  ~ "OPD IMSS BIENESTAR",
      derhab == "G"  ~ "Gratuidad",
      derhab == "99"  ~ "No response",
      TRUE ~ NA),
    insurance = factor(insurance,
                       levels = c("Uninsured", "IMSS", "ISSSTE", "PEMEX",
                                  "SEDENA", "SEMAR", "Other", "IMSS BIENESTAR", "ISSFAM",
                                  "INSABI", "OPD IMSS BIENESTAR", "Gratuidad", "No response")))

table(expenses_2024$insurance, useNA = "ifany")
table(expenses_2024$derhab, useNA = "ifany")


#### 9: state - recode variable values and rename  ----

# Step A: Check for non-Mexico residences
expenses_2024 %>%
  filter(entidad %in% c(37, 38)) %>%
  select(id, entidad) %>%
  head() 

# Step B: Recode state values to full names
expenses_2024 <- expenses_2024 %>%
  mutate(
    state = case_when(
      entidad == 1  ~ "Aguascalientes",
      entidad == 2  ~ "Baja California",
      entidad == 3  ~ "Baja California Sur",
      entidad == 4  ~ "Campeche",
      entidad == 5  ~ "Coahuila de Zaragoza",
      entidad == 6  ~ "Colima",
      entidad == 7  ~ "Chiapas",
      entidad == 8  ~ "Chihuahua",
      entidad == 9  ~ "Ciudad de México",
      entidad == 10 ~ "Durango",
      entidad == 11 ~ "Guanajuato",
      entidad == 12 ~ "Guerrero",
      entidad == 13 ~ "Hidalgo",
      entidad == 14 ~ "Jalisco",
      entidad == 15 ~ "México",
      entidad == 16 ~ "Michoacán de Ocampo",
      entidad == 17 ~ "Morelos",
      entidad == 18 ~ "Nayarit",
      entidad == 19 ~ "Nuevo León",
      entidad == 20 ~ "Oaxaca",
      entidad == 21 ~ "Puebla",
      entidad == 22 ~ "Querétaro de Arteaga",
      entidad == 23 ~ "Quintana Roo",
      entidad == 24 ~ "San Luis Potosí",
      entidad == 25 ~ "Sinaloa",
      entidad == 26 ~ "Sonora",
      entidad == 27 ~ "Tabasco",
      entidad == 28 ~ "Tamaulipas",
      entidad == 29 ~ "Tlaxcala",
      entidad == 30 ~ "Veracruz de Ignacio de la Llave",
      entidad == 31 ~ "Yucatán",
      entidad == 32 ~ "Zacatecas",
      entidad == 37 ~ "USA",
      entidad == 38 ~ "Rest of Latin America",
      entidad == 39 ~ "Other Continent",
      entidad == 88 ~ "Not Applicable",
      entidad == 99 ~ "No response",
      entidad == 0  ~ "No response",
      TRUE ~ NA),
    state = factor(state,
                   levels = c(
                     "Aguascalientes", "Baja California", "Baja California Sur", "Campeche",
                     "Coahuila de Zaragoza", "Colima", "Chiapas", "Chihuahua", "Ciudad de México",
                     "Durango", "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", "México",
                     "Michoacán de Ocampo", "Morelos", "Nayarit", "Nuevo León", "Oaxaca", "Puebla",
                     "Querétaro de Arteaga", "Quintana Roo", "San Luis Potosí", "Sinaloa", "Sonora",
                     "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz de Ignacio de la Llave",
                     "Yucatán", "Zacatecas", "USA", "Rest of Latin America", "Other Continent",
                     "Not Applicable", "No response")))

table(expenses_2024$state, useNA = "ifany")

#### 10: munic - convert to factor and pad with 0's ----
expenses_2024 <- expenses_2024 %>%
  mutate(
    munic = str_pad(as.character(munic), width = 3, pad = "0"),
    munic = factor(munic))

#### 11: entidad - convert to factor and pad with 0's ----
expenses_2024 <- expenses_2024 %>%
  mutate(
    entidad = str_pad(as.character(entidad), width = 2, pad = "0"),
    entidad = factor(entidad))

#### 12: munic_code - making the variable ----
expenses_2024 <- expenses_2024 %>%
  mutate(
    munic_code = paste0(entidad, munic))

#### 13: loc - convert to factor and pad with 0's ----
expenses_2024 <- expenses_2024 %>%
  mutate(
    loc = str_pad(as.character(loc), width = 4, pad = "0"),
    loc = factor(loc))

#### 14: indigena - make a new binary variable ----
table(expenses_2024$indigena, useNA = "ifany")

# Step 1: Create the binary variable
expenses_2024 <- expenses_2024 %>%
  mutate(
    indigena.num = case_when(
      indigena == 1 ~ 1,
      indigena == 2 ~ 0,
      indigena %in% c(8, 9) ~ 2))

## Check it's converted correctly
table(expenses_2024$indigena.num, useNA = "ifany")

# Step 2: Convert binary to factor with labels
expenses_2024 <- expenses_2024 %>%
  mutate(
    indigena.bin = factor(indigena.num, 
                          levels = c(0, 1, 2),
                          labels = c("No", "Yes", "No response")))


#### 15: speaks_indigen - create and relabel ----
table(expenses_2024$habla_lengua, useNA = "ifany")

# Step 1: Create numeric binary variable
expenses_2024 <- expenses_2024 %>%
  mutate(
    habla_lengua.num = case_when(
      habla_lengua == 1 ~ 1,
      habla_lengua == 2 ~ 0,
      habla_lengua %in% c(8, 9) ~ 2))

## Check it's converted correctly
table(expenses_2024$habla_lengua.num, useNA = "ifany")

# Step 2: Convert binary to factor with labels
expenses_2024 <- expenses_2024 %>%
  mutate(
    speaks_indigen = factor(habla_lengua.num,
                            levels = c(0, 1, 2),
                            labels = c("No", "Yes", "No response")))


#### 16: lengua_indigena - convert and relabel ----
unique(expenses_2024$lengua_indigena) # all values are not in our range??

#### Drop lengua_indigena
expenses_2024 <- expenses_2024 %>%
  select(-lengua_indigena)


#### 17: habla_esp no longer collected


#### 18: admiss_type - convert and relabel ----
expenses_2024 <- expenses_2024 %>%
  mutate(
    admiss_type = factor(tipserv,
                         levels = c("1", "2"),
                         labels = c("Normal", "Short stay")))

#### 19: origin - convert and relabel ----
expenses_2024 <- expenses_2024 %>%
  mutate(
    origin = factor(proced,
                    levels = c("1", "2", "3", "4", "5", "9"),
                    labels = c("Outpatient", "Emergency", "Referred", "Pathological nursery", "Other", "Not specified")))

table(expenses_2024$proced, useNA = "ifany")
table(expenses_2024$origin, useNA = "ifany")

#### 20: discharge_res - convert and relabel ----
expenses_2024 <- expenses_2024 %>%
  mutate(
    discharge_res = factor(motegre,
                           levels = c("1", "2", "3", "4", "5", "6", "7", "9"),
                           labels = c(
                             "Recovery",
                             "Improvement",
                             "Voluntary discharge",
                             "Transfer",
                             "Death",
                             "Escape",
                             "Other",
                             "Not specified")))

#### 21: month_discharge - convert and relabel ----
expenses_2024 <- expenses_2024 %>%
  mutate(
    month_discharge = factor(mes_estadistico,
                             levels = as.character(1:12),
                             labels = c(
                               "January", "February", "March", "April", "May", "June",
                               "July", "August", "September", "October", "November", "December")))


#### 22: relationship_status - convert and relabel ----
expenses_2024 <- expenses_2024 %>%
  mutate(
    relationship_status = factor(estado_conyugal_key,
                                 levels = c("1", "5", "3", "2", "4", "6", "8", "9", "0"),
                                 labels = c(
                                   "Single",
                                   "Married",
                                   "Divorced",
                                   "Widowed",
                                   "Domestic partnership",
                                   "Separated",
                                   "Not applicable",
                                   "Unknown",
                                   "Not specified")))

#### 23: admiss_readmit - convert and relabel ----
expenses_2024 <- expenses_2024 %>%
  mutate(
    admiss_readmit = factor(vez,
                            levels = c("1", "2", "9"),
                            labels = c(
                              "First time",
                              "Subsequent",
                              "Not specified")))




###### Rename variables some old obstet variables ----
expenses_2024 <- expenses_2024 %>%
  rename(
    gravida = gestas,
    parity = partos) 


##### Convert and relabel - hayprod ----
table(expenses_2024$hayprod, useNA = "ifany")

# Step 1: Create numeric binary variable
expenses_2024 <- expenses_2024 %>%
  mutate(
    hayprod.num = case_when(
      hayprod == 1 ~ 1,
      hayprod == 2 ~ 0,
      hayprod %in% c(8, 9) ~ 2))

## Check it's converted correctly
table(expenses_2024$hayprod.num, useNA = "ifany")

# Step 2: Convert binary to factor with labels
expenses_2024 <- expenses_2024 %>%
  mutate(
    hayprod.bin = factor(hayprod.num,
                         levels = c(0, 1, 2),
                         labels = c("No", "Yes", "No response")))

## Check it's converted correctly
table(expenses_2024$hayprod.bin, useNA = "ifany")


###### Factor and label planfam  ----
expenses_2024 <- expenses_2024 %>%
  mutate(
    contracep = case_when(
      planfam %in% c(2, 3, 12) ~ 2,  # Injectable
      planfam %in% c(4, 13) ~ 3,     # Implant
      TRUE ~ planfam) %>%
      factor(
        levels = c(0, 1, 2, 3, 5, 6, 7, 8, 9, 10, 11, 99, 88),
        labels = c(
          "None",
          "Pills",
          "Injectable",
          "Implant",    
          "Copper IUD",
          "Female condom",
          "Male condom",
          "Medicated IUD",
          "Patch",
          "Permanent",
          "Other",
          "Unspecified",
          "Not applicable")))



###### Change 99's to NA in gestac  ----
expenses_2024 <- expenses_2024 %>%
  mutate(gestat = na_if(gestac, 99))



#### AFECCIONES ----------------------------------------------------------------

###### Import the conditions data  ----
conditions_2024 <- read.delim("DATA/ssa_egresos_2024/AFECCIONES.txt",
                              sep = "|",
                              header = T,
                              stringsAsFactors = F)

###### Make variables lowercase  ----
conditions_2024 <- conditions_2024 %>%
  clean_names()


###### Reshape to wide format based on numafec  ----
conditions_2024_wide <- conditions_2024 %>%
  select(id, afec) %>%
  group_by(id) %>%
  mutate(condition_n = row_number()) %>%       # Count afec per id
  ungroup() %>%
  pivot_wider(
    names_from = condition_n,
    names_prefix = "afec",
    values_from = afec)

##### Verify pivot ran correctly ----
sum(duplicated(conditions_2024_wide$id))



#### PROCEDIMIENTOS ------------------------------------------------------------

###### Import the procedures data  ----
procedures_2024 <- read.delim("DATA/ssa_egresos_2024/PROCEDIMIENTOS.txt",
                              sep = "|",
                              header = T,
                              stringsAsFactors = F)

###### Make variables lowercase  ----
procedures_2024 <- procedures_2024 %>%
  clean_names()

##### Drop unnecessary variables - tipo, anest, quirof  ----
procedures_2024 <- procedures_2024 %>%
  select(-tipo, -anest, -quirof, -tiempo_quirofano)

##### Keep only first 10 procedures per id and pivot to wide format ----
procedures_2024_wide <- procedures_2024 %>%
  group_by(id) %>%
  arrange(id) %>%
  slice_head(n = 10) %>%
  mutate(procedure_n = row_number()) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = id,
    names_from = procedure_n,
    names_prefix = "promed",
    values_from = promed)


##### Create procedure variables ----

# Step 1: Collapse all promed variables into a single string column per row
procedures_2024_wide <- procedures_2024_wide %>%
  mutate(promed_concat = paste(promed1, promed2, promed3, promed4, promed5,
                               promed6, promed7, promed8, sep = " ")) #only 8 procedures found

# Step 2: Use str_detect() to flag presence of each procedure code
procedures_2024_wide <- procedures_2024_wide %>%
  mutate(
    proc_dc_ab     = as.integer(str_detect(promed_concat, "\\b6901\\b")),
    proc_dc_post   = as.integer(str_detect(promed_concat, "\\b6902\\b")),
    proc_asp_ab    = as.integer(str_detect(promed_concat, "\\b6951\\b")),
    proc_asp_post  = as.integer(str_detect(promed_concat, "\\b6952\\b")),
    proc_miso_ab   = as.integer(str_detect(promed_concat, "\\b75A1\\b")),
    proc_miso_post = as.integer(str_detect(promed_concat, "\\b75A2\\b")),
    proc_oth_ab    = as.integer(str_detect(promed_concat, "\\b75A3\\b")),
    proc_oth_post  = as.integer(str_detect(promed_concat, "\\b75A4\\b")))






#### PRODUCTOS -----------------------------------------------------------------

###### Import the products data  ----
products_2024 <- read.delim("DATA/ssa_egresos_2024/PRODUCTOS.txt",
                            sep = "|",
                            header = T,
                            stringsAsFactors = F)

###### Make variables lowercase  ----
products_2024 <- products_2024 %>%
  clean_names()

##### Drop unnecessary variables - condegre, naviapag, navirean, lactancia_exclusiva  ----
products_2024 <- products_2024 %>%
  select(-condegre, -naviapag, -navirean, -lactancia_exclusiva, -alojamiento_conjunto) %>%
  distinct()

##### Pivot wider so each product becomes a column  ----
products_2024_wide <- products_2024 %>%
  filter(!is.na(condnac)) %>%
  mutate(numproducto = paste0("product_", numproducto)) %>%
  pivot_wider(
    id_cols = id,
    names_from = numproducto,
    values_from = condnac)


##### Convert to factors with labels (Fetal Death/Live Birth/Not Specified)  ----
products_2024_wide <- products_2024_wide %>%
  mutate(across(starts_with("product_"),
                ~ factor(.x,
                         levels = c(1, 2, 9),
                         labels = c("Fetal Death", "Live Birth", "Not Specified"))))



#### Merge with EGRESOS and save the cleaned data ------------------------------

# Convert id to character in all data sets
expenses_2024 <- expenses_2024 %>% mutate(id = as.character(id))
conditions_2024_wide <- conditions_2024_wide %>% mutate(id = as.character(id))
procedures_2024_wide <- procedures_2024_wide %>% mutate(id = as.character(id))
products_2024_wide <- products_2024_wide %>% mutate(id = as.character(id))

# Merge with EGRESOS 
final_2024 <- expenses_2024 %>%
  left_join(conditions_2024_wide, by = "id") %>%
  left_join(procedures_2024_wide, by = "id") %>%
  left_join(products_2024_wide, by = "id")

# Create year variable to differentiate years for final join
final_2024 <- final_2024 %>%
  mutate(year = 2024)







# --- 2025 ---------------------------------------------------------------------

# DROP FOR NOW --- Data last updated 01-AUG-2025


#### EGRESO --------------------------------------------------------------------

###### Import the expenses data  ----
expenses_2025 <- read.delim("DATA/ssa_egresos_2025/EGRESOS.txt",
                            sep = "|",
                            header = T,
                            stringsAsFactors = F)

###### Make variables lowercase  ----
expenses_2025 <- expenses_2025 %>%
  clean_names()

#### 1: Drop everything except ICD-10 codes starting w/ O02-O08 and code Z303  ----
expenses_2025 <- expenses_2025 %>%
  rename(icd_code = afecprin) %>%
  filter(substr(icd_code, 1, 3) %in% c("O02", "O03", "O04", "O05", "O06", "O07", "O08") |
           icd_code == "Z303")

##### A. Table of frequencies of the primary ICD-10 codes  ----
table(expenses_2025$icd_code, useNA = "ifany")
nrow(expenses_2025)

#### 2: Drop unnecessary variables - tuhpsiq, servhc, servhp, nacioen, servicioingre, servicio02, servicio03, servicioegre, infec, mp, ttipaten, producto, tipnaci, cesareas  ----
expenses_2025 <- expenses_2025 %>%
  select(-tuhpsiq, -servhc, -servhp, -nacioen, -servicioingre,
         -servicio02, -servicio03, -servicioegre, -infec, -mp,
         -tipaten, -producto, -tipnaci, -cesareas)

#### 3: egreso, ingre - convert admission and discharge date variables ----
expenses_2025$egreso <- as.Date(expenses_2025$egreso)
expenses_2025$ingre  <- as.Date(expenses_2025$ingre)

#### 4: dias_esta - check outliers & negative values ----
summary(expenses_2025$dias_esta)

##### A. View negative values  ----
expenses_2025 %>%
  filter(dias_esta < 0) %>%
  select(id, ingre, egreso, dias_esta)

##### B. View extreme high values (> 30 days)  ----
expenses_2025 %>%
  filter(dias_esta > 30) %>%
  arrange(desc(dias_esta)) %>%
  select(id, ingre, egreso, dias_esta) %>%
  head(90)


#### 5: cveedad - check if anything is != 5 (not the age unit ~ years) ----

##### A. Check frequency of all cveedad values  ----
table(expenses_2025$cveedad, useNA = "ifany")

##### B. Identify rows where cveedad != 5  ----
expenses_2025 %>%
  filter(cveedad != 5) %>%
  select(id, cveedad, edad) %>%
  head() 

##### C. Convert cveedad to factor with labeled levels and name age_unit
expenses_2025 <- expenses_2025 %>%
  mutate(age_unit = factor(cveedad,
                           levels = c(2, 3, 4, 5, 9),
                           labels = c("hours", "days", "months", "years", "Not specified")))

##### D. Create age groups for edad and rename to age
expenses_2025 <- expenses_2025 %>%
  mutate(
    age_group = case_when(
      edad < 10 ~ 1,
      edad >= 10 & edad <= 14 ~ 2,
      edad >= 15 & edad <= 19 ~ 3,
      edad >= 20 & edad <= 24 ~ 4,
      edad >= 25 & edad <= 29 ~ 5,
      edad >= 30 & edad <= 34 ~ 6,
      edad >= 35 & edad <= 39 ~ 7,
      edad >= 40 & edad <= 44 ~ 8,
      edad >= 45 & edad <= 49 ~ 9,
      edad >= 50 & edad <= 54 ~ 10,
      edad >= 55 ~ 11,
      TRUE ~ NA),
    age_group = factor(age_group, levels = 1:11,
                       labels = c("<10", "10–14", "15–19", "20–24", "25–29",
                                  "30–34", "35–39", "40–44", "45–49", "50–54", "55+")))


#### 6: sex - check if anything != 2 ----

##### A. Check frequency of all sex values  ----
table(expenses_2025$sexo, useNA = "ifany")

##### B. sex - convert and relabel ----
expenses_2025 <- expenses_2025 %>%
  mutate(
    sex = case_when(
      sexo == "1" ~ "Male",
      sexo == "2" ~ "Female",
      sexo == "3" ~ "Intersex",
      sexo == "9" ~ "No response",
      TRUE ~ NA),
    sex = factor(sex, 
                 levels = c("Male", "Female", "Intersex", "No response")))


#### 7: weight and height - replace 999 values with NA ----
expenses_2025 <- expenses_2025 %>%
  mutate(
    weight = na_if(peso, 999),
    height = na_if(talla, 999))

# weight: ensure numeric
expenses_2025 <- expenses_2025 %>%
  mutate(weight = as.numeric(weight))

#### 8: insurance - rename and recode insurance types ----
expenses_2025 <- expenses_2025 %>%
  mutate(
    insurance = case_when(
      derhab == "0"  ~ "No response",
      derhab == "1"  ~ "Uninsured",
      derhab == "2"  ~ "IMSS",
      derhab == "3"  ~ "ISSSTE",
      derhab == "4"  ~ "PEMEX",
      derhab == "5"  ~ "SEDENA",
      derhab == "6"  ~ "SEMAR",
      derhab == "8"  ~ "Other",
      derhab == "10"  ~ "IMSS BIENESTAR",
      derhab == "11"  ~ "ISSFAM",
      derhab == "13"  ~ "INSABI",
      derhab == "14"  ~ "OPD IMSS BIENESTAR",
      derhab == "G"  ~ "Gratuidad",
      derhab == "99"  ~ "No response",
      TRUE ~ NA),
    insurance = factor(insurance,
                       levels = c("Uninsured", "IMSS", "ISSSTE", "PEMEX",
                                  "SEDENA", "SEMAR", "Other", "IMSS BIENESTAR", "ISSFAM",
                                  "INSABI", "OPD IMSS BIENESTAR", "Gratuidad", "No response")))

table(expenses_2025$insurance, useNA = "ifany")
table(expenses_2025$derhab, useNA = "ifany")


#### 9: state - recode variable values and rename  ----

# Step A: Check for non-Mexico residences
expenses_2025 %>%
  filter(entidad %in% c("37", "38")) %>%
  select(id, entidad) %>%
  head() 

# Step B: Recode state values to full names
expenses_2025 <- expenses_2025 %>%
  mutate(
    state = case_when(
      entidad == 1  ~ "Aguascalientes",
      entidad == 2  ~ "Baja California",
      entidad == 3  ~ "Baja California Sur",
      entidad == 4  ~ "Campeche",
      entidad == 5  ~ "Coahuila de Zaragoza",
      entidad == 6  ~ "Colima",
      entidad == 7  ~ "Chiapas",
      entidad == 8  ~ "Chihuahua",
      entidad == 9  ~ "Ciudad de México",
      entidad == 10 ~ "Durango",
      entidad == 11 ~ "Guanajuato",
      entidad == 12 ~ "Guerrero",
      entidad == 13 ~ "Hidalgo",
      entidad == 14 ~ "Jalisco",
      entidad == 15 ~ "México",
      entidad == 16 ~ "Michoacán de Ocampo",
      entidad == 17 ~ "Morelos",
      entidad == 18 ~ "Nayarit",
      entidad == 19 ~ "Nuevo León",
      entidad == 20 ~ "Oaxaca",
      entidad == 21 ~ "Puebla",
      entidad == 22 ~ "Querétaro de Arteaga",
      entidad == 23 ~ "Quintana Roo",
      entidad == 24 ~ "San Luis Potosí",
      entidad == 25 ~ "Sinaloa",
      entidad == 26 ~ "Sonora",
      entidad == 27 ~ "Tabasco",
      entidad == 28 ~ "Tamaulipas",
      entidad == 29 ~ "Tlaxcala",
      entidad == 30 ~ "Veracruz de Ignacio de la Llave",
      entidad == 31 ~ "Yucatán",
      entidad == 32 ~ "Zacatecas",
      entidad == 37 ~ "USA",
      entidad == 38 ~ "Rest of Latin America",
      entidad == 39 ~ "Other Continent",
      entidad == 88 ~ "Not Applicable",
      entidad == 99 ~ "No response",
      entidad == 0  ~ "No response",
      TRUE ~ NA),
    state = factor(state,
                   levels = c(
                     "Aguascalientes", "Baja California", "Baja California Sur", "Campeche",
                     "Coahuila de Zaragoza", "Colima", "Chiapas", "Chihuahua", "Ciudad de México",
                     "Durango", "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", "México",
                     "Michoacán de Ocampo", "Morelos", "Nayarit", "Nuevo León", "Oaxaca", "Puebla",
                     "Querétaro de Arteaga", "Quintana Roo", "San Luis Potosí", "Sinaloa", "Sonora",
                     "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz de Ignacio de la Llave",
                     "Yucatán", "Zacatecas", "USA", "Rest of Latin America", "Other Continent",
                     "Not Applicable", "No response")))

table(expenses_2025$state, useNA = "ifany")


#### 10: munic - convert to factor and pad with 0's ----
expenses_2025 <- expenses_2025 %>%
  mutate(
    munic = str_pad(as.character(munic), width = 3, pad = "0"),
    munic = factor(munic))

#### 11: entidad - convert to factor and pad with 0's ----
expenses_2025 <- expenses_2025 %>%
  mutate(
    entidad = str_pad(as.character(entidad), width = 2, pad = "0"),
    entidad = factor(entidad))

#### 12: munic_code - making the variable ----
expenses_2025 <- expenses_2025 %>%
  mutate(
    munic_code = paste0(entidad, munic))

#### 13: loc - convert to factor and pad with 0's ----
expenses_2025 <- expenses_2025 %>%
  mutate(
    loc = str_pad(as.character(loc), width = 4, pad = "0"),
    loc = factor(loc))

#### 14: indigena - make a new binary variable ----
table(expenses_2025$indigena, useNA = "ifany")

# Step 1: Create the binary variable
expenses_2025 <- expenses_2025 %>%
  mutate(
    indigena.num = case_when(
      indigena == 1 ~ 1,
      indigena == 2 ~ 0,
      indigena %in% c(8, 9) ~ 2))

## Check it's converted correctly
table(expenses_2025$indigena.num, useNA = "ifany")

# Step 2: Convert binary to factor with labels
expenses_2025 <- expenses_2025 %>%
  mutate(
    indigena.bin = factor(indigena.num, 
                          levels = c(0, 1, 2),
                          labels = c("No", "Yes", "No response")))


#### 15: speaks_indigen - create and relabel ----
table(expenses_2025$habla_lengua, useNA = "ifany")

# Step 1: Create numeric binary variable
expenses_2025 <- expenses_2025 %>%
  mutate(
    habla_lengua.num = case_when(
      habla_lengua == 1 ~ 1,
      habla_lengua == 2 ~ 0,
      habla_lengua %in% c(8, 9) ~ 2))

## Check it's converted correctly
table(expenses_2025$habla_lengua.num, useNA = "ifany")

# Step 2: Convert binary to factor with labels
expenses_2025 <- expenses_2025 %>%
  mutate(
    speaks_indigen = factor(habla_lengua.num,
                            levels = c(0, 1, 2),
                            labels = c("No", "Yes", "No response")))


#### 16: lengua_indigena - convert and relabel ----
unique(expenses_2025$lengua_indigena) # all values are not in our range??

#### Drop lengua_indigena
expenses_2025 <- expenses_2025 %>%
  select(-lengua_indigena)


#### 17: habla_esp no longer collected


#### 18: admiss_type - convert and relabel ----
expenses_2025 <- expenses_2025 %>%
  mutate(
    admiss_type = factor(tipserv,
                         levels = c("1", "2"),
                         labels = c("Normal", "Short stay")))

#### 19: origin - convert and relabel ----
expenses_2025 <- expenses_2025 %>%
  mutate(
    origin = factor(proced,
                    levels = c("1", "2", "3", "4", "5", "9"),
                    labels = c("Outpatient", "Emergency", "Referred", "Pathological nursery", "Other", "Not specified")))

#### 20: discharge_res - convert and relabel ----
expenses_2025 <- expenses_2025 %>%
  mutate(
    discharge_res = factor(motegre,
                           levels = c("1", "2", "3", "4", "5", "6", "7", "9"),
                           labels = c(
                             "Recovery",
                             "Improvement",
                             "Voluntary discharge",
                             "Transfer",
                             "Death",
                             "Escape",
                             "Other",
                             "Not specified")))

#### 21: month_discharge - convert and relabel ----
expenses_2025 <- expenses_2025 %>%
  mutate(
    month_discharge = factor(mes_estadistico,
                             levels = as.character(1:12),
                             labels = c(
                               "January", "February", "March", "April", "May", "June",
                               "July", "August", "September", "October", "November", "December")))


#### 22: relationship_status - convert and relabel ----
expenses_2025 <- expenses_2025 %>%
  mutate(
    relationship_status = factor(estado_conyugal_key,
                                 levels = c("1", "5", "3", "2", "4", "6", "8", "9", "0"),
                                 labels = c(
                                   "Single",
                                   "Married",
                                   "Divorced",
                                   "Widowed",
                                   "Domestic partnership",
                                   "Separated",
                                   "Not applicable",
                                   "Unknown",
                                   "Not specified")))

#### 23: admiss_readmit - convert and relabel ----
expenses_2025 <- expenses_2025 %>%
  mutate(
    admiss_readmit = factor(vez,
                            levels = c("1", "2", "9"),
                            labels = c(
                              "First time",
                              "Subsequent",
                              "Not specified")))




###### Rename variables some old obstet variables ----
expenses_2025 <- expenses_2025 %>%
  rename(
    gravida = gestas,
    parity = partos)  %>% 
  mutate(gravida = as.numeric(gravida))

expenses_2025 <- expenses_2025 %>%
  mutate(gravida = as.numeric(gravida))

##### Convert and relabel - hayprod ----
table(expenses_2025$hayprod, useNA = "ifany")

# Step 1: Create numeric binary variable
expenses_2025 <- expenses_2025 %>%
  mutate(
    hayprod.num = case_when(
      hayprod == 1 ~ 1,
      hayprod == 2 ~ 0,
      hayprod %in% c(8, 9) ~ 2))

## Check it's converted correctly
table(expenses_2025$hayprod.num, useNA = "ifany")

# Step 2: Convert binary to factor with labels
expenses_2025 <- expenses_2025 %>%
  mutate(
    hayprod.bin = factor(hayprod.num,
                         levels = c(0, 1, 2),
                         labels = c("No", "Yes", "No response")))

## Check it's converted correctly
table(expenses_2025$hayprod.bin, useNA = "ifany")


###### Factor and label planfam  ----
expenses_2025 <- expenses_2025 %>%
  mutate(
    contracep = case_when(
      planfam %in% c(2, 3, 12) ~ 2,  # Injectable
      planfam %in% c(4, 13) ~ 3,     # Implant
      TRUE ~ planfam) %>%
      factor(
        levels = c(0, 1, 2, 3, 5, 6, 7, 8, 9, 10, 11, 99, 88),
        labels = c(
          "None",
          "Pills",
          "Injectable",
          "Implant",    
          "Copper IUD",
          "Female condom",
          "Male condom",
          "Medicated IUD",
          "Patch",
          "Permanent",
          "Other",
          "Unspecified",
          "Not applicable")))



###### Change 99's to NA in gestac  ----
expenses_2025 <- expenses_2025 %>%
  mutate(gestat = na_if(gestac, 99))



#### AFECCIONES ----------------------------------------------------------------

###### Import the conditions data  ----
conditions_2025 <- read.delim("DATA/ssa_egresos_2025/AFECCIONES.txt",
                              sep = "|",
                              header = T,
                              stringsAsFactors = F)

###### Make variables lowercase  ----
conditions_2025 <- conditions_2025 %>%
  clean_names()


###### Reshape to wide format based on numafec  ----
conditions_2025_wide <- conditions_2025 %>%
  select(id, afec) %>%
  group_by(id) %>%
  mutate(condition_n = row_number()) %>%       # Count afec per id
  ungroup() %>%
  pivot_wider(
    names_from = condition_n,
    names_prefix = "afec",
    values_from = afec)

##### Verify pivot ran correctly ----
sum(duplicated(conditions_2025_wide$id))



#### PROCEDIMIENTOS ------------------------------------------------------------

###### Import the procedures data  ----
procedures_2025 <- read.delim("DATA/ssa_egresos_2025/PROCEDIMIENTOS.txt",
                              sep = "|",
                              header = T,
                              stringsAsFactors = F)

###### Make variables lowercase  ----
procedures_2025 <- procedures_2025 %>%
  clean_names()

##### Drop unnecessary variables - tipo, anest, quirof  ----
procedures_2025 <- procedures_2025 %>%
  select(-tipo, -anest, -quirof, -tiempo_quirofano)

##### Keep only first 10 procedures per id and pivot to wide format ----
procedures_2025_wide <- procedures_2025 %>%
  group_by(id) %>%
  arrange(id) %>%
  slice_head(n = 10) %>%
  mutate(procedure_n = row_number()) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = id,
    names_from = procedure_n,
    names_prefix = "promed",
    values_from = promed)


##### Create procedure variables ----

# Step 1: Collapse all promed variables into a single string column per row
procedures_2025_wide <- procedures_2025_wide %>%
  mutate(promed_concat = paste(promed1, promed2, promed3, promed4, promed5,
                               promed6, promed7, promed8, sep = " ")) #only 8 procedures found

# Step 2: Use str_detect() to flag presence of each procedure code
procedures_2025_wide <- procedures_2025_wide %>%
  mutate(
    proc_dc_ab     = as.integer(str_detect(promed_concat, "\\b6901\\b")),
    proc_dc_post   = as.integer(str_detect(promed_concat, "\\b6902\\b")),
    proc_asp_ab    = as.integer(str_detect(promed_concat, "\\b6951\\b")),
    proc_asp_post  = as.integer(str_detect(promed_concat, "\\b6952\\b")),
    proc_miso_ab   = as.integer(str_detect(promed_concat, "\\b75A1\\b")),
    proc_miso_post = as.integer(str_detect(promed_concat, "\\b75A2\\b")),
    proc_oth_ab    = as.integer(str_detect(promed_concat, "\\b75A3\\b")),
    proc_oth_post  = as.integer(str_detect(promed_concat, "\\b75A4\\b")))






#### PRODUCTOS -----------------------------------------------------------------

###### Import the products data  ----
products_2025 <- read.delim("DATA/ssa_egresos_2025/PRODUCTOS.txt",
                            sep = "|",
                            header = T,
                            stringsAsFactors = F)

###### Make variables lowercase  ----
products_2025 <- products_2025 %>%
  clean_names()

##### Drop unnecessary variables - condegre, naviapag, navirean, lactancia_exclusiva  ----
products_2025 <- products_2025 %>%
  select(-condegre, -naviapag, -navirean, -lactancia_exclusiva, -alojamiento_conjunto) %>%
  distinct()

##### Pivot wider so each product becomes a column  ----
products_2025_wide <- products_2025 %>%
  filter(!is.na(condnac)) %>%
  mutate(numproducto = paste0("product_", numproducto)) %>%
  pivot_wider(
    id_cols = id,
    names_from = numproducto,
    values_from = condnac)


##### Convert to factors with labels (Fetal Death/Live Birth/Not Specified)  ----
products_2025_wide <- products_2025_wide %>%
  mutate(across(starts_with("product_"),
                ~ factor(.x,
                         levels = c(1, 2, 9),
                         labels = c("Fetal Death", "Live Birth", "Not Specified"))))



#### Merge with EGRESOS and save the cleaned data ------------------------------

# Convert id to character in all data sets
expenses_2025 <- expenses_2025 %>% mutate(id = as.character(id))
conditions_2025_wide <- conditions_2025_wide %>% mutate(id = as.character(id))
procedures_2025_wide <- procedures_2025_wide %>% mutate(id = as.character(id))
products_2025_wide <- products_2025_wide %>% mutate(id = as.character(id))

# Merge with EGRESOS 
final_2025 <- expenses_2025 %>%
  left_join(conditions_2025_wide, by = "id") %>%
  left_join(procedures_2025_wide, by = "id") %>%
  left_join(products_2025_wide, by = "id")

# Create year variable to differentiate years for final join
final_2025 <- final_2025 %>%
  mutate(year = 2025)







# FINAL DATA MERGE -------------------------------------------------------------
datasets <- list(final_2018, final_2019, final_2020, final_2021,
                 final_2022, final_2023, final_2024#, final_2025
                 )

# Extend this vector with all columns that have type mismatches
cols_to_char <- c("gravida", "parity", "abortos")

convert_cols_to_char <- function(df, cols) {
  for (col in cols) {
    if (col %in% names(df)) {
      df <- df %>% mutate(!!col := as.character(.data[[col]]))}}
  df}

datasets_char <- map(datasets, ~ convert_cols_to_char(.x, cols_to_char))
final_saeh <- bind_rows(datasets_char)

#### Confirm bind ran correctly
table(final_saeh$year, useNA = "ifany")




# ADDITIONAL PROCEDURE VARIABLES -----------------------------------------------


# Create the new flag variables for 6909 and 6959
final_saeh <- final_saeh %>%
  mutate(
    proc_dc_ab_other     = as.integer(str_detect(promed_concat, "\\b6909\\b")), # Another dilation and curette
    proc_asp_ab_other    = as.integer(str_detect(promed_concat, "\\b6959\\b")))  # Another cutting and suction of the uterus

# Check counts of flagged rows
table(final_saeh$proc_dc_ab_other, useNA = "ifany")
table(final_saeh$proc_asp_ab_other, useNA = "ifany")





# SAVE FINAL DATA SET ----------------------------------------------------------

#### Save as rds
saveRDS(final_saeh, "SAEHdata_2018_2024.rds")

#### Save as .dta for Emily
names(final_saeh) <- gsub("\\.", "_", names(final_saeh))
write_dta(final_saeh, "SAEHdata_2018_2024.dta")
