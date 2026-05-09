# ================================================================================
# README - SAEH DATA
# ================================================================================
#
# Purpose: SAEH Data Cleaning Steps
# Author:   Kelsey MacKenzie
# Updated:  April 5, 2026
#
# --------------------------------------------------------------------------------
# PIPELINE ORDER -- RUN SCRIPTS IN THIS ORDER
# --------------------------------------------------------------------------------
#
#   1. SAEH_cleaning_2025.R          Raw SAEH data files -> SAEHdata_2018_2024.rds
#   2. SAEH_supp_merge.R             + supplemental merges -> SAEHdata_merged.rds
#   3. Unmatched_MunicCodes.R        + exclusions -> SAEHdata_clean.rds
#   4. SAEH_table1.R                 + derived variables -> SAEH_dataset.rds
#   5. SAEH_model_script.R           + models -> output files
#
# --------------------------------------------------------------------------------
# DATA SOURCES
# --------------------------------------------------------------------------------
#
# INDIVIDUAL-LEVEL
# ----------------
#
# Source:      Secretaria de Salud -- Datos Abiertos
# URL:         http://www.dgis.salud.gob.mx/contenidos/basesdedatos/Datos_Abiertos_gobmx.html
# Downloaded:  July 11, 2025
# Variables:   Abortive event procedure, admission/discharge dates, age, births,
#              gestational duration, ICD-10 code, indigenous identity, insurance,
#              municipality, parity, relationship status, state
#
#
# FACILITY-LEVEL
# --------------
#
# File:        ESTABLECIMIENTO_SALUD_202509.xlsx
# Source:      Secretaria de Salud -- CLUES registry
# URL:         http://www.dgis.salud.gob.mx/contenidos/intercambio/clues_gobmx.html
# Downloaded:  October 14, 2025
# Variables:   CLUES code, hospital care level, administrative unit, latitude, longitude
#
#
# MUNICIPALITY-LEVEL
# ------------------
#
# File:        inafed_bd_1760674084.csv
# Source:      INEGI / INAFED -- Population and Housing Census
# URL:         http://www.snim.rami.gob.mx/
# Downloaded:  October 16, 2025
# Year:        2020
# Variables:   Municipality population size
#
# File:        IMM_2020.xlsx (sheet: IMM_2020)
# Source:      CONAPO -- Marginalization Indices 2020
# URL:         https://www.gob.mx/conapo/documentos/indices-de-marginacion-2020-284372
# Downloaded:  October 16, 2025
# Year:        2020
# Variables:   Municipality marginalization grade (5-category: Muy bajo, Bajo,
#              Medio, Alto, Muy alto), continuous marginalization index
# Note:        Municipality 04013 (Dzitbalche, Campeche) does not exist in this
#              file -- it was created January 1, 2021 from Calkini (04001),
#              after the 2020 census. These 25 records are reassigned to 04001.
#              Source: http://periodicooficial.campeche.gob.mx/sipoec/public/periodicos/202103/PO1389SS24032021.pdf
#
# File:        2-poblacion-indigena-autoadscrita-por-municipio-muestra-censal-2020-2-1-.xlsx
# Source:      INPI -- Indigenous Population Indicators 2020
# URL:         https://www.inpi.gob.mx/indicadores2020/
# Downloaded:  November 29, 2025
# Year:        2020
# Variables:   Percentage of self-identified indigenous residents by municipality
#
# File:        IRS_ent_mun_2000_2020/IRS_entidades_mpios_2020.xlsx
#              (sheet: Municipios)
# Source:      CONEVAL -- Social Backwardness Index 2020
# URL:         https://www.coneval.org.mx/Medicion/IRS/Paginas/
#              Indice_de_Rezago_Social_2020_anexos.aspx
# Downloaded:  October 16, 2025
# Year:        2020
# Variables:   Municipality education score
#
# File:        tf_adolescente_municipal_2020.csv
# Source:      CONAPO -- Sexual and Reproductive Health Data
# URL:         https://datos.gob.mx/busca/dataset/salud-sexual-y-reproductiva
# Downloaded:  October 16, 2025
# Year:        2020
# Variables:   Municipality adolescent fertility rate
#
#
# STATE-LEVEL
# -----------
#
# File:        ConDem50a19_ProyPob20a70/0_Pob_Mitad_1950_2070.xlsx
# Source:      CONAPO -- Population Projections
# URL:         https://www.gob.mx/conapo/acciones-y-programas/conciliacion-demografica-de-1950-a-2019-y-proyecciones-de-la-poblacion-de-mexico-y-de-las-entidades-federativas-2020-a-2070
# Downloaded:  November 30, 2025
# Year:        2023
# Variables:   Women of reproductive age (15-49 and 15-45) by state
#
# File:        ENSANUT MC 2016 (used for region classification only)
# Source:      National Health and Nutrition Survey 2016
# URL:         https://www.gob.mx/cms/uploads/attachment/file/209093/ENSANUT.pdf
# Downloaded:  December 4, 2025
# Variables:   Geographic region (North, Central, Mexico City, South)
#
# --------------------------------------------------------------------------------
# SCRIPT 1: SAEH_cleaning_2025.R
# --------------------------------------------------------------------------------
#
# Input:   Raw SAEH annual files (2018-2025)
# Output:  SAEHdata_2018_2024.rds, SAEHdata_2018_2024.dta
#
#   Records from 2018-2024 after ICD-10 exclusions:        n = 509,605
#
# --------------------------------------------------------------------------------
# SCRIPT 2: SAEH_supp_merge.R
# --------------------------------------------------------------------------------
#
# Input:   SAEHdata_2018_2024.rds
# Output:  SAEHdata_merged.rds, SAEHdata_merged.dta
#
# Performs 7 sequential joins to add supplemental data sources:
#
#   Merge 1 -- Facility level
#
#   Merge 2 -- Municipality population
#
#   Merge 3 -- CONAPO marginalization index
#
#   Merge 4 -- Indigenous population percentage
#
#   Merge 5 -- Education score
#
#   Merge 6 -- Adolescent fertility rate 
#
#   Merge 7 -- State female population
#
# Key note: munic_merge maps 04013 -> 04001 for all municipality-level merges.
# The original munic_code value is preserved throughout. Do not use munic_merge
# in any downstream analysis -- use munic_id.
#
#   Starting n: 509,605 | Final n: 509,605
#   Exclusions: None -- all records retained after supplemental merges
#
# --------------------------------------------------------------------------------
# SCRIPT 3: Unmatched_MunicCodes.R
# --------------------------------------------------------------------------------
#
# Input:   SAEHdata_merged.rds
# Output:  SAEHdata_clean.rds, SAEHdata_clean.dta
#          SAEH_final_2.rds, SAEH_final_2.dta (keeps records with unknown municipio, but drops unknown state and other countries), 
#          SAEH_final_3.rds, SAEH_final_3.dta (only drops other countries)
#
# Identifies and documents all records with unresolvable municipality codes.
# Reassigns Dzitbalche (04013) to Calkini (04001).
# Applies the following exclusions:
#
#   Starting n: 509,605 | Final n: 501,262
# 
#   EXCLUSIONS:
#   State codes 88, 99, 00  (missing/unspecified state):        n =  4,080 
#   State codes 33-35, 37-39 (non-Mexican country):             n =    119
#   Municipality codes ending 997, 998, 999 (placeholder):      n =  4,144
#   TOTAL EXCLUDED:                                             n =  8,343
#   Final record count:                                         n =  501,262
#
# --------------------------------------------------------------------------------
# SCRIPT 4: SAEH_table1.R
# --------------------------------------------------------------------------------
#
# Input:   SAEHdata_clean.rds,
#          SAEH_final_2.rds,
#          SAEH_final_3.rds
# Output:  SAEH_dataset.rds, SAEH_dataset.dta
#          SAEH_dataset2.rds, SAEH_dataset2.dta (keeps records with unknown municipio, but drops unknown state and other countries), 
#          SAEH_dataset3.rds, SAEH_dataset3.dta (only drops other countries),
#          Table1.png
#
# Renames munic_code to munic_id (promotes clean municipality code, drops the
# locality-level munic_id from the raw data). Creates all derived
# variables needed for analysis and Table 1.
#
#   Starting n: 501,262 | Final n: 501,262
#   Exclusions: None -- no exclusions made in Script 4
#
# --------------------------------------------------------------------------------
# SCRIPT 5: SAEH_model_script.R
# --------------------------------------------------------------------------------
#
# Input:   SAEH_dataset.rds
# Output:  Model output and relevant plots
#
# SAMPLE CONSTRUCTION
# -------------------
# Recodes gestat 88 -> NA. Creates binary outcome late_pres (1 = >=13 weeks,
# 0 = <13 weeks). Creates centered year (year_c = year - 2020.92) and quadratic
# term (year_sq).
#
#   Records after municipality exclusions:          501,262
#   Excluded (missing/not-applicable gestat):       -54,119
#   FINAL ANALYTIC SAMPLE:                          447,143
#
# PRIMARY MODEL
# -------------
# Mixed-effects logistic regression with municipality-level random intercept.
#
# --------------------------------------------------------------------------------
# DATASETS
# --------------------------------------------------------------------------------
#
# SAEHdata_2018_2024.rds, SAEHdata_2018_2024.dta    Stacked cleaned annual records
# SAEHdata_merged.rds, SAEHdata_merged.dta            + all 7 supplemental merges
# SAEHdata_clean.rds, SAEHdata_clean.dta              + municipality exclusions applied
# SAEH_dataset.rds, SAEH_dataset.dta                  + derived variables
#
# ================================================================================
# SAEH Analysis Scripts
