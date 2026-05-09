##----------------------------------------------------------------------------##
## File name: SAEH Mapping                                                    ##  
## Programmer: Kelsey MacKenzie                                               ##
## Date: 30-NOV-2025                                                          ##
## Last modified: 06-JAN-2026                                                 ##
## Purpose: Create exploratory maps                                           ##
## PI: Emily Boniface                                                         ##
##----------------------------------------------------------------------------##

#### SETUP ---------------------------------------------------------------------

##### Load packages
library(dplyr)
library(sf)
library(ggplot2)
library(RColorBrewer)
library(grid)
library(scales)
library(viridis)


#### Import Data ---------------------------------------------------------------
SAEH_clean <- readRDS("SAEH_tableone.rds")

# Pull in shapefile for the states (source: https://diva-gis.org/data.html)
mexico_states <- st_read("/Users/kelseymackenzie/Documents/PE/MEX_adm/MEX_adm1.shp")

# Pull in shapefile for the municipalities (source: https://www.inegi.org.mx/app/biblioteca/ficha.html?upc=794551163061)
mexico_municipios <- st_read(
  "/Users/kelseymackenzie/Documents/PE/794551163061_s/mg_2025_integrado/conjunto_de_datos/00mun.shp",
  quiet = TRUE) %>%
  st_transform(crs = 4326) %>%     # match facilities CRS
  rename(munic_code = CVEGEO)


#### Making Maps ---------------------------------------------------------------

# Aggregate SAEH_clean to get total abortive events by year and state
saeh_aborts_state_year <- SAEH_clean %>%
  mutate(abort_event = proc_dc_ab + proc_dc_post +
           proc_asp_ab + proc_asp_post +
           proc_miso_ab + proc_miso_post +
           proc_oth_ab + proc_oth_post +
           proc_dc_ab_other + proc_asp_ab_other) %>%
  group_by(year, state) %>%
  summarise(total_abort_events = sum(abort_event, na.rm = TRUE), .groups = "drop")


# Crosswalk from SAEH names -> shapefile NAME_1
state_key <- tibble::tibble(
  state = c("Ciudad de México",
            "Coahuila de Zaragoza",
            "Michoacán de Ocampo",
            "Querétaro de Arteaga",
            "Veracruz de Ignacio de la Llave"),
  NAME_1 = c("Distrito Federal",
             "Coahuila",
             "Michoacán",
             "Querétaro",
             "Veracruz"))

# Apply crosswalk to get a NAME_1 column that matches the shapefile
saeh_aborts_state_year_fix <- saeh_aborts_state_year %>%
  left_join(state_key, by = "state") %>%
  mutate(NAME_1 = if_else(is.na(NAME_1), state, NAME_1))

# Rejoin to polygons using NAME_1
mexico_states_joined <- mexico_states %>%
  left_join(saeh_aborts_state_year_fix, by = "NAME_1")



# Prepare facilities sf points from SAEH_clean
facilities_sf <- SAEH_clean %>%
  select(year, clues, latitude, longitude) %>%
  distinct() %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)



#### Fix Outlier Facility ------------------------------------------------------

# Quick check of longitude range
summary(st_coordinates(facilities_sf)[, "X"])

# Look for extreme longitudes (e.g., > -80 or < -120 if data should be in Mexico)
outliers_2020 <- facilities_sf %>%
  filter(year == 2020) %>%
  mutate(lon = st_coordinates(geometry)[, "X"],
         lat = st_coordinates(geometry)[, "Y"]) %>%
  filter(lon > -80 | lon < -120)

outliers_2020

# Identify the bad point explicitly
facilities_sf %>%
  filter(year == 2020, clues == "TSSSA003621")

# Replace its geometry with the corrected coordinates
facilities_sf$geometry[
  facilities_sf$year == 2020 & facilities_sf$clues == "TSSSA003621"] <- sf::st_sfc(
  sf::st_point(c(-98.29763, 26.05543)),
  crs = sf::st_crs(facilities_sf))

# If you also keep numeric lon/lat columns, update them too
facilities_sf <- facilities_sf %>%
  mutate(
    lon = dplyr::if_else(
      year == 2020 & clues == "TSSSA003621",
      -98.29763,
      st_coordinates(geometry)[, "X"]),
    lat = st_coordinates(geometry)[, "Y"])

# Quick check
sf::st_coordinates(
  facilities_sf %>% filter(year == 2020, clues == "TSSSA003621"))



# Helper to build a plot for a single year -------------------------------
make_year_plot <- function(yy) {
  mexico_year <- mexico_states_joined %>% dplyr::filter(year == yy)
  fac_year    <- facilities_sf        %>% dplyr::filter(year == yy)
  
  ggplot() +
    # polygons: fill = total_abort_events
    geom_sf(data = mexico_year,
            aes(fill = total_abort_events),
            color = "grey50", size = 0.3) +
    # facilities: map color to a constant label so it gets a legend key
    geom_sf(data = fac_year,
            aes(color = "Facility"),
            fill = "yellow",
            shape = 21,
            size = 0.4,
            alpha = 0.5,
            show.legend = "point") +
    scale_fill_distiller(palette = "Reds",
                         direction = 1,
                         na.value = "white",
                         name = "Abortive events",
                         limits    = c(0, 11000)) +
    scale_color_manual(
      name   = NULL,                  # or "Point features"
      values = c("Facility" = "black"),
      breaks = "Facility",
      labels = "Facility") +
    labs(title = paste("Abortive events & facilities in Mexico -", yy),
         x = NULL, y = NULL) +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          legend.position = "right")
}


# Create separate plot objects for 2018–2024 -----------------------------
p_2018 <- make_year_plot(2018)
p_2019 <- make_year_plot(2019)
p_2020 <- make_year_plot(2020)
p_2021 <- make_year_plot(2021)
p_2022 <- make_year_plot(2022)
p_2023 <- make_year_plot(2023)
p_2024 <- make_year_plot(2024)


# Save them individually -------------------------------------------------
ggsave("SAEH_map_2018.png", p_2018, width = 7, height = 6, dpi = 300)
ggsave("SAEH_map_2019.png", p_2019, width = 7, height = 6, dpi = 300)
ggsave("SAEH_map_2020.png", p_2020, width = 7, height = 6, dpi = 300)
ggsave("SAEH_map_2021.png", p_2021, width = 7, height = 6, dpi = 300)
ggsave("SAEH_map_2022.png", p_2022, width = 7, height = 6, dpi = 300)
ggsave("SAEH_map_2023.png", p_2023, width = 7, height = 6, dpi = 300)
ggsave("SAEH_map_2024.png", p_2024, width = 7, height = 6, dpi = 300)




# Create Faceted Maps ----------------------------------------------------

##### ABORTIVE EVENTS BY STATE -----

# Filter data to 2018–2024
mexico_states_18_24 <- mexico_states_joined %>%
  dplyr::filter(year >= 2018, year <= 2024)

facilities_static <- facilities_sf %>%
  group_by(clues) %>%
  slice(1) %>%
  ungroup() %>%
  select(-year) %>%         
  st_as_sf()                     # sf structure

# Verify: should have NO year column
names(facilities_static)


# Map code
p_faceted_18_24 <- ggplot() +
  geom_sf(data = mexico_states_18_24,
          aes(fill = total_abort_events),
          color = "grey50", size = 0.3) +
  geom_sf(data = facilities_static,
          aes(color = "Facility"),
          fill = "blue",
          shape = 21,
          size = 0.01,
          alpha = 0.5,
          show.legend = "point") +
  scale_fill_distiller(
    palette   = "Reds",
    direction = 1,
    na.value  = "white",
    name      = "Abortive events",
    guide     = guide_colourbar(
      direction = "horizontal",
      title.position = "top",
      barwidth = unit(8, "cm"),
      barheight = unit(0.4, "cm"))) +
  scale_color_manual(
    name   = NULL,
    values = c("Facility" = "blue"),
    breaks = "Facility",
    labels = "Facility") +
  facet_wrap(~ year, ncol = 4) +
  labs(
    title = "Abortive Events by State in Mexico, 2018–2024",
    x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    panel.grid       = element_blank(),
    legend.position  = "bottom",
    legend.box       = "horizontal",
    legend.direction = "horizontal",
    axis.text.x      = element_blank(),
    axis.text.y      = element_blank(),
    axis.ticks       = element_blank(),
    legend.text  = element_text(size = 9),
    legend.title = element_text(size = 10, face = "bold"),
    legend.background = element_rect(fill  = "white", color = "grey70", linewidth = 0.3),
    legend.key = element_rect(fill  = "grey95", color = "grey70", linewidth = 0.3),
    legend.margin = margin(t = 6, r = 20, b = 6, l = 20, unit = "pt")) +
  guides(color = guide_legend(
    override.aes = list(
      size = 3, 
      alpha = 1)))

# Print
p_faceted_18_24

# Save high-res image
ggsave("mexico_abort_events_2018_2024_faceted.png",
       p_faceted_18_24,
       width = 20, height = 8, dpi = 300)


##### GRADE OF MARGINALIZATION (QUINTILES) -----

# Create municipality-level marginalization data
munic_marg <- SAEH_clean %>%
  filter(!is.na(grade_marg)) %>%
  distinct(munic_code, grade_marg) %>%
  mutate(
    grade_marg = case_when(
      grade_marg == "Muy bajo" ~ "Very low",
      grade_marg == "Bajo"     ~ "Low",
      grade_marg == "Medio"    ~ "Medium",
      grade_marg == "Alto"     ~ "High",
      grade_marg == "Muy alto" ~ "Very high"),
    grade_marg = factor(
      grade_marg,
      levels = c("Very low", "Low", "Medium", "High", "Very high"),
      ordered = TRUE))

# Join marginalization to municipality polygons
mexico_munic_map <- mexico_municipios %>%
  left_join(munic_marg, by = "munic_code")

# Create variables for years
years <- sort(unique(SAEH_clean$year))

# Duplicate polygons across years
mexico_munic_faceted <- mexico_munic_map %>%
  mutate(key = 1) %>%
  left_join(
    tibble(year = years, key = 1),
    by = "key",
    relationship = "many-to-many") %>%
  select(-key) %>%
  mutate(year = factor(year))

# Map faceted by year (no facility dots)
p_marg_quintiles <- ggplot() +
  geom_sf(
    data = mexico_munic_faceted,
    aes(fill = grade_marg),
    color = NA) +
  facet_wrap(~ year, ncol = 4) +
  scale_fill_brewer(
    palette   = "Blues",
    direction = 1,
    drop      = FALSE,
    na.value  = "white",
    name      = "Grade of marginalization",
    guide     = guide_legend(
      nrow = 1,
      byrow = TRUE,
      title.position = "top")) +
  labs(
    title    = "Grade of Marginalization by Municipality in Mexico, 2018–2024",
    subtitle = "INEGI Marco Geoestadístico 2025",
    x = NULL,
    y = NULL) +
  theme_minimal() +
  theme(
    panel.grid      = element_blank(),
    axis.text       = element_blank(),
    axis.ticks      = element_blank(),
    strip.text      = element_text(face = "bold"),
    legend.position = "bottom",
    legend.box      = "horizontal",
    legend.direction = "horizontal",
    legend.justification = "center",
    legend.key.width = unit(0.8, "cm"),
    legend.background = element_rect(
      fill  = "white",
      color = "grey70",
      linewidth = 0.3),
    legend.key = element_rect(
      fill  = "grey95",
      color = "grey70",
      linewidth = 0.3))


# Print
p_marg_quintiles

# Save
ggsave("mexico_grade_marg_quintiles_2018_2024_faceted.png",
       p_marg_quintiles,
       width = 20, height = 8, dpi = 300)



##### GRADE OF MARGINALIZATION (CONTINUOUS) -----

# Create municipal continuous marginalization (once per municipality)
munic_marg_cont <- SAEH_clean %>%
  group_by(munic_code) %>%
  summarise(
    grade_marg_cont = mean(grade_marg_cont, na.rm = TRUE),
    .groups = "drop")

# Join to municipality polygons
mexico_munic_map_cont <- mexico_municipios %>%
  left_join(munic_marg_cont, by = "munic_code")

# Duplicate polygons across years
mexico_munic_cont_faceted <- mexico_munic_map_cont %>%
  mutate(key = 1) %>%
  left_join(
    tibble(year = years, key = 1),
    by = "key",
    relationship = "many-to-many") %>%
  select(-key) %>%
  mutate(year = factor(year))


# Map faceted by year
p_marg_cont <- ggplot() +
  geom_sf(
    data  = mexico_munic_cont_faceted,
    aes(fill = grade_marg_cont),
    color = NA) +
  facet_wrap(~ year, ncol = 4) +
  scale_fill_distiller(
    palette   = "Purples",
    direction = 1,
    na.value  = "white",
    name      = "Grade of marginalization",
    guide     = guide_colorbar(
      direction      = "horizontal",
      title.position = "top",
      barwidth       = unit(10, "cm"),
      barheight      = unit(0.4, "cm"),
      label.position = "bottom")) +
  labs(
    title    = "Grade of Marginalization by Municipality in Mexico, 2018–2024",
    subtitle = "INEGI Marco Geoestadístico 2025",
    x = NULL,
    y = NULL) +
  theme_minimal() +
  theme(
    panel.grid      = element_blank(),
    axis.text       = element_blank(),
    axis.ticks      = element_blank(),
    strip.text      = element_text(face = "bold"),
    legend.position = "bottom",
    legend.background = element_rect(
      fill  = "white",
      color = "grey70",
      linewidth = 0.3),
    legend.key = element_rect(
      fill  = "grey95",
      color = "grey70",
      linewidth = 0.3))


# Print
p_marg_cont

# Save high-res image
ggsave("mexico_grade_marg_cont_2018_2024_faceted.png",
       p_marg_cont,
       width = 20, height = 8, dpi = 300)



##### BINARY GRADE OF MARGINALIZATION (marg_group) -----

# Create municipality-level binary marginalization data
munic_marg <- SAEH_clean %>%
  filter(!is.na(marg_group)) %>%
  distinct(munic_id, marg_group)

# Join marginalization to municipality polygons
mexico_munic_map <- mexico_municipios %>%
  left_join(munic_marg, by = c("munic_code" = "munic_id"))

# Quick check: how many municipalities have marginalization data?
table(is.na(mexico_munic_map$marg_group))

# Create variables for years
years <- sort(unique(SAEH_clean$year))

# Duplicate polygons across years
mexico_munic_faceted <- mexico_munic_map %>%
  mutate(key = 1) %>%
  left_join(
    tibble(year = years, key = 1),
    by = "key",
    relationship = "many-to-many") %>%
  select(-key) %>%
  mutate(year = factor(year))

# Map faceted by year (no facility dots)
p_marg_binary <- ggplot() +
  geom_sf(data = mexico_munic_faceted, 
          aes(fill = marg_group), 
          color = NA) +
  facet_wrap(~ year, ncol = 4) +
  scale_fill_manual(values = c("Less marginalized" = "#eff3ff", 
                               "More marginalized" = "#08519c"),
                    drop = FALSE, 
                    na.value = "white", 
                    name = "Marginalization group") +
  labs(title = "Municipal Marginalization Group in Mexico, 2018–2024",
       subtitle = "Less vs. more marginalized; INEGI Marco Geoestadístico 2025",
       x = NULL, 
       y = NULL) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(), 
    axis.text = element_blank(), 
    axis.ticks = element_blank(), 
    strip.text = element_text(face = "bold"),
    legend.position = "bottom", 
    legend.box = "horizontal", 
    legend.direction = "horizontal", 
    legend.justification = "center",
    legend.key.width = unit(0.8, "cm"),
    legend.background    = element_rect(
      fill      = "white",
      color     = "grey70",
      linewidth = 0.3),
    legend.key           = element_rect(
      fill      = "grey95",
      color     = "grey70",
      linewidth = 0.3))


# Print
p_marg_binary

# Save
ggsave("mexico_marg_group_2018_2024_faceted.png",
       p_marg_binary,
       width = 20, height = 8, dpi = 300)




##### BINARY GRADE OF MARGINALIZATION (marg_group) -----

# Create municipality-level binary marginalization data
munic_marg <- SAEH_clean %>%
  filter(!is.na(marg_group)) %>%
  distinct(munic_id, marg_group)  # Use munic_id

# Join to municipality polygons
mexico_munic_map <- mexico_municipios %>%
  left_join(munic_marg, by = c("munic_code" = "munic_id"))

# Single map
p_marg_binary <- ggplot() +
  geom_sf(data = mexico_munic_map, aes(fill = marg_group), color = NA) +
  scale_fill_manual(values = c("Less marginalized" = "#eff3ff", "More marginalized" = "#08519c"),
                    drop = FALSE, na.value = "white", name = "Marginalization group") +
  labs(title = "Municipal Marginalization Group in Mexico, 2020",
       subtitle = "INEGI Marco Geoestadístico", x = NULL, y = NULL) +
  theme_minimal() +
  theme(panel.grid = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
        legend.position = "bottom", legend.box = "horizontal", legend.direction = "horizontal",
        legend.justification = "center", legend.key.width = unit(0.8, "cm"),
        legend.background = element_rect(fill = "white", color = "grey70", linewidth = 0.3),
        legend.key = element_rect(fill = "grey95", color = "grey70", linewidth = 0.3))

p_marg_binary
ggsave("mexico_marg_group_single.png", p_marg_binary, width = 10, height = 8, dpi = 300)


##### GRADE OF MARGINALIZATION (QUINTILES) -----

# Create municipality-level quintile marginalization
munic_marg_quint <- SAEH_clean %>%
  filter(!is.na(grade_marg)) %>%  # Adjust variable name if needed
  mutate(grade_marg = factor(grade_marg, levels = c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto"))) %>%
  distinct(munic_id, grade_marg)

SAEH_full <- SAEH_model_data %>%
  mutate(gest_age_cat = case_when(
    gestat_clean <= 12 ~ "Early",
    gestat_clean >= 13 ~ "Late",
    is.na(gestat_clean) ~ NA_character_)) %>%
  mutate(gest_age_cat = factor(gest_age_cat, levels = c("Early", "Late")))

# Join to municipality polygons
mexico_munic_map_quint <- mexico_municipios %>%
  left_join(munic_marg_quint, by = c("munic_code" = "munic_id"))

# Single map with 5-color palette
p_marg_quint <- ggplot() +
  geom_sf(data = mexico_munic_map_quint, aes(fill = grade_marg), color = NA) +
  scale_fill_brewer(palette = "Blues", drop = FALSE, na.value = "white", 
                    name = "Marginalization quintile",
                    guide = guide_legend(
                      direction = "horizontal",
                      title.position = "top",
                      label.position = "bottom",
                      keywidth = unit(1, "cm"),     # Longer key
                      keyheight = unit(0.6, "cm"),  # Taller boxes
                      nrow = 1,                     # Force single row
                      byrow = TRUE)) +
  labs(title = "Grade of Marginalization Quintiles by Municipality in Mexico, 2020",
       subtitle = "INEGI Marco Geoestadístico", x = NULL, y = NULL) +
  theme_minimal() +
  theme(panel.grid = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill = "white", color = "grey70", linewidth = 0.3),
        legend.key = element_rect(fill = "grey95", color = "grey70", linewidth = 0.3))

p_marg_quint
ggsave("mexico_grade_marg_quintiles_single.png", p_marg_quint, width = 10, height = 8, dpi = 300)



