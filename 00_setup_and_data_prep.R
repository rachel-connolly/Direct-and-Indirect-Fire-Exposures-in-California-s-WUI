# ==============================================================================
# 00: Setup and shared data prep
# ==============================================================================
# Run this script first, in the same R session as the numbered scripts that
# follow it. It loads the core inputs and builds the objects
# that more than one downstream script depends on:
#
#   data_dir                          - path to the data folder (data/)
#   wui_rad, wui_only, non_wui_only   - WUI block polygons (all / WUI-only / non-WUI-only)
#   non_wui_only_df                   - non_wui_only with geometry dropped
#   fire_p                            - CAL FIRE FRAP perimeters, 2008-2018
#   ca_albers                         - shared CRS (from fire_p) used throughout
#   ca, ca_albers_sf                  - CA state outline (raw / projected)
#   ca_counties                       - CA county outline
#   cz, cz_prj                        - CA climate zone regions (raw / projected)
#   pm_fire_all                       - fire-only mean smoke PM2.5 raster, 2008-2018
#   wui_rad$cmaq_all                  - area-weighted mean smoke PM2.5 per block
#   wui_rad_census_df                 - wui_rad + income/age/ethnicity census join (no geometry)
#   wui_rad_census_r_df               - wui_rad + race-stratified census join (no geometry)
#   add_strip()                       - shared ggplot helper (gray title strip above a panel)
#
# Downstream scripts:
#   01_table1_wui_characterization.R
#   02_burned_area_table1_table2_figure2.R
#   03_table3_smoke_pm_exposure.R
#   04_table4_combined_exposure_and_figure4.R
#   05_figure3_smoke_by_climate_zone.R
#   06_figure1_main_map_panels.R
#   07_figureS1_wui_designation_map.R
#   08_mortality_analysis_and_figure.R
#   09_figureS4_monitoring_stations_map.R
# ==============================================================================

library(raster)        # raster manipulation
library(sf)
library(terra)
library(exactextractr)
library(dplyr)
library(lwgeom)
library(ggplot2)
library(viridis)
library(units)
library(ggtext)

options(scipen = 999)  # turn back on with options(scipen = 0)

# ------------------------------------------------------------------------
# Run these scripts with your R working directory set to this folder:
# either open WUI_analysis.Rproj in RStudio (sets it automatically), or
# call setwd("path/to/this/folder") yourself before sourcing 00 onward.
# data_dir, and every ggsave()/write.csv() output across all 10 scripts,
# are relative to that working directory.
#
# All data inputs used by these scripts are in one folder, "data/". Every
# st_read()/read.csv()/rast() call across all 10 scripts builds its path
# from this single `data_dir` variable.
#
# NOTE: the CMAQ daily rasters and the WUI block shapefile are not included
# in this repository (too large) - see README.md for the public sources to
# download them from before running this script.
# ------------------------------------------------------------------------
data_dir <- file.path(getwd(), "data")

# ------------------------------------------------------------------------
# WUI block polygons + WUI classification
# ------------------------------------------------------------------------
# UPDATE WITH NEW SHAPEFILE if the WUI layer is revised
wui_rad <- st_read(file.path(data_dir, "Shapefiles/CA_wui_block_1990_2020_change_v4_repr.shp"))

wui_rad$wui_class_update <- wui_rad$WUICLASS_2
wui_rad$wui_class_update[wui_rad$WUIFLAG202 == 1 | wui_rad$WUIFLAG202 == 2] <- "WUI"
wui_rad$wui_class_update[wui_rad$WUICLASS_2 == 'Very_Low_Dens_NoVeg' |
                            wui_rad$WUICLASS_2 == 'Low_Dens_NoVeg'] <- "Low_Very_Low_Housing_Density_no_veg"
wui_rad$wui_class_update[wui_rad$WUICLASS_2 == 'Med_Dens_NoVeg' |
                            wui_rad$WUICLASS_2 == 'High_Dens_NoVeg'] <- "Medium_high_housing_density_no_veg"
table(wui_rad$wui_class_update)

wui_only     <- subset(wui_rad, wui_rad$WUIFLAG202 == 1 | wui_rad$WUIFLAG202 == 2)
non_wui_only <- subset(wui_rad, wui_rad$WUIFLAG202 == 0)

st_crs(wui_rad) # wgs 84 pseudo mercator, before reprojection below

# ------------------------------------------------------------------------
# Fire perimeters + shared CRS (California Albers, from fire_p)
# ------------------------------------------------------------------------
fire_p <- st_read(file.path(data_dir, "Shapefiles/fire22_1.gdb/fire22_1.gdb"), layer = "firep22_1_2008to2018")
ca_albers <- st_crs(fire_p)

wui_rad      <- st_transform(wui_rad, ca_albers)
wui_only     <- st_transform(wui_only, ca_albers)
non_wui_only <- st_transform(non_wui_only, ca_albers)

# ------------------------------------------------------------------------
# Area (km2) - used by Table 1 (area) and Table 2 (% burned)
# ------------------------------------------------------------------------
wui_only$area_calc     <- st_area(wui_only)
non_wui_only$area_calc <- st_area(non_wui_only)
wui_only$area_calc_km2     <- wui_only$area_calc / 1000000
non_wui_only$area_calc_km2 <- non_wui_only$area_calc / 1000000

# ------------------------------------------------------------------------
# Smoke PM2.5 raster (fire-only mean, 2008-2018) - used by Table 3,
# Figure 1, Figure 3, and Figure S4
# ------------------------------------------------------------------------
pm_fire_all <- rast(file.path(data_dir, "fire_only_mean_08_18.tif"))

# Area-weighted mean smoke PM2.5 for every block in wui_rad - used by Table 3's
# ANOVA/Tukey test, and by the Figure 1 and Figure S4 concentration maps
wui_rad$cmaq_all <- exact_extract(pm_fire_all, wui_rad, 'mean')

# ------------------------------------------------------------------------
# California outline + climate zone regions - used by Figures 1, 3, 4, S1, S8
# ------------------------------------------------------------------------
ca <- st_read(file.path(data_dir, "Shapefiles/CA_State_TIGER2016.shp"))
ca_albers_sf <- st_transform(ca, ca_albers)

# CA county outline - used by the mortality maps (Figure S7) and Figure S4
ca_counties <- st_read(file.path(data_dir, "CA_Counties/CA_Counties_TIGER2016.shp"))

cz <- st_read(file.path(data_dir, "Shapefiles/Ca__4th_Climate_Change_Assessment_Regions__CaNAD83_.shp"))
cz_prj <- st_transform(cz, ca_albers)
cz_prj$cz <- "CA Climate Zones" # constant label, used for the shape legend in map figures
cz_prj$cz_abbrev <- dplyr::case_when(
  cz_prj$RegionName == "Los Angeles"             ~ "LA",
  cz_prj$RegionName == "San Diego"               ~ "SD",
  cz_prj$RegionName == "Inland Desert"           ~ "ID",
  cz_prj$RegionName == "North Coast"             ~ "NC",
  cz_prj$RegionName == "San Francisco Bay Area"  ~ "SF",
  cz_prj$RegionName == "Central Coast"           ~ "CC",
  cz_prj$RegionName == "Sacramento Valley"       ~ "SV",
  cz_prj$RegionName == "San Joaquin Valley"      ~ "SJV",
  cz_prj$RegionName == "Sierra Nevada Mountains" ~ "SN",
  TRUE ~ cz_prj$RegionName
)

# ------------------------------------------------------------------------
# Census block group key (needed to join both census extracts below)
# ------------------------------------------------------------------------
wui_rad$block_group <- substr(wui_rad$BLK20, 1, 12)
wui_rad$block_group <- as.numeric(wui_rad$block_group)

# ------------------------------------------------------------------------
# Census join #1: income / age / race-ethnicity summary (Table 1, Table 4)
# ------------------------------------------------------------------------
census <- read.csv(file.path(data_dir, "census_wui_use.csv"))
census <- census %>% rename(block_group = Block.group)
census$age_perc <- as.numeric(census$age_perc)
census$race_eth <- as.numeric(census$race_eth)

wui_rad_census <- left_join(wui_rad, census, by = "block_group")
wui_rad_census_df <- wui_rad_census
wui_rad_census_df$geometry <- NULL

# ------------------------------------------------------------------------
# Census join #2: race-stratified percentages (Table S3)
# ------------------------------------------------------------------------
census_race <- read.csv(file.path(data_dir, "census_wui_use_race_strat.csv"))
census_race <- census_race %>%
  rename(block_group = Block.group) %>%
  mutate(across(
    c(X..Hispanic, X..White.Alone, X..Black.or.African.American.Alone,
      X..American.Indian.and.Alaska.Native, X..Asian,
      X..Native.Hawaiian.and.other.Pacific.Islander, X..Other.Race.and.Two.or.More.Races),
    ~ as.numeric(.x)
  )) %>%
  rename(
    Perc_Hispanic  = X..Hispanic,
    Perc_White     = X..White.Alone,
    Perc_Black     = X..Black.or.African.American.Alone,
    Perc_Am_Indian = X..American.Indian.and.Alaska.Native,
    Perc_Asian     = X..Asian,
    Perc_PI        = X..Native.Hawaiian.and.other.Pacific.Islander,
    Perc_other     = X..Other.Race.and.Two.or.More.Races
  )

wui_rad_census_r <- left_join(wui_rad, census_race, by = "block_group")
wui_rad_census_r_df <- wui_rad_census_r
wui_rad_census_r_df$geometry <- NULL

# ------------------------------------------------------------------------
# Shared plotting helper - adds a gray "facet strip"-style title bar above a
# ggplot panel, used across Figures 1, 4, S1, and S8.
# ------------------------------------------------------------------------
add_strip <- function(p, title) {
  p + labs(title = title) +
    theme(
      plot.title.position = "plot",
      plot.title = ggtext::element_textbox_simple(
        face = "plain",
        size = 10,
        halign = 0.5,
        lineheight = 0.95,
        color = "grey20",
        fill = "grey90",
        box.color = "grey85",
        padding = margin(3, 6, 3, 6),
        margin  = margin(0, 0, 3, 0)
      )
    )
}

# ------------------------------------------------------------------------
# Non-spatial version of non_wui_only (geometry dropped) - reused across
# Table 1 (area, ownership), Table 2 (burned area by category), and
# Table 3 (smoke PM by category).
# ------------------------------------------------------------------------
non_wui_only_df <- non_wui_only
non_wui_only_df$geometry <- NULL
