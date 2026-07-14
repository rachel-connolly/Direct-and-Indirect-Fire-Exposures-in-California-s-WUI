# ==============================================================================
# 08: Mortality attributable to smoke exposure in the
# WUI (Table S8, Figure S7)
# ==============================================================================
# Requires: 00_setup_and_data_prep.R run first (uses wui_only, ca_albers,
# ca_counties, add_strip(), data_dir)
#
# NOTE: mortality estimates are from Connolly et al. 2024. That analysis
# itself is out of scope here - this script only joins its ZIP-code output
# to WUI geography.
# ==============================================================================

library(dplyr)
library(sf)
library(tmap)
library(ggplot2)
library(viridis)
library(patchwork)
library(ggspatial)

zip <- st_read(file.path(data_dir, "Shapefiles/zip_poly.shp"))
tm_shape(zip) + tm_fill()

zcta_prj <- st_transform(zip, ca_albers)

mortality <- read.csv(file.path(data_dir, "main_estimate_mortality_zip.csv"))
mortality$ZIP_CODE <- as.character(mortality$GEOID20)
zip_mort <- left_join(zcta_prj, mortality, by = "ZIP_CODE")
# ~220 deaths are lost in this join - ZIP codes present in the mortality
# estimates but not in the ZCTA shapefile

# ==============================================================================
# Any-overlap WUI/ZIP intersection (initial pass)
# ==============================================================================
mort_wui <- st_intersection(wui_only, st_make_valid(zip_mort))

mort_wui_filter <- mort_wui %>% filter(!is.na(deaths_base))
mort_wui_df <- as.data.frame(mort_wui_filter)
mort_wui_df$geometry <- NULL

mort_wui_unique <- mort_wui_df %>% distinct(ZIP_CODE, Year, .keep_all = TRUE)

mort_wui_sum <- mort_wui_unique %>%
  group_by(ZIP_CODE) %>%
  summarize(total_deaths = sum(deaths_base))
# 1,208 ZIP codes overlap with WUI areas (any overlap)

mort_wui_sf_allyr <- left_join(zcta_prj, mort_wui_sum, by = "ZIP_CODE")
mort_wui_sf_filter_allyr <- mort_wui_sf_allyr %>% filter(!is.na(total_deaths))

mort_wui_final_df <- as.data.frame(mort_wui_sf_filter_allyr)
mort_wui_final_df$geometry <- NULL
#write.csv(mort_wui_final_df, "mortality_wui_zips_all_years.csv")
#st_write(mort_wui_sf_filter_allyr, "mortality_wui_zips_all_years.shp")

tm_shape(mort_wui_sf_filter_allyr) + tm_polygons(fill = "total_deaths", col = NA, lwd = 0)

# ==============================================================================
# Table S8: mortality by % ZIP-code overlap with WUI (10/25/50/75/90% cutoffs)
# ==============================================================================
intersect_wui_zip <- st_intersection(wui_only, st_make_valid(zcta_prj))

zip_codes <- zcta_prj %>% mutate(zip_area = st_area(.))
intersections <- intersect_wui_zip %>% mutate(intersect_area = st_area(.))

zip_code_df <- as.data.frame(zip_codes)
intersections_df <- as.data.frame(intersections)

coverage <- intersections_df %>%
  group_by(ZIP_CODE) %>%
  summarize(total_intersect_area = sum(intersect_area, na.rm = TRUE)) %>%
  left_join(zip_code_df, by = "ZIP_CODE") %>%
  mutate(coverage_pct = as.numeric(total_intersect_area / zip_area * 100))

coverage_10 <- coverage %>% filter(coverage_pct >= 10)
coverage_25 <- coverage %>% filter(coverage_pct >= 25)
coverage_50 <- coverage %>% filter(coverage_pct >= 50)
coverage_75 <- coverage %>% filter(coverage_pct >= 75)
coverage_90 <- coverage %>% filter(coverage_pct >= 90)
# 717 ZIPs >= 10% covered, 485 >= 25%, 225 >= 50%

coverage_10_sf <- left_join(zip_mort, coverage_10, by = "ZIP_CODE")
coverage_10_sf_filt <- coverage_10_sf %>% filter(!is.na(coverage_pct))
length(unique(coverage_10_sf_filt$ZIP_CODE)) # should be 717

coverage_10_filt <- left_join(coverage_10, zip_mort, by = "ZIP_CODE")
coverage_10_summary <- coverage_10_filt %>% group_by(Year) %>% summarise(sum_deaths = sum(deaths_base))
coverage_25_filt <- left_join(coverage_25, zip_mort, by = "ZIP_CODE")
coverage_25_summary <- coverage_25_filt %>% group_by(Year) %>% summarise(sum_deaths = sum(deaths_base))
coverage_50_filt <- left_join(coverage_50, zip_mort, by = "ZIP_CODE")
coverage_50_summary <- coverage_50_filt %>% group_by(Year) %>% summarise(sum_deaths = sum(deaths_base))
coverage_75_filt <- left_join(coverage_75, zip_mort, by = "ZIP_CODE")
coverage_75_summary <- coverage_75_filt %>% group_by(Year) %>% summarise(sum_deaths = sum(deaths_base))
coverage_90_filt <- left_join(coverage_90, zip_mort, by = "ZIP_CODE")
coverage_90_summary <- coverage_90_filt %>% group_by(Year) %>% summarise(sum_deaths = sum(deaths_base))

combined_df <- coverage_10_summary %>%
  full_join(coverage_25_summary, by = "Year", suffix = c("_10", "_25")) %>%
  full_join(coverage_50_summary, by = "Year", suffix = c("", "_50")) %>%
  full_join(coverage_75_summary, by = "Year", suffix = c("", "_75")) %>%
  full_join(coverage_90_summary, by = "Year", suffix = c("", "_90")) %>%
  rename(sum_deaths_50 = sum_deaths, sum_deaths_75 = sum_deaths_75, sum_deaths_90 = sum_deaths_90)

cleaned_df <- combined_df %>% filter(!if_all(everything(), is.na))
cleaned_df <- cleaned_df %>% mutate(Year = as.character(Year))

total_row <- cleaned_df %>%
  summarise(across(starts_with("sum_deaths"), ~ round(sum(.x, na.rm = TRUE)))) %>%
  mutate(Year = "Total")

final_df <- bind_rows(cleaned_df, total_row) %>%
  mutate(across(starts_with("sum_deaths"), ~ formatC(.x, format = "f", big.mark = ",", digits = 0)))
final_df
# main text uses the 50% overlap definition;
# more stringent overlap definitions are noted as still roughly double the direct total

# ==============================================================================
# Figure S7: mortality maps at the 25% and 50% WUI-overlap thresholds
# ==============================================================================
coverage_map_data_50 <- coverage_50_filt %>% group_by(ZIP_CODE) %>% summarise(sum_deaths = sum(deaths_base))
map_data_50 <- left_join(zcta_prj, coverage_map_data_50, by = "ZIP_CODE")

coverage_map_data_25 <- coverage_25_filt %>% group_by(ZIP_CODE) %>% summarise(sum_deaths = sum(deaths_base))
map_data_25 <- left_join(zcta_prj, coverage_map_data_25, by = "ZIP_CODE")

lims <- range(c(map_data_50$sum_deaths, map_data_25$sum_deaths), na.rm = TRUE) # shared color scale

make_map <- function(dat, colorbar_title, show_legend = TRUE) {
  ggplot(dat) +
    geom_sf(aes(fill = sum_deaths), color = NA) +
    geom_sf(data = ca_counties, fill = NA, color = "lightgray", size = 0.4,
            aes(linetype = "CA Counties")) +
    scale_fill_viridis(
      option = "D", direction = -1, na.value = "#F5F5F5", limits = lims, oob = scales::squish,
      guide = if (show_legend) guide_colorbar(title = colorbar_title, title.position = "top",
                                               title.hjust = 0, barwidth = unit(0.5, "cm"),
                                               barheight = unit(4, "cm")) else "none"
    ) +
    scale_linetype_manual(
      name = NULL, values = c("CA Counties" = "solid"),
      guide = guide_legend(order = 3, override.aes = list(color = "lightgray"))
    ) +
    ggspatial::annotation_scale(location = "bl", width_hint = 0.2) + # applies to both S7 panels, since both call make_map()
    theme(
      axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank(),
      panel.background = element_rect(fill = "white", color = NA), legend.title.align = 0,
      legend.position = if (show_legend) "right" else "none",
      legend.text = element_text(size = 9), legend.title = element_text(size = 10)
    )
}

p50 <- make_map(map_data_50, "Total Deaths, 2008-2018\nZIP Codes meeting 50% WUI Criteria", show_legend = FALSE)
p25 <- make_map(map_data_25, "Total Deaths, 2008-2018", show_legend = TRUE)

p25 <- p25 +
  guides(
    fill = guide_colorbar(order = 1, title = "Total Deaths, 2008-2018", title.position = "top", title.hjust = 0),
    linetype = guide_legend(order = 2, title = NULL, override.aes = list(color = "lightgray"))
  )

p50_strip <- add_strip(p50, "ZIP Codes Meeting 50% WUI Criterion")
p25_strip <- add_strip(p25, "ZIP Codes Meeting 25% WUI Criterion")
p25_strip <- p25_strip +
  theme(legend.position = c(0.96, 0.75), legend.justification = c(1, 0.5))

final_panel <- p50_strip + p25_strip
final_panel

ggsave("wui_deaths_50_25_panel_v10.pdf", final_panel, width = 11, height = 10)
ggsave("wui_deaths_50_25_panel_v10.png", final_panel, width = 11, height = 10)
