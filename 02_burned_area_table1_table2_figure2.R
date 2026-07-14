# ==============================================================================
# 02: Burned area - Table 1 (% burned), Table 2 (annual
# summary), Table S4/S5 (sensitivity checks), Figure 2 (burned area by year)
# ==============================================================================
# Requires: 00_setup_and_data_prep.R run first (uses wui_rad, wui_only,
# non_wui_only, non_wui_only_df, fire_p, ca_albers)
#
# ==============================================================================

library(dplyr)
library(sf)
library(units)
library(ggplot2)
library(reshape2)

# ==============================================================================
# Re-burn area: % of California's burned area that burned more than once,
# 2008-2018 (quoted in Results: "re-burns (4.9% of total burned area)")
# ==============================================================================
union_area <- st_area(st_union(fire_p)) |> sum() # total burned area, no overlap
print(union_area)
# 32527446231 [m^2]

intersection <- st_intersection(fire_p)
intersection$area_int <- st_area(intersection)
intersection_dup <- subset(intersection, intersection$n.overlaps > 1)

dissolved_geom <- st_union(intersection_dup)
sf_diss_r <- st_sf(
  geometry = st_cast(
    st_collection_extract(dissolved_geom, "POLYGON", warn = FALSE),
    "POLYGON",
    warn = FALSE
  )
)
sf_diss_r$area_test <- st_area(sf_diss_r)

total_area_int <- sum(st_area(dissolved_geom))
total_area_km2 <- set_units(total_area_int, "km^2")
print(total_area_km2)
# 1588.569 [km^2] burned more than once
# 32527446231 [m^2] unique total burned area
# 1588569175 / 32527446231 = 4.9%

# ==============================================================================
# Total burned area, WUI vs. non-WUI, and by non-WUI category (feeds Table 1)
# ==============================================================================
fire_p_wui    <- st_intersection(fire_p, wui_only)
fire_p_nonwui <- st_intersection(fire_p, non_wui_only)

fire_p_wui$area_calc    <- st_area(fire_p_wui)
fire_p_nonwui$area_calc <- st_area(fire_p_nonwui)

sum(fire_p_wui$area_calc) / 1000000
# 1321.349

# area-weighted % of WUI that burned
(sum(fire_p_wui$area_calc) / 1000000) / (sum(wui_only$area_calc_km2))
# 0.04753661 -> 4.8%

sum(fire_p_nonwui$area_calc) / 1000000
# 31909.72

fire_p_nonwui_df <- fire_p_nonwui
fire_p_nonwui_df$geometry <- NULL

# Precise burned-area percentages by non-WUI class
area_nonwui_all <- non_wui_only_df %>%
  group_by(wui_class_update) %>%
  summarise(sum_area = sum(area_calc) / 1000000)

burn_area_nonwui <- fire_p_nonwui_df %>%
  group_by(wui_class_update) %>%
  summarise(sum_burned_area = sum(area_calc) / 1000000)

nonwui_areas_tbl <- burn_area_nonwui
nonwui_areas_tbl$sum_area <- area_nonwui_all$sum_area
nonwui_areas_tbl$sum_burned_area_km <- as.numeric(nonwui_areas_tbl$sum_burned_area)
nonwui_areas_tbl$sum_area_km <- as.numeric(nonwui_areas_tbl$sum_area)

sum_values <- colSums(nonwui_areas_tbl[sapply(nonwui_areas_tbl, is.numeric)])
sum_row <- as.data.frame(t(sum_values))
sum_row <- cbind(wui_class_update = "Total", sum_row)
nonwui_areas_tbl_ <- rbind(nonwui_areas_tbl, sum_row)

nonwui_areas_tbl_$percent <- with(nonwui_areas_tbl_,
                                   ifelse(sum_burned_area_km > 0, sum_burned_area_km / sum_area_km, NA))
nonwui_areas_tbl_$percent_col <- with(nonwui_areas_tbl_,
                                       ifelse(sum_burned_area_km > 0,
                                              sprintf("%.1f%%", (sum_burned_area_km / sum_area_km) * 100), NA))
nonwui_areas_tbl_

# ==============================================================================
# Burned area significance tests: WUI vs. each non-WUI category (Table 1)
# ==============================================================================
fire_p$area_calc <- st_area(fire_p)

fire_p_nonwui$area_calc_burn <- st_area(fire_p_nonwui)
fire_p_nonwui$area_calc_burn_km2 <- fire_p_nonwui$area_calc_burn / 1000000
fire_p_wui$area_calc_burn <- st_area(fire_p_wui)
fire_p_wui$area_calc_burn_km2 <- fire_p_wui$area_calc_burn / 1000000

fire_p_nonwui_df2 <- as.data.frame(fire_p_nonwui)
fire_p_nonwui_df2$geometry <- NULL
fire_p_wui_df2 <- as.data.frame(fire_p_wui)
fire_p_wui_df2$geometry <- NULL

# aggregate to block level first - a block can have multiple overlapping fire records
fire_p_nonwui_agg <- fire_p_nonwui_df2 %>%
  group_by(BLK20) %>%
  summarize(area_burn_sum = sum(area_calc_burn_km2), area_blocks_sum = sum(area_calc_km2))
fire_p_nonwui_agg$perc_burn <- as.numeric(fire_p_nonwui_agg$area_burn_sum / fire_p_nonwui_agg$area_blocks_sum)

fire_p_wui_agg <- fire_p_wui_df2 %>%
  group_by(BLK20) %>%
  summarize(area_burn_sum = sum(area_calc_burn_km2), area_blocks_sum = sum(area_calc_km2))
fire_p_wui_agg$perc_burn <- as.numeric(fire_p_wui_agg$area_burn_sum / fire_p_wui_agg$area_blocks_sum)

t.test(fire_p_wui_agg$perc_burn, fire_p_nonwui_agg$perc_burn, var.equal = FALSE)
# not significant here - still needs blocks with 0% burned area added back (below)

wui_only_df <- as.data.frame(wui_only)
wui_only_df$geometry <- NULL
# non_wui_only_df (from 00_setup) already has BLK20 + area_calc, which is all
# this block-level aggregation needs - reused directly rather than making
# another geometry-dropped copy of non_wui_only.

wui_only_df_blk <- wui_only_df %>%
  group_by(BLK20) %>%
  summarize(area_notuse = sum(area_calc))
wui_area_join <- left_join(wui_only_df_blk, fire_p_wui_agg, by = "BLK20")
wui_area_join$perc_burn[is.na(wui_area_join$perc_burn)] <- 0

nonwui_only_df_blk <- non_wui_only_df %>%
  group_by(BLK20) %>%
  summarize(area_notuse = sum(area_calc))
nonwui_area_join <- left_join(nonwui_only_df_blk, fire_p_nonwui_agg, by = "BLK20")
nonwui_area_join$perc_burn[is.na(nonwui_area_join$perc_burn)] <- 0

t.test(wui_area_join$perc_burn, nonwui_area_join$perc_burn, var.equal = FALSE)
# significantly higher mean % burned in non-WUI once 0%-burned blocks are included

# Assign each non-WUI block a single classification (the one with the most area,
# since ~a small share of blocks have multiple classifications)
primary_classification <- non_wui_only_df %>%
  group_by(BLK20) %>%
  slice_max(area_calc, with_ties = FALSE) %>%
  select(BLK20, wui_class_update) %>%
  ungroup()

nonwui_area_join2 <- left_join(nonwui_area_join, primary_classification, by = "BLK20")

wui_df <- wui_area_join %>%
  select(perc_burn) %>%
  mutate(wui_class_update = "WUI")

nonwui_df <- nonwui_area_join2 %>%
  select(perc_burn, wui_class_update)

combined_df <- bind_rows(wui_df, nonwui_df)

nonwui_categories <- c("Medium_high_housing_density_no_veg",
                        "Low_Very_Low_Housing_Density_no_veg",
                        "Uninhabited_NoVeg", "Uninhabited_Veg",
                        "Very_Low_Dens_Veg", "Water")

# Exclude water and non-vegetated uninhabited areas (not meaningful comparisons for Table 1)
nonwui_categories_filtered <- nonwui_categories[!nonwui_categories %in% c("Uninhabited_NoVeg", "Water")]

t_test_results <- lapply(nonwui_categories_filtered, function(category) {
  test_result <- t.test(perc_burn ~ wui_class_update,
                         data = combined_df %>%
                           filter(wui_class_update %in% c("WUI", category)))

  mean_wui <- combined_df %>%
    filter(wui_class_update == "WUI") %>%
    summarize(mean_perc = mean(perc_burn, na.rm = TRUE)) %>%
    pull(mean_perc)

  mean_nonwui <- combined_df %>%
    filter(wui_class_update == category) %>%
    summarize(mean_perc = mean(perc_burn, na.rm = TRUE)) %>%
    pull(mean_perc)

  p_value <- round(test_result$p.value, 10)
  comparison_result <- ifelse(mean_wui > mean_nonwui, "WUI > Non-WUI", "Non-WUI > WUI")

  data.frame(
    Comparison = paste("WUI vs", category),
    Mean_WUI = mean_wui,
    Mean_NonWUI = mean_nonwui,
    Mean_Diff = mean_wui - mean_nonwui,
    CI_Lower = test_result$conf.int[1],
    CI_Upper = test_result$conf.int[2],
    P_Value = p_value,
    WUI_Greater = comparison_result
  )
})
names(t_test_results) <- nonwui_categories_filtered

t_test_summary <- do.call(rbind, t_test_results)
print(t_test_summary)

# Mean values are the average % burned area across all blocks in each category,
# including blocks with 0% burned area. For non-WUI blocks with more than one
# classification (a small share of the data), the classification with the
# majority of that block's area is used.

# ==============================================================================
# Table 2: annual burned area within the WUI, 2008-2018
# ==============================================================================
fire_p_wui_df <- fire_p_wui
fire_p_wui_df$geometry <- NULL

burn_acres_summary_wui <- fire_p_wui_df %>%
  group_by(YEAR_, wui_class_update) %>%
  summarize(Burned_area = sum(area_calc))
burn_acres_summary_wui$Burned_area_km <- burn_acres_summary_wui$Burned_area / 1000000
burn_acres_summary_wui$Burned_area_10_3_km <- burn_acres_summary_wui$Burned_area / 1000000000

sum(wui_only$area_calc_km2)
# 27796.44

burn_acres_summary_wui$percent <- burn_acres_summary_wui$Burned_area_km / (sum(wui_only$area_calc_km2))
burn_acres_summary_wui$percent_column <- sprintf("%.2f%%", burn_acres_summary_wui$percent * 100)
burn_acres_summary_wui

# ==============================================================================
# Table S5: sensitivity check - WUI defined as WUI in EITHER 2010 or 2020
# ==============================================================================
wui_only_both_yr <- subset(wui_rad, wui_rad$WUIFLAG202 == 1 | wui_rad$WUIFLAG202 == 2 |
                              wui_rad$WUIFLAG201 == 1 | wui_rad$WUIFLAG201 == 2)
# total of 156,979 aligns with the prior WUI comparison analysis (Oct 2024)

fire_p_wui_both_yr <- st_intersection(fire_p, wui_only_both_yr)
fire_p_wui_both_yr$area_calc <- st_area(fire_p_wui_both_yr)

wui_only_both_yr$area_calc <- st_area(wui_only_both_yr)
sum(wui_only_both_yr$area_calc)
# 29470983355 [m^2] = 29470.983355 [km^2] = 29470.983355 [10^3 km^2] (used for % below)

fire_p_wui_both_yr$wui_class_update <- "wui"
fire_p_wui_both_yr_df <- fire_p_wui_both_yr
fire_p_wui_both_yr_df$geometry <- NULL

burn_acres_summary_wui_both_yr <- fire_p_wui_both_yr_df %>%
  group_by(YEAR_, wui_class_update) %>%
  summarize(Burned_area = sum(area_calc))
burn_acres_summary_wui_both_yr$Burned_area_km <- burn_acres_summary_wui_both_yr$Burned_area / 1000000
sum(burn_acres_summary_wui_both_yr$Burned_area_km)
# 1550.172

burn_acres_summary_wui_both_yr$percent <- burn_acres_summary_wui_both_yr$Burned_area_km / 29470.983355
burn_acres_summary_wui_both_yr$percent_column <- burn_acres_summary_wui_both_yr$percent * 100
burn_acres_summary_wui_both_yr

# ==============================================================================
# Figure 2: annual burned area, WUI vs. non-WUI by category
# ==============================================================================
# Recode to the reader-friendly labels used specifically for this figure's legend
# (distinct from wui_class_update's underscore-style labels used in the tables above)
fire_p_nonwui$wui_class_update <- fire_p_nonwui$WUICLASS_2
fire_p_nonwui$wui_class_update[fire_p_nonwui$WUICLASS_2 == 'Very_Low_Dens_NoVeg' |
                                  fire_p_nonwui$WUICLASS_2 == 'Low_Dens_NoVeg'] <- "Low/Very Low Housing Density"
fire_p_nonwui$wui_class_update[fire_p_nonwui$WUICLASS_2 == 'Med_Dens_NoVeg' |
                                  fire_p_nonwui$WUICLASS_2 == 'High_Dens_NoVeg'] <- "Medium/High Housing Density"
fire_p_nonwui$wui_class_update[fire_p_nonwui$WUICLASS_2 == 'Uninhabited_Veg'] <- "No Housing"
fire_p_nonwui$wui_class_update[fire_p_nonwui$WUICLASS_2 == 'Very_Low_Dens_Veg'] <- "Very Low Housing Density"
table(fire_p_nonwui$wui_class_update)

fire_p_nonwui_df <- fire_p_nonwui
fire_p_nonwui_df$geometry <- NULL
fire_nonwui_areas <- fire_p_nonwui_df %>%
  group_by(YEAR_, wui_class_update) %>%
  summarize(area = sum(area_calc))

fire_wui_areas <- fire_p_wui_df %>%
  group_by(YEAR_) %>%
  summarize(area = sum(area_calc))
fire_wui_areas$wui_class_update <- 'WUI'

fire_areas_by_type <- rbind(fire_wui_areas, fire_nonwui_areas)
fire_areas_by_type <- subset(fire_areas_by_type,
                              fire_areas_by_type$wui_class_update != 'Water' &
                                fire_areas_by_type$wui_class_update != "Uninhabited_NoVeg")

fire_areas_by_type$'Burned area (acres)' <- set_units(fire_areas_by_type$area, acre)

fire_plot <- fire_areas_by_type %>%
  ggplot(aes(x = YEAR_, y = `Burned area (acres)`, group = wui_class_update, color = wui_class_update)) +
  geom_line() +
  ggtitle("CA Burned Area: WUI vs. Non-WUI") +
  ylab("Burned Area") +
  xlab("Year") +
  scale_color_discrete(name = "")
fire_plot

sum(fire_areas_by_type$`Burned area (acres)`)
# slightly over 8 million acres total, consistent with the totals above
