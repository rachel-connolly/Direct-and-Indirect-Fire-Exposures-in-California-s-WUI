# ==============================================================================
# 01: Table 1 (WUI characterization) + Table S2 + Table S3
# ==============================================================================
# Requires: 00_setup_and_data_prep.R run first (uses wui_rad, wui_only,
# non_wui_only, non_wui_only_df, wui_rad_census_df, wui_rad_census_r_df,
# ca_albers, data_dir)
#
# Produces:
#   - Table 1: income, race/ethnicity, age, population density, area, by WUI class
#   - Table S2: public ownership by WUI class
#   - Table S3: race-stratified percentages + WUI vs. non-WUI significance tests
# ==============================================================================

library(dplyr)
library(sf)

# ------------------------------------------------------------------------
# Income / age / race-ethnicity summary by WUI class
# ------------------------------------------------------------------------
# QC: check how many blocks failed to match a census block group after the join
review_inc  <- wui_rad_census_df[is.na(wui_rad_census_df$Median_hh_income), ]
review_race <- wui_rad_census_df[is.na(wui_rad_census_df$race_eth), ]
review_age  <- wui_rad_census_df[is.na(wui_rad_census_df$age_perc), ]

wui_rad_census_df_pop <- subset(wui_rad_census_df, wui_rad_census_df$POP2020 > 0) # populated blocks only

wui_census_sum <- wui_rad_census_df_pop %>%
  group_by(wui_class_update) %>%
  summarize(med_income = mean(Median_hh_income, na.rm = TRUE),
            perc_race_eth = mean(race_eth, na.rm = TRUE),
            perc_age = mean(age_perc, na.rm = TRUE))
wui_census_sum

# ------------------------------------------------------------------------
# Significance test: race/ethnicity across WUI vs. two non-WUI density classes
# (this is the ANOVA/Tukey result quoted in the WUI characterization section
# of the results, "mean differences of 14.7%... and 19.0%...")
# ------------------------------------------------------------------------
wui_rad_census_df_pop_3cls <- wui_rad_census_df_pop %>%
  st_drop_geometry() %>%
  filter(wui_class_update %in% c(
    "WUI",
    "Low_Very_Low_Housing_Density_no_veg",
    "Medium_high_housing_density_no_veg"
  )) %>%
  select(race_eth, wui_class_update) %>%
  filter(!is.na(race_eth), !is.na(wui_class_update))

wui_rad_census_df_pop_3cls$wui_class_update <- factor(
  wui_rad_census_df_pop_3cls$wui_class_update,
  levels = c("WUI",
             "Low_Very_Low_Housing_Density_no_veg",
             "Medium_high_housing_density_no_veg")
)

wui_rad_census_df_pop_3cls_stats <- wui_rad_census_df_pop_3cls %>%
  group_by(wui_class_update) %>%
  summarise(n = n(), mean = mean(race_eth), sd = sd(race_eth), .groups = "drop")
wui_rad_census_df_pop_3cls_stats

fit_aov_wui_pop3 <- aov(race_eth ~ wui_class_update, data = wui_rad_census_df_pop_3cls)
summary(fit_aov_wui_pop3)

tukey_res <- TukeyHSD(fit_aov_wui_pop3)
tukey_res

# Welch ANOVA (variance-robust) - reported alongside Tukey as a robustness check
oneway.test(race_eth ~ wui_class_update, data = wui_rad_census_df_pop_3cls, var.equal = FALSE)

# Tukey results as a data frame for the manuscript text (mean diffs + 95% CIs)
tukey_df <- as.data.frame(tukey_res$wui_class_update)
tukey_df$comparison <- rownames(tukey_df)
rownames(tukey_df) <- NULL
tukey_df ## RESULTS QUOTED IN WUI CHARACTERIZATION SECTION (RACE/ETHNICITY)

# ------------------------------------------------------------------------
# Non-WUI summary (all non-WUI, excluding water/uninhabited) for comparison
# ------------------------------------------------------------------------
all_nonwui <- subset(wui_rad_census_df_pop, wui_rad_census_df_pop$WUIFLAG202 == 0)
all_nonwui_use <- subset(all_nonwui, !all_nonwui$wui_class_update == "Water" &
                            !all_nonwui$wui_class_update == "Uninhabited_NoVeg")

wui_census_sum_nw <- all_nonwui_use %>%
  summarize(med_income = mean(Median_hh_income, na.rm = TRUE),
            perc_race_eth = mean(race_eth, na.rm = TRUE),
            perc_age = mean(age_perc, na.rm = TRUE))
wui_census_sum_nw

# ==============================================================================
# Table S3: race-stratified percentages by WUI class
# ==============================================================================
review_race_strat <- wui_rad_census_r_df[is.na(wui_rad_census_r_df$Perc_Hispanic), ] # 601 blocks unmatched

wui_rad_census_r_df_pop <- subset(wui_rad_census_r_df, wui_rad_census_r_df$POP2020 > 0)

wui_census_r_sum <- wui_rad_census_r_df_pop %>%
  group_by(wui_class_update) %>%
  summarize(hispanic = mean(Perc_Hispanic, na.rm = TRUE),
            white = mean(Perc_White, na.rm = TRUE),
            black = mean(Perc_Black, na.rm = TRUE),
            am_indian = mean(Perc_Am_Indian, na.rm = TRUE),
            asian = mean(Perc_Asian, na.rm = TRUE),
            PI = mean(Perc_PI, na.rm = TRUE),
            other = mean(Perc_other, na.rm = TRUE))
wui_census_r_sum

# Compare WUI vs. all non-WUI (excluding water/uninhabited) by race/ethnicity
wui_rad_census_r_df_pop_WUI <- wui_rad_census_r_df_pop %>%
  filter(wui_class_update == "WUI")

all_nonwui_r <- subset(wui_rad_census_r_df_pop, wui_rad_census_r_df_pop$WUIFLAG202 == 0)
all_nonwui_r_use <- subset(all_nonwui_r, !all_nonwui_r$wui_class_update == "Water" &
                              !all_nonwui_r$wui_class_update == "Uninhabited_NoVeg")

t.test(na.omit(all_nonwui_r_use$Perc_Hispanic),  na.omit(wui_rad_census_r_df_pop_WUI$Perc_Hispanic))
t.test(na.omit(all_nonwui_r_use$Perc_White),     na.omit(wui_rad_census_r_df_pop_WUI$Perc_White))
t.test(na.omit(all_nonwui_r_use$Perc_Black),     na.omit(wui_rad_census_r_df_pop_WUI$Perc_Black))
t.test(na.omit(all_nonwui_r_use$Perc_Am_Indian), na.omit(wui_rad_census_r_df_pop_WUI$Perc_Am_Indian))
t.test(na.omit(all_nonwui_r_use$Perc_Asian),     na.omit(wui_rad_census_r_df_pop_WUI$Perc_Asian))
t.test(na.omit(all_nonwui_r_use$Perc_PI),        na.omit(wui_rad_census_r_df_pop_WUI$Perc_PI))
t.test(na.omit(all_nonwui_r_use$Perc_other),     na.omit(wui_rad_census_r_df_pop_WUI$Perc_other))

# Effect size check for PI (small group - a sanity check on the t-test above)
x <- na.omit(all_nonwui_r_use$Perc_PI)
y <- na.omit(wui_rad_census_r_df_pop_WUI$Perc_PI)
sp <- sqrt(((length(x) - 1) * var(x) + (length(y) - 1) * var(y)) / (length(x) + length(y) - 2))
cohens_d <- (mean(x) - mean(y)) / sp
cohens_d # small effect size for PI, as expected given group size

wui_census_sum_nw_r <- all_nonwui_r_use %>%
  summarize(hispanic = mean(Perc_Hispanic, na.rm = TRUE),
            white = mean(Perc_White, na.rm = TRUE),
            black = mean(Perc_Black, na.rm = TRUE),
            am_indian = mean(Perc_Am_Indian, na.rm = TRUE),
            asian = mean(Perc_Asian, na.rm = TRUE),
            PI = mean(Perc_PI, na.rm = TRUE),
            other = mean(Perc_other, na.rm = TRUE))
wui_census_sum_nw_r

# ==============================================================================
# Area (Table 1) - km2 of WUI vs. non-WUI, sanity-checked against statewide area
# ==============================================================================
area_nonwui <- non_wui_only_df %>%
  group_by(WUICLASS_2) %>%
  summarise(area_sum = sum(area_calc_km2), count = length(BLK20))
area_nonwui$area_sum_table <- area_nonwui$area_sum / 1000
sum(area_nonwui$area_sum_table)

sum(wui_only$area_calc_km2)      # [1] 27796.44
sum(non_wui_only$area_calc_km2)  # [1] 396168.9

## Compare to the pre-existing Shape_Area field in the shapefile as a sanity check
sum(wui_only$Shape_Area)     # 27796444014
sum(non_wui_only$Shape_Area) # 396168916382

396168.9 + 27796.44 # CHECKING STATEWIDE AREA
# [1] 423965.3 -- matches closely: 423,967 km2 on Google/Britannica

# From ownership analysis below: 1370.9 km2 of WUI is publicly owned
27796.44 - 1370.94115295825
# 26425.5
# for non-WUI:
396168.9 - 217950.09541398
# 178218.8

# WUI percent of CA area
27796.44 / 423965.3
# [1] 0.06556301

# ==============================================================================
# Population density (Table 1, Figure S2)
# ==============================================================================
mean(wui_only$POPDEN2020)
# 2510.03
mean(non_wui_only$POPDEN2020)
# 2420.576
# Mean population density for populated non-WUI blocks specifically:
# mean(subset(non_wui_only, non_wui_only$POP2020 > 0)$POPDEN2020)

pop_den_nonwui <- non_wui_only_df %>%
  group_by(wui_class_update) %>%
  summarise(pop_den_avg = mean(POPDEN2020))

pop_den_nonwui_all <- non_wui_only_df %>%
  summarise(pop_den_avg = mean(POPDEN2020))

# ==============================================================================
# WUI population histogram (Figure S2)
# ==============================================================================
hist(wui_only$POP2020, col = 'blue', xlab = "Population in WUI Blocks (2020)",
     main = "Histogram of WUI Population", breaks = 100)
hist(wui_only$POP2020, col = 'blue', xlab = "Population in WUI Blocks (2020)",
     main = "Histogram of WUI Population", breaks = 100, xlim = c(0, 500))
# this shows ~98% of census blocks
hist(wui_only$POP2020, col = 'blue', xlab = "Population in WUI Blocks (2020)",
     main = "Histogram of WUI Population", breaks = 500, xlim = c(0, 500), ylim = c(0, 20000))
hist(wui_only$POP2020, col = 'blue', xlab = "Population in WUI Blocks (2020)",
     main = "Histogram of WUI Population", breaks = seq(0, 6000, by = 1), xlim = c(0, 50), ylim = c(0, 2500))

# ==============================================================================
# Table S2: public ownership by WUI class
# ==============================================================================
# California Land Ownership data - st_buffer(dist = 0) below repairs any
# self-intersecting geometries in the source shapefile before use.
ca_own <- st_read(file.path(data_dir, "Shapefiles/ownership23_1.shp"))
ca_own <- sf::st_buffer(ca_own, dist = 0)
ca_own <- st_transform(ca_own, ca_albers)

own_int <- st_intersection(wui_rad, ca_own) # intermittently fails - rerun if needed
own_wui_only <- subset(own_int, own_int$WUIFLAG202 == 1 | own_int$WUIFLAG202 == 2)
own_non_wui  <- subset(own_int, own_int$WUIFLAG202 == 0)

own_wui_only$area_calc <- st_area(own_wui_only)
own_non_wui$area_calc  <- st_area(own_non_wui)

table(own_non_wui$wui_class_update)

own_wui_df <- own_wui_only
own_wui_df$geometry <- NULL

# Top ownership categories within the WUI
own_wui_areas <- own_wui_df %>%
  group_by(OWN_GROUP) %>%
  summarize(area = sum(area_calc))
own_wui_areas$perc <- as.numeric(own_wui_areas$area / sum(own_wui_areas$area)) * 100

# Ownership percent for non-WUI, by WUI class
own_non_wui_df <- own_non_wui
own_non_wui_df$geometry <- NULL

own_non_wui_area <- own_non_wui_df %>%
  group_by(wui_class_update) %>%
  summarize(area = sum(area_calc))

own_non_wui_area$total <- area_nonwui$area_sum
own_non_wui_area$perc <- own_non_wui_area$area / own_non_wui_area$total
own_non_wui_area
#write.csv(own_non_wui_area, "pub_ownership_non_wui_types.csv")

# To break ownership down further by populated vs. unpopulated non-WUI
# blocks, filter first with something like:
# nonwui_pop_only <- subset(non_wui_only, non_wui_only$POP2020 > 0)
