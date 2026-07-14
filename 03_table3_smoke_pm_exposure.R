# ==============================================================================
# 03: Table 3 (annual average smoke PM2.5 by WUI class)
# ==============================================================================
# Requires: 00_setup_and_data_prep.R run first (uses wui_rad, wui_only,
# non_wui_only, non_wui_only_df, pm_fire_all, wui_rad$cmaq_all, data_dir)
# ==============================================================================

library(dplyr)
library(sf)
library(exactextractr)
library(terra)

# ==============================================================================
# Mean smoke PM2.5 (2008-2018), WUI vs. non-WUI, and by non-WUI category
# ==============================================================================
pm_fire_wui <- exact_extract(pm_fire_all, wui_only, 'mean')
wui_only$cmaq_all <- pm_fire_wui
mean(wui_only$cmaq_all) ## ALL WUI

pm_fire_nonwui <- exact_extract(pm_fire_all, non_wui_only, 'mean')
non_wui_only$cmaq_all <- pm_fire_nonwui
mean(non_wui_only$cmaq_all) ## ALL NON-WUI

# non_wui_only_df (from 00_setup) is derived before cmaq_all exists on
# non_wui_only - attach it here so the grouping below has the column it needs
non_wui_only_df$cmaq_all <- non_wui_only$cmaq_all

avg_fire_pm <- non_wui_only_df %>%
  group_by(wui_class_update) %>%
  summarise(mean_conc = mean(cmaq_all))
avg_fire_pm

avg_fire_pm_all <- non_wui_only_df %>%
  summarise(mean_conc = mean(cmaq_all))
avg_fire_pm_all

# ==============================================================================
# Significance test: smoke PM2.5 across all wui_class_update categories
# ==============================================================================
stat_test_df <- wui_rad %>%
  st_drop_geometry() %>%
  filter(!is.na(cmaq_all), !is.na(wui_class_update)) %>%
  mutate(wui_class_update = as.factor(wui_class_update))

group_stats <- stat_test_df %>%
  group_by(wui_class_update) %>%
  summarise(n = n(), mean = mean(cmaq_all), sd = sd(cmaq_all), .groups = "drop")
group_stats

fit_aov <- aov(cmaq_all ~ wui_class_update, data = stat_test_df)
summary(fit_aov)

TukeyHSD(fit_aov)

# Welch t-test, WUI vs. Low/Very Low Housing Density specifically 
# stat_test_df still has the
# underscore-style labels here (the readable relabeling below doesn't
# happen until the Table 3 section further down), so filter on
# "Low_Very_Low_Housing_Density_no_veg", not "Low/Very Low Housing Density".
df2 <- stat_test_df %>%
  filter(wui_class_update %in% c("WUI", "Low_Very_Low_Housing_Density_no_veg")) %>%
  mutate(wui_class_update = droplevels(factor(wui_class_update)))

df2 %>%
  group_by(wui_class_update) %>%
  summarise(n = n(), mean = mean(cmaq_all), sd = sd(cmaq_all), .groups = "drop")

t.test(cmaq_all ~ wui_class_update, data = df2)
# consistent with the ANOVA/Tukey result above

# ==============================================================================
# Table 3: annual average smoke PM2.5 by WUI class, 2008-2018
# ==============================================================================
pm_fire_2008 <- rast(file.path(data_dir, "CMAQ Daily Rasters/2008_fireonly.tif"))
pm_fire_2008_mean <- app(pm_fire_2008, mean)
wui_rad$cmaq_2008 <- exact_extract(pm_fire_2008_mean, wui_rad, 'mean')

pm_fire_2009 <- rast(file.path(data_dir, "CMAQ Daily Rasters/2009_fireonly.tif"))
pm_fire_2009_mean <- app(pm_fire_2009, mean)
wui_rad$cmaq_2009 <- exact_extract(pm_fire_2009_mean, wui_rad, 'mean')

pm_fire_2010 <- rast(file.path(data_dir, "CMAQ Daily Rasters/2010_fireonly.tif"))
pm_fire_2010_mean <- app(pm_fire_2010, mean)
wui_rad$cmaq_2010 <- exact_extract(pm_fire_2010_mean, wui_rad, 'mean')

pm_fire_2011 <- rast(file.path(data_dir, "CMAQ Daily Rasters/2011_fireonly.tif"))
pm_fire_2011_mean <- app(pm_fire_2011, mean)
wui_rad$cmaq_2011 <- exact_extract(pm_fire_2011_mean, wui_rad, 'mean')

pm_fire_2012 <- rast(file.path(data_dir, "CMAQ Daily Rasters/2012_fireonly.tif"))
pm_fire_2012_mean <- app(pm_fire_2012, mean)
wui_rad$cmaq_2012 <- exact_extract(pm_fire_2012_mean, wui_rad, 'mean')

pm_fire_2013 <- rast(file.path(data_dir, "CMAQ Daily Rasters/2013_fireonly.tif"))
pm_fire_2013_mean <- app(pm_fire_2013, mean)
wui_rad$cmaq_2013 <- exact_extract(pm_fire_2013_mean, wui_rad, 'mean')

pm_fire_2014 <- rast(file.path(data_dir, "CMAQ Daily Rasters/2014_fireonly.tif"))
pm_fire_2014_mean <- app(pm_fire_2014, mean)
wui_rad$cmaq_2014 <- exact_extract(pm_fire_2014_mean, wui_rad, 'mean')

pm_fire_2015 <- rast(file.path(data_dir, "CMAQ Daily Rasters/2015_fireonly.tif"))
pm_fire_2015_mean <- app(pm_fire_2015, mean)
wui_rad$cmaq_2015 <- exact_extract(pm_fire_2015_mean, wui_rad, 'mean')

pm_fire_2016 <- rast(file.path(data_dir, "CMAQ Daily Rasters/2016_fireonly.tif"))
pm_fire_2016_mean <- app(pm_fire_2016, mean)
wui_rad$cmaq_2016 <- exact_extract(pm_fire_2016_mean, wui_rad, 'mean')

pm_fire_2017 <- rast(file.path(data_dir, "CMAQ Daily Rasters/2017_fireonly.tif"))
pm_fire_2017_mean <- app(pm_fire_2017, mean)
wui_rad$cmaq_2017 <- exact_extract(pm_fire_2017_mean, wui_rad, 'mean')

pm_fire_2018 <- rast(file.path(data_dir, "CMAQ Daily Rasters/2018_fireonly.tif"))
pm_fire_2018_mean <- app(pm_fire_2018, mean)
wui_rad$cmaq_2018 <- exact_extract(pm_fire_2018_mean, wui_rad, 'mean')

# Recode to the reader-friendly labels used for this table's row groups
# (distinct from the underscore-style wui_class_update used elsewhere)
wui_rad$wui_class_update[wui_rad$wui_class_update == 'Low_Very_Low_Housing_Density_no_veg'] <- "Low/Very Low Housing Density"
wui_rad$wui_class_update[wui_rad$wui_class_update == 'Medium_high_housing_density_no_veg'] <- "Medium/High Housing Density"
wui_rad$wui_class_update[wui_rad$wui_class_update == 'Uninhabited_Veg'] <- "No Housing"
wui_rad$wui_class_update[wui_rad$wui_class_update == 'Very_Low_Dens_Veg'] <- "Very Low Housing Density"
table(wui_rad$wui_class_update)
# NOTE: this permanently overwrites wui_rad$wui_class_update from the
# underscore-style labels to the reader-friendly ones for the rest of the
# session. If you re-run an earlier table/figure script (01 or 02) after
# this one in the same session, re-run 00_setup_and_data_prep.R first to
# reset the labels.

wui_rad_df <- wui_rad
wui_rad_df$geometry <- NULL

fire_pm <- wui_rad_df %>%
  group_by(wui_class_update) %>%
  summarize('2008' = mean(cmaq_2008), '2009' = mean(cmaq_2009), '2010' = mean(cmaq_2010),
            '2011' = mean(cmaq_2011), '2012' = mean(cmaq_2012), '2013' = mean(cmaq_2013),
            '2014' = mean(cmaq_2014), '2015' = mean(cmaq_2015), '2016' = mean(cmaq_2016),
            '2017' = mean(cmaq_2017), '2018' = mean(cmaq_2018))
fire_pm
#write.csv(fire_pm, "fire_pm_by_nonwui_type.csv")

# QC: all non-WUI combined, to check against previously reported numbers
nonwui_qc <- subset(wui_rad_df, wui_rad_df$wui_class_update != 'WUI')
fire_pm_nonwui <- nonwui_qc %>%
  summarize('2008' = mean(cmaq_2008), '2009' = mean(cmaq_2009), '2010' = mean(cmaq_2010),
            '2011' = mean(cmaq_2011), '2012' = mean(cmaq_2012), '2013' = mean(cmaq_2013),
            '2014' = mean(cmaq_2014), '2015' = mean(cmaq_2015), '2016' = mean(cmaq_2016),
            '2017' = mean(cmaq_2017), '2018' = mean(cmaq_2018))
fire_pm_nonwui

# ==============================================================================
# Smoke PM2.5 by WUI type (intermix vs. interface) - referenced in Discussion
# ==============================================================================
wui_types <- wui_rad_df[(wui_rad_df$WUIFLAG202 == 1 | wui_rad_df$WUIFLAG202 == 2), ]
# 1 = intermix, 2 = interface

fire_pm_wuitype <- wui_types %>%
  group_by(WUIFLAG202) %>%
  summarize('2008' = mean(cmaq_2008), '2009' = mean(cmaq_2009), '2010' = mean(cmaq_2010),
            '2011' = mean(cmaq_2011), '2012' = mean(cmaq_2012), '2013' = mean(cmaq_2013),
            '2014' = mean(cmaq_2014), '2015' = mean(cmaq_2015), '2016' = mean(cmaq_2016),
            '2017' = mean(cmaq_2017), '2018' = mean(cmaq_2018))
fire_pm_wuitype

wui_only_df <- wui_only
wui_only_df$geometry <- NULL
t_res <- t.test(cmaq_all ~ as.factor(WUIFLAG202), data = wui_only_df)
t_res
# intermix higher than interface
