# ==============================================================================
# 05: Figure 3 (annual smoke PM2.5 by climate zone) +
# climate-zone supplementary PM tables
# ==============================================================================
# Requires: 00_setup_and_data_prep.R run first (uses wui_only, non_wui_only,
# cz_prj, pm_fire_all, data_dir)
# ==============================================================================

library(dplyr)
library(sf)
library(terra)
library(exactextractr)
library(reshape2)
library(ggplot2)
library(RColorBrewer)

# METHOD NOTE: zonal statistics in ArcGIS Pro can undercount here, because
# the software only counts raster cells whose center overlaps a polygon,
# and the WUI polygons within a larger climate zone polygon are small
# enough that this matters. Results don't end up very different, but this
# is why the extraction is done in R.

# ==============================================================================
# Intersect WUI / non-WUI blocks with climate zones, extract annual mean
# smoke PM2.5 for each year 2008-2018
# ==============================================================================
cz_wui    <- st_intersection(wui_only, cz_prj)
cz_nonwui <- st_intersection(non_wui_only, cz_prj)

pm_fire_2008 <- rast(file.path(data_dir, "CMAQ Daily Rasters/2008_fireonly.tif")); pm_fire_2008_mean <- app(pm_fire_2008, mean)
pm_fire_2009 <- rast(file.path(data_dir, "CMAQ Daily Rasters/2009_fireonly.tif")); pm_fire_2009_mean <- app(pm_fire_2009, mean)
pm_fire_2010 <- rast(file.path(data_dir, "CMAQ Daily Rasters/2010_fireonly.tif")); pm_fire_2010_mean <- app(pm_fire_2010, mean)
pm_fire_2011 <- rast(file.path(data_dir, "CMAQ Daily Rasters/2011_fireonly.tif")); pm_fire_2011_mean <- app(pm_fire_2011, mean)
pm_fire_2012 <- rast(file.path(data_dir, "CMAQ Daily Rasters/2012_fireonly.tif")); pm_fire_2012_mean <- app(pm_fire_2012, mean)
pm_fire_2013 <- rast(file.path(data_dir, "CMAQ Daily Rasters/2013_fireonly.tif")); pm_fire_2013_mean <- app(pm_fire_2013, mean)
pm_fire_2014 <- rast(file.path(data_dir, "CMAQ Daily Rasters/2014_fireonly.tif")); pm_fire_2014_mean <- app(pm_fire_2014, mean)
pm_fire_2015 <- rast(file.path(data_dir, "CMAQ Daily Rasters/2015_fireonly.tif")); pm_fire_2015_mean <- app(pm_fire_2015, mean)
pm_fire_2016 <- rast(file.path(data_dir, "CMAQ Daily Rasters/2016_fireonly.tif")); pm_fire_2016_mean <- app(pm_fire_2016, mean)
pm_fire_2017 <- rast(file.path(data_dir, "CMAQ Daily Rasters/2017_fireonly.tif")); pm_fire_2017_mean <- app(pm_fire_2017, mean)
pm_fire_2018 <- rast(file.path(data_dir, "CMAQ Daily Rasters/2018_fireonly.tif")); pm_fire_2018_mean <- app(pm_fire_2018, mean)

cz_wui$cmaq_2008 <- exact_extract(pm_fire_2008_mean, cz_wui, 'mean')
cz_wui$cmaq_2009 <- exact_extract(pm_fire_2009_mean, cz_wui, 'mean')
cz_wui$cmaq_2010 <- exact_extract(pm_fire_2010_mean, cz_wui, 'mean')
cz_wui$cmaq_2011 <- exact_extract(pm_fire_2011_mean, cz_wui, 'mean')
cz_wui$cmaq_2012 <- exact_extract(pm_fire_2012_mean, cz_wui, 'mean')
cz_wui$cmaq_2013 <- exact_extract(pm_fire_2013_mean, cz_wui, 'mean')
cz_wui$cmaq_2014 <- exact_extract(pm_fire_2014_mean, cz_wui, 'mean')
cz_wui$cmaq_2015 <- exact_extract(pm_fire_2015_mean, cz_wui, 'mean')
cz_wui$cmaq_2016 <- exact_extract(pm_fire_2016_mean, cz_wui, 'mean')
cz_wui$cmaq_2017 <- exact_extract(pm_fire_2017_mean, cz_wui, 'mean')
cz_wui$cmaq_2018 <- exact_extract(pm_fire_2018_mean, cz_wui, 'mean')

cz_nonwui$cmaq_2008 <- exact_extract(pm_fire_2008_mean, cz_nonwui, 'mean')
cz_nonwui$cmaq_2009 <- exact_extract(pm_fire_2009_mean, cz_nonwui, 'mean')
cz_nonwui$cmaq_2010 <- exact_extract(pm_fire_2010_mean, cz_nonwui, 'mean')
cz_nonwui$cmaq_2011 <- exact_extract(pm_fire_2011_mean, cz_nonwui, 'mean')
cz_nonwui$cmaq_2012 <- exact_extract(pm_fire_2012_mean, cz_nonwui, 'mean')
cz_nonwui$cmaq_2013 <- exact_extract(pm_fire_2013_mean, cz_nonwui, 'mean')
cz_nonwui$cmaq_2014 <- exact_extract(pm_fire_2014_mean, cz_nonwui, 'mean')
cz_nonwui$cmaq_2015 <- exact_extract(pm_fire_2015_mean, cz_nonwui, 'mean')
cz_nonwui$cmaq_2016 <- exact_extract(pm_fire_2016_mean, cz_nonwui, 'mean')
cz_nonwui$cmaq_2017 <- exact_extract(pm_fire_2017_mean, cz_nonwui, 'mean')
cz_nonwui$cmaq_2018 <- exact_extract(pm_fire_2018_mean, cz_nonwui, 'mean')

# ==============================================================================
# Significance test: mean smoke PM2.5 (all years) across climate zones, WUI only
# ==============================================================================
cz_wui$cmaq_all <- exact_extract(pm_fire_all, cz_wui, 'mean')

cz_wui_ <- cz_wui %>%
  st_drop_geometry() %>%
  select(RegionName, cmaq_all, cmaq_2008:cmaq_2018)

cz_wui_$RegionName <- as.factor(cz_wui_$RegionName)

fit_cz <- aov(cmaq_all ~ RegionName, data = cz_wui_)
summary(fit_cz)
TukeyHSD(fit_cz)


cz_wui_allyrs_check <- cz_wui_ %>%
  group_by(RegionName) %>%
  summarize(mean_pm = mean(cmaq_all))
cz_wui_allyrs_check
# (a quick sanity check - the by-year summary used for the actual figure
# and supplementary tables is built separately below)

# ==============================================================================
# By-year summary (used for Figure 3's line plot and the supplementary tables)
# ==============================================================================
cz_wui_melt <- melt(cz_wui_, id.vars = c("RegionName"))
cz_wui_melt$Year <- as.numeric(substr(cz_wui_melt$variable, 6, 9))


cz_wui_summary <- cz_wui_melt %>%
  group_by(RegionName, Year) %>%
  summarize(mean_pm = mean(value), .groups = "drop")

cz_wui_summary_allyrs <- cz_wui_melt %>%
  group_by(RegionName) %>%
  summarize(mean_pm = mean(value))

cz_wui_summary$Year <- as.character(cz_wui_summary$Year)
cz_wui_summary_allyrs$Year <- "All Years"
cz_wui_summary_final <- rbind(cz_wui_summary, cz_wui_summary_allyrs)

cz_nonwui_ <- cz_nonwui %>%
  st_drop_geometry() %>%
  select(RegionName, WUICLASS_2, cmaq_2008:cmaq_2018) %>%
  as.data.frame()

cz_nonwui_$wui_class_update <- cz_nonwui_$WUICLASS_2
cz_nonwui_$wui_class_update[cz_nonwui_$WUICLASS_2 %in% c('Very_Low_Dens_NoVeg', 'Low_Dens_NoVeg',
                                                           'Med_Dens_NoVeg', 'High_Dens_NoVeg')] <- "No_Veg"
cz_nonwui_$wui_class_update[cz_nonwui_$WUICLASS_2 %in% c('Very_Low_Dens_Veg', 'Uninhabited_Veg')] <- "Veg"
table(cz_nonwui_$wui_class_update)
cz_nonwui_$WUICLASS_2 <- NULL

cz_nonwui_melt <- melt(cz_nonwui_, id.vars = c("RegionName", "wui_class_update"))
cz_nonwui_melt$Year <- as.numeric(substr(cz_nonwui_melt$variable, 6, 9))
cz_nonwui_melt <- subset(cz_nonwui_melt, cz_nonwui_melt$wui_class_update %in% c("Veg", "No_Veg"))
table(cz_nonwui_melt$wui_class_update)

cz_nonwui_summary <- cz_nonwui_melt %>%
  group_by(RegionName, wui_class_update, Year) %>%
  summarize(mean_pm = mean(value), .groups = "drop")

cz_nonwui_summary_allyrs <- cz_nonwui_melt %>%
  group_by(RegionName, wui_class_update) %>%
  summarize(mean_pm = mean(value), .groups = "drop")
cz_nonwui_summary_allyrs$Year <- "All Years"
cz_nonwui_summary$Year <- as.character(cz_nonwui_summary$Year)

cz_nonwui_summary_final <- rbind(cz_nonwui_summary, cz_nonwui_summary_allyrs)

## cz_wui_summary_final and cz_nonwui_summary_final are the supplementary table outputs
#write.csv(cz_wui_summary_final, "wui_cz_fire_pm_11.29.23.csv")
#write.csv(cz_nonwui_summary_final, "nonwui_cz_fire_pm_11.29.23.csv")

# ==============================================================================
# Figure 3: annual smoke PM2.5 in the WUI, by climate zone
# ==============================================================================
cz_wui_plot_data <- cz_wui_summary
cz_wui_plot_data$WUI <- cz_wui_plot_data$mean_pm
cz_wui_plot_data$mean_pm <- NULL
cz_wui_plot_data$Year_ <- as.character(cz_wui_plot_data$Year)
cz_wui_plot_data <- cz_wui_plot_data %>% rename("Region Name" = "RegionName")

cz_wui_plot <- cz_wui_plot_data %>%
  ggplot(aes(x = Year_, y = WUI, group = `Region Name`, color = `Region Name`)) +
  geom_line() +
  ggtitle("Fire PM2.5 in California's WUI") +
  ylab("Fire-only PM2.5 (ug/m3)") +
  xlab("Year") +
  ylim(0, 10) +
  scale_color_brewer(name = "", palette = "Set1")
cz_wui_plot

write.csv(cz_wui_plot_data, "fig_5_cz_wui_pm.csv")

# The non-WUI climate-zone summary is `cz_nonwui_summary_final` above.
