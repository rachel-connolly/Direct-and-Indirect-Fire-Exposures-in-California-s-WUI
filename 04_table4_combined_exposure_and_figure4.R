# ==============================================================================
# 04: Table 4 (combined direct + indirect exposure),
# Figure 4 (fire count / NAAQS exceedance maps), Table S9, Figure S8
# ==============================================================================
# Requires, in this order, in the same R session:
#   1. 00_setup_and_data_prep.R      (wui_rad, wui_only, wui_rad_census_df, ca, cz, cz_prj, add_strip(), data_dir)
#   2. 02_burned_area_table1_table2_figure2.R  (fire_p_wui - the fire x WUI intersection)
#   3. 03_table3_smoke_pm_exposure.R (wui_only$cmaq_all)
# ==============================================================================

library(dplyr)
library(sf)
library(terra)
library(exactextractr)
library(ggplot2)
library(patchwork)
library(ggspatial)

# ==============================================================================
# Fire count per WUI census block (Table 4)
# ==============================================================================
fires_wui <- subset(fire_p_wui, fire_p_wui$WUIFLAG202 == 1 | fire_p_wui$WUIFLAG202 == 2)
count_fires <- fires_wui %>%
  group_by(BLK20) %>%
  summarize(count_fire = length(BLK20))

count_fires_sum <- count_fires %>%
  group_by(count_fire) %>%
  summarize(count_fire_sum = length(count_fire))
count_fires_sum

# total WUI blocks: 153,383; WUI blocks with >=1 fire: 4,228 (see count_fires)
153383 - 4228
# 149155

wui_cens_count <- left_join(wui_rad_census_df, count_fires, by = "BLK20")
wui_cens_count <- subset(wui_cens_count, wui_cens_count$wui_class_update == "WUI")
table(wui_cens_count$count_fire)
# NOTE: these counts don't match the headline numbers exactly - 3 blocks
# have a different bufveg characteristic and are counted twice throughout
# the analysis, so there are ~100 census blocks with repeats out of 153,393
# total in the WUI. The manuscript uses 149,152 / 3,842 rather than
# 149,155 / 3,839 to keep tracts independent, consistent with the rest of
# the analysis.

wui_cens_count$count_fire[is.na(wui_cens_count$count_fire)] <- 0
wui_cens_count$count_fire_grp <- wui_cens_count$count_fire
wui_cens_count$count_fire_grp[wui_cens_count$count_fire > 3.5] <- "4-6"
table(wui_cens_count$count_fire)

wui_cens_fires_grp <- wui_cens_count %>%
  group_by(count_fire_grp) %>%
  summarize(med_income = mean(Median_hh_income, na.rm = TRUE),
            race_eth = mean(race_eth, na.rm = TRUE),
            count_blk = length(BLK20))
wui_cens_fires_grp
# Attributing structure/facility damage to a specific block within a fire
# perimeter isn't possible without risking double-counting when a fire
# crosses multiple blocks - that would need its own block-level damage
# dataset joined in here.

# ==============================================================================
# Days per year with fire-only PM2.5 > 24-hr NAAQS (35 ug/m3), summed 2008-2018
# ==============================================================================
pm_fire_2008 <- rast(file.path(data_dir, "CMAQ Daily Rasters/2008_fireonly.tif"))
pm_fire_2009 <- rast(file.path(data_dir, "CMAQ Daily Rasters/2009_fireonly.tif"))
pm_fire_2010 <- rast(file.path(data_dir, "CMAQ Daily Rasters/2010_fireonly.tif"))
pm_fire_2011 <- rast(file.path(data_dir, "CMAQ Daily Rasters/2011_fireonly.tif"))
pm_fire_2012 <- rast(file.path(data_dir, "CMAQ Daily Rasters/2012_fireonly.tif"))
pm_fire_2013 <- rast(file.path(data_dir, "CMAQ Daily Rasters/2013_fireonly.tif"))
pm_fire_2014 <- rast(file.path(data_dir, "CMAQ Daily Rasters/2014_fireonly.tif"))
pm_fire_2015 <- rast(file.path(data_dir, "CMAQ Daily Rasters/2015_fireonly.tif"))
pm_fire_2016 <- rast(file.path(data_dir, "CMAQ Daily Rasters/2016_fireonly.tif"))
pm_fire_2017 <- rast(file.path(data_dir, "CMAQ Daily Rasters/2017_fireonly.tif"))
pm_fire_2018 <- rast(file.path(data_dir, "CMAQ Daily Rasters/2018_fireonly.tif"))

pm_2008_exceed <- pm_fire_2008
pm_2009_exceed <- pm_fire_2009
pm_2010_exceed <- pm_fire_2010
pm_2011_exceed <- pm_fire_2011
pm_2012_exceed <- pm_fire_2012
pm_2013_exceed <- pm_fire_2013
pm_2014_exceed <- pm_fire_2014
pm_2015_exceed <- pm_fire_2015
pm_2016_exceed <- pm_fire_2016
pm_2017_exceed <- pm_fire_2017
pm_2018_exceed <- pm_fire_2018

# Binary daily exceedance flag (1 if day's PM2.5 > 35 ug/m3, else 0)
pm_2008_exceed[pm_2008_exceed < 35] <- 0; pm_2008_exceed[pm_2008_exceed > 34.999] <- 1
pm_2009_exceed[pm_2009_exceed < 35] <- 0; pm_2009_exceed[pm_2009_exceed > 34.999] <- 1
pm_2010_exceed[pm_2010_exceed < 35] <- 0; pm_2010_exceed[pm_2010_exceed > 34.999] <- 1
pm_2011_exceed[pm_2011_exceed < 35] <- 0; pm_2011_exceed[pm_2011_exceed > 34.999] <- 1
pm_2012_exceed[pm_2012_exceed < 35] <- 0; pm_2012_exceed[pm_2012_exceed > 34.999] <- 1
pm_2013_exceed[pm_2013_exceed < 35] <- 0; pm_2013_exceed[pm_2013_exceed > 34.999] <- 1
pm_2014_exceed[pm_2014_exceed < 35] <- 0; pm_2014_exceed[pm_2014_exceed > 34.999] <- 1
pm_2015_exceed[pm_2015_exceed < 35] <- 0; pm_2015_exceed[pm_2015_exceed > 34.999] <- 1
pm_2016_exceed[pm_2016_exceed < 35] <- 0; pm_2016_exceed[pm_2016_exceed > 34.999] <- 1
pm_2017_exceed[pm_2017_exceed < 35] <- 0; pm_2017_exceed[pm_2017_exceed > 34.999] <- 1
pm_2018_exceed[pm_2018_exceed < 35] <- 0; pm_2018_exceed[pm_2018_exceed > 34.999] <- 1

pm_2008_exceed_sum <- app(pm_2008_exceed, sum) # count of exceedance days per year
pm_2009_exceed_sum <- app(pm_2009_exceed, sum)
pm_2010_exceed_sum <- app(pm_2010_exceed, sum)
pm_2011_exceed_sum <- app(pm_2011_exceed, sum)
pm_2012_exceed_sum <- app(pm_2012_exceed, sum)
pm_2013_exceed_sum <- app(pm_2013_exceed, sum)
pm_2014_exceed_sum <- app(pm_2014_exceed, sum)
pm_2015_exceed_sum <- app(pm_2015_exceed, sum)
pm_2016_exceed_sum <- app(pm_2016_exceed, sum)
pm_2017_exceed_sum <- app(pm_2017_exceed, sum)
pm_2018_exceed_sum <- app(pm_2018_exceed, sum)

# 2008-2012 rasters have a slightly different extent than 2013-2018; crop to match
ext(pm_fire_2008)
ext(pm_fire_2018)
pm_2008_exceed_sum_ <- crop(pm_2008_exceed_sum, pm_2018_exceed_sum)
pm_2009_exceed_sum_ <- crop(pm_2009_exceed_sum, pm_2018_exceed_sum)
pm_2010_exceed_sum_ <- crop(pm_2010_exceed_sum, pm_2018_exceed_sum)
pm_2011_exceed_sum_ <- crop(pm_2011_exceed_sum, pm_2018_exceed_sum)
pm_2012_exceed_sum_ <- crop(pm_2012_exceed_sum, pm_2018_exceed_sum)

exceedances <- mosaic(pm_2008_exceed_sum_, pm_2009_exceed_sum_, pm_2010_exceed_sum_,
                       pm_2011_exceed_sum_, pm_2012_exceed_sum_, pm_2013_exceed_sum,
                       pm_2014_exceed_sum, pm_2015_exceed_sum, pm_2016_exceed_sum,
                       pm_2017_exceed_sum, pm_2018_exceed_sum, fun = "sum")

# Crop to California using a local reprojection of `ca` (kept separate from
# the shared ca_albers_sf built in 00_setup, since this needs the raster's
# own CRS rather than ca_albers)
ca_lcc <- st_transform(ca, st_crs(exceedances))
ca_exc <- crop(exceedances, ca_lcc)
ca_vec <- vect(as(ca_lcc, "Spatial"))
plot(ca_exc)
lines(ca_vec)

wui_only$cmaq_exc <- exact_extract(ca_exc, wui_only, 'mean')

count.fires <- as.data.frame(count_fires)
wui_only_fire_exc <- left_join(wui_only, count.fires, by = "BLK20")
wui_only_fire_exc$count_fire[is.na(wui_only_fire_exc$count_fire)] <- 0

wui_count <- wui_only_fire_exc %>% dplyr::select(BLK20, cmaq_all, cmaq_exc, count_fire)

# ------------------------------------------------------------------------
# Cache to shapefile - the pipeline above (esp. the raster mosaic) is slow,
# so the result is cached to disk and re-read from there. Uncomment
# st_write() to regenerate the cache after an input changes.
# ------------------------------------------------------------------------
#st_write(wui_count, file.path(data_dir, "Shapefiles/fire_count_exc_wui.shp"))
wui_count <- st_read(file.path(data_dir, "Shapefiles/fire_count_exc_wui.shp"))

# ==============================================================================
# Figure 4: count of WUI fires (left) and days exceeding NAAQS (right)
# ==============================================================================
br_ct <- c(-1, .9, 1.1, 2.1, 6.1)
wui_count$values_plot <- cut(wui_count$count_fire, breaks = br_ct, dig.lab = 2)
labs_plot_ct <- c("0", "1", "2", "3-6")
pal_ct <- c("#FDB07A", "#C34C8A", "#7A2A7D", "#1B0C41") # darkest -> lighter

count_plot <- ggplot() +
  geom_sf(data = wui_count, aes(fill = values_plot), color = NA) +
  geom_sf(data = cz_prj, fill = NA, aes(shape = cz)) +
  labs(title = expression(Count~of~WUI~Fires~(2008-2018)),
       fill = expression(Total~Count)) +
  scale_fill_manual(values = pal_ct, drop = FALSE, na.value = "grey80", label = labs_plot_ct,
                     guide = guide_legend(direction = "vertical", nrow = 8, label.position = "right")) +
  scale_shape(name = NULL, labels = c("CA Climate Zones")) +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white"), legend.position = "right")

wui_count_exc_plt <- subset(wui_count, !is.na(wui_count$cmaq_exc))

br <- c(-1, .1, 5, 20.1, 50.1, 100.1, 150.1, 212)
wui_count_exc_plt$values_plot <- cut(wui_count_exc_plt$cmaq_exc, breaks = br, dig.lab = 2)
labs_plot <- c("0", "[1-5]", "(5-20]", "(20-50]", "(50-100]", "(100-150]", ">150")
pal <- hcl.colors(7, "Viridis", rev = TRUE, alpha = 0.7)

exc_plot <- ggplot() +
  geom_sf(data = wui_count_exc_plt, aes(fill = values_plot), color = NA) +
  geom_sf(data = cz_prj, fill = NA, aes(shape = cz)) +
  labs(title = expression(Days~with~Fire~PM[2.5]~Higher~than~NAAQS~(2008-2018)),
       fill = expression(Total~Number~of~Days)) +
  scale_fill_manual(values = pal, drop = FALSE, na.value = "grey80", label = labs_plot,
                     guide = guide_legend(direction = "vertical", nrow = 8, label.position = "right")) +
  scale_shape(name = NULL, labels = c("CA Climate Zones")) +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white"), legend.position = "right")

count_plot_strip <- add_strip(count_plot + guides(shape = "none"), "Count of WUI Fires")
exc_plot_strip <- add_strip(exc_plot, "Number of Days Exceeding NAAQS")

count_plot_aligned <- count_plot_strip +
  guides(shape = "none") +
  theme(
    legend.position = "right", legend.direction = "vertical",
    legend.box = "vertical", legend.box.just = "top", legend.justification = c(0, 1),
    legend.title = element_text(size = 10), legend.text = element_text(size = 9),
    legend.key.height = unit(16, "pt"), legend.key.width = unit(20, "pt"),
    legend.spacing.y = unit(4, "pt"), legend.spacing.x = unit(6, "pt"),
    legend.box.margin = margin(t = 16, r = 0, b = 0, l = 10),
    legend.box.spacing = unit(2, "pt"), legend.margin = margin(0, 0, 0, 0),
    plot.margin = margin(5.5, 12, 5.5, 5.5)
  )

exc_plot_aligned <- exc_plot_strip +
  theme(
    legend.position = "right", legend.direction = "vertical",
    legend.box = "vertical", legend.box.just = "top", legend.justification = c(0, 1),
    legend.title = element_text(size = 10), legend.text = element_text(size = 9),
    legend.key.height = unit(16, "pt"), legend.key.width = unit(20, "pt"),
    legend.spacing.y = unit(4, "pt"), legend.spacing.x = unit(6, "pt"),
    legend.box.margin = margin(t = 16, r = 0, b = 0, l = 10),
    legend.box.spacing = unit(2, "pt"), legend.margin = margin(0, 0, 0, 0),
    plot.margin = margin(5.5, 12, 5.5, 5.5)
  )

panel_fig4 <- count_plot_aligned + exc_plot_aligned + plot_layout(ncol = 3)
panel_fig4

ggsave("wui_fig4.png", panel_fig4, width = 11, height = 10)
ggsave("wui_fig4.jpeg", panel_fig4, width = 11, height = 10)
ggsave("wui_fig4_.png", panel_fig4, width = 14, height = 12.5)
ggsave("wui_fig4_.jpeg", panel_fig4, width = 14, height = 12.5)

# ==============================================================================
# Table S9: exceedance/count summary by census block, with dominant climate zone
# ==============================================================================
duplicated_blk20 <- duplicated(wui_count$BLK20) | duplicated(wui_count$BLK20, fromLast = TRUE)
duplicate_rows <- wui_count[duplicated_blk20, ]
# a handful of blocks are duplicated (differ only in geometry) - see note in Table 4 above

wui_count$id <- seq_len(nrow(wui_count))
wui_count_cz <- st_intersection(wui_count, cz_prj)
wui_count_cz$area <- st_area(wui_count_cz)
wui_count_cz_df <- as.data.frame(wui_count_cz)

block_counts <- wui_count_cz_df %>%
  group_by(BLK20, id) %>%
  summarise(entry_count = n_distinct(RegionName), .groups = "drop")

multi_entry_blocks <- block_counts %>%
  filter(entry_count > 1) %>%
  select(BLK20, id)

wui_count_cz_filtered <- wui_count_cz_df %>%
  semi_join(multi_entry_blocks, by = c("BLK20", "id"))

# For blocks straddling more than one climate zone, assign the zone with the
# larger share of the block's area
dominant_zone <- wui_count_cz_filtered %>%
  group_by(BLK20, id) %>%
  mutate(zone_area_prop = area / sum(area)) %>%
  summarise(climate_zone = RegionName[which.max(zone_area_prop)], .groups = "drop")

wui_count_cz_df_adj <- merge(wui_count_cz_df, dominant_zone, by = c("BLK20", "id"), all.x = TRUE)

wui_count_cz_df_v2 <- wui_count_cz_df_adj %>%
  group_by(BLK20, id) %>%
  mutate(
    dominant_zone = ifelse(
      any(!is.na(climate_zone)),
      first(climate_zone[!is.na(climate_zone)]),
      first(RegionName)
    )
  )

wui_count_use <- as.data.frame(wui_count)
wui_count_cz_table <- left_join(wui_count_use, wui_count_cz_df_v2[, c("BLK20", "dominant_zone", "id")],
                                 by = c("BLK20", "id"))

wui_count_cz_table$geometry <- NULL
collapsed_table <- wui_count_cz_table %>%
  group_by(id) %>%
  summarise(dominant_zone = first(dominant_zone))

wui_count_cz_table_use <- left_join(wui_count, collapsed_table, by = "id")
wui_count_cz_table_use <- data.frame(wui_count_cz_table_use)

wui_count_cz_table_use$count_fire_cat <- wui_count_cz_table_use$count_fire
wui_count_cz_table_use$count_fire_cat[wui_count_cz_table_use$count_fire %in% c(4, 5, 6)] <- "4-6"

count_sum_cz <- wui_count_cz_table_use %>%
  group_by(count_fire_cat, dominant_zone) %>%
  summarize(mean_exc = mean(cmaq_exc, na.rm = TRUE), mean_conc = mean(cmaq_all),
            count_blk = length(count_fire_cat))
count_sum_cz

wui_count_df <- as.data.frame(wui_count)
wui_count_df$count_fire_cat <- wui_count_df$count_fire
wui_count_df$count_fire_cat[wui_count_df$count_fire %in% c(4, 5, 6)] <- "4-6"

count_sum <- wui_count_df %>%
  group_by(count_fire_cat) %>%
  summarize(mean_exc = mean(cmaq_exc, na.rm = TRUE), mean_conc = mean(cmaq_all))
count_sum

# ==============================================================================
# Figure S8: count of WUI fires, faceted by climate zone
# ==============================================================================
cz_wui <- st_intersection(wui_count, cz_prj)

br_ct2 <- c(-1, .9, 1.1, 2.1, 6.1)
cz_wui$values_plot <- cut(cz_wui$count_fire, breaks = br_ct2, dig.lab = 2)
labs_plot_ct2 <- c("0", "1", "2", "3-6")
pal_ct2 <- c("#FDB07A", "#C34C8A", "#7A2A7D", "#1B0C41")
# (ggspatial is already loaded at the top of this file, used above for Figure 4's scale bars)

pad_bbox <- function(bb, xfrac = 0.03, yfrac = 0.08) {
  dx <- (bb["xmax"] - bb["xmin"]) * xfrac
  dy <- (bb["ymax"] - bb["ymin"]) * yfrac
  c(xmin = bb["xmin"] - dx, xmax = bb["xmax"] + dx, ymin = bb["ymin"] - dy, ymax = bb["ymax"] + dy)
}

region_levels <- sort(unique(cz_wui$RegionName))

make_panel <- function(region, show_legend = FALSE) {
  df    <- cz_wui[cz_wui$RegionName == region, ]
  cz_df <- cz[cz$RegionName == region, ]
  bb0   <- sf::st_bbox(df)
  bb    <- pad_bbox(bb0, xfrac = 0.03, yfrac = 0.08)

  scale_loc <- if (region == "North Coast" | region == "Sierra Nevada Mountains") "br" else "bl"
  pad_y_val <- if (region == "North Coast" | region == "Sierra Nevada Mountains") grid::unit(35, "mm") else grid::unit(4, "mm")

  p <- ggplot() +
    geom_sf(data = df, aes(fill = values_plot), color = NA) +
    geom_sf(data = cz_df, fill = NA, color = "black", linewidth = 0.2, show.legend = FALSE) +
    scale_fill_manual(values = pal_ct2, drop = FALSE, na.value = "grey80", labels = labs_plot_ct2,
                       guide = if (show_legend) guide_legend(nrow = 1) else "none") +
    labs(title = NULL, fill = expression(Total~Count)) +
    coord_sf(xlim = c(bb["xmin"], bb["xmax"]), ylim = c(bb["ymin"], bb["ymax"]), expand = FALSE, datum = NA) +
    ggspatial::annotation_scale(location = scale_loc, width_hint = 0.08, height_hint = 0.006,
                                 pad_x = grid::unit(6, "mm"), pad_y = pad_y_val) +
    theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(),
          panel.background = element_rect(fill = "white"),
          legend.position = if (show_legend) "bottom" else "none")

  add_strip(p, region)
}

plots <- Map(function(reg, i) make_panel(reg, show_legend = i == 1L), region_levels, seq_along(region_levels))

combined <- patchwork::wrap_plots(plots, ncol = 3, guides = "collect") &
  theme(legend.position = "bottom")
combined

ggsave("wui_counts_3x3_padded_v9.pdf", combined, width = 11, height = 10)
ggsave("wui_counts_3x3_padded_v9.png", combined, width = 11, height = 10)
