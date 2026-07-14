# ==============================================================================
# 09: Figure S4 (smoke PM2.5 concentration map with
# EPA AQS monitoring station locations)
# ==============================================================================
# Requires: 00_setup_and_data_prep.R run first (uses wui_rad, ca_albers,
# ca_counties, wui_rad$cmaq_all, add_strip(), data_dir)
# ==============================================================================

library(dplyr)
library(sf)
library(ggplot2)
library(ggspatial)

# 147 EPA AQS sites, first sampled 1998-2018, last sampled 2008-2024 (code
# 88101, PM2.5 local conditions) - updated May 2025
aqs_nad83 <- read.csv(file.path(data_dir, "AQS data/aqs sites nad83 upload_v2.csv"))
aqs_wgs84 <- read.csv(file.path(data_dir, "AQS data/aqs sites wgs84 upload_v2.csv"))

aqs_wgs84_sf <- st_as_sf(aqs_wgs84, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)
aqs_nad83_sf <- st_as_sf(aqs_nad83, coords = c("Longitude", "Latitude"), crs = 4269, remove = FALSE)

aqs_nad83_sf_prj <- st_transform(aqs_nad83_sf, ca_albers)
aqs_wgs_sf_prj <- st_transform(aqs_wgs84_sf, ca_albers)
aqs_combined_sf <- rbind(aqs_nad83_sf_prj, aqs_wgs_sf_prj)

# Same binning as Figure 1's smoke concentration panel (06) - recomputed here
# so this script can run on its own without depending on 06 having run first
br_conc_new <- c(0.25, 1.2, 1.9, 2.7, 3.7, 10.5, 17)
wui_rad$values_conc <- cut(wui_rad$cmaq_all, breaks = br_conc_new, dig.lab = 2)
labs_plot_conc <- c("(0.25-1.2]", "(1.2-1.9]", "(1.9-2.7]", "(2.7-3.7]", "(3.7-10.5]", "(10.5-16.2)")
pal_conc <- viridisLite::plasma(6, alpha = 0.7, direction = -1)

figs4_plot <- ggplot() +
  geom_sf(data = wui_rad, aes(fill = values_conc), color = NA) +
  geom_sf(data = ca_counties, fill = NA, color = "lightgray", size = 0.4, aes(linetype = "CA Counties")) +
  geom_sf(data = aqs_combined_sf, aes(color = "EPA AQS Stations"), size = 1.5) +
  labs(
    title = expression(Wildland~Fire~Smoke~PM[2.5]),
    fill  = expression(paste("Smoke PM"[2.5], " (", mu, "g/", m^3, ")")),
    color = "Monitor Type",
    linetype = NULL
  ) +
  scale_fill_manual(values = pal_conc, drop = FALSE, na.value = "grey80", labels = labs_plot_conc,
                     guide = guide_legend(order = 1)) +
  scale_color_manual(values = c("EPA AQS Stations" = "#80DEEA"), guide = guide_legend(order = 2)) +
  scale_linetype_manual(values = c("CA Counties" = "solid"), guide = guide_legend(order = 3)) +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = c(0.985, 0.7), legend.justification = c(1, 0.5), legend.direction = "vertical",
    legend.box = "vertical", legend.box.just = "top",
    legend.title = element_text(size = 10), legend.text = element_text(size = 9),
    legend.key.height = unit(16, "pt"), legend.key.width = unit(20, "pt"),
    legend.spacing.y = unit(4, "pt"), legend.spacing.x = unit(6, "pt"),
    legend.box.spacing = unit(3, "pt")
  )
figs4_plot

figs4_plot_strip <- add_strip(figs4_plot, "Smoke Concentrations & Monitoring Stations")
ggsave("wui_figs4_v8.pdf", figs4_plot_strip, width = 7, height = 6.5)
ggsave("wui_figs4_v8.png", figs4_plot_strip, width = 7, height = 6.5)
