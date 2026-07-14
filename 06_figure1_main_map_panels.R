# ==============================================================================
# 06: Figure 1 (WUI categories / fire perimeters /
# smoke concentration, 3-panel map)
# ==============================================================================
# Requires: 00_setup_and_data_prep.R run first (uses wui_rad, wui_only,
# fire_p, cz_prj, wui_rad$cmaq_all, add_strip())
# ==============================================================================

library(dplyr)
library(sf)
library(ggplot2)
library(ggtext)
library(patchwork)
library(cowplot)
library(ggspatial)

# ==============================================================================
# Panel A - WUI categories (intermix / interface)
# ==============================================================================
labs_plot_wui <- c("Intermix", "Interface")
pal_wui <- c("Intermix" = "orange", "Interface" = "darkgreen")

wui_only <- wui_only |>
  dplyr::mutate(wui_cat = factor(WUIFLAG202, levels = c(1, 2), labels = labs_plot_wui))

wui_plot <- ggplot() +
  geom_sf(data = wui_only, aes(fill = wui_cat), color = NA) +
  geom_sf(data = cz_prj, fill = NA, aes(shape = cz)) +
  labs(title = expression(WUI~Categories), fill = "WUI Type") +
  scale_fill_manual(values = pal_wui, drop = FALSE, na.value = "grey80",
                     guide = guide_legend(direction = "vertical", label.position = "right")) +
  scale_shape(name = NULL, labels = c("CA Climate Zones")) +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white"), legend.position = "right")
wui_plot

# ==============================================================================
# Panel B - fire perimeters
# ==============================================================================
# NOTE: plotting the full fire_p layer with ggplot is slow. 
fire_plot <- ggplot() +
  geom_sf(data = fire_p, aes(color = "Fire Perimeters"), fill = NA, linewidth = 0.1) +
  geom_sf(data = cz_prj, fill = NA, aes(shape = cz)) +
  labs(title = expression(Fire~Perimeters), color = NULL) +
  scale_color_manual(values = c("Fire Perimeters" = "red"),
                      guide = guide_legend(override.aes = list(fill = NA), title = NULL,
                                            direction = "vertical", label.position = "right")) +
  scale_shape(name = NULL, labels = c("CA Climate Zones")) +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white"), legend.position = "right")
fire_plot

# ==============================================================================
# Panel C - smoke PM2.5 concentration (all years mean)
# ==============================================================================
br_conc_new <- c(0.25, 1.2, 1.9, 2.7, 3.7, 10.5, 17)
wui_rad$values_conc <- cut(wui_rad$cmaq_all, breaks = br_conc_new, dig.lab = 2)
labs_plot_conc <- c("(0.25-1.2]", "(1.2-1.9]", "(1.9-2.7]", "(2.7-3.7]", "(3.7-10.5]", "(10.5-16.2)")
pal_conc <- viridisLite::plasma(6, alpha = 0.7, direction = -1)

conc_plot <- ggplot() +
  geom_sf(data = wui_rad, aes(fill = values_conc), color = NA) +
  geom_sf(data = cz_prj, fill = NA, aes(shape = cz)) +
  ggtext::geom_richtext(
    data = cz_prj, aes(label = cz_abbrev, geometry = geometry), stat = "sf_coordinates",
    color = "black", size = 3, fontface = "bold", fill = "white", label.color = NA,
    label.padding = grid::unit(c(0.1, 0.2, 0.1, 0.2), "lines"), label.r = unit(0.15, "lines")
  ) +
  labs(title = expression(Wildland~Fire~Smoke~PM[2.5]),
       fill = expression(paste("Smoke PM"[2.5], " (", mu, "g/", m^3, ")"))) +
  scale_fill_manual(values = pal_conc, drop = FALSE, na.value = "grey80", label = labs_plot_conc,
                     guide = guide_legend(direction = "vertical", nrow = 8, label.position = "right")) +
  scale_shape(name = NULL, labels = c("CA Climate Zones")) +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white"), legend.position = "right")
conc_plot

# ==============================================================================
# Combine into Figure 1
# ==============================================================================
# Panel labels (a/b/c) are prefixed onto the gray strip title (bold, via
# ggtext markdown) to match the title-strip style used on Figures 4, S1,
# S4, S7, and S8.
wui_plot_strip  <- add_strip(wui_plot + guides(shape = "none"), "**a.** WUI Designation")
fire_plot_strip <- add_strip(fire_plot + guides(shape = "none"), "**b.** Fire Perimeters")
conc_plot_strip <- add_strip(conc_plot, "**c.** Smoke Concentrations")

wui_plot_aligned <- wui_plot_strip +
  guides(shape = "none") +
  theme(
    legend.position = c(0.985, 0.8), legend.justification = c(1, 0.5),
    legend.direction = "vertical", legend.box = "vertical", legend.box.just = "top",
    legend.title = element_text(size = 10), legend.text = element_text(size = 9),
    legend.key.height = unit(16, "pt"), legend.key.width = unit(20, "pt"),
    legend.spacing.y = unit(4, "pt"), legend.spacing.x = unit(6, "pt"),
    legend.box.margin = margin(t = 16, r = 0, b = 0, l = 10),
    legend.box.spacing = unit(2, "pt"), legend.margin = margin(0, 0, 0, 0),
    plot.margin = margin(5.5, 12, 5.5, 5.5)
  )

fire_plot_aligned <- fire_plot_strip +
  guides(shape = "none") +
  theme(
    legend.position = c(0.985, 0.8), legend.justification = c(1, 0.5),
    legend.direction = "vertical", legend.box = "vertical", legend.box.just = "top",
    legend.title = element_text(size = 10), legend.text = element_text(size = 9),
    legend.key.height = unit(16, "pt"), legend.key.width = unit(20, "pt"),
    legend.spacing.y = unit(4, "pt"), legend.spacing.x = unit(6, "pt"),
    legend.box.margin = margin(t = 16, r = 0, b = 0, l = 10),
    legend.box.spacing = unit(2, "pt"), legend.margin = margin(0, 0, 0, 0),
    plot.margin = margin(5.5, 12, 5.5, 5.5)
  )

conc_plot_aligned <- conc_plot_strip +
  theme(
    legend.position = "right", legend.direction = "vertical",
    legend.box = "vertical", legend.box.just = "top", legend.justification = c(0, 1),
    legend.title = element_text(size = 10), legend.text = element_text(size = 9),
    legend.key.height = unit(16, "pt"), legend.key.width = unit(20, "pt"),
    legend.spacing.y = unit(4, "pt"), legend.spacing.x = unit(6, "pt"),
    legend.box.spacing = unit(0, "pt"),
    legend.box.margin = margin(t = 16, r = 0, b = 0, l = 10),
    legend.margin = margin(0, 0, 0, 0), plot.margin = margin(5.5, 12, 5.5, 5.5)
  )

panel_fig1 <- wui_plot_aligned + fire_plot_aligned + conc_plot_aligned + plot_layout(ncol = 3)
panel_fig1

ggsave("wui_fig1_v8.png", panel_fig1, width = 11, height = 10)
ggsave("wui_fig1_v8.jpeg", panel_fig1, width = 11, height = 10)
