# ==============================================================================
# 07: Figure S1 (WUI designation map)
# ==============================================================================
# Requires: 00_setup_and_data_prep.R run first (uses wui_rad, ca_albers_sf, add_strip())
# ==============================================================================

library(dplyr)
library(sf)
library(ggplot2)
library(ggtext)
library(ggspatial)

# Recode to the reader-friendly category labels used for this map's legend
# (a third labeling scheme, distinct from wui_class_update elsewhere - this
# one splits WUI into Intermix/Interface and spells out the non-WUI categories)
wui_map_df <- wui_rad
wui_map_df$wui_class_update <- wui_map_df$WUICLASS_2
wui_map_df$wui_class_update[wui_map_df$WUIFLAG202 == 1] <- "Intermix WUI"
wui_map_df$wui_class_update[wui_map_df$WUIFLAG202 == 2] <- "Interface WUI"
wui_map_df$wui_class_update[wui_map_df$WUICLASS_2 == 'Very_Low_Dens_NoVeg' |
                               wui_map_df$WUICLASS_2 == 'Low_Dens_NoVeg'] <- "Non-Vegetated or Agriculture: Low/Very Low Housing Density"
wui_map_df$wui_class_update[wui_map_df$WUICLASS_2 == 'Med_Dens_NoVeg' |
                               wui_map_df$WUICLASS_2 == 'High_Dens_NoVeg'] <- "Non-Vegetated or Agriculture: Medium/High Housing Density"
wui_map_df$wui_class_update[wui_map_df$WUICLASS_2 == 'Uninhabited_Veg'] <- "Non-WUI Vegetated: No Housing"
wui_map_df$wui_class_update[wui_map_df$WUICLASS_2 == 'Very_Low_Dens_Veg'] <- "Non-WUI Vegetated: Very Low Housing Density"
wui_map_df$wui_class_update[wui_map_df$WUICLASS_2 == 'Uninhabited_NoVeg'] <- "Non-Vegetated/Uninhabited"
table(wui_map_df$wui_class_update)

wui_map_df_2 <- subset(wui_map_df, !wui_map_df$wui_class_update == "Non-Vegetated/Uninhabited")

group.colors <- c(
  "Interface WUI" = "darkgreen",
  "Intermix WUI" = "orange",
  "Non-Vegetated or Agriculture: Low/Very Low Housing Density" = "#FFFFFF",
  "Non-Vegetated or Agriculture: Medium/High Housing Density" = "#990000",
  "Non-WUI Vegetated: No Housing" = "#C5E384",
  "Non-WUI Vegetated: Very Low Housing Density" = "#FFFFCC",
  "Water" = "#99CCFF"
)

wui_designation_plot <- ggplot() +
  geom_sf(data = wui_map_df_2, aes(fill = wui_class_update), color = NA) +
  geom_sf(data = ca_albers_sf, fill = NA, color = "black", linewidth = 0.1) +
  coord_sf(datum = NA, expand = FALSE) +
  labs(title = expression(WUI~Map), fill = expression(Categories)) +
  scale_fill_manual(values = group.colors, drop = FALSE, na.value = "grey80") +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white"), legend.position = "right")

wui_des_plot_strip <- add_strip(wui_designation_plot, "WUI Designations")

wui_des_plot_aligned <- wui_des_plot_strip +
  guides(fill = guide_legend(ncol = 1)) +
  theme(
    legend.position = c(1.1, 0.8), legend.justification = c(1, 0.5), legend.direction = "vertical",
    legend.box.background = element_rect(fill = "white", color = NA), legend.background = element_blank(),
    legend.title = element_text(size = 10), legend.text = element_text(size = 9),
    legend.key.height = unit(16, "pt"), legend.key.width = unit(20, "pt"),
    legend.spacing.y = unit(4, "pt"), legend.spacing.x = unit(6, "pt"),
    plot.margin = margin(5.5, 12, 5.5, 5.5)
  )
wui_des_plot_aligned

ggsave("wui_designations_v8.pdf", wui_des_plot_aligned, width = 9, height = 8)
ggsave("figs1_wui_designations.png", wui_des_plot_aligned, width = 9, height = 8)
