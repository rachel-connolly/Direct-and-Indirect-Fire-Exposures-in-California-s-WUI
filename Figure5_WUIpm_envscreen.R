# WUI wildfire exposure x CalEnviroScreen bivariate maps (Figure 5)
# direct = fire count, indirect = smoke PM2.5, x-axis = CES percentile
# (or individual EJ variables for the supplement figure)

# setwd("path/to/repo")  # uncomment + edit if you're not already running from here

library(sf)
library(dplyr)
library(tmap)
library(tigris)

##### 1. build the fire_count/EJ dataset from source layers #####

#  load blocks x cmaq x CalEnviroScreen, filter to WUI 
fire_count <- st_read("cmaq_wui_es.shp", quiet = TRUE)
fire_count <- fire_count %>% filter(WUIFLAG201 %in% c(1, 2))
fire_count <- st_make_valid(fire_count)
cat("WUI blocks (WUIFLAG201 1/2):", nrow(fire_count), "rows,", length(unique(fire_count$BLK20)), "distinct BLK20\n")

#  fire count: intersect against 2008-2018 perimeters
fire_gdb <- "/Users/cschollaert/Library/CloudStorage/Box-Box/WUI Datasets/Fire Perimeters/fire22_1.gdb"
fire_perims <- st_read(fire_gdb, layer = "firep22_1_2008to2018_repr", quiet = TRUE)
fire_perims <- st_make_valid(fire_perims)
cat("fire perimeters 2008-2018:", nrow(fire_perims), "\n")

fire_count$row_id <- seq_len(nrow(fire_count))
hits <- st_join(fire_count["row_id"], fire_perims["OBJECTID"], join = st_intersects)
st_geometry(hits) <- NULL
fir_cnt_tab <- hits %>% group_by(row_id) %>% summarise(fir_cnt = sum(!is.na(OBJECTID)), .groups = "drop")

fire_count <- fire_count %>% left_join(fir_cnt_tab, by = "row_id")
fire_count$fir_cnt[is.na(fire_count$fir_cnt)] <- 0
cat("fir_cnt distribution:\n")
print(table(fire_count$fir_cnt))

fire_count$fr_cnt_ <- pmin(fire_count$fir_cnt, 3) + 1

# cmaq_pr: equal-frequency quartile of the raw cmaq value
fire_count$cmaq_pr <- dplyr::ntile(fire_count$cmaq, 4)

#  clean values (any negative % / percentile is invalid - catches -999, -1998, etc.) 
pct_cols <- c("CIscoreP", "LowBirWP", "EducatP", "Ling_IsolP", "HousBurdP",
              "AsthmaP", "PovertyP", "UnemplP", "CardiovasP")
for (cc in pct_cols) fire_count[[cc]][fire_count[[cc]] < 0] <- NA

share_cols <- c("Child_10", "Elderly65", "AAPI", "AfricanAm", "White", "NativeAm", "Hispanic", "OtherMult")
for (cc in share_cols) fire_count[[cc]][fire_count[[cc]] < 0] <- NA

# rename percentile columns to match the shorter names used below
fire_count <- fire_count %>% rename(CIscorP = CIscoreP, LowBrWP = LowBirWP, Lng_IsP = Ling_IsolP, HosBrdP = HousBurdP)

#  EJ variables 
# Asthm_p/Pvrty_p/Crdvs_p: clean quartiles of the official CalEnviroScreen percentile, no zero code
fire_count$Asthm_p <- dplyr::ntile(fire_count$AsthmaP, 4)
fire_count$Pvrty_p <- dplyr::ntile(fire_count$PovertyP, 4)
fire_count$Crdvs_p <- dplyr::ntile(fire_count$CardiovasP, 4)

# quartile-of-nonzero + explicit 0 code, for variables where "0" is a real value
zero_plus_quartile <- function(x) {
  out <- rep(NA_integer_, length(x))
  out[!is.na(x) & x == 0] <- 0L
  pos <- !is.na(x) & x > 0
  out[pos] <- dplyr::ntile(x[pos], 4)
  out
}

fire_count$Unmpl_p <- zero_plus_quartile(fire_count$UnemplP)
fire_count$Chl_10_ <- zero_plus_quartile(fire_count$Child_10)
fire_count$Eldr65_ <- zero_plus_quartile(fire_count$Elderly65)
fire_count$AAPI_pr <- zero_plus_quartile(fire_count$AAPI)
fire_count$AfrcnA_ <- zero_plus_quartile(fire_count$AfricanAm)
fire_count$Whit_pr <- zero_plus_quartile(fire_count$White)
fire_count$NtvAm_p <- zero_plus_quartile(fire_count$NativeAm)
fire_count$Hspnc_p <- zero_plus_quartile(fire_count$Hispanic)
fire_count$OthrMl_ <- zero_plus_quartile(fire_count$OtherMult)

# keep what the figure code below actually needs
keep_cols <- c(
  "BLK20", "cmaq", "cmaq_pr", "fir_cnt", "fr_cnt_",
  "CIscorP", "CIscore", "LowBrWP", "EducatP", "Lng_IsP", "HosBrdP",
  "Asthm_p", "Pvrty_p", "Unmpl_p", "Crdvs_p",
  "Chl_10_", "Eldr65_", "AAPI_pr", "AfrcnA_", "Whit_pr", "NtvAm_p", "Hspnc_p", "OthrMl_"
)
fire_count <- fire_count[, keep_cols]

st_write(fire_count, "fire_count_EJ_v2.gpkg", delete_dsn = TRUE, quiet = TRUE)
cat("saved fire_count_EJ_v2.gpkg,", nrow(fire_count), "rows\n\n")

##### 2. figure code #####

exp_fire <- "fr_cnt_"
exp_fire_prebinned <- TRUE
exp_cmaq <- "cmaq_pr"

ej_vars <- c(
  "Asthm_p","Pvrty_p","Unmpl_p","Crdvs_p","LowBrWP",
  "EducatP","Lng_IsP","HosBrdP","Chl_10_","Eldr65_",
  "AAPI_pr","AfrcnA_","Whit_pr","NtvAm_p","Hspnc_p", "OthrMl_"
)

ej_vars_prebinned <- c(
  "Asthm_p", "Pvrty_p", "Crdvs_p",
  "Unmpl_p", "Chl_10_", "Eldr65_", "AAPI_pr", "AfrcnA_", "Whit_pr", "NtvAm_p", "Hspnc_p", "OthrMl_"
)
ej_vars_zero_merge <- c("Unmpl_p", "Chl_10_", "Eldr65_", "AAPI_pr", "AfrcnA_", "Whit_pr", "NtvAm_p", "Hspnc_p", "OthrMl_")
for (v in ej_vars_zero_merge) {
  fire_count[[v]][fire_count[[v]] == 0] <- 1
}

# makes the x/y quantile bins and pastes into a "1-1" ... "4-4" bivar code.

make_bivar <- function(df, ej_var, expvar, n = 4, bin_ej = TRUE, bin_exp = TRUE) {
  out <- df %>%
    mutate(
      .ej  = .data[[ej_var]],
      .exp = .data[[expvar]],
      x_q  = if (bin_ej)  dplyr::ntile(.ej,  n) else as.integer(.ej),
      y_q  = if (bin_exp) dplyr::ntile(.exp, n) else as.integer(.exp),
      bivar = paste0(x_q, "-", y_q)
    )
  # column-major order so the palette lines up: 1-1,2-1,3-1,4-1,1-2,...,4-4
  labels_4 <- as.vector(outer(1:4, 1:4, paste, sep = "-"))
  out$bivar <- factor(out$bivar, levels = labels_4)
  out
}

#  bivariate palettes, picked by hand 
labels_4 <- as.vector(outer(1:4, 1:4, paste, sep = "-"))

# builds the 4x4 legend grid as its own little tmap object
make_bivar_legend <- function(palette, title_x = "EJ variable →", title_y = "Exposure ↑") {
  grid <- expand.grid(x = 1:4, y = 1:4)
  grid$bivar <- paste0(grid$y, "-", grid$x)  # matches the row-col color naming below

  polys <- lapply(1:nrow(grid), function(i) {
    st_polygon(list(rbind(
      c(grid$x[i]-1, grid$y[i]-1),
      c(grid$x[i],   grid$y[i]-1),
      c(grid$x[i],   grid$y[i]),
      c(grid$x[i]-1, grid$y[i]),
      c(grid$x[i]-1, grid$y[i]-1)
    )))
  })

  sf_grid <- st_sf(grid, geometry = st_sfc(polys))

  tm_shape(sf_grid) +
    tm_polygons("bivar", palette = palette, border.col = "grey30", lwd = 0.2) +
    tm_layout(
      legend.show = FALSE,
      main.title = paste(title_y, "\n", title_x),
      main.title.size = 0.8,
      frame = FALSE
    )
}

# fire count palette: red -> violet
fc_colors <- c(
  "4-1" = "#b30000", "4-2" = "#8f1d5f", "4-3" = "#6a176e", "4-4" = "#3f007d",
  "3-1" = "#e07b91", "3-2" = "#c172a4", "3-3" = "#9966a0", "3-4" = "#7363A6",
  "2-1" = "#f2b6c2", "2-2" = "#d4a6c0", "2-3" = "#b58cb8", "2-4" = "#7b8ec0",
  "1-1" = "#d3d3d3", "1-2" = "#a6bcc7", "1-3" = "#77a5bb", "1-4" = "#478fb0"
)

# smoke PM2.5 palette: gold -> green
cmaq_colors <- c(
  "4-1" = "#dfa307", "4-2" = "#af9100", "4-3" = "#7c8000", "4-4" = "#4c6e02",
  "3-1" = "#d8b752", "3-2" = "#aca34f", "3-3" = "#7b914a", "3-4" = "#4b7b44",
  "2-1" = "#d5c599", "2-2" = "#a9b090", "2-3" = "#7a9c86", "2-4" = "#49867e",
  "1-1" = "#d3d3d3", "1-2" = "#a6bcc7", "1-3" = "#77a5bb", "1-4" = "#478fb0"
)

legend_fc   <- make_bivar_legend(fc_colors, title_x = "EJ →", title_y = "Fire Count ↑")
legend_cmaq <- make_bivar_legend(cmaq_colors, title_x = "EJ →", title_y = "CMAQ PM2.5 ↑")

tmap_save(legend_fc, "legend_fc.png", width = 4, height = 4, units = "in", dpi = 300)
tmap_save(legend_cmaq, "legend_cmaq.png", width = 4, height = 4, units = "in", dpi = 300)

tmap_mode("plot")
tmap_options(check.and.fix = TRUE)  

state_outline <- states(cb = TRUE) %>%
  filter(STUSPS == "CA") %>%
  st_transform(st_crs(fire_count))

# makes one map per EJ variable, fire count and cmaq separately (32 PNGs).
# this is only for the supplement - flip to TRUE when you actually need it,
# takes a while to run since it's rendering ~193k polygons 32 times over
RUN_ALL_EJ_MAPS <- FALSE

if (RUN_ALL_EJ_MAPS) {
for (v in ej_vars) {
  v_bin_ej <- !(v %in% ej_vars_prebinned)  # FALSE = already 1-4 coded, don't re-bin

  # fire count vs this EJ var
  dat_fc <- make_bivar(fire_count, v, exp_fire, n = 4, bin_ej = v_bin_ej, bin_exp = !exp_fire_prebinned)
  tm_fc <- tm_shape(dat_fc) +
    tm_polygons("bivar",
                palette = fc_colors,
                border.col = "grey85", lwd = 0.001,
                colorNA = "transparent") +
    tm_shape(state_outline) +
    tm_borders(lwd = 1, col = "black") +
    tm_layout(
      main.title = paste("Direct (Fire Count) vs", v),
      legend.show = FALSE,
      frame = FALSE
    )
  tmap_save(tm_fc,
            filename = paste0("map_fc_", v, ".png"),
            width = 1800, height = 1800, units = "px", dpi = 300)

  # cmaq vs this EJ var
  dat_cq <- make_bivar(fire_count, v, exp_cmaq, n = 4, bin_ej = v_bin_ej)
  tm_cq <- tm_shape(dat_cq) +
    tm_polygons("bivar",
                palette = cmaq_colors,
                border.col = "grey85", lwd = 0.001,
                colorNA = "transparent") +
    tm_shape(state_outline) +
    tm_borders(lwd = 1, col = "black") +
    tm_layout(
      main.title = paste("Indirect (CMAQ PM2.5) vs", v),
      legend.show = FALSE,
      frame = FALSE
    )
  tmap_save(tm_cq,
            filename = paste0("map_cmaq_", v, ".png"),
            width = 1800, height = 1800, units = "px", dpi = 300)
}
}

### main text + inset maps

fire_count <- make_bivar(fire_count, ej_var = "cmaq_pr", expvar = "CIscorP", n = 4)
names(fire_count)[names(fire_count) == "bivar"] <- "cmaq_CI_bivar"

# fr_cnt_ is the 0/1/2/3+ code again, not a raw count - bin_ej = FALSE keeps it as-is
fire_count <- make_bivar(fire_count, ej_var = "fr_cnt_", expvar = "CIscorP", n = 4, bin_ej = FALSE)
names(fire_count)[names(fire_count) == "bivar"] <- "fc_CI_bivar"

# bounding boxes for the two zoomed insets (bay area/sierra foothills, LA/inland empire)
bbox1 <- st_as_sfc(st_bbox(c(
  xmin = -122.822323, ymin = 37.018408,
  xmax = -120.533508, ymax = 39.471070
), crs = 4326))

bbox2 <- st_as_sfc(st_bbox(c(
  xmin = -118.671219, ymin = 32.666461,
  xmax = -116.443863, ymax = 34.713620
), crs = 4326))

map_state_cmaq <- tm_shape(fire_count) +
  tm_polygons("cmaq_CI_bivar", palette = cmaq_colors, border.col = NA, lwd=0.0013, colorNA = "transparent") +
  tm_shape(bbox1) + tm_borders(col = "#C9C9C9", lwd = 1.5) +
  tm_shape(bbox2) + tm_borders(col = "#C9C9C9", lwd = 1.5) +
  tm_shape(state_outline) + tm_borders(lwd = 0.6, col = "black") +
  tm_scale_bar(position = c("right", "bottom"), text.size = 1.1, lwd = 1.5) +
  tm_layout(main.title = "CMAQ vs CIscore (Statewide)",
            legend.show = FALSE, frame = FALSE)

map_state_fc <- tm_shape(fire_count) +
  tm_polygons("fc_CI_bivar", palette = fc_colors, border.col = NA, lwd=0.001, colorNA = "transparent") +
  tm_shape(bbox1) + tm_borders(col = "#C9C9C9", lwd = 1.5) +
  tm_shape(bbox2) + tm_borders(col = "#C9C9C9", lwd = 1.5) +
  tm_shape(state_outline) + tm_borders(lwd = 0.6, col = "black") +
  tm_scale_bar(position = c("right", "bottom"), text.size = 1.1, lwd = 1.5) +
  tm_layout(main.title = "Fire Count vs CIscore (Statewide)",
            legend.show = FALSE, frame = FALSE)

# cmaq zooms
map_cmaq_zoom1 <- tm_shape(fire_count, bbox = st_bbox(bbox1)) +
  tm_polygons("cmaq_CI_bivar", palette = cmaq_colors, border.col = NA, lwd=0.001, colorNA = "transparent") +
  tm_borders(lwd = 0.4) +
  tm_shape(state_outline) + tm_borders(lwd = 0.8, col = "black") +
  tm_scale_bar(position = c("left", "bottom"), text.size = 1.1, lwd = 1.5) +
  tm_layout(main.title = "CMAQ vs CIscore (Zoom 1)",
            legend.show = FALSE, frame = FALSE)

map_cmaq_zoom2 <- tm_shape(fire_count, bbox = st_bbox(bbox2)) +
  tm_polygons("cmaq_CI_bivar", palette = cmaq_colors, border.col = NA, lwd=0.001, colorNA = "transparent") +
  tm_borders(lwd = 0.4) +
  tm_shape(state_outline) + tm_borders(lwd = 0.8, col = "black") +
  tm_scale_bar(position = c("left", "bottom"), text.size = 1.1, lwd = 1.5) +
  tm_layout(main.title = "CMAQ vs CIscore (Zoom 2)",
            legend.show = FALSE, frame = FALSE)

# fire count zooms
map_fc_zoom1 <- tm_shape(fire_count, bbox = st_bbox(bbox1)) +
  tm_polygons("fc_CI_bivar", palette = fc_colors, border.col = NA, lwd=0.001, colorNA = "transparent") +
  tm_borders(lwd = 0.4) +
  tm_shape(state_outline) + tm_borders(lwd = 0.8, col = "black") +
  tm_scale_bar(position = c("left", "bottom"), text.size = 1.1, lwd = 1.5) +
  tm_layout(main.title = "Fire Count vs CIscore (Zoom 1)",
            legend.show = FALSE, frame = FALSE)

map_fc_zoom2 <- tm_shape(fire_count, bbox = st_bbox(bbox2)) +
  tm_polygons("fc_CI_bivar", palette = fc_colors, border.col = NA, lwd=0.001, colorNA = "transparent") +
  tm_borders(lwd = 0.4) +
  tm_shape(state_outline) + tm_borders(lwd = 0.8, col = "black") +
  tm_scale_bar(position = c("left", "bottom"), text.size = 1.1, lwd = 1.5) +
  tm_layout(main.title = "Fire Count vs CIscore (Zoom 2)",
            legend.show = FALSE, frame = FALSE)

tmap_save(map_state_cmaq, "map_state_cmaq.png", width = 8, height = 10, units = "in", dpi = 300)
tmap_save(map_state_fc,   "map_state_fc.png",   width = 8, height = 10, units = "in", dpi = 300)

tmap_save(map_cmaq_zoom1, "map_cmaq_zoom1.png", width = 6, height = 6, units = "in", dpi = 300)
tmap_save(map_cmaq_zoom2, "map_cmaq_zoom2.png", width = 6, height = 6, units = "in", dpi = 300)

tmap_save(map_fc_zoom1,   "map_fc_zoom1.png",   width = 6, height = 6, units = "in", dpi = 300)
tmap_save(map_fc_zoom2,   "map_fc_zoom2.png",   width = 6, height = 6, units = "in", dpi = 300)

##### stats - are more vulnerable blocks (top CES quartile) more exposed to fire?

fire_count$CI_quartile <- ntile(fire_count$CIscorP, 4)

# exposed = ever had a fire
fire_count$exposed <- ifelse(fire_count$fir_cnt > 0, 1, 0)

tab <- fire_count %>%
  group_by(CI_quartile) %>%
  summarise(exposed_n = sum(exposed),
            total_n   = n(),
            prop_exposed = mean(exposed))

least <- tab %>% filter(CI_quartile == 1)
most  <- tab %>% filter(CI_quartile == 4)

prop.test(x = c(least$exposed_n, most$exposed_n),
          n = c(least$total_n,   most$total_n),
          alternative = "two.sided")

glm_fit <- glm(exposed ~ CI_quartile, data = fire_count, family = binomial)
summary(glm_fit)

## same thing but CI_quartile as a factor - gives the Q1 vs Q4 OR directly off the coefficient table
fire_count$CI_quartile_f <- factor(fire_count$CI_quartile)
fit_fac <- glm(exposed ~ CI_quartile_f, family = binomial, data = fire_count)
summary(fit_fac)

exp(coef(fit_fac))
exp(confint(fit_fac))

# read the % exposed directly instead of pulling it out of prop.test/glm
n_least   <- sum(fire_count$CI_quartile == 1, na.rm = TRUE)
n_most    <- sum(fire_count$CI_quartile == 4, na.rm = TRUE)

pct_least <- 100 * mean(fire_count$exposed[fire_count$CI_quartile == 1], na.rm = TRUE)
pct_most  <- 100 * mean(fire_count$exposed[fire_count$CI_quartile == 4], na.rm = TRUE)

cat(sprintf("Least-vulnerable quartile (CI_quartile == 1): %.2f%% of %d blocks have >= 1 fire\n",
            pct_least, n_least))
cat(sprintf("Most-vulnerable quartile (CI_quartile == 4):  %.2f%% of %d blocks have >= 1 fire\n",
            pct_most, n_most))
