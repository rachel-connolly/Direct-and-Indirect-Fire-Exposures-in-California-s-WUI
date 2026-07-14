# WUI wildfire exposure analysis

R scripts supporting the analysis of direct and indirect wildland fire
exposures and impacts in California's wildland-urban interface (WUI) by Marlier et al.

Apart from this script series 00-09, there is one other R script (Figures 5 and S9)
and a Python script (Figures 2 [noting there is a version of this in the R script as well; 
moved to Python for consistency with other figure formatting], S2, S3, S5, S6). 

## How to run

1. Open `WUI_analysis.Rproj` in RStudio (this sets your working directory
   to this folder automatically), or run `setwd("path/to/this/folder")`
   yourself before sourcing anything.
2. Run `00_setup_and_data_prep.R` first, in the same R session as the
   numbered scripts that follow.
3. Run the remaining scripts (`01` through `09`) in order, as needed - each
   one is independent once `00` has been run, and lists what it depends on
   in its own header comment.

## Scripts

| Script | Produces |
|---|---|
| `00_setup_and_data_prep.R` | Shared data loading - run first |
| `01_table1_wui_characterization.R` | Table 1 (WUI characterization), Table S2, Table S3 |
| `02_burned_area_table1_table2_figure2.R` | Burned area statistics, Table 2, Table S4/S5, Figure 2 |
| `03_table3_smoke_pm_exposure.R` | Table 3 (smoke PM2.5 by WUI class) |
| `04_table4_combined_exposure_and_figure4.R` | Table 4, Table S9, Figure 4, Figure S8 |
| `05_figure3_smoke_by_climate_zone.R` | Figure 3 (smoke PM2.5 by climate zone) |
| `06_figure1_main_map_panels.R` | Figure 1 (WUI categories / fire perimeters / smoke concentration) |
| `07_figureS1_wui_designation_map.R` | Figure S1 (WUI designation map) |
| `08_mortality_analysis_and_figure.R` | Table S8, Figure S7 (mortality maps) |
| `09_figureS4_monitoring_stations_map.R` | Figure S4 (monitoring station map) |

## Data availability

Most of the data used by these scripts is included in `data/`. Two large
inputs are not bundled here - download them from their original public
sources and place them at the paths below before running
`00_setup_and_data_prep.R`:

- **CMAQ smoke PM2.5 rasters** (`data/CMAQ Daily Rasters/2008_fireonly.tif` ... `2018_fireonly.tif`):
  available at [https://doi.org/10.5061/dryad.sxksn03b3](https://doi.org/10.5061/dryad.sxksn03b3)
- **WUI block shapefile** (`data/Shapefiles/CA_wui_block_1990_2020_change_v4_repr.*`):
  available at [https://silvis.forest.wisc.edu/data/wui-change/](https://silvis.forest.wisc.edu/data/wui-change/)

A third file, `data/Shapefiles/fire_count_exc_wui.shp`, is also not bundled
here - it's an intermediate output of the pipeline in
`04_table4_combined_exposure_and_figure4.R` (fire count + NAAQS exceedance
days per WUI block), not a primary input. Once the two datasets above are
in place, regenerate it by uncommenting the `st_write()` line just above
where it's read in that script and running the script once.
