# Raster QA/QC Viewer

This is a standalone Shiny app for browsing CCL_layers raster outputs stored in S3.
It's intended to make QA/QC easier.

## What it does

- Lists tiles for a selected `city / aoi / infrastructure / scenario`
- Lists available `ccl_layers/*.tif` rasters under that scenario
- Loads and merges the selected raster across tiles
- Displays the raster on an interactive map with pan, zoom, identify, and
  optional satellite basemap
- Optionally overlays a vector file from an HTTP `geojson` or `json` URL
- Shows summary statistics plus a density plot for continuous rasters or a
  histogram for nominal rasters

## Requirements

- R
- Internet access to the public `wri-cities-tcm` bucket

## Run

From this folder:

```bash
Rscript run_app.R
```

On first run:

1. `.Rprofile` installs `renv` if it is missing.
2. `renv` restores the project library from `renv.lock`.
3. The app launches locally in your browser.

## Alternative launch

If you prefer an interactive R session:

```r
source(".Rprofile")
shiny::runApp()
```

## Instructions

1. Choose a city from the dropdown--the aoi name will auto populate.
2. Choose an infrastructure from the dropdown—-the scenario name will auto populate.
3. Click 'List available layers'. The Layer dropdown will populate with the raster layers
in the ccl_layers tile folders.
4. Choose whether the raster should be rendered as continuous or discrete.
5. Optionally add a URL pointing to a geojson layer like a boundary.
6. Choose the basemap.
7. Click 'Load selected layer' to add the layer to the map. It has to load and merge
the layer for all tiles, so it takes a minute, but your patience will be rewarded.
8. The raster will display in the map, alongside summary statistics and a density
plot for continuous rasters and a histogram for discrete raster. You can also click
on the map and the pixel value will be displayed in the box at the lower left of the page.

## Repo contents

- `app.R`: the Shiny app
- `R/s3_helpers.R`: local copies of the helper functions needed for
  `list_tiles()` and `load_and_merge()`
- `.Rprofile`: bootstraps and restores the `renv` environment
- `renv.lock`: locked package versions for the app
- `run_app.R`: one-command launcher for end users

## Notes

- The app reads rasters and lists scenario contents directly from the public S3
  bucket over HTTPS.
- The map is displayed in leaflet-friendly lon/lat coordinates, but identify
  queries are still run against the loaded raster values.
