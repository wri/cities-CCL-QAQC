# Raster QA/QC Viewer

This folder is a standalone Shiny app for browsing raster outputs stored in S3.
It does not depend on the rest of the `cities-heat-resilient-infrastructure`
repo once copied out as its own repository.

## What it does

- Lists tiles for a selected `city / aoi / infrastructure / scenario`
- Lists available `ccl_layers/*.tif` rasters under that scenario
- Loads and merges the selected raster across tiles
- Displays the raster on an interactive map with pan, zoom, identify, and
  optional satellite basemap
- Optionally overlays a vector file from an HTTP `geojson`, `json`, or
  `parquet` URL
- Shows summary statistics plus a density plot for continuous rasters or a
  histogram for nominal rasters

## Requirements

- R
- Internet access to the public `wri-cities-tcm` bucket

## First run

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
