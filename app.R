clear_conflicting_geospatial_env <- function() {
  vars <- c("PROJ_LIB", "PROJ_DATA", "GDAL_DATA")

  for (var in vars) {
    value <- Sys.getenv(var, unset = "")
    if (nzchar(value) && grepl("(anaconda|miniconda|conda)", value, ignore.case = TRUE)) {
      Sys.unsetenv(var)
    }
  }
}

clear_conflicting_geospatial_env()

library(shiny)
library(leaflet)
library(terra)
library(ggplot2)
library(glue)

# Fixed app configuration shared by all users of this app.
bucket_name <- "wri-cities-tcm"

# Source the local standalone S3/raster helpers that were copied out of the
# main project repo.
source(file.path("R", "s3_helpers.R"), local = TRUE)

# Build the scenario folder used by list_tiles() and the HTTP raster paths.
build_scenario_prefix <- function(city, aoi_name, infrastructure, scenario) {
  glue(
    "city_projects/{trimws(city)}/{trimws(aoi_name)}/scenarios/{trimws(infrastructure)}/{trimws(scenario)}"
  )
}

build_s3_url <- function(prefix) {
  glue("s3://{bucket_name}/{sub('/+$', '', prefix)}/")
}

build_http_base <- function() {
  glue("https://{bucket_name}.s3.us-east-1.amazonaws.com")
}

# List all GeoTIFF files under ccl_layers for one scenario folder.
list_ccl_layers <- function(scenario_prefix) {
  listed <- s3_list_bucket_public(
    bucket = bucket_name,
    prefix = paste0(sub("/+$", "", scenario_prefix), "/")
  )

  tif_keys <- grep("ccl_layers/[^/]+\\.tif$", listed$keys, value = TRUE)
  sort(unique(sub("^.*ccl_layers/([^/]+\\.tif)$", "\\1", tif_keys)))
}

# Convert tile ids into the HTTP URLs used by load_and_merge().
build_layer_paths <- function(scenario_prefix, tiles, layer_name) {
  http_base <- build_http_base()
  glue("{http_base}/{scenario_prefix}/{tiles}/ccl_layers/{layer_name}")
}

# Compute the most common value in a numeric vector for nominal raster previews.
vector_mode <- function(x, na.rm = TRUE, ...) {
  if (isTRUE(na.rm)) {
    x <- x[!is.na(x)]
  }

  if (!length(x)) {
    return(NA_real_)
  }

  values <- unique(x)
  values[which.max(tabulate(match(x, values)))]
}

# Sample raster values directly from the source raster without aggregating first.
sample_source_values <- function(rast, size = 50000) {
  sampled <- terra::spatSample(
    rast,
    size = min(size, terra::ncell(rast)),
    method = "regular",
    na.rm = TRUE,
    values = TRUE,
    as.df = TRUE
  )

  vals <- sampled[[1]]
  vals[is.finite(vals)]
}

# Reproject the full raster to lon/lat for leaflet display without changing its
# native cell size through aggregation. Nearest-neighbor keeps cell values intact.
prepare_map_raster <- function(rast, data_type) {
  if (terra::is.lonlat(rast)) {
    return(rast)
  }

  raster_crs <- tryCatch(terra::crs(rast), error = function(e) "")
  if (!nzchar(trimws(raster_crs))) {
    stop(
      paste(
        "Raster CRS could not be read on this machine.",
        "This is often caused by a conflicting PROJ installation such as Anaconda.",
        "Try launching the app outside Conda or clearing PROJ_LIB / PROJ_DATA."
      ),
      call. = FALSE
    )
  }

  tryCatch(
    terra::project(rast, "EPSG:4326", method = "near"),
    error = function(e) {
      stop(
        paste(
          "Could not reproject the raster for map display.",
          "This usually indicates a PROJ/GDAL configuration issue on this machine.",
          conditionMessage(e)
        ),
        call. = FALSE
      )
    }
  )
}

# Sample raster values for the chart without reading every pixel into memory.
sample_values <- function(rast, data_type, max_cells = 50000) {
  if (data_type == "continuous") {
    sample_source_values(rast, size = max_cells)
  } else {
    sample_source_values(rast, size = max_cells)
  }
}

# Summary stats for continuous rasters.
continuous_stats <- function(rast) {
  min_max <- terra::minmax(rast)
  c(
    Min = as.numeric(min_max[1, 1]),
    Mean = as.numeric(terra::global(rast, fun = mean, na.rm = TRUE)[1, 1]),
    Median = as.numeric(terra::global(rast, fun = median, na.rm = TRUE)[1, 1]),
    Max = as.numeric(min_max[2, 1])
  )
}

# Use the display raster for map colors so reprojection/resampling does not
# generate values outside the palette domain.
display_range <- function(rast) {
  min_max <- terra::minmax(rast)
  range_vals <- c(as.numeric(min_max[1, 1]), as.numeric(min_max[2, 1]))

  if (isTRUE(all.equal(range_vals[1], range_vals[2]))) {
    range_vals <- range_vals + c(-0.5, 0.5)
  } else {
    padding <- max(.Machine$double.eps^0.5, diff(range_vals) * 1e-8)
    range_vals <- c(range_vals[1] - padding, range_vals[2] + padding)
  }

  range_vals
}

# Get every distinct class in the displayed nominal raster so the map palette
# covers all classes, not just the sampled chart values.
nominal_classes <- function(rast) {
  freq_table <- terra::freq(rast)

  if (is.null(freq_table) || !nrow(freq_table)) {
    return(numeric(0))
  }

  classes <- as.numeric(freq_table[, "value"])
  sort(unique(classes[is.finite(classes)]))
}

# Summary stats for nominal rasters.
nominal_stats <- function(values) {
  counts <- sort(table(values), decreasing = TRUE)
  c(
    Classes = length(counts),
    "Sampled cells" = length(values),
    "Most common class" = names(counts)[1],
    "Most common count" = as.integer(counts[[1]])
  )
}

# Read an optional vector overlay from an HTTP URL.
load_vector_layer <- function(vector_url) {
  if (!nzchar(trimws(vector_url))) {
    return(NULL)
  }

  clean_url <- sub("\\?.*$", "", trimws(vector_url))
  ext <- tolower(tools::file_ext(clean_url))

  vector_data <- switch(
    ext,
    geojson = sf::st_read(vector_url, quiet = TRUE),
    json = sf::st_read(vector_url, quiet = TRUE),
    stop("Vector URL must point to a .geojson or .json file.", call. = FALSE)
  )

  if (!inherits(vector_data, "sf")) {
    stop("Vector file could not be read as sf data.", call. = FALSE)
  }

  sf::st_transform(vector_data, 4326)
}

# Add points, lines, and polygons to the leaflet map with simple styling.
add_vector_overlay <- function(map, vector_data) {
  if (is.null(vector_data) || !nrow(vector_data)) {
    return(map)
  }

  geom_types <- as.character(sf::st_geometry_type(vector_data, by_geometry = TRUE))
  polygon_rows <- geom_types %in% c("POLYGON", "MULTIPOLYGON")
  line_rows <- geom_types %in% c("LINESTRING", "MULTILINESTRING")
  point_rows <- geom_types %in% c("POINT", "MULTIPOINT")

  if (any(polygon_rows)) {
    map <- map |>
      addPolygons(
        data = vector_data[polygon_rows, ],
        color = "#de2d26",
        weight = 2,
        fill = TRUE,
        fillOpacity = 0.08,
        group = "Vector overlay"
      )
  }

  if (any(line_rows)) {
    map <- map |>
      addPolylines(
        data = vector_data[line_rows, ],
        color = "#de2d26",
        weight = 3,
        opacity = 0.9,
        group = "Vector overlay"
      )
  }

  if (any(point_rows)) {
    map <- map |>
      addCircleMarkers(
        data = vector_data[point_rows, ],
        radius = 4,
        stroke = TRUE,
        weight = 1,
        color = "#a50f15",
        fillOpacity = 0.9,
        group = "Vector overlay"
      )
  }

  map
}

# Helper formatting for the table and identify output.
format_value <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return("NA")
  }

  if (length(x) > 1) {
    return(vapply(x, format_value, character(1)))
  }

  if (is.na(x)) {
    return("NA")
  }

  if (is.numeric(x)) {
    return(format(signif(x, 6), trim = TRUE, scientific = FALSE, big.mark = ","))
  }

  as.character(x)
}

# Use the first token in the filename for a shorter legend label.
layer_label <- function(layer_name) {
  if (is.null(layer_name) || !nzchar(layer_name)) {
    return("")
  }

  sub("__.*$", "", layer_name)
}

# Get map bounds from a raster already prepared for leaflet.
get_raster_bounds <- function(rast) {
  ext <- terra::ext(rast)
  list(
    lng1 = ext[1],
    lng2 = ext[2],
    lat1 = ext[3],
    lat2 = ext[4]
  )
}

# Get map bounds from a vector overlay that has been transformed to EPSG:4326.
get_vector_bounds <- function(vector_data) {
  if (is.null(vector_data) || !nrow(vector_data)) {
    return(NULL)
  }

  bbox <- sf::st_bbox(vector_data)
  list(
    lng1 = as.numeric(bbox["xmin"]),
    lng2 = as.numeric(bbox["xmax"]),
    lat1 = as.numeric(bbox["ymin"]),
    lat2 = as.numeric(bbox["ymax"])
  )
}

# Merge raster and vector extents so the map fits everything that is displayed.
combine_bounds <- function(raster_bounds = NULL, vector_bounds = NULL) {
  bounds_list <- Filter(Negate(is.null), list(raster_bounds, vector_bounds))

  if (!length(bounds_list)) {
    return(NULL)
  }

  list(
    lng1 = min(vapply(bounds_list, `[[`, numeric(1), "lng1")),
    lng2 = max(vapply(bounds_list, `[[`, numeric(1), "lng2")),
    lat1 = min(vapply(bounds_list, `[[`, numeric(1), "lat1")),
    lat2 = max(vapply(bounds_list, `[[`, numeric(1), "lat2"))
  )
}

basemap_choices <- c(
  Light = "CartoDB.Positron",
  Satellite = "Esri.WorldImagery",
  Topographic = "Esri.WorldTopoMap"
)

city_aoi_pairs <- data.frame(
  city = c(
    "BRA-Rio_de_Janeiro",
    "MEX-Monterrey",
    "BRA-Teresina",
    "ARG-Buenos_Aires",
    "ZAF-Johannesburg",
    "ZAF-Cape_Town",
    "IND-Bhopal",
    "BRA-Campinas",
    "BRA-Florianopolis",
    "BRA-Fortaleza",
    "BRA-Recife"
  ),
  aoi_name = c(
    "low_emission_zone",
    "mitras_centro",
    "accelerator_area_big",
    "cildenez_padre_rodolfo_ricciardelli",
    "jukskei-river",
    "business_district",
    "tt_nagar",
    "accelerator_area",
    "accelerator_area",
    "accelerator_area",
    "accelerator_area"
  ),
  stringsAsFactors = FALSE
)

city_choices <- unique(city_aoi_pairs$city)
aoi_choices <- unique(city_aoi_pairs$aoi_name)

infrastructure_choices <- c(
  baseline = "baseline",
  trees = "trees",
  "cool-roofs" = "cool-roofs",
  "shade-structures" = "shade-structures",
  "cool-roofs_trees" = "cool-roofs_trees"
)

scenario_choices <- list(
  baseline = "baseline",
  trees = "pedestrian-achievable-90pctl",
  "cool-roofs" = "all-buildings",
  "shade-structures" = "all-parks",
  "cool-roofs_trees" = "all-buildings_pedestrian-achievable-90pctl"
)

# The UI stays small: scenario inputs, raster controls, and an optional vector URL.
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      #list_layers, #load_layer {
        background-color: #96b4c5;
        border-color: #96b4c5;
        color: #ffffff;
      }
      #list_layers:hover, #load_layer:hover,
      #list_layers:focus, #load_layer:focus,
      #list_layers:active, #load_layer:active {
        background-color: #86a7ba;
        border-color: #86a7ba;
        color: #ffffff;
      }
    "))
  ),
  titlePanel("Raster QA/QC Viewer"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        "city",
        "City",
        choices = city_choices,
        selected = "BRA-Campinas",
        options = list(create = TRUE, persist = FALSE)
      ),
      selectizeInput(
        "aoi_name",
        "AOI name",
        choices = aoi_choices,
        selected = "accelerator_area",
        options = list(create = TRUE, persist = FALSE)
      ),
      selectInput("infrastructure", "Infrastructure", choices = infrastructure_choices, selected = "baseline"),
      selectInput("scenario", "Scenario", choices = scenario_choices[["baseline"]], selected = "baseline"),
      actionButton("list_layers", "List available layers"),
      tags$hr(),
      selectInput("layer_name", "Layer", choices = character(0)),
      radioButtons(
        "data_type",
        "Layer type",
        choices = c("Continuous" = "continuous", "Discrete" = "nominal"),
        selected = "continuous",
        inline = TRUE
      ),
      textInput(
        "vector_url",
        "Optional vector URL",
        placeholder = "https://.../aoi.geojson"
      ),
      selectInput("basemap_name", "Basemap", choices = basemap_choices, selected = "CartoDB.Positron"),
      sliderInput("opacity", "Raster opacity", min = 0.2, max = 1, value = 0.85, step = 0.05),
      actionButton("load_layer", "Load selected layer"),
      tags$hr(),
      strong("Scenario prefix"),
      textOutput("scenario_prefix"),
      tags$br(),
      strong("Tiles"),
      textOutput("tile_summary"),
      tags$br(),
      strong("Vector overlay"),
      textOutput("vector_summary"),
      tags$br(),
      strong("Pixel value"),
      verbatimTextOutput("pixel_value", placeholder = TRUE),
      width = 3
    ),
    mainPanel(
      leafletOutput("map", height = 620),
      tags$br(),
      tableOutput("stats_table"),
      tags$br(),
      plotOutput("value_plot", height = 320)
    )
  )
)

server <- function(input, output, session) {
  safe_notify_error <- function(message_text) {
    if (is.function(session$sendNotification)) {
      tryCatch(
        showNotification(message_text, type = "error", duration = NULL),
        error = function(e) {
          message(message_text)
        }
      )
    } else {
      message(message_text)
    }
  }

  # Store the current raster, vector, and chart state in one place.
  rv <- reactiveValues(
    tiles = character(0),
    full_raster = NULL,
    map_raster = NULL,
    chart_values = NULL,
    stats_vector = NULL,
    vector_data = NULL,
    loaded_layer_name = NULL,
    loaded_data_type = NULL,
    map_bounds = NULL,
    pixel_text = "Click the map to read a pixel value."
  )

  # Build the selected scenario folder from the UI inputs.
  scenario_prefix <- reactive({
    build_scenario_prefix(input$city, input$aoi_name, input$infrastructure, input$scenario)
  })

  # Keep scenario choices aligned to the selected infrastructure.
  observeEvent(input$infrastructure, {
    choices <- scenario_choices[[input$infrastructure]]
    updateSelectInput(session, "scenario", choices = choices, selected = choices[[1]])
  }, ignoreInit = TRUE)

  # When a known city is selected, default the AOI selector to its paired AOI.
  observeEvent(input$city, {
    matched_rows <- city_aoi_pairs[city_aoi_pairs$city == input$city, , drop = FALSE]

    if (!nrow(matched_rows)) {
      return()
    }

    city_aois <- unique(matched_rows$aoi_name)
    selected_aoi <- if (input$aoi_name %in% city_aois) input$aoi_name else city_aois[[1]]
    updated_choices <- unique(c(aoi_choices, city_aois, input$aoi_name))

    updateSelectizeInput(
      session,
      "aoi_name",
      choices = updated_choices,
      selected = selected_aoi,
      server = FALSE
    )
  }, ignoreInit = TRUE)

  output$scenario_prefix <- renderText({
    build_s3_url(scenario_prefix())
  })

  output$tile_summary <- renderText({
    if (!length(rv$tiles)) {
      "No tiles loaded yet."
    } else {
      paste(length(rv$tiles), "tiles")
    }
  })

  output$vector_summary <- renderText({
    if (is.null(rv$vector_data)) {
      "No vector overlay loaded."
    } else {
      paste(nrow(rv$vector_data), "features loaded")
    }
  })

  output$pixel_value <- renderText({
    rv$pixel_text
  })

  # Prepare the displayed raster, chart values, stats, and extent in one place.
  update_derived_state <- function(full_raster, vector_data, data_type) {
    map_raster <- prepare_map_raster(full_raster, data_type = data_type)

    chart_values <- sample_values(full_raster, data_type = data_type)
    if (!length(chart_values)) {
      stop("Selected raster has no finite values.", call. = FALSE)
    }

    stats_vector <- if (data_type == "continuous") {
      continuous_stats(full_raster)
    } else {
      nominal_stats(chart_values)
    }

    rv$map_raster <- map_raster
    rv$chart_values <- chart_values
    rv$stats_vector <- stats_vector
    rv$map_bounds <- combine_bounds(
      raster_bounds = get_raster_bounds(map_raster),
      vector_bounds = get_vector_bounds(vector_data)
    )
  }

  # List available raster layers for the selected scenario.
  observeEvent(input$list_layers, {
    rv$tiles <- character(0)
    rv$full_raster <- NULL
    rv$map_raster <- NULL
    rv$chart_values <- NULL
    rv$stats_vector <- NULL
    rv$vector_data <- NULL
    rv$loaded_layer_name <- NULL
    rv$loaded_data_type <- NULL
    rv$map_bounds <- NULL
    rv$pixel_text <- "Click the map to read a pixel value."
    updateSelectInput(session, "layer_name", choices = character(0))

    tryCatch(
      {
        tiles <- list_tiles(build_s3_url(scenario_prefix()))
        if (!length(tiles)) {
          stop("No tiles were found for that scenario.", call. = FALSE)
        }

        layers <- list_ccl_layers(scenario_prefix())
        if (!length(layers)) {
          stop("No GeoTIFFs were found in ccl_layers for that scenario.", call. = FALSE)
        }

        rv$tiles <- tiles
        updateSelectInput(session, "layer_name", choices = layers, selected = layers[[1]])
      },
      error = function(e) {
        safe_notify_error(conditionMessage(e))
      }
    )
  })

  # Load the selected raster and optional vector overlay after the cleared map
  # has been flushed to the browser.
  load_selected_layer <- function(layer_name, scenario_prefix_value, vector_url, tiles, data_type) {
    raster_paths <- build_layer_paths(scenario_prefix_value, tiles, layer_name)
    full_raster <- load_and_merge(raster_paths, quiet = TRUE)
    vector_data <- load_vector_layer(vector_url)

    rv$full_raster <- full_raster
    rv$vector_data <- vector_data
    rv$loaded_layer_name <- layer_name
    rv$loaded_data_type <- data_type
    update_derived_state(full_raster, vector_data, data_type)
  }

  observeEvent(input$load_layer, {
    req(length(rv$tiles) > 0, nzchar(input$layer_name))

    layer_name <- input$layer_name
    scenario_prefix_value <- scenario_prefix()
    vector_url <- input$vector_url
    tiles <- rv$tiles
    data_type <- input$data_type

    rv$full_raster <- NULL
    rv$map_raster <- NULL
    rv$chart_values <- NULL
    rv$stats_vector <- NULL
    rv$vector_data <- NULL
    rv$loaded_layer_name <- NULL
    rv$loaded_data_type <- NULL
    rv$map_bounds <- NULL
    rv$pixel_text <- "Click the map to read a pixel value."

    leafletProxy("map") |>
      clearImages() |>
      clearShapes() |>
      clearControls() |>
      clearMarkers()

    later::later(function() {
      tryCatch(
        {
          load_selected_layer(layer_name, scenario_prefix_value, vector_url, tiles, data_type)
        },
        error = function(e) {
          safe_notify_error(conditionMessage(e))
        }
      )
    }, delay = 0.1)
  })

  # Initialize the leaflet widget once so zoom/pan state is not reset by rerenders.
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(preferCanvas = TRUE)) |>
      addProviderTiles(providers[[input$basemap_name]])
  })

  # Update the base tiles, overlays, legend, and layer control without rebuilding the map.
  observe({
    proxy <- leafletProxy("map")
    proxy <- proxy |>
      clearTiles() |>
      addProviderTiles(providers[[input$basemap_name]]) |>
      clearImages() |>
      clearShapes() |>
      clearControls()

    if (!is.null(rv$map_raster)) {
      map_raster <- rv$map_raster
      loaded_data_type <- if (is.null(rv$loaded_data_type)) "continuous" else rv$loaded_data_type

      if (loaded_data_type == "continuous") {
        map_range <- display_range(map_raster)
        palette <- colorNumeric(
          palette = viridisLite::viridis(256),
          domain = map_range,
          na.color = "transparent"
        )

        proxy <- proxy |>
          addRasterImage(
            map_raster,
            colors = palette,
            opacity = input$opacity,
            project = FALSE,
            maxBytes = 128 * 1024 * 1024,
            group = "Raster layer"
          ) |>
          addLegend(
            position = "bottomright",
            pal = palette,
            values = map_range,
            title = layer_label(rv$loaded_layer_name),
            opacity = input$opacity,
            group = "Raster layer"
          )
      } else {
        classes <- nominal_classes(map_raster)
        colors <- viridisLite::viridis(length(classes))
        palette <- colorFactor(colors, domain = classes, na.color = "transparent")

        proxy <- proxy |>
          addRasterImage(
            map_raster,
            colors = palette,
            opacity = input$opacity,
            project = FALSE,
            maxBytes = 128 * 1024 * 1024,
            group = "Raster layer"
          ) |>
          addLegend(
            position = "bottomright",
            colors = colors,
            labels = format_value(classes),
            title = layer_label(rv$loaded_layer_name),
            opacity = input$opacity,
            group = "Raster layer"
          )
      }
    }

    if (!is.null(rv$vector_data) && nrow(rv$vector_data)) {
      proxy <- add_vector_overlay(proxy, rv$vector_data)
    }

    overlay_groups <- character(0)
    if (!is.null(rv$map_raster)) {
      overlay_groups <- c(overlay_groups, "Raster layer")
    }
    if (!is.null(rv$vector_data) && nrow(rv$vector_data)) {
      overlay_groups <- c(overlay_groups, "Vector overlay")
    }

    if (length(overlay_groups)) {
      proxy |>
        addLayersControl(
          position = "topright",
          overlayGroups = overlay_groups,
          options = layersControlOptions(collapsed = FALSE)
        )
    }
  })

  # Only zoom to extent when a new layer is loaded, not on every redraw.
  observeEvent(rv$map_bounds, {
    req(rv$map_bounds)

    leafletProxy("map") |>
      fitBounds(
        rv$map_bounds$lng1,
        rv$map_bounds$lat1,
        rv$map_bounds$lng2,
        rv$map_bounds$lat2
      )
  }, ignoreInit = TRUE)

  # Clicking the map queries the full-resolution raster, not the downsampled preview.
  observeEvent(input$map_click, {
    req(rv$full_raster)

    click_vect <- terra::vect(
      data.frame(x = input$map_click$lng, y = input$map_click$lat),
      geom = c("x", "y"),
      crs = "EPSG:4326"
    )
    native_click <- terra::project(click_vect, terra::crs(rv$full_raster))
    extracted <- terra::extract(rv$full_raster, native_click)
    value <- if (ncol(extracted) >= 2) extracted[1, 2] else NA_real_

    rv$pixel_text <- paste0(
      "Longitude: ", format_value(input$map_click$lng), "\n",
      "Latitude: ", format_value(input$map_click$lat), "\n",
      "Value: ", format_value(value)
    )

    leafletProxy("map") |>
      clearMarkers() |>
      addCircleMarkers(
        lng = input$map_click$lng,
        lat = input$map_click$lat,
        radius = 5,
        stroke = FALSE,
        fillOpacity = 1,
        color = "#d94801",
        group = "Identify point"
      )
  })

  # Show the summary statistics in a simple two-column table.
  output$stats_table <- renderTable({
    req(rv$stats_vector)
    stats_vector <- rv$stats_vector

    data.frame(
      Statistic = names(stats_vector),
      Value = vapply(stats_vector, format_value, character(1)),
      row.names = NULL
    )
  })

  # Use a density plot for continuous rasters and a histogram for nominal rasters.
  output$value_plot <- renderPlot({
    req(rv$chart_values)
    chart_values <- rv$chart_values
    loaded_data_type <- if (is.null(rv$loaded_data_type)) "continuous" else rv$loaded_data_type

    if (loaded_data_type == "continuous") {
      stats <- rv$stats_vector
      stats_frame <- data.frame(
        statistic = factor(c("Min", "Mean", "Median", "Max"), levels = c("Min", "Mean", "Median", "Max")),
        value = unname(stats)
      )

      ggplot(data.frame(value = chart_values), aes(x = value)) +
        geom_density(fill = "#9ecae1", alpha = 0.5, color = "#2171b5") +
        geom_vline(
          data = stats_frame,
          aes(xintercept = value, color = statistic),
          linetype = "dashed",
          linewidth = 0.9,
          show.legend = TRUE
        ) +
        scale_color_manual(
          values = c(Min = "#1f78b4", Mean = "#33a02c", Median = "#ff7f00", Max = "#e31a1c")
        ) +
        labs(x = "Pixel value", y = "Density", color = NULL) +
        theme_minimal(base_size = 12)
    } else {
      ggplot(data.frame(value = factor(chart_values)), aes(x = value)) +
        geom_bar(fill = "#6baed6", color = "#2171b5") +
        labs(x = "Class value", y = "Count") +
        theme_minimal(base_size = 12)
    }
  })
}

shinyApp(ui, server)
