#library(shiny)
library(sf)
library(terra)
library(exactextractr)
library(dplyr)
library(tidyr)
library(stringr)
library(leaflet)
library(leaflet.extras)
library(DT)
library(geojsonsf)
library(jsonlite)
library(shinyjs)

# ── Data ─────────────────────────────────────────────────────────────────────

#setwd("~/GitHub/TribalStewardshipExplorer")

land_raster   <- rast("Land_All_Class.tif")
lf_raster     <- rast("Landform_All_Class.tif")
bodies        <- st_read("bodies_aoi.shp",  quiet = TRUE)
streams       <- st_read("streams_aoi.shp", quiet = TRUE)

LAND_CLASSES <- levels(land_raster)[[1]] %>%
  pull(class)

LANDFORM_CLASSES <- levels(lf_raster)[[1]] %>%
  pull(class)

STREAM_CLASSES <- sort(unique(streams$SrvyHbt))
WATER_CLASSES <- sort(unique(bodies$SrvyHbt))

ebci <- st_read("Boundary_EBCI.shp")
ebci <- st_transform(ebci, 4326)
aoi <- st_read("CLAE_AOI.shp")
aoi <- st_transform(aoi, 4326)

pixel_utility_norm  <- rast("pixel_utility_norm_agg3.tif")
habitat_value_norm  <- rast("habitat_value_norm_agg3.tif")
habitat_rarity_norm <- rast("habitat_rarity_norm_agg3.tif")
habitat_avail_norm  <- rast("habitat_avail_norm_agg3.tif")
proximity           <- rast("proximity_norm_agg3.tif")
connectivity        <- rast("connectivity_norm_agg3.tif")
climate_similarity  <- rast("climate_similarity_norm_agg3.tif")

pixel_utility_pct <- rast("pixel_utility_pct_agg3.tif")
habitat_value_pct <- rast("habitat_value_pct_agg3.tif")
habitat_rarity_pct <- rast("habitat_rarity_pct_agg3.tif")
habitat_avail_pct <- rast("habitat_avail_pct_agg3.tif")
proximity_pct <- rast("proximity_pct_agg3.tif")
connectivity_pct <- rast("connectivity_pct_agg3.tif")
climate_sim_pct <- rast("climate_sim_pct_agg3.tif")

AVG <- list(
  pixel_utility    = 0.4796,
  habitat_value    = 0.3664,
  habitat_rarity   = 0.2504,
  habitat_avail    = 0.6104,
  proximity        = 0.3788,
  connectivity     = 0.5187,
  climate_sim      = 0.5959
)

target_crs <- st_crs(land_raster)

# ── Helpers ───────────────────────────────────────────────────────────────────

extract_mean <- function(r, poly) {
  exact_extract(r, poly, "mean")[[1]]
}

extract_categorical <- function(r, poly) {
  
  vals <- terra::extract(
    r,
    terra::vect(poly)
  )[,2]
  
  vals <- vals[!is.na(vals)]
  
  tibble(class = vals) |>
    count(class, name = "count") |>
    mutate(
      `Percent Cover` = round(
        100 * count / sum(count),
        2
      )
    ) |>
    select(class, `Percent Cover`) |>
    arrange(desc(`Percent Cover`))
}

poly_to_target_crs <- function(poly) { 
  poly |> 
  st_union() |> 
  st_as_sf() |> 
  st_transform(target_crs) }

score_interpretation <- function(score, avg) {
  ifelse(score >= avg,
         "Above Average",
         "Below Average")
}

percentile_interpretation <- function(p) {
  
  pct <- round(100 * p)
  
  case_when(
    pct >= 90 ~ "Top 10%",
    pct >= 75 ~ "Top 25%",
    pct >= 50 ~ "Above Median",
    pct >= 25 ~ "Below Median",
    pct >= 10 ~ "Bottom 25%",
    TRUE      ~ "Bottom 10%"
  )
}

# ── UI ────────────────────────────────────────────────────────────────────────

ui <- fluidPage(
  
  useShinyjs(),
  
  tags$head(tags$style(HTML("
    html, body { height: 100%; margin: 0; }
    #map { height: calc(100vh - 20px) !important; }
    .sidebar-panel { padding: 10px; }
    .results-panel {
      position: absolute;
      top: 10px;
      right: 10px;
      z-index: 1000;
      background: white;
      border-radius: 6px;
      box-shadow: 0 2px 8px rgba(0,0,0,0.3);
      width: 420px;
      max-height: calc(100vh - 30px);
      overflow-y: auto;
      padding: 10px;
      display: none;
    }
    .results-panel.visible { display: block; }
    .sidebar {
      position: absolute;
      bottom: 20px;
      left: 10px;
      z-index: 1000;
      background: white;
      border-radius: 6px;
      box-shadow: 0 2px 8px rgba(0,0,0,0.3);
      width: 300px;
      padding: 12px;
    }
    .sidebar h4 { margin-top: 0; }
    .sidebar .form-group { margin-bottom: 8px; }
    .nav-tabs > li > a { font-size: 12px; padding: 5px 8px; }
  "))),
  
  div(style = "position: relative;",
      
      # Full-screen map
      leafletOutput("map", width = "100%", height = "100vh"),
      
      # Floating sidebar (top-left)
      div(class = "sidebar",
          h4("EBCI Co-Stewardship Explorer"),
          
          radioButtons("geom_source", "Input Method",
                       choices = c("Draw Polygon", "Upload Shapefile"),
                       selected = "Draw Polygon"
          ),
          
          conditionalPanel("input.geom_source === 'Upload Shapefile'",
                           fileInput("shp_zip", "Zipped shapefile (.zip)", accept = ".zip")
          ),
          
          hr(style = "margin: 8px 0;"),
          
          actionButton("run_report", "Generate Report",
                       class = "btn-primary",
                       style = "width:100%;"
          )
      ),
      
      # Floating results panel (top-right)
      div(id = "results_panel", class = "results-panel",
          tabsetPanel(
            tabPanel("Objectives",   DTOutput("objective_tbl")),
            tabPanel("Land Cover",   DTOutput("land_tbl")),
            tabPanel("Landforms",    DTOutput("landform_tbl")),
            tabPanel("Streams",      DTOutput("stream_tbl")),
            tabPanel("Waterbodies",  DTOutput("water_tbl"))
          )
      )
      
  ),
  
  # Learn More Button
  absolutePanel(
    bottom = 40, right = -50, width = 220,
    style = "z-index: 1000;",
    actionButton("learn_more", "Learn More")
  ),
  
  # Info Panel
  conditionalPanel(
    condition = "input.learn_more % 2 == 1",
    
    absolutePanel(
      bottom = 80, right = -50, width = 320,
      style = "
        background-color: white;
        padding: 12px;
        z-index: 1000;
        max-height: 80vh;
        overflow-y: auto;
      ",
      
      h4("About this tool"),
      p("The purpose of this tool is to summarize tribal objectives and habitat distributions for prospective areas the tribe may want to co-steward."),
      
      p("Draw or upload an area of interest to understand how well it aligns with tribal objectives."),
      
      tags$hr(),
      
      p(
        tags$b("Author: "),
        tags$a(
          href = "https://savannahswinea.github.io",
          "Savannah Swinea",
          target = "_blank"
        )
      ),
      
      p(
        "Source code available on ",
        tags$a(
          href = "https://github.com/savannahswinea/TribalStewardshipExplorer",
          "GitHub",
          target = "_blank"
        )
      )
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {
  
  options(shiny.error = browser)
  
  # ── Map ────────────────────────────────────────────────────────────────────
  
  output$map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$Esri.WorldImagery) |>
      addPolygons(
        data = aoi,
        color = "cyan",
        weight = 2,
        fillOpacity = 0.15,
        group = "Priority Area"
      ) |>
      addPolygons(
        data = ebci,
        color = "red",
        weight = 3,
        fill = FALSE,
        group = "Tribal Boundary"
      ) |>
      addSearchOSM(
        options = searchOptions(
          collapsed = FALSE,
          autoCollapse = FALSE,
          zoom = 15
        )
      ) |>
      addDrawToolbar(
        targetGroup = "drawn",
        position = "topright",
        polygonOptions = drawPolygonOptions(),
        editOptions    = editToolbarOptions(),
        # hide tools that aren't polygon drawing
        markerOptions     = FALSE,
        circleOptions     = FALSE,
        rectangleOptions  = FALSE,
        polylineOptions   = FALSE,
        circleMarkerOptions = FALSE
      ) |>
      fitBounds(
        lng1 = -84.1,
        lat1 = 35.2,
        lng2 = -83.0,
        lat2 = 35.8
      )
  })
  
  # ── Drawn polygon ─────────────────────────────────────────────────────────
  
  drawn_poly <- reactive({
    req(input$map_draw_new_feature)
    geojson_sf(toJSON(input$map_draw_new_feature, auto_unbox = TRUE)) |>
      poly_to_target_crs()
  })
  
  # ── Uploaded shapefile ────────────────────────────────────────────────────
  
  uploaded_poly <- reactive({
    
    req(input$shp_zip)
    
    td <- tempfile()
    dir.create(td)
    
    unzip(input$shp_zip$datapath, exdir = td)
    
    print(list.files(td, recursive = TRUE))
    
    shp <- list.files(
      td,
      pattern = "\\.shp$",
      full.names = TRUE,
      recursive = TRUE
    )
    
    print(shp)
    
    if (length(shp) == 0) {
      stop("No .shp file found in uploaded zip")
    }
    
    st_read(shp[1], quiet = TRUE) |>
      poly_to_target_crs()
  })
  
  # Render shapefile on map and zoom to it
  observeEvent(uploaded_poly(), {
    poly_wgs <- st_transform(uploaded_poly(), 4326)
    bb       <- st_bbox(poly_wgs)
    
    leafletProxy("map") |>
      clearGroup("uploaded") |>
      addPolygons(
        data   = poly_wgs,
        group  = "uploaded",
        color  = "#00BFFF",
        weight = 2,
        fillOpacity = 0.15
      ) |>
      fitBounds(
        lng1 = bb[["xmin"]], lat1 = bb[["ymin"]],
        lng2 = bb[["xmax"]], lat2 = bb[["ymax"]]
      )
  })
  
  # ── Active polygon ────────────────────────────────────────────────────────
  
  active_poly <- reactive({
    if (input$geom_source == "Draw Polygon") {
      req(input$map_draw_new_feature)
      drawn_poly()
    } else {
      req(input$shp_zip)
      uploaded_poly()
    }
  })
  
  # ── Analysis ──────────────────────────────────────────────────────────────
  
  results <- eventReactive(input$run_report, {
    
    poly <- tryCatch(active_poly(), error = function(e) NULL)
    if (is.null(poly)) {
      showNotification(
        "Please draw or upload a polygon first.",
        type = "error")
      return(NULL)
      }
    
    withProgress(message = "Generating report...", value = 0, {
      
      incProgress(0.05,  detail = "Pixel utility")
      pu   <- extract_mean(pixel_utility_norm,  poly)
      pu_pct   <- extract_mean(pixel_utility_pct, poly)
      
      incProgress(0.15,  detail = "Habitat value")
      hv   <- extract_mean(habitat_value_norm,  poly)
      hv_pct   <- extract_mean(habitat_value_pct, poly)
      
      incProgress(0.25,  detail = "Habitat rarity")
      hr   <- extract_mean(habitat_rarity_norm, poly)
      hr_pct   <- extract_mean(habitat_rarity_pct, poly)
      
      incProgress(0.35,  detail = "Habitat availability")
      ha   <- extract_mean(habitat_avail_norm, poly)
      ha_pct   <- extract_mean(habitat_avail_pct, poly)
      
      incProgress(0.45,  detail = "Proximity")
      prox <- extract_mean(proximity, poly)
      prox_pct <- extract_mean(proximity_pct, poly)
      
      incProgress(0.55,  detail = "Connectivity")
      conn <- extract_mean(connectivity, poly)
      conn_pct <- extract_mean(connectivity_pct, poly)
      
      incProgress(0.65,  detail = "Climate similarity")
      clim <- extract_mean(climate_similarity, poly)
      clim_pct <- extract_mean(climate_sim_pct, poly)
      
      incProgress(0.75,  detail = "Land cover")
      land_sum <- extract_categorical(land_raster, poly) |>
        mutate(
          class = recode(
            as.character(class),
            "Other" = "Other (e.g., developed, agricultural)"
          )
        ) |>
        complete(
          class = LAND_CLASSES,
          fill = list(`Percent Cover` = 0)
        ) |>
        arrange(desc(`Percent Cover`))
      
      incProgress(0.85,  detail = "Landforms")
      lf_sum   <- extract_categorical(lf_raster, poly) |>
        complete(
          class = LANDFORM_CLASSES,
          fill = list(`Percent Cover` = 0)
        )
      
      incProgress(0.92,  detail = "Streams")
      stream_sum <- st_intersection(streams, poly) |>
        st_drop_geometry() |>
        group_by(Habitat = SrvyHbt) |>
        summarise(
          `Total Length (km)` = round(sum(LENGTHK), 2),
          Count = n(),
          .groups = "drop"
        ) |>
        complete(
          Habitat = STREAM_CLASSES,
          fill = list(
            `Total Length (km)` = 0,
            Count = 0
          )
        )
      
      incProgress(0.97,  detail = "Waterbodies")
      aoi_area_sqkm <- as.numeric(st_area(poly)) / 1e6
      water_sum <- st_intersection(bodies, poly) |>
        st_drop_geometry() |>
        group_by(Habitat = SrvyHbt) |>
        summarise(
          `Total Area (km²)` = round(sum(AREASQK), 2),
          Count = n(),
          .groups = "drop"
        ) |>
        mutate(
          `Percent Cover` = round(
            100 * `Total Area (km²)` / aoi_area_sqkm,
            2
          )
        ) |>
        complete(
          Habitat = WATER_CLASSES,
          fill = list(
            `Total Area (km²)` = 0,
            `Percent Cover` = 0,
            Count = 0
          )
        ) |>
        arrange(desc(`Percent Cover`))
      
      scores <- c(
        pu, hv, hr, ha,
        prox, conn, clim
      )
      
      percentiles <- c(
        pu_pct, hv_pct, hr_pct, ha_pct,
        prox_pct, conn_pct, clim_pct
      )
      
      objectives <- tibble(
        Objective = c(
          "Pixel Utility",
          "Habitat Value",
          "Habitat Rarity",
          "Habitat Availability",
          "Proximity",
          "Connectivity",
          "Climate Similarity"
        ),
        `Score (0-1)` = round(scores, 3),
        Comparison = mapply(
          score_interpretation,
          scores,
          unlist(AVG)
        ),
        Percentile = sapply(
          percentiles,
          percentile_interpretation
        )
      )
      
      print("LAND COVER")
      print(land_sum)
      
      print("LANDFORM")
      print(lf_sum)
      
      print("STREAMS")
      print(stream_sum)
      
      print("WATER")
      print(water_sum)
      
      list(
        objectives   = objectives,
        land_sum     = land_sum,
        lf_sum       = lf_sum,
        stream_sum   = stream_sum,
        water_sum    = water_sum
      )
    })
  })
  
  # Show results panel when results are ready
  observeEvent(results(), {
    shinyjs::runjs("document.getElementById('results_panel').classList.add('visible');")
  })
  
  # ── Table outputs ─────────────────────────────────────────────────────────
  
  dt_opts <- list(pageLength = 10, scrollX = TRUE, dom = "tp")
  
  output$objective_tbl <- renderDT(results()$objectives,    options = dt_opts, rownames = FALSE)
  output$land_tbl      <- renderDT(results()$land_sum,      options = dt_opts, rownames = FALSE)
  output$landform_tbl  <- renderDT(results()$lf_sum,        options = dt_opts, rownames = FALSE)
  output$stream_tbl    <- renderDT(results()$stream_sum,    options = dt_opts, rownames = FALSE)
  output$water_tbl     <- renderDT(results()$water_sum,     options = dt_opts, rownames = FALSE)
}

shinyApp(ui, server)

# For deploying
#setwd("~/GitHub/TribalStewardshipExplorer")
#rsconnect::deployApp()
