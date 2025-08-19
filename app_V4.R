library(shiny)
library(dplyr)
library(tidyr)
library(dbplyr)
library(RPostgres)
library(duckdb)
library(DT)
library(leaflet)
library(leaflet.extras)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(obisindicators)
library(measoshapes)
library(ggplot2)
library(markdown)
library(shinyWidgets)
library(bslib)
library(h3)


# Load spatial data for the map
SO_wkt_string <- "POLYGON((180 -44.3,173 -44.3,173 -47.5,170 -47.5,157 -47.5,157 -45.9,150 -45.9,150 -47.5,143 -47.5,143 -45.8,140 -45.8,140 -44.5,137 -44.5,137 -43,135 -43,135 -41.7,131 -41.7,131 -40.1,115 -40.1,92 -40.1,92 -41.4,78 -41.4,78 -42.3,69 -42.3,69 -43.3,47 -43.3,47 -41.7,30 -41.7,12 -41.7,12 -40.3,10 -40.3,10 -38.3,-5 -38.3,-5 -38.9,-9 -38.9,-9 -40.2,-13 -40.2,-13 -41.4,-21 -41.4,-21 -42.5,-39 -42.5,-39 -40.7,-49 -40.7,-49 -48.6,-54 -48.6,-54 -55.7,-62.7972582608082 -55.7,-64 -55.7,-64 -57.8,-71 -57.8,-71 -58.9,-80 -58.9,-80 -40,-125 -40,-167 -40,-167 -42.6,-171 -42.6,-171 -44.3,-180 -44.3,-180 -90, 0 -90,180 -90,180 -44.3))"
SO_sf <- st_as_sfc(SO_wkt_string, crs = 4326) %>%
  st_sf() %>%
  mutate(name = "Southern Ocean")

# Load land data
land <- ne_countries(scale = "medium", returnclass = "sf")

measo_names <- tibble(
  name = c("AOA", "AON", "AOS", "AOT", "CIA", "CIN", "CIS", "CIT", "EIA", "EIN", "EIS", "EIT",
           "EPA", "EPN", "EPS", "EPT", "WPA", "WPN", "WPS", "WPT"),
  sector = c("Atlantic", "Atlantic", "Atlantic", "Atlantic", "CentralIndian", "CentralIndian", "CentralIndian", "CentralIndian", "EastIndian", "EastIndian", "EastIndian", "EastIndian",
             "EastPacific", "EastPacific", "EastPacific", "EastPacific", "WestPacific", "WestPacific", "WestPacific", "WestPacific"),
  zone = c("Antarctic", "Northern", "Subantarctic", "Undefined", "Antarctic", "Northern", "Subantarctic", "Undefined", "Antarctic", "Northern", "Subantarctic", "Undefined",
           "Antarctic", "Northern", "Subantarctic", "Undefined", "Antarctic", "Northern", "Subantarctic", "Undefined"),
  fill = c("#BCECFE", "#054e70", "#1094AF", "Undefined", "#FFFFFF", "#4B7D7E", "#5F9EA0", "Undefined", "#EAFAFF", "#016074", "#00AFD5", "Undefined",
           "#FFFFFF", "#4B7D7E", "#5F9EA0", "Undefined", "#016074", "#054e70", "#1094AF", "Undefined")
)

# Define UI for application
modern_theme <- bs_theme(
  version = 5,                     # Bootstrap 5
  bootswatch = "cosmo",            # Cosmo gives a clean, modern look
  primary = "#0077b6",             # Deep ocean blue
  secondary = "#00b4d8",           # Lighter accent blue
  base_font = font_google("Roboto"),
  heading_font = font_google("Poppins"),
  code_font = font_google("Fira Code")
)

ui <- navbarPage(
  tagList(
    tags$head(
      tags$style(HTML("
      .navbar-brand {
        display: flex;
        align-items: center;
        justify-content: space-between;
        font-weight: 700;
        font-size: 1.5em;
        background: linear-gradient(90deg, #0077b6, #00b4d8);
        -webkit-background-clip: text;
        -webkit-text-fill-color: transparent;
      }
      .fish-image {
        height: 55px;
        margin-left: 12px;
        filter: drop-shadow(0 0 4px rgba(0, 180, 216, 0.6));
        animation: bob 3s ease-in-out infinite;
      }
      @keyframes bob {
        0%, 100% { transform: translateY(0px); }
        50% { transform: translateY(-3px); }
      }
      @media (max-width: 1000px) {
        .fish-image {
          height: 45px;
          margin-left: 8px;
        }
        .navbar-brand {
          font-size: 1.2em;
        }
      }
    "))
    ),
    span("SOfish Diversity"),
    tags$img(
      src = "https://www.seafoodwatch.org/globalassets/sfw-data-blocks/species/toothfish/chilean-seabass.png",
      # src = "https://www.fishbase.org/images/species/Dicap_u0.gif",
      class = "fish-image"
    )
  ),
  theme = modern_theme,
  
  # --- Display Tab ---
  tabPanel(
    icon = icon("map-marked-alt"), "Display",
    leafletOutput("spatial_plot", height = "85vh")
  ),
  
  # --- Filters Tab ---
  tabPanel(
    icon = icon("filter"), "Filters",
    fluidRow(
      column(
        4,
        h4("Filter by Taxonomy", class = "text-primary"),
        searchInput("param_taxon", "Taxon Search", placeholder = "e.g. Stomiiformes", value = "Stomiiformes", btnSearch = icon("search")),
        numericInput("param_aphia_id", "Aphia ID", value = NA),
        numericInput("param_fishbase_id", "FishBase ID", value = NA),
        numericInput("param_gbif_id", "GBIF ID", value = NA),
        hr(),
        h4("Display Options", class = "text-primary"),
        radioGroupButtons("color_by", "Color points by:",
                          choices = c("Family", "Genus", "Species", "iucn_status", "Vulnerability"),
                          justified = TRUE, status = "primary"),
        radioGroupButtons("size_by", "Size points by:",
                          choices = c("Vulnerability", "Length_cm"),
                          justified = TRUE, status = "primary"),
        hr(),
        tags$b("Interactive Map Controls:"),
        p("Use the drawing tools on the map to filter points by a custom polygon."),
        actionBttn("clear_polygon", "Clear Drawn Polygon", style = "fill", color = "warning"),
        hr(),
        actionBttn("reset_button", "Reset All Filters", style = "fill", color = "danger"),
        hr(),
        verbatimTextOutput("record_count")
      ),
      column(
        4,
        h4("Filter by Traits", class = "text-primary"),
        sliderInput("param_length_cm", "Length (cm)", min = 0, max = 1000, value = c(0, 1000)),
        sliderInput("param_weight", "Weight (g)", min = 0, max = 2500000, value = c(0, 2500000), round = TRUE),
        pickerInput("param_AnaCat", "Migration strategy", choices = c("All" = ""), options = list(`live-search` = TRUE)),
        pickerInput("param_DemersPelag", "Watercolumn niche", choices = c("All" = ""), options = list(`live-search` = TRUE)),
        sliderInput("param_shallow_depth", "Shallow Depth (m)", min = 0, max = 8000, value = c(0, 8000)),
        sliderInput("param_deep_depth", "Deep Depth (m)", min = 0, max = 8000, value = c(0, 8000))
      ),
      column(
        4,
        h4("More Filters", class = "text-primary"),
        pickerInput("param_iucn", "IUCN Status", choices = c("All" = "")),
        pickerInput("param_importance", "Commercial Importance", choices = c("All" = "")),
        pickerInput("param_price", "Price Category", choices = c("All" = "")),
        pickerInput("param_catching_method", "Catching Method", choices = c("All" = "")),
        sliderInput("param_com_shallow_depth", "Commercial Shallow Depth (m)", min = 0, max = 8000, value = c(0, 8000)),
        sliderInput("param_com_deep_depth", "Commercial Deep Depth (m)", min = 0, max = 8000, value = c(0, 8000))
      )
    )
  ),
  
  # --- Table Tab ---
  tabPanel(
    icon = icon("table"), "Table",
    DTOutput("occurrence_table")
  ),
  
  # --- Spatial Diversity Tab ---
  tabPanel(
    icon = icon("globe"), "Spatial Diversity",
    h3("Diversity Metrics for SO Filtered Data"),
    p("Calculated using filtered species occurrences (drawn polygon and filters tab)"),
    DTOutput("diversity_table"),
    hr(),
    h3("Diversity Metrics by MEASO Region"),
    p("Diversity metrics for each MEASO region, calculated from the currently filtered data (excluding the drawn polygon filter)."),
    DTOutput("measo_diversity_table")
  ),
  
  # --- Temporal Diversity Tab ---
  tabPanel(
    icon = icon("clock"), "Temporal Diversity",
    sidebarLayout(
      sidebarPanel(
        h4("Temporal Diversity Controls", class = "text-primary"),
        pickerInput("temporal_metric", "Select Diversity Metric",
                    choices = c("Shannon Index" = "shannon",
                                "Simpson Index" = "simpson",
                                "Number of Species" = "sp",
                                "Effective Species Number (ES50)" = "es"),
                    selected = "shannon_ma"),
        pickerInput("temporal_area", "Select Geographic Area",
                    choices = NULL),
        sliderInput("temporal_window", "Moving Average Window Size (years)",
                    min = 1, max = 25, value = 5, step = 1),
        hr(),
        p(strong("Note:"), "The moving average is only calculated for the years where a full window of data is available.")
      ),
      mainPanel(
        h3("Temporal diversity: Moving Average"),
        plotly::plotlyOutput("temporal_diversity_plot"),
        hr(),
        DTOutput("temporal_diversity_table")
      )
    )
  ),
  # --- H3 Diversity Polygons Tab ---
  tabPanel(
    icon = icon("th"),"Spatial H3 Polygons",
    sidebarLayout(
      sidebarPanel(
        h4("H3 Polygon Controls"),
        selectInput("h3_metric", "Select Metric", 
                    choices = c("Species richness" = "richness",
                                "Shannon Diversity" = "shannon_diversity",
                                "Simpson Diversity" = "simpson_diversity",
                                "ES50 (Hulbert's index)" = "ES",
                                "Dominance (Max P)" = "maxp",
                                "Hill1 (exp(shannon))" = "hill_1",
                                "Hill2 (1/simpson)" = "hill_2",
                                "Hill3 (Hill3 (1/maxp)" = "hill_inf"),
                    selected = "richness"),
        sliderInput("h3_resolution", "Resolution (0-15)", min = 0, max = 15, value = 3, step = 1),
        actionButton("calculate_h3_polygons", "Calculate H3 Polygons"),
        actionButton("clear_h3_polygons", "Clear H3 Polygons"),
        hr(),
        p("Click 'Calculate H3 Polygons' to visualize diversity metrics for the currently filtered data within hexagonal grid cells.")
      ),
      mainPanel(
        leafletOutput("h3_plot", height = "85vh")
      )
    )
  ),
  
  # --- About Tab ---
  tabPanel(
    icon = icon("info-circle"), "About",
    includeMarkdown("about.md")
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Set up database connection and disconnect on app exit
  duckdb_file_path <- "TraitMapper_occurrences_v2.duckdb"
  table_name <- "occurrences_with_traits"
  
  con <- dbConnect(duckdb::duckdb(), dbdir = duckdb_file_path, read_only = TRUE)
  message("Connected to DuckDB.")
  
  dbExecute(con, "INSTALL spatial;")
  dbExecute(con, "LOAD spatial;")
  message("DuckDB spatial extension loaded.")
  
  onStop(function() {
    dbDisconnect(con, shutdown = TRUE)
    message("Disconnected from DuckDB.")
  })
  
  initial_slider_ranges <- reactiveValues(
    length = NULL,
    weight = NULL,
    vulnerability = NULL,
    shallow_depth = NULL,
    deep_depth = NULL,
    com_deep_depth = NULL,
    com_shallow_depth = NULL
  )
  
  # Make queries to DuckDB to fetch data
  observeEvent(TRUE, {
    message("--- Initializing filters and slider ranges ---")
    
    ana_cat_choices <- dbGetQuery(con, "SELECT DISTINCT AnaCat FROM occurrences_with_traits WHERE AnaCat IS NOT NULL")
    updatePickerInput(session, "param_AnaCat", choices = c("All" = "", sort(ana_cat_choices$AnaCat)))
    
    dem_pel_choices <- dbGetQuery(con, "SELECT DISTINCT DemersPelag FROM occurrences_with_traits WHERE DemersPelag IS NOT NULL")
    updatePickerInput(session, "param_DemersPelag", choices = c("All" = "", sort(dem_pel_choices$DemersPelag)))
    
    iucn_choices <- dbGetQuery(con, "SELECT DISTINCT iucn_status FROM occurrences_with_traits WHERE iucn_status IS NOT NULL")
    updatePickerInput(session, "param_iucn", choices = c("All" = "", sort(iucn_choices$iucn_status)))
    
    importance_choices <- dbGetQuery(con, "SELECT DISTINCT Importance FROM occurrences_with_traits WHERE Importance IS NOT NULL")
    updatePickerInput(session, "param_importance", choices = c("All" = "", sort(importance_choices$Importance)))
    
    price_choices <- dbGetQuery(con, "SELECT DISTINCT Price FROM occurrences_with_traits WHERE Price IS NOT NULL")
    updatePickerInput(session, "param_price", choices = c("All" = "", sort(price_choices$Price)))
    
    catching_method_choices <- dbGetQuery(con, "SELECT DISTINCT Catchingmethod FROM occurrences_with_traits WHERE Catchingmethod IS NOT NULL")
    updatePickerInput(session, "param_catching_method", choices = c("All" = "", sort(catching_method_choices$Catchingmethod)))
    
    length_range_raw <- dbGetQuery(con, "SELECT MIN(Length_cm) AS min_val, MAX(Length_cm) AS max_val FROM occurrences_with_traits WHERE Length_cm IS NOT NULL")
    initial_slider_ranges$length <- c(floor(length_range_raw$min_val), ceiling(length_range_raw$max_val))
    updateSliderInput(session, "param_length_cm",
                      min = initial_slider_ranges$length[1],
                      max = initial_slider_ranges$length[2],
                      value = initial_slider_ranges$length)
    
    # Initialize Weight slider range
    weight_range_raw <- dbGetQuery(con, "SELECT MIN(Weight) AS min_val, MAX(Weight) AS max_val FROM occurrences_with_traits WHERE Weight IS NOT NULL")
    initial_slider_ranges$weight <- c(floor(weight_range_raw$min_val), ceiling(weight_range_raw$max_val))
    updateSliderInput(session, "param_weight",
                      min = initial_slider_ranges$weight[1],
                      max = initial_slider_ranges$weight[2],
                      value = initial_slider_ranges$weight)
    
    vulnerability_range_raw <- dbGetQuery(con, "SELECT MIN(Vulnerability) AS min_val, MAX(Vulnerability) AS max_val FROM occurrences_with_traits WHERE Vulnerability IS NOT NULL")
    initial_slider_ranges$vulnerability <- c(floor(vulnerability_range_raw$min_val), ceiling(vulnerability_range_raw$max_val))
    updateSliderInput(session, "param_vulnerability",
                      min = initial_slider_ranges$vulnerability[1],
                      max = initial_slider_ranges$vulnerability[2],
                      value = initial_slider_ranges$vulnerability)
    
    shallow_depth_range_raw <- dbGetQuery(con, "SELECT MIN(DepthRangeShallow) AS min_val, MAX(DepthRangeShallow) AS max_val FROM occurrences_with_traits WHERE DepthRangeShallow IS NOT NULL")
    initial_slider_ranges$shallow_depth <- c(floor(shallow_depth_range_raw$min_val), ceiling(shallow_depth_range_raw$max_val))
    updateSliderInput(session, "param_shallow_depth",
                      min = initial_slider_ranges$shallow_depth[1],
                      max = initial_slider_ranges$shallow_depth[2],
                      value = initial_slider_ranges$shallow_depth)
    
    deep_depth_range_raw <- dbGetQuery(con, "SELECT MIN(DepthRangeDeep) AS min_val, MAX(DepthRangeDeep) AS max_val FROM occurrences_with_traits WHERE DepthRangeDeep IS NOT NULL")
    initial_slider_ranges$deep_depth <- c(floor(deep_depth_range_raw$min_val), ceiling(deep_depth_range_raw$max_val))
    updateSliderInput(session, "param_deep_depth",
                      min = initial_slider_ranges$deep_depth[1],
                      max = initial_slider_ranges$deep_depth[2],
                      value = initial_slider_ranges$deep_depth)
    
    com_deep_depth_range_raw <- dbGetQuery(con, "SELECT MIN(DepthRangeComDeep) AS min_val, MAX(DepthRangeComDeep) AS max_val FROM occurrences_with_traits WHERE DepthRangeComDeep IS NOT NULL")
    initial_slider_ranges$com_deep_depth <- c(floor(com_deep_depth_range_raw$min_val), ceiling(com_deep_depth_range_raw$max_val))
    updateSliderInput(session, "param_com_deep_depth",
                      min = initial_slider_ranges$com_deep_depth[1],
                      max = initial_slider_ranges$com_deep_depth[2],
                      value = initial_slider_ranges$com_deep_depth)
    
    com_shallow_depth_range_raw <- dbGetQuery(con, "SELECT MIN(DepthRangeComShallow) AS min_val, MAX(DepthRangeComShallow) AS max_val FROM occurrences_with_traits WHERE DepthRangeComShallow IS NOT NULL")
    initial_slider_ranges$com_shallow_depth <- c(floor(com_shallow_depth_range_raw$min_val), ceiling(com_shallow_depth_range_raw$max_val))
    updateSliderInput(session, "param_com_shallow_depth",
                      min = initial_slider_ranges$com_shallow_depth[1],
                      max = initial_slider_ranges$com_shallow_depth[2],
                      value = initial_slider_ranges$com_shallow_depth)
    
    message("--- Initial filter setup complete ---")
    
  }, once = TRUE, ignoreNULL = FALSE)
  
  # Filtering options
  # Coding the drawn polygon
  drawn_polygon_wkt <- reactiveVal(NULL)
  
  observeEvent(input$spatial_plot_draw_new_feature, {
    feature <- input$spatial_plot_draw_new_feature
    if (feature$properties$feature_type == "polygon") {
      lon_lat_coords <- feature$geometry$coordinates[[1]]
      wkt_coords <- paste(sapply(lon_lat_coords, function(x) paste(x[1], x[2])), collapse = ",")
      polygon_wkt_str <- paste0("POLYGON((", wkt_coords, "))")
      
      drawn_polygon_wkt(polygon_wkt_str)
      message("Polygon drawn and WKT stored: ", polygon_wkt_str)
    }
  })
  
  observeEvent(input$clear_polygon, {
    drawn_polygon_wkt(NULL)
    leafletProxy("spatial_plot") %>% clearGroup("drawn_polygon")
    message("Drawn polygon cleared from map and filter.")
  })
  
  # Filter options
  build_where_clauses <- function(include_spatial_filter = TRUE) {
    message("Current taxon value in where clause: [", input$param_taxon, "]")
    where_clauses <- c()
    if (nzchar(input$param_taxon) > 0) {
      search_term_lower <- tolower(input$param_taxon)
      quoted_like_pattern <- DBI::dbQuoteLiteral(con, paste0('%', search_term_lower, '%'))
      quoted_exact_term <- DBI::dbQuoteLiteral(con, search_term_lower)
      
      q_scientificName <- DBI::dbQuoteIdentifier(con, "scientificName")
      q_Species <- DBI::dbQuoteIdentifier(con, "Species")
      q_Genus <- DBI::dbQuoteIdentifier(con, "Genus")
      q_Family <- DBI::dbQuoteIdentifier(con, "Family")
      q_Order <- DBI::dbQuoteIdentifier(con, "Order")
      q_Class <- DBI::dbQuoteIdentifier(con, "Class")
      
      clauses_list <- c(
        paste0("LOWER(", q_scientificName, ") LIKE ", quoted_like_pattern),
        paste0("LOWER(", q_Species, ") LIKE ", quoted_like_pattern),
        paste0("LOWER(", q_Genus, ") = ", quoted_exact_term),
        paste0("LOWER(", q_Family, ") = ", quoted_exact_term),
        paste0("LOWER(", q_Order, ") = ", quoted_exact_term),
        paste0("LOWER(", q_Class, ") = ", quoted_exact_term)
      )
      clause <- paste0("(", paste(clauses_list, collapse = " OR "), ")")
      where_clauses <- c(where_clauses, clause)
    }
    
    if (!is.null(input$param_aphia_id) && !is.na(input$param_aphia_id)) {
      where_clauses <- c(where_clauses, paste0("AphiaID = ", as.integer(input$param_aphia_id)))
    }
    if (!is.null(input$param_fishbase_id) && !is.na(input$param_fishbase_id)) {
      where_clauses <- c(where_clauses, paste0("FishBaseID = ", as.integer(input$param_fishbase_id)))
    }
    if (!is.null(input$param_gbif_id) && !is.na(input$param_gbif_id)) {
      where_clauses <- c(where_clauses, paste0("gbifID = ", as.integer(input$param_gbif_id)))
    }
    
    handle_nulls_in_slider_filter <- function(input_param_name, db_col_name, initial_range) {
      if (!is.null(input[[input_param_name]]) && length(input[[input_param_name]]) == 2 && !any(is.na(input[[input_param_name]]))) {
        if (all(input[[input_param_name]] == initial_range)) {
          return(paste0("(", db_col_name, " BETWEEN ", input[[input_param_name]][1], " AND ", input[[input_param_name]][2], " OR ", db_col_name, " IS NULL)"))
        } else {
          return(paste0(db_col_name, " BETWEEN ", input[[input_param_name]][1], " AND ", input[[input_param_name]][2]))
        }
      }
      return(NULL)
    }
    
    if (!is.null(clause <- handle_nulls_in_slider_filter("param_length_cm", "Length_cm", initial_slider_ranges$length))) {
      where_clauses <- c(where_clauses, clause)
    }
    # Add Weight filter using "Weight" column
    if (!is.null(clause <- handle_nulls_in_slider_filter("param_weight", "Weight", initial_slider_ranges$weight))) {
      where_clauses <- c(where_clauses, clause)
    }
    if (!is.null(clause <- handle_nulls_in_slider_filter("param_vulnerability", "Vulnerability", initial_slider_ranges$vulnerability))) {
      where_clauses <- c(where_clauses, clause)
    }
    if (!is.null(clause <- handle_nulls_in_slider_filter("param_shallow_depth", "DepthRangeShallow", initial_slider_ranges$shallow_depth))) {
      where_clauses <- c(where_clauses, clause)
    }
    if (!is.null(clause <- handle_nulls_in_slider_filter("param_deep_depth", "DepthRangeDeep", initial_slider_ranges$deep_depth))) {
      where_clauses <- c(where_clauses, clause)
    }
    if (!is.null(clause <- handle_nulls_in_slider_filter("param_com_deep_depth", "DepthRangeComDeep", initial_slider_ranges$com_deep_depth))) {
      where_clauses <- c(where_clauses, clause)
    }
    if (!is.null(clause <- handle_nulls_in_slider_filter("param_com_shallow_depth", "DepthRangeComShallow", initial_slider_ranges$com_shallow_depth))) {
      where_clauses <- c(where_clauses, clause)
    }
    
    if (input$param_AnaCat != "") {
      where_clauses <- c(where_clauses, paste0("AnaCat = ", DBI::dbQuoteLiteral(con, input$param_AnaCat)))
    }
    if (input$param_DemersPelag != "") {
      where_clauses <- c(where_clauses, paste0("DemersPelag = ", DBI::dbQuoteLiteral(con, input$param_DemersPelag)))
    }
    if (input$param_iucn != "") {
      where_clauses <- c(where_clauses, paste0("iucn_status = ", DBI::dbQuoteLiteral(con, input$param_iucn)))
    }
    if (input$param_importance != "") {
      where_clauses <- c(where_clauses, paste0("Importance = ", DBI::dbQuoteLiteral(con, input$param_importance)))
    }
    if (input$param_price != "") {
      where_clauses <- c(where_clauses, paste0("Price = ", DBI::dbQuoteLiteral(con, input$param_price)))
    }
    if (input$param_catching_method != "") {
      where_clauses <- c(where_clauses, paste0("Catchingmethod = ", DBI::dbQuoteLiteral(con, input$param_catching_method)))
    }
    
    if (include_spatial_filter) {
      current_polygon_wkt <- drawn_polygon_wkt()
      if (!is.null(current_polygon_wkt)) {
        where_clauses <- c(where_clauses, "decimalLongitude IS NOT NULL", "decimalLatitude IS NOT NULL")
        quoted_polygon_wkt <- DBI::dbQuoteLiteral(con, current_polygon_wkt)
        spatial_clause <- paste0("ST_Within(ST_Point(decimalLongitude, decimalLatitude), ST_GeomFromText(", quoted_polygon_wkt, "))")
        where_clauses <- c(where_clauses, spatial_clause)
        message("Added spatial filter to SQL query.")
      }
    }
    return(where_clauses)
  }
  
  # Filtering the data and create data for further processing
  filtered_data_from_db <- reactive({
    req(
      initial_slider_ranges$length, initial_slider_ranges$vulnerability,
      initial_slider_ranges$shallow_depth, initial_slider_ranges$deep_depth,
      initial_slider_ranges$com_deep_depth, initial_slider_ranges$com_shallow_depth,
      initial_slider_ranges$weight
    )
    
    # These lines are important to make sure the reactive expression re-runs when inputs change
    input$param_taxon; input$param_aphia_id; input$param_fishbase_id; input$param_gbif_id
    input$param_length_cm; input$param_weight; input$param_vulnerability; input$param_AnaCat; input$param_DemersPelag;
    input$param_iucn; input$param_shallow_depth; input$param_deep_depth;
    input$param_com_deep_depth; input$param_com_shallow_depth; input$param_importance;
    input$param_price; input$param_catching_method; input$color_by; input$size_by
    drawn_polygon_wkt() # Dependency on drawn polygon
    
    message("--- Running filtered_data_from_db reactive (with drawn polygon) ---")
    where_clauses <- build_where_clauses(include_spatial_filter = TRUE)
    
    query <- paste("SELECT * FROM", table_name)
    if (length(where_clauses) > 0) {
      query <- paste(query, "WHERE", paste(where_clauses, collapse = " AND "))
    }
    
    message("Final SQL Query for map/table:\n", query)
    df <- dbGetQuery(con, query)
    
    df <- df %>%
      mutate(
        decimalLongitude = as.numeric(decimalLongitude),
        decimalLatitude = as.numeric(decimalLatitude),
        Vulnerability = as.numeric(Vulnerability),
        Length_cm = as.numeric(Length_cm),
        Weight = as.numeric(Weight),
        year = as.numeric(as.character(year))
      )
    message(paste("Records returned from DB after ALL filters:", nrow(df)))
    df
  })
  
  # Filtering data to calculate measo diversity (no spatial filters applied)
  filtered_data_for_measo_diversity <- reactive({
    req(
      initial_slider_ranges$length, initial_slider_ranges$vulnerability,
      initial_slider_ranges$shallow_depth, initial_slider_ranges$deep_depth,
      initial_slider_ranges$com_deep_depth, initial_slider_ranges$com_shallow_depth,
      initial_slider_ranges$weight
    )
    
    # These lines are important to make sure the reactive expression re-runs when inputs change
    input$param_taxon; input$param_aphia_id; input$param_fishbase_id; input$param_gbif_id
    input$param_length_cm; input$param_weight; input$param_vulnerability; input$param_AnaCat; input$param_DemersPelag;
    input$param_iucn; input$param_shallow_depth; input$param_deep_depth;
    input$param_com_deep_depth; input$param_com_shallow_depth; input$param_importance;
    input$param_price; input$param_catching_method
    
    message("--- Running filtered_data_for_measo_diversity reactive (NO drawn polygon) ---")
    where_clauses <- build_where_clauses(include_spatial_filter = FALSE)
    
    query <- paste("SELECT * FROM", table_name)
    if (length(where_clauses) > 0) {
      query <- paste(query, "WHERE", paste(where_clauses, collapse = " AND "))
    }
    
    message("Final SQL Query for diversity (no drawn polygon):\n", query)
    df <- dbGetQuery(con, query)
    
    df <- df %>%
      mutate(
        decimalLongitude = as.numeric(decimalLongitude),
        decimalLatitude = as.numeric(decimalLatitude),
        Vulnerability = as.numeric(Vulnerability),
        Length_cm = as.numeric(Length_cm),
        Weight = as.numeric(Weight),
        year = as.numeric(as.character(year))
      )
    message(paste("Records returned from DB for diversity (no drawn polygon):", nrow(df)))
    df
  })
  
  point_color_col <- reactive(req(input$color_by))
  
  point_size_col <- reactive(req(input$size_by))
  
  color_palette_func <- reactive({
    df <- filtered_data_from_db()
    req(df)
    color_col_name <- point_color_col()
    
    if (!color_col_name %in% names(df)) {
      warning(paste("Color-by column '", color_col_name, "' not found in data. Using 'Unknown' for coloring."))
      domain_values <- "Unknown"
    } else {
      domain_values <- unique(df[[color_col_name]])
      domain_values <- domain_values[!is.na(domain_values) & df[[color_col_name]] != ""]
      if(length(domain_values) == 0){
        warning("No valid values for color mapping. Using 'Unknown' for coloring.")
        domain_values <- "Unknown"
      }
    }
    
    colorFactor(palette = "Set3", domain = domain_values)
  })
  
  point_radius_func <- reactive({
    df <- filtered_data_from_db()
    req(df)
    size_col_name <- point_size_col()
    
    if (!size_col_name %in% names(df)) {
      warning(paste("Size-by column '", size_col_name, "' not found in data. Defaulting radius to 5px."))
      return(function(x) 5)
    }
    
    values <- df[[size_col_name]]
    values <- values[!is.na(values)]
    if (length(values) == 0 || all(values == 0)) { return(function(x) 5) }
    min_val <- min(values, na.rm = TRUE)
    max_val <- max(values, na.rm = TRUE)
    min_radius_px <- 3
    max_radius_px <- 10
    if (max_val == min_val) { return(function(x) (min_radius_px + max_radius_px) / 2) }
    radius_scale_func <- function(val) {
      scaled_val <- pmax(min_val, pmin(max_val, val))
      min_radius_px + (scaled_val - min_val) / (max_val - min_val) * (max_radius_px - min_radius_px)
    }
    return(radius_scale_func)
  })
  
  # Code a button to reset filters
  observeEvent(input$reset_button, {
    message("--- Resetting Filters ---")
    updateTextInput(session, "param_taxon", value = "THISISATEST")
    message("CURRENT TAXON (after reset): [", input$param_taxon, "]")
    updateNumericInput(session, "param_aphia_id", value = NA)
    updateNumericInput(session, "param_fishbase_id", value = NA)
    updateNumericInput(session, "param_gbif_id", value = NA)
    
    if (!is.null(initial_slider_ranges$length)) updateSliderInput(session, "param_length_cm", value = initial_slider_ranges$length)
    if (!is.null(initial_slider_ranges$weight)) updateSliderInput(session, "param_weight", value = initial_slider_ranges$weight)
    if (!is.null(initial_slider_ranges$vulnerability)) updateSliderInput(session, "param_vulnerability", value = initial_slider_ranges$vulnerability)
    if (!is.null(initial_slider_ranges$shallow_depth)) updateSliderInput(session, "param_shallow_depth", value = initial_slider_ranges$shallow_depth)
    if (!is.null(initial_slider_ranges$deep_depth)) updateSliderInput(session, "param_deep_depth", value = initial_slider_ranges$deep_depth)
    if (!is.null(initial_slider_ranges$com_deep_depth)) updateSliderInput(session, "param_com_deep_depth", value = initial_slider_ranges$com_deep_depth)
    if (!is.null(initial_slider_ranges$com_shallow_depth)) updateSliderInput(session, "param_com_shallow_depth", value = initial_slider_ranges$com_shallow_depth)
    
    updateSelectInput(session, "param_AnaCat", selected = "")
    updateSelectInput(session, "param_DemersPelag", selected = "")
    updateSelectInput(session, "param_iucn", selected = "")
    updateSelectInput(session, "param_importance", selected = "")
    updateSelectInput(session, "param_price", selected = "")
    updateSelectInput(session, "param_catching_method", selected = "")
    
    updateRadioButtons(session, "color_by", selected = "Family")
    updateRadioButtons(session, "size_by", selected = "Vulnerability")
    
    drawn_polygon_wkt(NULL)
    leafletProxy("spatial_plot") %>% clearGroup("drawn_polygon")
  })

  
  # Code the map with leaflet
  output$spatial_plot <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.OceanBasemap, group = "Esri - Ocean Basemap") %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels, group = "cartoDB - no labels") %>%
      addLayersControl(
        baseGroups = c("Esri - Ocean Basemap", "cartoDB - no labels"),
        overlayGroups = c("MEASO Regions", "drawn_polygon", "Southern Ocean Boundary", "Land"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      addPolygons(data = st_transform(land, 4326),
                  fillColor = "grey90", color = "black", weight = 0.3,
                  label = ~name, group = "Land") %>%
      addPolygons(data = SO_sf, fill = FALSE, color = "darkblue",
                  dashArray = "5, 10", weight = 1,
                  label = ~name, group = "Southern Ocean Boundary") %>%
      addPolygons(data = measo_regions_info_sf(),
                  fillColor = ~fill,
                  fillOpacity = 0.25,
                  color = "white",
                  weight = 0.5,
                  label = ~display_label, 
                  group = "MEASO Regions") %>%
      addDrawToolbar(
        targetGroup = "drawn_polygon",
        editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()),
        polygonOptions = drawPolygonOptions(
          shapeOptions = drawShapeOptions(fillOpacity = 0.4, color = "red", weight = 3),
          repeatMode = FALSE
        ),
        polylineOptions = FALSE, circleOptions = FALSE, markerOptions = FALSE,
        circleMarkerOptions = FALSE, rectangleOptions = FALSE,
      ) %>%
      setView(lng = 0, lat = -70, zoom = 3)
  })
  
  observe({
    df <- filtered_data_from_db()
    req(df)
    
    df_valid_coords <- df %>%
      filter(!is.na(decimalLongitude) & !is.na(decimalLatitude) &
               is.finite(decimalLongitude) & is.finite(decimalLatitude))
    
    if (nrow(df_valid_coords) == 0) {
      leafletProxy("spatial_plot") %>% clearMarkers() %>% clearControls()
      output$record_count <- renderText({ "Records after applying filters: 0" })
      return()
    }
    
    color_pal <- color_palette_func()
    radius_func <- point_radius_func()
    current_color_col <- point_color_col()
    current_size_col <- point_size_col()
    
    df_valid_coords <- df_valid_coords %>%
      mutate(popup_text = paste(
        "<b>Scientific Name:</b>", scientificName, "<br>",
        "<b>Genus:</b>", Genus, "<br>",
        "<b>Family:</b>", Family, "<br>",
        "<b>Length (cm):</b>", Length_cm, "<br>",
        "<b>Weight (g):</b>", Weight, "<br>",
        "<b>Vulnerability:</b>", Vulnerability, "<br>",
        "<b>IUCN Status:</b>", iucn_status, "<br>",
        "<b>Commercial Importance:</b>", Importance, "<br>",
        "<b>Price:</b>", Price, "<br>",
        "<b>Catching Method:</b>", Catchingmethod, "<br>",
        "<b>Commercial Shallow Depth:</b>", DepthRangeShallow, "<br>",
        case_when(
          !is.na(Fishbase_url) & (Fishbase_url != "") ~ # Use single '&' for vectorized AND
            paste0("<b>FishBase Link:</b> <a href='", Fishbase_url, "' target='_blank'>Go to FishBase</a><br>"),
          TRUE ~ "" # Default case for NA or empty Fishbase_url
        )
      ))
    
    df_valid_coords$marker_color_val <- df_valid_coords[[current_color_col]]
    df_valid_coords$marker_size_val <- df_valid_coords[[current_size_col]]
    
    df_valid_coords$marker_color_val[is.na(df_valid_coords$marker_color_val) | df_valid_coords$marker_color_val == ""] <- "Unknown"
    
    df_valid_coords$marker_size_val[is.na(df_valid_coords$marker_size_val)] <- 0
    df_valid_coords$marker_size_val <- replace(df_valid_coords$marker_size_val, is.infinite(df_valid_coords$marker_size_val), 0)
    
    
    proxy <- leafletProxy("spatial_plot", data = df_valid_coords) %>%
      clearMarkers() %>%
      clearControls()
    
    proxy %>% addCircleMarkers(
      lng = ~decimalLongitude,
      lat = ~decimalLatitude,
      radius = ~radius_func(marker_size_val),
      color = ~color_pal(marker_color_val),
      stroke = FALSE,
      fillOpacity = 0.7,
      popup = ~popup_text
    )
    
    proxy %>% leaflet::addLegend(
      position = "bottomright",
      pal = color_pal,
      values = df_valid_coords$marker_color_val,
      title = paste("Color by", current_color_col),
      layerId = "colorLegend"
    )
    
    output$record_count <- renderText({
      paste("Records after applying filters:", nrow(df_valid_coords))
    })
  })
  
  observeEvent(drawn_polygon_wkt(), {
    current_wkt <- drawn_polygon_wkt()
    proxy <- leafletProxy("spatial_plot") %>% clearGroup("drawn_polygon")
    
    if (!is.null(current_wkt)) {
      drawn_coords_sf <- tryCatch(
        st_sf(geometry = st_as_sfc(current_wkt, crs = 4326)),
        error = function(e) {
          message("Error converting drawn polygon WKT to sf for plotting: ", e$message)
          return(NULL)
        }
      )
      
      if (!is.null(drawn_coords_sf)) {
        proxy %>%
          addPolygons(data = drawn_coords_sf,
                      color = "red", fillOpacity = 0.2, group = "drawn_polygon")
      }
    }
  })
  
  # --- Spatial diversity server logic ---
  # 1. Calculate diversity in whole SO area or drawn polygon
  
  output$occurrence_table <- DT::renderDataTable({
    df_for_table <- filtered_data_from_db()
    
    if ("geometry" %in% names(df_for_table)) {
      df_for_table <- st_drop_geometry(df_for_table)
    }
    
    DT::datatable(df_for_table,
                  options = list(pageLength = 10, scrollX = TRUE),
                  rownames = FALSE)
  })
  
  diversity_metrics <- reactive({
    message("--- Calculating overall diversity metrics ---")
    
    df <- filtered_data_from_db()
    
    if (nrow(df) == 0 || !("Species" %in% names(df))) {
      message("No data or 'Species' column missing for overall diversity calculation.")
      return(data.frame(Message = "No data or 'Species' column missing for overall diversity calculation."))
    }
    
    div_prep <- data.frame(species = df$Species) %>%
      na.omit() %>%
      group_by(species) %>%
      summarise(records = n(), cell = "Overall SO Diversity")
    
    if (nrow(div_prep) == 0) {
      message("No species records found after preparing for overall diversity calculation.")
      return(data.frame(Message = "No species records after preparation. Ensure 'Species' column is populated."))
    }
    
    total_records <- sum(div_prep$records)
    esn_val <- if (total_records < 50) total_records else 50
    if (total_records == 0) esn_val <- NULL
    
    diversity_df <- NULL
    
    if (!is.null(esn_val) && esn_val > 0) {
      diversity_df <- tryCatch({
        obisindicators::calc_indicators(df = div_prep, esn = esn_val)
      }, error = function(e) {
        message("Error calculating overall diversity indicators: ", e$message)
        data.frame(Message = paste("Error calculating overall diversity: ", e$message))
      })
    } else {
      diversity_df <- data.frame(Message = "Not enough records to calculate overall diversity metrics (minimum 1 species and 1 record required).")
    }
    
    if (inherits(diversity_df, "data.frame") &&
        all(c("shannon", "simpson", "maxp", "es", "hill_1", "hill_2", "hill_inf") %in% names(diversity_df))) {
      
      diversity_df <- diversity_df %>%
        mutate(across(c(shannon, simpson, maxp, es, hill_1, hill_2, hill_inf), round, 3))
    }
    
    if("cell" %in% names(diversity_df)){
      diversity_df <- diversity_df %>%
        rename(
          `Diversity Type` = cell,
          `Number of Records` = n,
          `Number of Species` = sp,
          `Shannon Index` = shannon,
          `Simpson Index` = simpson,
          `Dominance (Max P)` = maxp,
          `Effective Species Number (ES50)` = es,
          `Hill1 (exp(shannon))` = hill_1,
          `Hill2 (1/simpson)` = hill_2,
          `Hill3 (1/maxp)` = hill_inf
        )
    }
    
    message("Overall diversity metrics calculated.")
    diversity_df
  })
  
  output$diversity_table <- DT::renderDataTable({
    req(diversity_metrics())
    DT::datatable(diversity_metrics(), options = list(pageLength = 5, dom = 't'), rownames = FALSE)
  })
  
  
  # Create MEASO polygons with tricks to avoid artifacts when plotting
  measo_regions_info_sf <- reactive({
    req(measoshapes::measo_regions05)
    req(measo_names)
    
    # Get the MEASO regions data and ensure it's valid
    measo_regions <- measoshapes::measo_regions05_coastline %>%
      st_make_valid() %>%
      st_transform(crs = 4326)
    measo_delete <- c("WPT", "EPT", "AOT", "CIT", "EIT")
    measo_regions <- measo_regions[!(measo_regions$name %in% measo_delete),]
    
    # Use st_wrap_dateline to fix the antimeridian issue
    measo_regions <- st_wrap_dateline(measo_regions, options = c("WRAPDATELINE=YES", "DATELINEOFFSET=179.9999"))
    
    # Join with the measo_names data to get proper labels and colors
    measo_with_info <- measo_regions %>%
      left_join(measo_names, by = "name") %>%
      # Only keep rows that have matching names in measo_names
      filter(!is.na(sector)) %>%
      # Create a proper display label that combines name, sector, and zone
      mutate(
        display_label = case_when(
          !is.na(zone) ~ paste0(name, " (", sector, " - ", zone, ")"),
          TRUE ~ paste0(name, " (", sector, ")")
        ),
        # Ensure fill colors are available, use a default if missing
        fill = ifelse(is.na(fill), "#CCCCCC", fill)
      )
    
    return(measo_with_info)
  })
  
  
  # 2. Calculate diversity estimators within measo polygons
  measo_diversity_metrics <- reactive({
    message("--- Calculating MEASO region diversity metrics ---")
    
    df <- filtered_data_for_measo_diversity()
    req(df)
    req(measo_regions_info_sf())
    
    if (nrow(df) == 0 || !("Species" %in% names(df)) || !("decimalLongitude" %in% names(df)) || !("decimalLatitude" %in% names(df))) {
      message("No data, 'Species', or coordinate columns missing for MEASO diversity calculation.")
      return(data.frame(Message = "No data, 'Species', or coordinate columns available for MEASO region diversity calculation."))
    }
    
    data_sf <- df %>%
      filter(!is.na(decimalLongitude) & !is.na(decimalLatitude)) %>%
      st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
    
    if (nrow(data_sf) == 0) {
      message("No valid coordinates in filtered data for spatial join.")
      return(data.frame(Message = "No valid occurrence coordinates in filtered data for MEASO region analysis."))
    }
    
    data_with_measo <- data_sf %>%
      st_join(measo_regions_info_sf(), join = st_intersects)
    
    data_with_measo <- data_with_measo %>%
      filter(!is.na(name))
    
    if (nrow(data_with_measo) == 0) {
      message("No filtered data points intersect with MEASO regions.")
      return(data.frame(Message = "No filtered data points found within any MEASO region."))
    }
    
    div_prep_measo <- data_with_measo %>%
      st_drop_geometry() %>%
      drop_na(sector, zone) %>%
      group_by(MEASO_Region = name, Sector = sector, Zone = zone) %>%
      summarise(
        species_list = list(Species),
        record_count = n(),
        .groups = 'drop'
      ) %>%
      rowwise() %>%
      mutate(
        temp_df_for_obis = list(
          tibble(species = unlist(species_list)) %>%
            group_by(species) %>% summarise(records = n(), .groups = 'drop')
        )
      ) %>%
      ungroup()
    
    if (nrow(div_prep_measo) == 0) {
      message("No data after grouping by MEASO regions, sectors, and zones.")
      return(data.frame(Message = "No data found within MEASO regions after filtering and grouping."))
    }
    
    results_list <- list()
    for (i in 1:nrow(div_prep_measo)) {
      row_data <- div_prep_measo[i, ]
      current_species_data <- row_data$temp_df_for_obis[[1]]
      
      num_records <- sum(current_species_data$records)
      num_species <- nrow(current_species_data)
      esn_val <- if (num_records < 50) num_records else 50
      if (num_records == 0 || num_species == 0) esn_val <- NULL
      
      if (!is.null(esn_val) && esn_val > 0 && num_species > 0) {
        current_species_data$cell <- paste(row_data$MEASO_Region, row_data$Sector, row_data$Zone, sep = " - ")
        indicators <- tryCatch({
          calc_indicators(df = current_species_data, esn = esn_val)
        }, error = function(e) {
          message(paste("Error calculating MEASO indicators for", row_data$MEASO_Region, ":", e$message))
          NULL
        })
        
        if (!is.null(indicators)) {
          indicators_with_region <- indicators %>%
            mutate(
              MEASO_Region = row_data$MEASO_Region,
              Sector = row_data$Sector,
              Zone = row_data$Zone,
              .before = 1
            ) %>%
            select(-cell)
          results_list[[i]] <- indicators_with_region
        }
      } else {
        results_list[[i]] <- tibble(
          MEASO_Region = row_data$MEASO_Region,
          Sector = row_data$Sector,
          Zone = row_data$Zone,
          n = num_records,
          sp = num_species,
          shannon = NA_real_, simpson = NA_real_, maxp = NA_real_,
          es = NA_real_, hill_1 = NA_real_, hill_2 = NA_real_, hill_inf = NA_real_,
          Message = "Not enough data for diversity calculation"
        )
      }
    }
    
    if (length(results_list) == 0) {
      message("No diversity results generated for any MEASO region.")
      return(data.frame(Message = "No diversity results available for MEASO regions with current filters."))
    }
    
    final_measo_df <- bind_rows(results_list)
    
    if("shannon" %in% names(final_measo_df)){
      final_measo_df <- final_measo_df %>%
        rename(
          `Number of Records` = n,
          `Number of Species` = sp,
          `Shannon Index` = shannon,
          `Simpson Index` = simpson,
          `Dominance (Max P)` = maxp,
          `Effective Species Number (ES50)` = es,
          `Hill (q=1)` = hill_1,
          `Hill (q=2)` = hill_2,
          `Hill (q=inf)` = hill_inf
        ) %>%
        mutate(across(c(`Shannon Index`, `Simpson Index`, `Dominance (Max P)`,
                        `Effective Species Number (ES50)`, `Hill (q=1)`, `Hill (q=2)`, `Hill (q=inf)`),
                      ~round(., 3)))
    } else if ("n" %in% names(final_measo_df) && "sp" %in% names(final_measo_df)) {
      final_measo_df <- final_measo_df %>%
        rename(
          `Number of Records` = n,
          `Number of Species` = sp
        )
    }
    
    message("MEASO region diversity metrics calculated.")
    final_measo_df
  })
  
  output$measo_diversity_table <- DT::renderDataTable({
    req(measo_diversity_metrics())
    DT::datatable(measo_diversity_metrics(), options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
  })
  
  
  # --- Temporal diversity server logic ---
  # 1. Dynamically populate the temporal area selection dropdown
  observeEvent(measo_regions_info_sf(), {
    req(measo_regions_info_sf())
    measo_names <- unique(measo_regions_info_sf()$name)
    
    updatePickerInput(
      session,
      "temporal_area",
      choices = c("Drawn Polygon" = "Drawn Polygon", sort(measo_names)),
      selected = "Drawn Polygon"
    )
  })
  
  # 2. Reactive expression to select the correct dataframe based on the user's choice
  temporal_data_source <- reactive({
    req(input$temporal_area)
    
    # If "Drawn Polygon" is selected, use the standard filtered data
    if (input$temporal_area == "Drawn Polygon") {
      message("Using data from drawn polygon for temporal analysis.")
      # Check if a polygon is actually drawn, otherwise return all filtered data
      if (!is.null(drawn_polygon_wkt())) {
        # filtered_data_from_db() already handles the spatial filtering
        return(filtered_data_from_db())
      } else {
        # Fallback to general filtered data if no polygon is drawn
        return(filtered_data_from_db())
      }
    } else {
      # If a MEASO area is selected, filter the data without the drawn polygon
      message("Using data from MEASO region ", input$temporal_area, " for temporal analysis.")
      df_no_poly <- filtered_data_for_measo_diversity()
      
      # Ensure measo_regions_info_sf() is a simple, valid sf object
      measo_regions_sf <- measo_regions_info_sf()
      
      # Filter the sf object for the selected MEASO region and rename the name column
      selected_measo_sf <- measo_regions_sf %>% 
        dplyr::filter(name == input$temporal_area) %>%
        dplyr::rename(measo_name = name) # Rename to a unique name to avoid .y suffix
      
      if (nrow(df_no_poly) > 0 && nrow(selected_measo_sf) > 0) {
        data_sf <- df_no_poly %>%
          filter(!is.na(decimalLongitude) & !is.na(decimalLatitude)) %>%
          st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
        
        # Perform the spatial join. The measo_name column is now unique.
        data_in_measo <- data_sf %>%
          st_join(selected_measo_sf, join = st_intersects) %>%
          filter(!is.na(measo_name)) %>% # Filter using the new unique column name
          st_drop_geometry() %>%
          # Keep the original columns from the occurrence data
          select(any_of(names(df_no_poly))) 
        
        # The `select(any_of(names(df_no_poly)))` line ensures that only the original
        # columns are returned, preventing any extra columns from the spatial join from
        # causing downstream errors in your `calculate_temporal_diversity` function.
        
        return(data_in_measo)
      } else {
        message("No data available for the selected MEASO region.")
        return(NULL)
      }
    }
  })
  
  # 3. The function to estimate the moving average diversity
  calculate_temporal_diversity <- function(df, window_size = 5, esn = 50) {
    
    # 1. Initial data cleaning and validation
    df <- df %>% 
      mutate(year = as.numeric(as.character(year))) %>%
      filter(!is.na(year) & year > 0) %>% # filter out unrealistic or missing dates
      filter(!is.na(Species)) # filter out records without species level identification
    
    if (is.null(df) || nrow(df) == 0 || length(unique(df$year)) < window_size) {
      message("Insufficient data after filtering out NA and invalid years.")
      return(NULL)
    }
    
    # 2. Prepare data for calc_indicators
    df_temp <- df %>% 
      group_by(year, Species) %>% 
      summarise(records = n(), .groups = 'drop') %>%
      ungroup() %>% 
      mutate(cell = as.character(year), species = Species)
    
    if (nrow(df_temp) == 0) {
      message("No species data available after aggregation.")
      return(NULL)
    }
    
    diversity_by_year <- obisindicators::calc_indicators(df_temp, esn = esn)
    
    # 3. Calculate effort by year
    effort_by_year <- df %>% 
      group_by(year) %>% 
      summarise(records = n(), .groups = 'drop') %>%
      ungroup()
    
    # If no records, return NULL to prevent errors
    if (nrow(effort_by_year) == 0) {
      message("No effort data available after aggregation.")
      return(NULL)
    }
    
    # Ensure the years match before joining
    effort_by_year <- effort_by_year %>% filter(year %in% diversity_by_year$cell)
    
    # 4. Calculate moving averages (rest of your code is fine)
    moving_average_shannon <- zoo::rollmean(diversity_by_year$shannon, k = window_size, fill = NA, align = "right")
    moving_average_simpson <- zoo::rollmean(diversity_by_year$simpson, k = window_size, fill = NA, align = "right")
    moving_average_sp <- zoo::rollmean(diversity_by_year$sp, k = window_size, fill = NA, align = "right")
    moving_average_es <- zoo::rollapply(diversity_by_year$es, width = window_size, FUN = mean, na.rm = TRUE, fill = NA, align = "right")
    # If diversity_by_year$es is all NA for some consecutive years, the moving 
    # average can’t be calculated properly and will just return NA (or fail 
    # depending on zoo’s settings).
    # The above soluation with rollapply removes NAs so that we can still have an estimate. 
    # Very conservative and not practical: moving_average_es <- zoo::rollmean(diversity_by_year$es, k = window_size, fill = NA, align = "right")
    
    # 5. Combine and return as a data frame
    output_df <- data.frame(
      year = diversity_by_year$cell,
      sp_ma = moving_average_sp,
      shannon_ma = moving_average_shannon,
      simpson_ma = moving_average_simpson,
      es_ma = moving_average_es,
      sp = diversity_by_year$sp,
      shannon = diversity_by_year$shannon,
      simpson = diversity_by_year$simpson,
      es = diversity_by_year$es
    )
    
    # Assign the effort column using match()
    output_df$effort <- effort_by_year$records[match(output_df$year, effort_by_year$year)]
    
    return(output_df)
  }
  
  # 4. Reactive expression to run the diversity calculation
  temporal_diversity_results <- reactive({
    df <- temporal_data_source()
    req(df) 
    
    # Drop geometry column if present, as it's not needed for temporal analysis
    if ("geometry" %in% names(df)) {
      df <- sf::st_drop_geometry(df)
    }
    
    # Make sure the 'year' column exists and is a number
    if (!"year" %in% names(df) || !is.numeric(df$year)) {
      message("Data source lacks a valid 'year' column. Cannot perform temporal analysis.")
      return(NULL)
    }
    
    # Ensure enough data exists for the chosen window size
    if (length(unique(df$year)) < input$temporal_window) {
      message("Not enough unique years in the data for the selected moving average window size.")
      return(NULL)
    }
    
    message("Calculating temporal diversity with window size: ", input$temporal_window)
    calculate_temporal_diversity(df = df, window_size = input$temporal_window)
  })
  
  # 5. Output for the interactive Plotly plot
  output$temporal_diversity_plot <- plotly::renderPlotly({
    results <- temporal_diversity_results()
    req(results)
    
    # Check if a metric is selected and the results data frame is not empty
    req(input$temporal_metric, nrow(results) > 0)
    
    # Ensure the selected columns exist
    metric_col_ma <- paste0(input$temporal_metric, "_ma")
    if (!all(c(metric_col_ma, "effort") %in% names(results))) {
      return(ggplot() + labs(title = "Missing data for selected metric or effort."))
    }
    
    results <- results %>% mutate(year = as.numeric(year))
    
    # Set the plot title based on the selected metric
    plot_title <- paste("Diversity metric:", input$temporal_metric, "|| Window size:", input$temporal_window)
    
    # Create the ggplot object
    p <- ggplot(results, aes(x = year)) +
      geom_line(aes_string(y = metric_col_ma), color = "#4285F4") +
      geom_line(aes(y = effort / max(effort, na.rm = TRUE) * max(results[[metric_col_ma]], na.rm = TRUE)), color = "gray", linetype = "dashed") +
      labs(
        title = plot_title,
        x = "Year",
        y = paste("Moving Average", input$temporal_metric, "Index")
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        axis.title.y.left = element_text(colour = "#4285F4"),
        axis.title.y.right = element_text(colour = "grey")
      )
    
    # Add a secondary y-axis for effort, scaled to match the primary axis
    p <- p + scale_y_continuous(
      sec.axis = sec_axis(
        ~ . / max(results[[metric_col_ma]], na.rm = TRUE) * max(results$effort, na.rm = TRUE),
        name = "Effort (Records)"
      )
    )
    
    # Convert the ggplot object to an interactive plotly object
    plotly::ggplotly(p)
  })
  
  # 6. Output for the data table
  output$temporal_diversity_table <- DT::renderDataTable({
    df_for_table <- temporal_diversity_results()
    req(df_for_table)
    
    # Clean up column names for display
    df_for_table <- df_for_table %>%
      rename(
        `Year` = year,
        `Moving Average Species` = sp_ma,
        `Moving Average Shannon` = shannon_ma,
        `Moving Average Simpson` = simpson_ma,
        `Moving Average ES50` = es_ma,
        `Raw Species` = sp,
        `Raw Shannon` = shannon,
        `Raw Simpson` = simpson,
        `Raw ES50` = es,
        `Effort (Records)` = effort
      ) %>%
      mutate(across(starts_with("MA"), round, 3)) # Round the moving average columns
    
    DT::datatable(df_for_table,
                  options = list(pageLength = 10, scrollX = TRUE),
                  rownames = FALSE)
  })
  
  
  # --- H3 Polygons Server Logic ---
  # Reactive values to store the H3 polygons
  rv <- reactiveValues(
    h3_polygons = NULL
  )
  
  # Reactive expression to calculate H3 polygons (triggered by a button)
  h3_polygons_calculated <- eventReactive(input$calculate_h3_polygons, {
    # Ensure there's data to work with
    req(filtered_data_from_db())
    
    # Use the logic you provided
    occurs_cells <- filtered_data_from_db() %>%
      mutate(
        lng = as.numeric(decimalLongitude),
        lat = as.numeric(decimalLatitude),
        species = Species
      ) %>%
      mutate(cell = h3::geo_to_h3(data.frame(lat = lat, lng = lng), res = input$h3_resolution)) %>%
      filter(!is.na(species), species != "") %>%
      group_by(cell, species) %>%
      summarise(records = n(), .groups = "drop")
    
    # Calculate diversity indicators
    cell_diversity <- obisindicators::calc_indicators(occurs_cells, esn = 50)
    
    # Convert h3 cells to sf polygons
    polygons <- h3::h3_to_geo_boundary_sf(cell_diversity$cell) %>%
      dplyr::mutate(cell = cell_diversity$cell,
                    richness = cell_diversity$sp,
                    shannon_diversity = cell_diversity$shannon,
                    simpson_diversity = cell_diversity$simpson,
                    ES = cell_diversity$es,
                    maxp = cell_diversity$maxp,
                    hill_1 = cell_diversity$hill_1,
                    hill_2 = cell_diversity$hill_2,
                    hill_inf = cell_diversity$hill_inf) %>%
      st_transform(crs = 4326) # leaflet requires EPSG:4326
    
    # Prevent wrapping around the antimeridian
    polygons <- sf::st_wrap_dateline(polygons, options = c("WRAPDATELINE=YES", "DATELINEOFFSET=179.9"))
    
    # Store the polygons in a reactive value
    rv$h3_polygons <- polygons
    
    return(polygons)
  })
  
  # Observe the clear button
  observeEvent(input$clear_h3_polygons, {
    rv$h3_polygons <- NULL
    leafletProxy("h3_plot") %>%
      clearGroup("H3 Polygons") %>%
      removeControl("h3_legend")
  })
  
  # Create the leaflet for the H3 polygons tab
  output$h3_plot <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.OceanBasemap, group = "Esri - Ocean Basemap") %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels, group = "cartoDB - no labels") %>%
      addLayersControl(
        baseGroups = c("Esri - Ocean Basemap", "cartoDB - no labels"),
        overlayGroups = c("H3 Polygons"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      setView(lng = 0, lat = -70, zoom = 3)
  })
  
  # Reactive observer to add H3 polygons to the new map
  observeEvent(h3_polygons_calculated(), {
    req(rv$h3_polygons)
    
    leafletProxy("h3_plot") %>%
      clearGroup("H3 Polygons") %>%
      removeControl("h3_legend") # Remove old legend if it exists
    
    metric_data <- rv$h3_polygons[[input$h3_metric]]
    color_palette <- colorNumeric(palette = "viridis", domain = metric_data, na.color = "#00000000")
    
    leafletProxy("h3_plot") %>%
      addPolygons(
        data = rv$h3_polygons,
        fillColor = ~color_palette(metric_data),
        fillOpacity = 0.7,
        color = "white",
        weight = 0.5,
        group = "H3 Polygons",
        label = ~paste0("Diversity: ", round(metric_data, 2), " (", input$h3_metric, ")"),
        highlightOptions = highlightOptions(
          color = "yellow", weight = 2,
          bringToFront = TRUE
        )
      ) %>%
      leaflet::addLegend(
        position = "bottomright",
        pal = color_palette,
        values = metric_data,
        title = input$h3_metric,
        opacity = 1,
        layerId = "h3_legend"
      )
  })
  
  
}

shinyApp(ui = ui, server = server)