#############################################################################################
# SCRIPT: Species Occurrence Database Pipeline for Actinopterygii in the SO
# AUTHOR: Pablo Deschepper
# DESCRIPTION: This script fetches, cleans, annotates, and saves species occurrence data
#              for Actinopterygii, including taxonomy and trait annotations for the SO.
#############################################################################################

# ==============================================================================
# 1. SETUP AND LIBRARIES
# ==============================================================================
# Load required libraries
library(rgbif)        # Interface to GBIF occurrence data
library(robis)        # Interface to OBIS occurrence data
library(rfishbase)    # Interface to FishBase trait data
library(dplyr)        # Data manipulation (filter, select, mutate, etc.)
library(tidyr)        # Data tidying (pivot_wider, separate, etc.)
library(sf)           # Spatial data handling (st_as_sfc, st_sf, etc.)
library(httr2)        # HTTP requests (for GBIF API)
library(worrms)       # WoRMS taxonomy (wm_name2id, wm_external)
library(CoordinateCleaner) # Clean geographic coordinates (clean_coordinates, cc_sea)
library(taxize)       # Taxonomic resolution (classification)
library(purrr)        # Functional programming (map, map2_chr)
library(arrow)        # Read/write Parquet files (write_parquet)
library(DBI)          # Database interface (dbConnect)
library(dbplyr)       # Database dplyr backend (dbGetQuery)
library(rredlist)     # IUCN Red List data (rl_species)
library(stringr)      # String manipulation (str_split, gsub)
library(data.table)   # Fast data reading/writing (fread)

# Set working directory
setwd("C:/Users/pdeschepper/Desktop/WORK/ADVANCE/VRI/Rcode/TraitMapper/")

# ==============================================================================
# 2. QUERY PARAMETERS
# ==============================================================================
# Define the scientific name and selected taxa for the analysis
scientific.name <- "Actinopterygii"
selected_taxa <- c(
  "Acipenseriformes", "Albuliformes", "Amiiformes", "Anguilliformes",
  "Ateleopodiformes", "Atheriniformes", "Aulopiformes", "Batrachoidiformes",
  "Beloniformes", "Beryciformes", "Cetomimiformes", "Characiformes",
  "Clupeiformes", "Cypriniformes", "Cyprinodontiformes", "Elopiformes",
  "Esociformes", "Gadiformes", "Gasterosteiformes", "Gobiesociformes",
  "Gonorynchiformes", "Gymnotiformes", "Lampriformes", "Lepisosteiformes",
  "Lophiiformes", "Mugiliformes", "Myctophiformes", "Notacanthiformes",
  "Ophidiiformes", "Osmeriformes", "Osteoglossiformes", "Perciformes",
  "Percopsiformes", "Pleuronectiformes", "Polymixiiformes", "Polypteriformes",
  "Saccopharyngiformes", "Salmoniformes", "Scorpaeniformes", "Siluriformes",
  "Stephanoberyciformes", "Stomiiformes", "Synbranchiformes", "Syngnathiformes",
  "Tetraodontiformes", "Zeiformes"
)

# ==============================================================================
# 3. GET DIFFERENT IDs FOR LOOKUP
# ==============================================================================
# Get the AphiaID for the main taxon
AphiaID <- worrms::wm_name2id(name = scientific.name)

# Function to fetch GBIF backbone ID for a given scientific name
get_gbif_backbone_id <- function(scientific_name) {
  message("Processing: ", scientific_name)
  aphia_id <- tryCatch({
    worrms::wm_name2id(name = scientific_name)
  }, error = function(e) {
    message("  Warning: Could not get AphiaID for ", scientific_name, ": ", e$message)
    return(NA_integer_)
  })
  if (is.na(aphia_id)) {
    return(NA_integer_)
  }
  sourceId <- paste0('urn:lsid:marinespecies.org:taxname:', aphia_id)
  response <- tryCatch({
    request(base_url = 'https://api.gbif.org/v1/species') %>%
      req_url_query(`datasetKey` = '2d59e5db-57ad-41ff-97d6-11f5fb264527',
                    `sourceId` = sourceId) %>%
      req_perform() %>%
      resp_body_json()
  }, error = function(e) {
    message("  Warning: GBIF API request failed for ", scientific_name, ": ", e$message)
    return(NULL)
  })
  if (!is.null(response) && length(response$results) > 0) {
    return(response$results[[1]]$nubKey)
  } else {
    message("  Warning: No GBIF backbone ID found for ", scientific_name)
    return(NA_integer_)
  }
}

# Fetch GBIF backbone IDs for all selected taxa
GBIF_backboneIDs_list <- purrr::map(selected_taxa, function(fam) {
  id <- get_gbif_backbone_id(fam)
  Sys.sleep(0.1) # Small delay between requests to be gentle to the API
  return(id)
})
GBIF_backboneIDs_vector <- GBIF_backboneIDs_list %>%
  unlist() %>%
  na.omit() %>%
  as.integer()

# ==============================================================================
# 4. POLYGONS FOR MEASO REGIONS AND SOUTHERN OCEAN
# ==============================================================================
# MEASO regions (if needed, uncomment and install measoshapes)
# measo <- measoshapes::measo_regions05 %>% group_by(name) %>% summarize()

# Southern Ocean polygon (WKT string)
SO_polygon <- "POLYGON((180 -44.3,173 -44.3,173 -47.5,170 -47.5,157 -47.5,157 -45.9,150 -45.9,150 -47.5,143 -47.5,143 -45.8,140 -45.8,140 -44.5,137 -44.5,137 -43,135 -43,135 -41.7,131 -41.7,131 -40.1,115 -40.1,92 -40.1,92 -41.4,78 -41.4,78 -42.3,69 -42.3,69 -43.3,47 -43.3,47 -41.7,30 -41.7,12 -41.7,12 -40.3,10 -40.3,10 -38.3,-5 -38.3,-5 -38.9,-9 -38.9,-9 -40.2,-13 -40.2,-13 -41.4,-21 -41.4,-21 -42.5,-39 -42.5,-39 -40.7,-49 -40.7,-49 -48.6,-54 -48.6,-54 -55.7,-62.7972582608082 -55.7,-64 -55.7,-64 -57.8,-71 -57.8,-71 -58.9,-80 -58.9,-80 -40,-125 -40,-167 -40,-167 -42.6,-171 -42.6,-171 -44.3,-180 -44.3,-180 -90, 0 -90,180 -90,180 -44.3))"
SO_polygon_sfc <- st_as_sfc(SO_polygon, crs = 4326)
SO_sf <- st_sf(geometry = SO_polygon_sfc, name = "Southern Ocean Boundary")

# ==============================================================================
# 5. GET OCCURRENCE RECORDS FOR SPECIFIED TAXON
# ==============================================================================
# Query OBIS for occurrence records
obis_results <- robis::occurrence(taxonid = AphiaID, geometry = SO_polygon)
write.table(obis_results, file = "Actinopterygii_obis.csv", sep = ";", col.names = TRUE, quote = FALSE)

# Query GBIF for occurrence records
gbif_results <- rgbif::occ_download(
  pred("hasGeospatialIssue", FALSE),
  pred("hasCoordinate", TRUE),
  pred("occurrenceStatus", "PRESENT"),
  pred_in("taxonKey", GBIF_backboneIDs_vector),
  pred_within(SO_polygon),
  format = "SIMPLE_CSV"
)

# Check download status and retrieve data
occ_download_wait(gbif_results)
d <- occ_download_get(gbif_results) %>%
  occ_download_import()
gbif_citation(gbif_results)

# ==============================================================================
# 6. MERGE OBIS AND GBIF DATA
# ==============================================================================
# Load GBIF data from CSV
gbif_data_csv <- "C:/Users/pdeschepper/Desktop/WORK/ADVANCE/VRI/Rcode/TraitMapper/Actinopterygii_gbif/0011306-250711103210423.csv"
gbif_data <- data.table::fread(gbif_data_csv, sep = ";", fill = TRUE, header = TRUE)
gbif_data[, sapply(gbif_data, is.character)] <- lapply(gbif_data[, sapply(gbif_data, is.character)], as.factor)

# Load OBIS data from CSV
obis_data_csv <- "C:/Users/pdeschepper/Desktop/WORK/ADVANCE/VRI/Rcode/TraitMapper/Actinopterygii_obis.csv"
obis_data <- data.table::fread(obis_data_csv, sep = ";", fill = TRUE, header = TRUE, stringsAsFactors = FALSE)

# Correct OBIS column names
new_colnames_obis <- c("V1", names(obis_data))
obis_data <- cbind(obis_data, rep("NA", nrow(obis_data)))
colnames(obis_data) <- new_colnames_obis
obis_data <- subset(obis_data, select = -1)

# Define columns to keep from both datasets
obis_cols_to_keep <- c("scientificName", "decimalLatitude", "decimalLongitude", "basisOfRecord", "year", "issues")
gbif_cols_to_keep <- c("species", "decimalLatitude", "decimalLongitude", "basisOfRecord", "year", "issues")

# Select and rename columns for consistency
obis_selected <- obis_results %>%
  select(any_of(obis_cols_to_keep)) %>%
  mutate(year = as.factor(year), source = "obis")
gbif_selected <- gbif_data %>%
  dplyr::select(any_of(gbif_cols_to_keep)) %>%
  mutate(
    decimalLatitude = as.numeric(as.character(decimalLatitude)),
    decimalLongitude = as.numeric(as.character(decimalLongitude)),
    basisOfRecord = as.character(basisOfRecord),
    year = as.factor(year),
    source = "gbif",
    scientificName = as.character(species)
  ) %>%
  select(-species)

# Merge OBIS and GBIF data
merged_data <- bind_rows(obis_selected, gbif_selected)

# ==============================================================================
# 7. CLEAN DATA WITH COORDINATECLEANER
# ==============================================================================
# Convert latitude and longitude to numeric
merged_data_clean <- merged_data %>%
  mutate(
    decimalLatitude = as.numeric(decimalLatitude),
    decimalLongitude = as.numeric(decimalLongitude)
  )

# Filter out invalid coordinates
pre_cleaned_data <- merged_data_clean %>%
  filter(!is.na(decimalLatitude) & decimalLatitude >= -90 & decimalLatitude <= 90) %>%
  filter(!is.na(decimalLongitude) & decimalLongitude >= -180 & decimalLongitude <= 180)

# Clean coordinates using CoordinateCleaner
cleaned_coords_data <- clean_coordinates(
  x = pre_cleaned_data,
  lon = "decimalLongitude",
  lat = "decimalLatitude",
  species = "scientificName",
  tests = c("cap", "cent", "con", "dupl", "equ", "gbif", "inst", "sea", "zeros"),
  value = "clean",
  verbose = TRUE
)

# Keep only marine records
Marine_flagged <- CoordinateCleaner::cc_sea(cleaned_coords_data, value = "flagged")
cleaned_coords_data <- cleaned_coords_data[!Marine_flagged, ]

# Remove rows with NA scientificName
cleaned_coords_data <- cleaned_coords_data[!is.na(cleaned_coords_data["scientificName"]), ]
dim(cleaned_coords_data)

# ==============================================================================
# 8. ANNOTATE WITH HIERARCHICAL TAXONOMY
# ==============================================================================
# Get unique species and resolve taxonomy
unique_species <- unique(cleaned_coords_data$scientificName)
tax_resolved <- taxize::classification(unique_species, db = "worms")

# Function to filter out unresolved taxonomies
is_not_logical_na <- function(x) {
  !(is.logical(x) && length(x) == 1 && is.na(x))
}
tax_resolved <- Filter(is_not_logical_na, tax_resolved)

# Flatten taxonomy data
list_of_wide_tibbles <- lapply(seq_along(tax_resolved), function(i) {
  species_tibble <- tax_resolved[[i]]
  scientific_name <- names(tax_resolved)[i]
  species_tibble %>%
    select(name, rank) %>%
    pivot_wider(names_from = rank, values_from = name) %>%
    mutate(scientificName = scientific_name, .before = 1)
})
flattened_taxonomy_df <- bind_rows(list_of_wide_tibbles)

# Select desired taxonomic levels
desired_taxon_levels <- c("scientificName", "Species", "Genus", "Family", "Order", "Class")
final_taxonomy_df <- flattened_taxonomy_df %>%
  select(any_of(desired_taxon_levels))

# Annotate occurrence records with taxonomy
Occ_taxonomy <- left_join(cleaned_coords_data, final_taxonomy_df, by = "scientificName")

# ==============================================================================
# 9. GET TRAIT DATA
# ==============================================================================
# 9.1 FISHBASE TRAITS
# Get unique scientific names
unique_scientific_names <- unique(Occ_taxonomy$scientificName)
message(paste("\nFound", length(unique_scientific_names), "unique scientific names for trait lookup."))

# Initialize lookup table for IDs
species_id_lookup <- tibble(
  scientificName = character(),
  AphiaID = integer(),
  FishBaseID = integer(),
  GbifID = integer()
)

# Loop through unique species to fetch IDs
for (sci_name in unique_scientific_names) {
  message(paste("Processing unique species:", sci_name))
  aphia_id <- NA_integer_
  fb_id <- NA_integer_
  gbif_id <- NA_integer_
  
  # Get AphiaID
  aphia_id_result <- tryCatch({
    worrms::wm_name2id(name = sci_name)
  }, error = function(e) {
    message(paste("Error getting AphiaID for", sci_name, ":", e$message))
    return(NULL)
  })
  if (is.integer(aphia_id_result) && length(aphia_id_result) > 0) {
    aphia_id <- aphia_id_result[1]
  } else if (is.data.frame(aphia_id_result) && nrow(aphia_id_result) > 0) {
    aphia_id <- aphia_id_result$AphiaID[1]
  }
  
  # Get FishBaseID (for two-word species)
  is_two_word_species <- length(strsplit(sci_name, " ")[[1]]) == 2
  if (!is.na(aphia_id) && is_two_word_species) {
    fb_id_result <- tryCatch({
      worrms::wm_external(id = aphia_id, type = "fishbase")
    }, error = function(e) {
      message(paste("Error getting FishBaseID for AphiaID", aphia_id, ":", e$message))
      return(NULL)
    })
    if (is.integer(fb_id_result) && length(fb_id_result) > 0) {
      fb_id <- fb_id_result[1]
    } else if (is.data.frame(fb_id_result) && nrow(fb_id_result) > 0) {
      fb_id_row <- fb_id_result %>% filter(type == "FishBase")
      if (nrow(fb_id_row) > 0) {
        fb_id <- as.integer(fb_id_row$identifier[1])
      }
    }
  }
  
  # Get GBIF ID with retry
  gbif_result <- NULL
  attempts <- 0
  max_attempts <- 3
  while (is.null(gbif_result) && attempts < max_attempts) {
    attempts <- attempts + 1
    message(paste("Attempting GBIF ID lookup for", sci_name, "(Attempt", attempts, "of", max_attempts, ")"))
    gbif_result <- tryCatch({
      rgbif::name_backbone(name = sci_name)
    }, error = function(e) {
      message(paste("Error getting GBIF ID for", sci_name, ":", e$message))
      Sys.sleep(2)
      return(NULL)
    })
    if (!is.null(gbif_result) && nrow(gbif_result) > 0 && "usageKey" %in% names(gbif_result)) {
      gbif_id <- as.integer(gbif_result$usageKey[1])
      break
    }
  }
  
  # Append to lookup table
  current_species_ids <- tibble(
    scientificName = sci_name,
    AphiaID = aphia_id,
    FishBaseID = fb_id,
    GbifID = if (!is.null(gbif_result)) as.integer(gbif_result$usageKey[1]) else NA_integer_
  )
  species_id_lookup <- bind_rows(species_id_lookup, current_species_ids)
  Sys.sleep(0.1)
}

# Join IDs to occurrence data
Occ_taxonomy_with_ids <- left_join(Occ_taxonomy, species_id_lookup, by = "scientificName")

# Fetch FishBase traits
message("\nFetching trait data from FishBase...")
fishbase_species_traits <- rfishbase::species()
selected_traits <- fishbase_species_traits %>%
  select(
    AnaCat,
    FishBaseID = SpecCode,
    Length_cm = Length,
    Weight,
    DemersPelag,
    DepthRangeShallow,
    DepthRangeDeep,
    DepthRangeComShallow,
    DepthRangeComDeep,
    Vulnerability,
    Importance,
    Price = PriceCateg,
    Catchingmethod = MainCatchingMethod
  )

# Join traits to occurrence data
final_data_with_traits <- left_join(Occ_taxonomy_with_ids, selected_traits, by = "FishBaseID")

# 9.2 IUCN RED LIST STATUS
# Function to retrieve IUCN status
get_iucn_status <- function(genus, species) {
  full_name <- paste(genus, species)
  if (is.na(genus) || is.na(species) || genus == "" || species == "") {
    return(NA_character_)
  }
  message("Looking up status for: ", full_name)
  status_response <- tryCatch({
    rl_species(genus = genus, species = species)$assessments
  }, error = function(e) {
    message("  Error retrieving status for ", full_name, ": ", e$message)
    return(NULL)
  })
  if (is.data.frame(status_response) && nrow(status_response) > 0) {
    is_global <- purrr::map_lgl(status_response$scopes, ~any(.x$en == "Global"))
    latest_global_assessment <- status_response %>% filter(is_global, latest == TRUE)
    if (nrow(latest_global_assessment) > 0) {
      return(latest_global_assessment$red_list_category_code[1])
    } else {
      latest_assessment <- status_response %>% filter(latest == TRUE)
      if (nrow(latest_assessment) > 0) {
        return(latest_assessment$red_list_category_code[1])
      } else {
        return(status_response$red_list_category_code[1])
      }
    }
  } else {
    return("DD")
  }
}

# Get unique species for IUCN lookup
unique_species_to_lookup <- final_data_with_traits %>%
  select(Species) %>%
  filter(!is.na(Species), Species != "") %>%
  distinct() %>%
  tidyr::separate(col = Species, into = c("Genus", "Species"), sep = " ", extra = "merge")

# Apply IUCN status function
iucn_statuses <- purrr::map2_chr(
  unique_species_to_lookup$Genus,
  unique_species_to_lookup$Species,
  ~get_iucn_status(.x, .y)
)

# Create IUCN lookup table
iucn_lookup_table <- unique_species_to_lookup %>%
  mutate(iucn_status = iucn_statuses, scientificName = paste(Genus, Species))

# Join IUCN status to main dataset
final_data_with_traits <- final_data_with_traits %>%
  left_join(iucn_lookup_table %>% select(scientificName, iucn_status), by = "scientificName") %>%
  mutate(
    iucn_status = na_if(iucn_status, "NA"),
    iucn_status = replace_na(iucn_status, "NE"),
    Fishbase_url = case_when(
      !is.na(Species) & grepl(" ", Species) ~
        paste0("https://www.fishbase.se/summary/", gsub(" ", "-", Species, fixed = TRUE)),
      TRUE ~ NA_character_
    )
  )

# ==============================================================================
# 10. CREATE PARQUET FILE
# ==============================================================================
parquet_name <- "Actinopterygii_database.parquet"
arrow::write_parquet(final_data_with_traits, parquet_name)

# ==============================================================================
# 11. WRITE/READ TO .TXT FILE
# ==============================================================================
write.table(
  x = final_data_with_traits,
  file = "Actinopterygii_database.txt",
  sep = ",",
  quote = FALSE,
  col.names = TRUE
)
final_data_with_traits <- read.table(file = "Actinopterygii_database.txt", sep = ",")

# ==============================================================================
# 12. LOAD PARQUET DATABASE
# ==============================================================================
con <- dbConnect(duckdb::duckdb(), dbdir = "Actinopterygii_database.parquet", read_only = FALSE)
dbGetQuery(con, "SELECT * FROM 'Actinopterygii_database.parquet' WHERE Vulnerability > 75")
