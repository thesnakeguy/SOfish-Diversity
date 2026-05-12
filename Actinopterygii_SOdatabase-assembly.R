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
library(httr)         # HTTP request (for RAMS)
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
setwd("C:/Users/pdeschepper/OneDrive - Institute of Natural Sciences/Desktop/WORK/ADVANCE/VRI/Rcode/SOfish_diversity/Cleaned/")

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
# Batch-fetch AphiaIDs for all selected taxa
aphia_ids <- worrms::wm_name2id_(name = selected_taxa)

# Named integer vector, dropping NAs
aphiaid_vector <- na.omit(unlist(aphia_ids))

# Map AphiaIDs to GBIF backbone keys via LSID lookup
get_gbif_key <- function(aphia_id, taxon_name) {
  sourceId <- paste0("urn:lsid:marinespecies.org:taxname:", aphia_id)
  response <- httr2::request("https://api.gbif.org/v1/species") %>%
    httr2::req_url_query(
      datasetKey = "2d59e5db-57ad-41ff-97d6-11f5fb264527",
      sourceId   = sourceId
    ) %>%
    httr2::req_perform() %>%
    httr2::resp_body_json()
  
  if (length(response$results) > 0) {
    return(response$results[[1]]$nubKey)
  } else {
    message("No GBIF backbone match for: ", taxon_name)
    return(NA_integer_)
  }
}

# Apply over the named AphiaID vector
gbif_keys <- purrr::imap_int(aphiaid_vector, get_gbif_key)

# Overview of unmatched taxa
no_match <- names(gbif_keys[is.na(gbif_keys)])
if (length(no_match) > 0) message("Unmatched taxa: ", paste(no_match, collapse = ", "))

# Drop NAs for use in download
GBIF_backboneIDs_vector <- na.omit(gbif_keys)

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
# OBIS
# Query OBIS for occurrence records
obis_results <- robis::occurrence(taxonid = aphiaid_vector, geometry = SO_polygon)
# Optional: save OBIS results
# write.table(obis_results, file = "Actinopterygii_obis.csv", sep = ";", col.names = TRUE, quote = FALSE)

# GBIF
# Query GBIF for occurrence records
gbif_results <- rgbif::occ_download(
  pred("hasGeospatialIssue", FALSE),
  pred("hasCoordinate", TRUE),
  pred("occurrenceStatus", "PRESENT"),
  pred_in("taxonKey", GBIF_backboneIDs_vector),
  pred_within(SO_polygon),
  format = "SIMPLE_CSV"
)

# Check download status
occ_download_wait(gbif_results)  
d  <- occ_download_get(gbif_results, overwrite = TRUE)
df <- occ_download_import(d)

gbif_citation(d)

# ==============================================================================
# 6. MERGE OBIS AND GBIF DATA
# ==============================================================================
# Load GBIF data from CSV if needed
# gbif_data_csv <- "C:/Users/pdeschepper/OneDrive - Institute of Natural Sciences/Desktop/WORK/ADVANCE/VRI/Rcode/SOfish_diversity/Cleaned/" # link your datapath here
# gbif_data <- data.table::fread(gbif_data_csv, sep = ";", fill = TRUE, header = TRUE)
gbif_data <- df
gbif_data[, sapply(gbif_data, is.character)] <- lapply(gbif_data[, sapply(gbif_data, is.character)], as.factor)

# Load OBIS data from CSV if needed
# obis_data_csv <- "C:/Users/pdeschepper/Desktop/WORK/ADVANCE/VRI/Rcode/TraitMapper/Actinopterygii_obis.csv"
# obis_data <- data.table::fread(obis_data_csv, sep = ";", fill = TRUE, header = TRUE, stringsAsFactors = FALSE)
obis_data <- obis_results

# Define columns to keep from both datasets
obis_cols_to_keep <- c("scientificName", "decimalLatitude", "decimalLongitude", "basisOfRecord", "year", "dataset_id", "issues")
gbif_cols_to_keep <- c("species", "decimalLatitude", "decimalLongitude", "basisOfRecord", "year", "datasetKey", "issues")

# Select and rename columns for consistency
obis_selected <- obis_results %>%
  select(any_of(obis_cols_to_keep)) %>%
  mutate(year = as.factor(year),
         source = "obis",
         datasetID = dataset_id) %>% 
  select(-dataset_id)
gbif_selected <- gbif_data %>%
  dplyr::select(any_of(gbif_cols_to_keep)) %>%
  mutate(
    decimalLatitude = as.numeric(as.character(decimalLatitude)),
    decimalLongitude = as.numeric(as.character(decimalLongitude)),
    basisOfRecord = as.character(basisOfRecord),
    year = as.factor(year),
    source = "gbif",
    scientificName = as.character(species),
    datasetID = datasetKey
  ) %>%
  select(-species) %>%
  select(-datasetKey)

# Merge OBIS and GBIF data
merged_data <- bind_rows(gbif_selected, obis_selected)

# Fix different writings of basisOfRecord values
merged_data <- merged_data %>%
  mutate(basisOfRecord_clean = basisOfRecord %>%
           str_trim() %>%                 # remove extra spaces
           str_to_lower())
merged_data <- merged_data %>%
  mutate(basisOfRecord_clean = case_when(
    str_detect(basisOfRecord_clean, "human") ~ "human_observation",
    str_detect(basisOfRecord_clean, "machine") ~ "machine_observation",
    str_detect(basisOfRecord_clean, "fossil") ~ "fossil_specimen",
    str_detect(basisOfRecord_clean, "material_sample|materialsample") ~ "material_sample",
    str_detect(basisOfRecord_clean, "materialcitation|material_citation") ~ "material_citation",
    str_detect(basisOfRecord_clean, "preserved") ~ "preserved_specimen",
    str_detect(basisOfRecord_clean, "occurrence") ~ "occurrence",
    str_detect(basisOfRecord_clean, "observation") ~ "observation",
    TRUE ~ basisOfRecord_clean
  ))

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
# 9. GET VALID NAME, IDs and RAMS TAG
# ==============================================================================

# 1. Clean unique names
unique_scientific_names <- unique(Occ_taxonomy$scientificName)
unique_scientific_names <- unique_scientific_names[nzchar(unique_scientific_names) & !is.na(unique_scientific_names)]
message(paste("\nFound", length(unique_scientific_names), "unique scientific names for trait lookup."))

# 2. VECTORIZED WORMS/FISHBASE LOOKUP 
message("Performing vectorized WoRMS lookup...")
# wm_name2id_ returns a list; we simplify it to a vector
aphia_ids_raw <- worrms::wm_name2id_(unique_scientific_names)
aphia_ids_raw <- unlist(aphia_ids_raw, use.names = FALSE)

# Get valid ID/Names for all found IDs in bulk
aphia_records <- wm_record_(aphia_ids_raw)
aphia_ids_valid <- sapply(aphia_records, function(df) df$valid_AphiaID)
scientificName_valid <- sapply(aphia_records, function(df) df$valid_name) %>% unname()

fb_df <- worrms::wm_external_(id = aphia_ids_valid, type = "fishbase") %>% stack() 
colnames(fb_df) <- c("Fishbase","Aphia")
fb_df$Aphia <- as.integer(as.character(fb_df$Aphia))
Aphia_data <- data.frame(scientificName = scientificName_valid,
                         originalName = unique_scientific_names, #this is the species name given in the dataset
                         aphia_id = as.integer(aphia_ids_valid))
Aphia_data <- left_join(Aphia_data, fb_df, by = join_by(aphia_id == Aphia))

# 3. RAMS CHECK FUNCTION
check_rams <- function(aphia_id) {
  if(is.na(aphia_id)) return(NA_character_)
  url <- paste0("https://www.marinespecies.org/RAMS/aphia.php?p=taxdetails&id=", aphia_id)
  
  res <- tryCatch({
    resp <- httr::GET(url)
    cont <- httr::content(resp, as = "text", encoding = "UTF-8")
    if (grepl("Not found", cont, ignore.case = TRUE)) "no" else "yes"
  }, error = function(e) return("error"))
  return(res)
}

# 4. LOOP FOR GBIF ID AND POPULATE RAMS TAG
# Initialize the final lookup table
species_id_lookup <- tibble()

for (i in 1:nrow(Aphia_data)) {
  original_name <- Aphia_data$originalName[i]
  sci_name <- Aphia_data$scientificName[i]
  aphia_id <- Aphia_data$aphia_id[i]
  # Pull the Fishbase ID that we already joined in Step 2
  fb_id    <- Aphia_data$Fishbase[i] 
  
  message(paste("Processing supplemental IDs for:", sci_name))
  
  gbif_id <- NA_integer_
  
  # Get GBIF ID with retry logic
  attempts <- 0
  max_attempts <- 3
  while (attempts < max_attempts) {
    attempts <- attempts + 1
    gbif_result <- tryCatch({
      rgbif::name_backbone(name = sci_name)
    }, error = function(e) NULL)
    
    # Check if we got a valid result from GBIF
    if (!is.null(gbif_result) && !is.null(gbif_result$usageKey)) {
      gbif_id <- as.integer(gbif_result$usageKey[1])
      break
    }
    Sys.sleep(0.5) 
  }
  
  # Append results to the lookup table
  # We use the RAMS check function here as it requires a per-ID URL check
  species_id_lookup <- bind_rows(species_id_lookup, tibble(
    originalName = original_name,
    scientificName_valid = sci_name,
    AphiaID        = aphia_id,
    FishBaseID     = fb_id,
    GbifID         = gbif_id,
    RAMS_species   = check_rams(aphia_id)
  ))
  
  # Polite pause to avoid hitting GBIF/RAMS servers too hard
  Sys.sleep(0.1)
}

# Final cleanup: Remove duplicates if any were generated during lookup
species_id_lookup <- distinct(species_id_lookup)

# ==============================================================================
# 10. GET TRAIT DATA
# ==============================================================================
# 10.1 FISHBASE TRAITS

# Join IDs to occurrence data
Occ_taxonomy_with_ids <- left_join(Occ_taxonomy, species_id_lookup, by = join_by(scientificName == originalName))

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

# Populate the "Species" column with the scientificName_valid of it has genus and species level information. IMPORTANT
final_data_with_traits <- final_data_with_traits %>%
  mutate(Species = ifelse(str_count(scientificName_valid, "\\S+") == 2, 
                          scientificName_valid, 
                          Species))

# 10.2 IUCN RED LIST STATUS
# Function to retrieve IUCN status (make sure to put your key in .Renviron as IUCN_REDLIST_KEY)
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
final_data_with_traits_XXX <- final_data_with_traits %>%
  left_join(iucn_lookup_table[,3:4], by = join_by(scientificName_valid == scientificName)) %>%
  mutate(iucn_status = na_if(iucn_status, "NA"),
         iucn_status = replace_na(iucn_status, "NE"),
         Fishbase_url = case_when(
          !is.na(Species) & grepl(" ", Species) ~
            paste0("https://www.fishbase.se/summary/", gsub(" ", "-", Species, fixed = TRUE)),
          TRUE ~ NA_character_
        )
      )

# ==============================================================================
# 11. DATA CLEAN UP
# ============================================================================== 
# Remove basisOfRecord / scientificName
final_data_with_traits_XXX <- final_data_with_traits_XXX %>% 
  select(-c(basisOfRecord, scientificName))
# Rename BasisOfRecord_clean / scientificName_valid
final_data_with_traits_XXX <- final_data_with_traits_XXX %>%
  rename(scientificName = scientificName_valid,
         basisOfRecord = basisOfRecord_clean)

# ==============================================================================
# 12. CREATE PARQUET FILE
# ==============================================================================
parquet_name <- "Actinopterygii_database.parquet"
arrow::write_parquet(final_data_with_traits_XXX, parquet_name)

# ==============================================================================
# 13. WRITE/READ TO .TXT FILE
# ==============================================================================
write.table(
  x = final_data_with_traits_XXX,
  file = "Actinopterygii_database.txt",
  sep = ",",
  quote = FALSE,
  col.names = TRUE
)
final_data_with_traits <- read.table(file = "Actinopterygii_database.txt", sep = ",")

# ==============================================================================
# 14. LOAD PARQUET DATABASE
# ==============================================================================
con <- dbConnect(duckdb::duckdb(), dbdir = "Actinopterygii_database.parquet", read_only = FALSE)
dbGetQuery(con, "SELECT * FROM 'Actinopterygii_database.parquet' WHERE Vulnerability > 75")
# Make data.frame
df <- dbGetQuery(con, "SELECT * FROM Actinopterygii_database.parquet")
