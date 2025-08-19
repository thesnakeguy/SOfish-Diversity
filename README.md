# Welcome to SOfish Diversity! 🎣

This application allows you to explore the diversity of fish species in the Southern Ocean.

***

### Key Features
* **Interactive Map:** Visualize species occurrences on a live map. You can zoom in, pan, and draw a polygon to filter data in a specific area.
* **Flexible Filters:** Use the **`Filters`** tab to narrow down your search by taxonomy, traits, IUCN status, and more.
* **Diversity Analysis:** The app calculates and displays diversity metrics for your filtered data, both spatially (by MEASO region) and temporally (as a moving average over time).
* **Temporal Trends:** The **`Temporal Diversity`** tab shows how diversity metrics like the Shannon Index and Species Richness change over time, alongside a measure of sampling effort.
* **H3 Diversity Grid:** The **`Spatial H3 Polygons`** tab flexibly visualizes different diversity estimators.

***

### How to Use
1.  Navigate to the **`Filters`** tab to set your desired criteria.
2.  Use the map on the **`Display`** tab to visualize the results.
3.  Draw a polygon on the map to define a custom area of interest.
4.  Check the different tabs to see the calculated metrics and maps based on your filters.

***

### Data Source
All occurrence data is sourced from the GBIF and OBIS. Traits are sourced from Fishbase.<br>
GBIF Occurrence Download https://www.gbif.org/occurrence/download/0019157-250802193616735 Accessed from R via rgbif (https://github.com/ropensci/rgbif) on 2025-08-06
