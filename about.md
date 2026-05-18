# SOfish Diversity

**Explore the fish diversity of the Southern Ocean — filter, map, and analyse occurrence data enriched with taxonomy and biological traits.**

---

### What this app does

| Tab | Purpose |
|---|---|
| **Display** | Interactive occurrence map — colour and size points by family, IUCN status, vulnerability, and more |
| **Filters** | Narrow records by taxonomy, body size, depth, ecology, conservation status, commercial traits, and record metadata |
| **Active Filters** | Review what filters are currently applied and how many records and species remain |
| **Table** | Browse and download the filtered occurrence dataset as CSV |
| **Spatial Diversity** | Diversity indicators (Shannon, Simpson, ES50, Hill numbers) overall and broken down by MEASO region |
| **Temporal Diversity** | Moving-average diversity trends over time, with sampling effort overlay |
| **Spatial H3 Polygons** | Hexagonal grid map of spatial diversity at any H3 resolution |

---

### Quick start

1. Open the **Filters** tab and select a taxon (e.g. a family or order) to scope the dataset.
2. Switch to **Display** to see matching occurrences on the map. Optionally draw a polygon to restrict results to a custom area.
3. Explore the **Spatial Diversity** and **Temporal Diversity** tabs to analyse patterns in your filtered data.
4. Download any table as CSV using the buttons within each tab.

---

### Data sources

- **Occurrences** — [GBIF](https://www.gbif.org/) and [OBIS](https://obis.org/)
- **Taxonomy** — [WoRMS](https://www.marinespecies.org/) (all scientific names and AphiaIDs are WoRMS-validated)
- **Biological traits** — [FishBase](https://www.fishbase.se/) (length, weight, depth range, vulnerability, commercial importance, etc.)
- **RAMS checklist** — [Register of Antarctic Marine Species](https://www.marinespecies.org/rams/)

---

<div style="background:#f0f6ff; border-radius:10px; padding:14px 18px; border:1px solid #c5d8ee; font-size:0.82em; line-height:1.7;">

**License** — Released under the [MIT License](https://opensource.org/licenses/MIT).

**Citation** — If you use this app or its data in your research, please cite the relevant data sources and this tool.  
GBIF occurrence download: <a href="https://doi.org/10.15468/dl.uf8fd2" target="_blank">doi:10.15468/dl.uf8fd2</a>

**Source code & issues** — <a href="https://github.com/thesnakeguy/SOfish-Diversity" target="_blank">github.com/thesnakeguy/SOfish-Diversity</a>

</div>
