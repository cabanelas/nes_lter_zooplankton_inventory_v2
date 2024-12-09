# nes_lter_zooplankton_inventory_v2
# NES-LTER Zooplankton Inventory Data Package Scripts
This repository contains R scripts for creating the NES-LTER zooplankton sample inventory data package submitted to the Environmental Data Initiative (EDI) repository. The data package provides an inventory of zooplankton samples collected during Northeast U.S. Shelf Long-Term Ecological Research (NES-LTER) Transect cruises, ongoing since 2017.

## Repository Contents and Script Workflow
The following scripts are included in this repository. Run them in the listed order for best results:
1. `Ship_speed_data.R`
- This script processes ship speed data.
- It can be run either before or after All_zp_net_bongo_logs.R
2. `All_zp_net_bongo_logs.R`
- Processes data from zooplankton net and bongo logs.
- Can be run before or after Ship_speed_data.R.
3. `Shipspeed_eventlog_merge.R`
- Adds ship speed data to the event log data.
4. `bongoEventLogMerge.R`
- Processes and merges bongo-specific event log data.
5. `Volume_water_sampled_calculations.R`
- Calculates the volume of water sampled during net tows.

The output includes a tidy dataset ready for inclusion in the NES-LTER data package.

## Data Package Overview

### Title
Zooplankton Sample Inventory for Northeast U.S. Shelf Long Term Ecological Research (NES-LTER) Transect Cruises, Ongoing Since 2017

### Summary 

This dataset provides an inventory of physical zooplankton samples collected using Bongo and/or ring nets during the NES-LTER Transect cruises. The NES-LTER transect, located south of Martha’s Vineyard, Massachusetts, consists of standard stations L1–L11, extending 150 km offshore.

Key details:
- Sampling periods: Seasonal (winter, spring, summer, fall).
- Net types and mesh sizes:
-- Bongo net (150 µm and 335 µm mesh).
-- Ring net (20 µm mesh and 150 µm mesh for vertical tows in earlier cruises).
- Sample purposes: DNA metabarcoding, stable isotope analysis, and morphological identification.
- Collaborations: Includes data collected in partnership with the Ocean Observatories Initiative during early cruises.

## Requirements 

These scripts require the following R packages: 
- here
- readr
- dplyr
- lubridate
- tidyverse
- maps
- sf
- listviewer
- tidyr
For plotting:
- RColorBrewer
- ggthemes
- ggpubr
- cowplot
- ggplot2
- plotly

Install the required packages using:

```r
install.packages(c("here", "readr", "dplyr", "lubridate", "tidyverse", 
                   "ggplot2", "maps", "sf", "plotly", "listviewer", 
                   "tidyr", "RColorBrewer", "ggthemes", "ggpubr", "cowplot"))
