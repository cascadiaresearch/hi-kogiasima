# hi-kogiasima
Scripts used for analyses of dwarf sperm whale sightings and associated data for: Baird, R.W., S.D. Mahaffy, and J.K. Lerma. 2021. Site fidelity Spatial Use, and Behavior of Dwarf Sperm Whales in Hawaiian Waters: Using Small-Boat Surveys, Photo-Identification, and Unmanned Aerial Systems to Study a Difficult-to-Study Species. *Marine Mammal Science* 38(1):326-348. doi:10.1111/mms.12861. 

## DistBTSightings.R
Script to calculate distances between all possible pairs of sightings.

## Kogiasima_Drone_DistBearing.R
Script to estimate distances and bearings between consecutive drone locations during flights over dwarf sperm whales. Script also includes code to generate KMLs of individual drone segments.

## Kogiasima_HI_Maps.R
Script to generate two maps: (1) all CRC-only Ks sightings overlaid on vessel effort tracklines off Hawaii Island, with tracklines colored by sea state (low: B0-2, high: >B2); (2) map of all HIKs020 sightings (including non-CRC) with bathymetric depth contour lines.

## Kogiasima_SuppMap_EffortSight.R
Script to generate supplemental figure of sightings/effort tracklines off Maui Nui, Oahu, and Kauai/Niihau (stacked). Similar to Hawaii Island, effort tracklines are colored by sea state.
