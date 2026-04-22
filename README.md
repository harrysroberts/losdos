# losdos

**L**evel-**o**f-**s**ervice attribute estimation using _**d**odgr_ with the **O**rdnance **S**urvey Multimodal Routing Network.

## Overview

`losdos` is an R package that computes distance and time estimates for origin-destination pairs across multiple transportation modes (walk, bicycle, and car) using the UK Ordnance Survey Multimodal Routing Network (OSMRN). 

The package incorporates:
- **Slope-aware walk times** (Weidmann model)
- **Slope-aware bike times** (Parkin-Rotheram model)
- **Time-varying car speeds** across 14 different time periods
- **Turn restrictions** integrated from the OSMRN
- **Minimum-time routing** via the `dodgr` package

All distances and times are computed along minimum-time paths and returned in standard units (metres for distance, minutes for time).

## Installation

### Prerequisites

You will need:
- **R** >= 4.0
- **GDAL** and **GEOS** libraries (for `sf` spatial operations)
- OSMRN data files: `osmrn.gpkg` — OS Multimodal Routing Network (GeoPackage format)
- Study area boundary `boundary.gpkg` —  (GeoPackage format, EPSG:27700 (BNG))

### Install from GitHub

```r
# Install devtools if you haven't already
if (!require("devtools", quietly = TRUE)) install.packages("devtools")

# Install losdos
devtools::install_github("harrysroberts/losdos")
```

## Usage

### Basic Example

```r
library(losdos)

# Prepare your trips dataset
trips <- data.frame(
  trip_id = c(1, 2, 3),
  period = c("MoFr09001200", "MoFr09001200", "SaSu14001900"),
  from_easting = c(432500, 432600, 432700),
  from_northing = c(434200, 434300, 434400),
  to_easting = c(432700, 432800, 432900),
  to_northing = c(434400, 434500, 434600)
)

# Compute mode and time-specific routing attributes
results <- osmrn_trip_attributes(
  trips,
  walk_speed = 4.824,      # km/h (default MatSim value)
  bike_speed = 21.636      # km/h (default MatSim value)
)

# View results
head(results)
```

### Input Requirements

The `trips` data frame must include:
- `trip_id` — Unique trip identifier
- `period` — Time period code (one of 14 periods: e.g., "MoFr09001200", "SaSu14001900")
- `from_easting`, `from_northing` — Origin coordinates (EPSG:27700)
- `to_easting`, `to_northing` — Destination coordinates (EPSG:27700)

### Available Time Periods

The OSMRN includes speeds for 14 time periods:

**Weekdays (Mo-Fr):**
- `MoFr04000700`, `MoFr07000900`, `MoFr09001200`, `MoFr12001400`
- `MoFr14001600`, `MoFr16001900`, `MoFr19002200`, `MoFr22000400`

**Weekends (Sa-Su):**
- `SaSu04000700`, `SaSu07001000`, `SaSu10001400`, `SaSu14001900`
- `SaSu19002200`, `SaSu22000400`

### Output

The function returns the input trips data frame with additional columns:

| Column | Description |
|--------|-------------|
| `walk_distance` | Walking distance (metres) |
| `walk_time` | Walking time (minutes) |
| `bike_distance` | Cycling distance (metres) |
| `bike_time` | Cycling time (minutes) |
| `car_distance` | Driving distance (metres) |
| `car_time` | Driving time (minutes) |

### Custom Walk/Bike Speeds

```r
# Use custom speeds
results <- osmrn_trip_attributes(
  trips,
  walk_speed = 5.0,        # km/h
  bike_speed = 20.0        # km/h
)
```

## Data Setup

Place your OS-MRN data files in the working directory:

```
working_directory/
├── input/
│   └── raw/
│       ├── boundary.gpkg        # Study area boundary
│       └── osmrn.gpkg           # OS Multimodal Routing Network
```

The first call to `osmrn_trip_attributes()` will automatically:
1. Process the raw OS-MRN files
2. Filter to your study area boundary
3. Compute slope-adjusted walk and bike times
4. Expand car speeds by time period
5. Cache the processed networks in `input/processed/`

Subsequent calls will reuse the cached data for speed.

## Package Functions

### Main Function
- **`osmrn_trip_attributes()`** — Compute distance and time for origin-destination pairs

### Internal Functions
- `create_base_network()` — Build base network with all time-of-day variations
- `generate_modal_networks()` — Extract modal (walk/bike/car) networks
- `generate_origin_links()` — Connect trip origins to the network
- `generate_destination_links()` — Connect trip destinations to the network
- `generate_augmented_networks()` — Append origin/destination links to networks
- `generate_dual_networks()` — Convert to dual representation with turn restrictions
- `compute_attributes()` — Compute distance/time via dodgr routing

## References

The package implements models from:
- **Walking times:** Weidmann (1993) doi: [10.3929/ethz-a-000687810](https://doi.org/10.3929/ethz-a-000687810)
- **Cycling times:** Parkin and Rotheram (2010) doi: [10.1016/j.tranpol.2010.03.001](https://doi.org/10.1016/j.tranpol.2010.03.001)

as implement in [MATSim](https://github.com/matsim-org) by Horni et al. (2016) doi: [10.5334/baw](https://doi.org/10.5334/baw)

## License

MIT License. See LICENSE file for details.

## Author

Harry Roberts ([ts22hr@leeds.ac.uk](mailto:ts22hr@leeds.ac.uk))

Institute for Transport Studies, University of Leeds

