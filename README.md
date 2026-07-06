# losdos

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.21222208.svg)](https://doi.org/10.5281/zenodo.21222208)

**L**evel-**o**f-**s**ervice attribute estimation using _**d**odgr_ with the **O**rdnance **S**urvey Multimodal Routing Network.

`losdos` is an R package that computes distance and time estimates for origin-destination pairs across multiple transportation modes (walk, bicycle, and car) using the UK Ordnance Survey Multimodal Routing Network (OSMRN). 

This method is described in detail in the paper:
> Roberts, H. S., Calastri, C., Batley, R. (under review) "Evaluating open-source approaches for estimating level-of-service attributes in transport choice modelling". Manuscript submitted for publication.

The package incorporates:

- **Time-varying and traffic-sensitive car speeds** across 14 different time periods, based on real average speed data
- **Slope-aware walk times** (Weidmann model)
- **Slope-aware bike times** (Parkin-Rotheram model) that are **traffic-sensitive** and include the **option to walk-and-push**
- **Turn restrictions** integrated from the OSMRN
- **Minimum-time routing** via the `dodgr` package

All distances and times are computed along minimum-time paths and returned in standard units (metres for distance, minutes for time).

## Installation

### Prerequisites

You will need:
- **R** version 4.0 or above
- OSMRN data files: `osmrn.gpkg` — OS Multimodal Routing Network (GeoPackage format)
- Study area boundary `boundary.gpkg` —  (GeoPackage format, EPSG:27700 (BNG))

### OSMRN Dataset

This package is designed to work with the Multimodal Routing Network, published by the Ordnance Survey. **The dataset is not included in this package** and must be obtained separately by the user.

- **Source:** [Official website](https://docs.os.uk/os-downloads/products/transport-network-portfolio/os-multi-modal-routing-network)
  -  It is also available for educational and research purposes through the [EDINA Digimap platform](https://digimap.edina.ac.uk/)
- **License:** Use of the dataset is subject to the Ordnance Survey's licensing terms. **Users are responsible for reviewing and complying with these terms.**
- **Format expected:** The package expects the dataset in Geopackage format as published, with no further adjustment by the user.

### Install from GitHub

```r
# Install devtools if you haven't already
if (!require("devtools", quietly = TRUE)) install.packages("devtools")

# Install losdos
devtools::install_github("harrysroberts/losdos")
```

## Usage

There are two main functions for computing attributes:

- `osmrn_trip_attributes()` — Compute distance and time by each mode for a set of defined trips with fixed origins, destinations and time periods
- `osmrn_matrix_attributes()` — Compute walk/bike/car distance and time for all pairs of origins and destinations across specified time periods


### Basic Examples

Example of `osmrn_trip_attributes()`:

```r
library(losdos)

# Prepare your trips dataset
trips <- data.frame(
  id = c(1, 2, 3),
  period = c("MoFr09001200", "MoFr19002200", "SaSu14001900"),
  from_easting = c(429180, 427750, 435741),
  from_northing = c(434731, 435747, 432124),
  to_easting = c(429906, 430454, 430731),
  to_northing = c(433271, 433532, 441858)
)

# Compute mode and time-specific attributes
results <- osmrn_trip_attributes(trips)

# View output
results

#        id       period ... walk_distance walk_time bike_distance bike_time car_distance car_time
# 1       1 MoFr09001200 ...      2143.134  26.32291      2145.671  10.09154     2588.870 10.59448
# 2       2 MoFr19002200 ...      4226.703  52.46928      4079.125  14.82283     4181.384 10.42310
# 3       3 SaSu14001900 ...     13172.694 164.69240     13061.157  44.89270    15626.795 28.32776
```

Example of `osmrn_matrix_attributes()`:

```r
library(losdos)

# Prepare your origins dataset
origins <- data.frame(
  id = c(1, 2, 3),
  easting = c(429180, 427750, 435741),
  northing = c(434731, 435747, 432124)
)

# Prepare your destinations dataset
destinations <- data.frame(
  id = c(1, 2, 3),
  easting = c(429906, 430454, 430731),
  northing = c(433271, 433532, 441858)
)

# Compute mode and time-specific attributes for periods of interest
results <- osmrn_matrix_attributes(
  origins,
  destinations,
  periods =  c("MoFr09001200", "MoFr19002200", "SaSu14001900")
)

# View output
results

#    origin destination       period ... walk_distance walk_time bike_distance bike_time car_distance  car_time
# 1       1           1 MoFr09001200 ...      2143.134  26.32291      2145.671 10.091541     2588.870 10.594476
# 2       1           1 MoFr19002200 ...      2143.134  26.32291      2145.671  9.686857     2588.870  8.842980
# 3       1           1 SaSu14001900 ...      2143.134  26.32291      2148.427  9.844002     2588.870  9.014744
# 4       1           2 MoFr09001200 ...      2158.624  26.53713      2158.624 11.197209     2551.594  8.479723
# 5       1           2 MoFr19002200 ...      2158.624  26.53713      2158.624  9.109990     2551.594  6.614542
# 6       1           2 SaSu14001900 ...      2158.624  26.53713      2158.624  9.572531     2551.594  7.865528 
# 7       1           3 MoFr09001200 ...      8761.434 110.58688      8664.031 32.905502     9717.246 18.881990
# 8       1           3 MoFr19002200 ...      8761.434 110.58688      8664.031 31.731030     9717.246 17.302694
# 9       1           3 SaSu14001900 ...      8761.434 110.58688      8664.031 32.861117     9717.246 18.127696
# ...
# 25      3           3 MoFr09001200 ...     13172.694 164.69240     13061.157 45.259649    15626.795 30.226330
# 26      3           3 MoFr19002200 ...     13172.694 164.69240     13061.157 44.158099    15626.795 26.941794
# 27      3           3 SaSu14001900 ...     13172.694 164.69240     13061.157 44.892704    15626.795 28.327760
```

### Input Requirements

The `trips` data frame must include:
- `id` — Unique trip identifier
- `period` — Time period code (one of 14 periods: e.g., "MoFr09001200", "SaSu14001900")
- `from_easting`, `from_northing` — Origin coordinates (EPSG:27700)
- `to_easting`, `to_northing` — Destination coordinates (EPSG:27700)

The `origins` data frame must include:
- `id` — Unique origin identifier
- `easting`, `northing` — Origin coordinates (EPSG:27700)

The `destinations` data frame must include:
- `id` — Unique destination identifier
- `easting`, `northing` — Destination coordinates (EPSG:27700)

### Available Time Periods

The OSMRN includes speeds for 14 time periods:

**Weekdays (Mo-Fr):**
- `MoFr04000700`, `MoFr07000900`, `MoFr09001200`, `MoFr12001400`
- `MoFr14001600`, `MoFr16001900`, `MoFr19002200`, `MoFr22000400`

**Weekends (Sa-Su):**
- `SaSu04000700`, `SaSu07001000`, `SaSu10001400`, `SaSu14001900`
- `SaSu19002200`, `SaSu22000400`

### Output

The `osmrn_trip_attributes()` function returns the input trips data frame with additional columns:

| Column | Description |
|--------|-------------|
| `walk_distance` | Walking distance (metres) |
| `walk_time` | Walking time (minutes) |
| `bike_distance` | Cycling distance (metres) |
| `bike_time` | Cycling time (minutes) |
| `car_distance` | Driving distance (metres) |
| `car_time` | Driving time (minutes) |

The `osmrn_matrix_attributes()` function returns a data frame with one row per origin-destination-period combination, including the same distance and time columns as above.

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
1. Process the raw OSMRN files
2. Filter to your study area boundary
3. Compute walk, bike and car distance and time attributes for each trip
5. Save the processed networks in `input/processed/`

Subsequent calls will reuse the pre-processed data for speed.

Both functions also allow for caching and retrieving modal networks from `input/processed/` to speed up repeated analyses. The logical argument `make_cache = TRUE` will save the processed networks for future use, while `use_cache = TRUE` (default setting) will load from cache if available.

## Package Functions

### Main Function
- **`osmrn_trip_attributes()`** — Compute distance and time by each mode for origin-destination pairs
- **`osmrn_matrix_attributes()`** — Compute walk/bike/car distance and time for all origin-destination pairs across specified periods

### Internal Functions
- `create_base_network()` — Build base network with all time-of-day variations
- `generate_modal_networks()` — Extract modal (walk/bike/car) networks, one for each time period in the case of bike and car
- `generate_origin_links()` — Create links connecting trip origins to the nearest network node
- `generate_destination_links()` — Create links connecting trip destinations to the nearest network node
- `generate_augmented_networks()` — Append origin/destination links to each modal network
- `generate_dual_networks()` — Convert to dual representation with turn restrictions
- `compute_trip_attributes()` — Compute distance/time of each trip by each mode via dodgr routing
- `compute_matrix_attributes()` — Compute distance/time by each mode for all origin-destination pairs and specified periods via dodgr routing

## References

The package implements models from:
- **Walking times:** Weidmann (1993) doi: [10.3929/ethz-a-000687810](https://doi.org/10.3929/ethz-a-000687810)
- **Cycling times:** Parkin and Rotheram (2010) doi: [10.1016/j.tranpol.2010.03.001](https://doi.org/10.1016/j.tranpol.2010.03.001)

as implement in [MATSim](https://github.com/matsim-org) by Horni et al. (2016) doi: [10.5334/baw](https://doi.org/10.5334/baw)

## Citation

If you use this package in your research, the following citations are appreciated:

**Paper describing the method:**

 Roberts, H. S., Calastri, C., Batley, R. (under review) "Evaluating open-source approaches for estimating level-of-service attributes in transport choice modelling". Manuscript submitted for publication.

**This software:**

Roberts, H.S. (2026) “losdos”. Zenodo. doi:10.5281/zenodo.21222208.

**Dependency:**

This package builds on [dodgr](https://github.com/UrbanAnalyst/dodgr). Please also cite that package.

Padgham, M. (2019) "dodgr: An R package for network flow aggregation." Transport Findings, 2(14). doi:10.32866/6945

**OSMRN dataset:**

This package is intended to be used in conjunction with the Ordnance Survey Multimodal Routing Network. Users of this dataset are encouraged to cite their use of this dataset.

Ordnance Survey (2026) "Multi-modal Routing Network". url:https://www.ordnancesurvey.co.uk/products/os-multi-modal-routing-network 

**BibTeX:**

```bibtex
@article{roberts_under_review_evaluating,
  author      = {Roberts, Harry Samuel and Calastri, Chiara and Batley, Richard},
  title       = {{Paper Title}},
  year        = {under review},
  note        = {Manuscript submitted for publication}
}

@software{roberts_2026_21222208,
  author      = {Roberts, Harry Samuel},
  title       = {losdos},
  month       = jul,
  year        = 2026,
  publisher   = {Zenodo},
  version     = {v1.0.0},
  doi         = {10.5281/zenodo.21222208},
  url         = {https://doi.org/10.5281/zenodo.21222208},
}

@Article{padgham_2019_dodgr,
  journal     = {Transport Findings},
  doi         = {10.32866/6945},
  publisher   = {Network Design Lab},
  title       = {dodgr: An R package for network flow aggregation},
  author      = {{Mark Padgham}},
  year        = {2019},
  month       = {2},
}

@misc{ordnance_survey_multi-modal_2026,
  title       = {Multi-modal {Routing} {Network}},
  url         = {https://www.ordnancesurvey.co.uk/products/os-multi-modal-routing-network},
  urldate     = {2026-03-18},
  author      = {{Ordnance Survey}},
  year        = {2026},
}
```
## License

MIT License. See LICENSE file for details.

## Author

Harry Roberts ([ts22hr@leeds.ac.uk](mailto:ts22hr@leeds.ac.uk))

Institute for Transport Studies, University of Leeds
