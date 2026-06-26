# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

`sits` is an R package (v1.5.4) for satellite image time series analysis and land use/cover classification using Earth observation data cubes. It connects to cloud image providers (AWS, MPC, BDC, CDSE, DEAfrica, etc.), builds regularized data cubes, trains ML/DL models, classifies rasters, and post-processes results.

Minimum requirements: 16 GB RAM and 4 CPU dual-core.

## Common Commands

All development is done from within R. There is no Makefile.

```r
# Install dependencies and build
devtools::install_deps()
devtools::build()

# Compile C++ code (Rcpp/RcppArmadillo)
Rcpp::compileAttributes()
devtools::build(vignettes = FALSE)

# Regenerate documentation from roxygen2 comments
devtools::document()

# Run all tests (requires SITS_RUN_TESTS env var)
Sys.setenv("SITS_RUN_TESTS" = "YES")
devtools::test()

# Run a single test file
testthat::test_file("tests/testthat/test-ml.R")

# Run R CMD CHECK
devtools::check()

# Enable/disable example execution
Sys.setenv("SITS_RUN_EXAMPLES" = "YES")  # or "NO"
```

Tests that require external cloud access use `testthat::skip_if()` checks and will silently skip if the service is unreachable. Tests run in this order per `Config/testthat/start-first`: cube, raster, regularize, data, ml.

## Architecture

### Two-Layer Code Organization

The `R/` directory follows a strict two-layer pattern:

- **`sits_*.R`** — Public API exported to users. These functions are documented with roxygen2, exported in NAMESPACE, and define the user-facing workflow (`sits_cube`, `sits_regularize`, `sits_train`, `sits_classify`, `sits_smooth`, `sits_label_classification`, etc.).
- **`api_*.R`** — Internal implementation layer (prefixed with `.`). Not exported, not directly callable by users. Contains helper functions, S3 generics for dispatch, and source-specific overrides.

### Core Data Types

All public functions consume and return one of three types:

1. **`sits` tibble** — Time series data. First six columns are metadata (lon, lat, start/end date, label, cube). The `time_series` column holds a tibble per row. All time series objects have class `sits`.

2. **`cube` tibble** — Image cube metadata organized by tile. Each row is one tile; the `file_info` column holds a nested tibble of file paths and dates. Specialized subtypes: `raster_cube`, `vector_cube`, `probs_cube`, `probs_vector_cube`, `uncertainty_cube`, `class_cube`.

3. **`ml_model` / `torch_model`** — ML/DL model closures output by `sits_train()`. All carry class `ml_model` plus a method-specific second class (e.g., `rfor_model`, `svm_model`, `torch_model`). The second class drives `plot` dispatch and determines GPU eligibility.

### OOP Conventions

- Use **S3** everywhere. No S4 allowed — it breaks existing dispatch logic.
- `torch`-based deep learning models use **R6** (required for `torch` compatibility). See `sits_tempcnn.R` and `api_torch.R` for the pattern.
- Use **generic functions** to replace `if-else` dispatch on type. Look at `sits_bands()` for a worked example.

### Configuration System

No literal values belong in R code. All constants live in YAML files under `inst/extdata/`:

| File | Purpose |
|---|---|
| `config.yml` | User-relevant parameters (visualization, plotting) |
| `config_internals.yml` | Developer-only parameters |
| `config_messages.yml` | All error messages |
| `config_colors.yml` | Default color table and legends |
| `sources/config_source_*.yml` | Per-provider STAC catalogue descriptions |

Access values via the `.conf()` function: `.conf("view", "leaflet_megabytes")`. Access messages via `.conf("messages", ".check_na_parameter")`.

User overrides: set `SITS_CONFIG_USER_FILE` env var or call `sits_config(config_user_file = ...)`. See `inst/extdata/config_user_example.yml` for format.

### Data Source / STAC Extensions

To add a new STAC catalogue:
1. Write a YAML descriptor in `inst/extdata/sources/` (follow `config_source_mpc.yml` as a template).
2. The generic STAC interface is in `api_source_stac.R` (uses `rstac`).
3. Override only provider-specific quirks in a new `api_source_<name>.R`.

### ML/DL Model Pattern

ML models are closures returned by `sits_train(samples, ml_method)`. Each method (e.g., `sits_rfor`, `sits_svm`, `sits_tempcnn`) returns a function that, when called, produces the closure. The closure classifies input values and carries training metadata. Once compatible with this interface, models work automatically with `sits_classify`.

For new `torch` DL models, study `sits_tempcnn.R` and `sits_lighttae.R`, and read the [Technical Annex](https://e-sensing.github.io/sitsbook/technical-annex.html).

### Parallelism and C++ Extensions

- Parallel processing uses R's `parallel` package; cluster state is managed in `sits_env` (a package-level environment). See `api_parallel.R`.
- Performance-critical operations are implemented in C++ via Rcpp/RcppArmadillo. Sources are in `src/`. After modifying `.cpp` files run `Rcpp::compileAttributes()`.
- `sits_env` (defined in `zzz.R`) is the global state store — holds the config, cluster handle, debug flag, and model formula.

### Tidyverse Conventions

Internal code uses `dplyr`/`tidyr` for data wrangling, `purrr`/`slider` for iteration, and `lubridate` for dates. Spatial operations use `sf` and `terra`.
