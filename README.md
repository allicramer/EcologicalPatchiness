# Data and code for "When are bacteria really gazelles? Comparing patchy ecologies with dimensionless numbers"

This repository contains all data and scripts required to reproduce the analyses and figures in Urmy et al. (2022), "When are bacteria really gazelles? Comparing patchy ecologies with dimensionless numbers," published in *Ecology Letters*.

## File and directory descriptions

* `.gitignore` : Tells Git which files not to track.
* `basic_plots.R` : R script to perform basic analyses and produce plots.
* `clustering.R` : R script to perform clustering and produce related plots.
* `consolidate_units.jl` : Julia script to check dimensional consistency of source data and transform all phisical units to base SI.
* `EcologicalPatchiness.Rproj` : R project file.
* `Manifest.toml` and `Project.toml` : Record a snapshot of all the Julia packages used and their specific versions, to enable reproducibility.
* `data/`
  * `PatchinessData_QC.csv` : Source data on all interactions after manual quality-control checking.
  * `PatchinessData_processed.csv`: Same data as above, with physical units transformed and homogenized.
  * `patch_scales.csv` : Table of spatial and temporal scales of all patches, in meters/seconds
  * `patch_items.csv` : Table recording whether each patch is composed of multiple food items (e.g. a school of fish), a single item (e.g. a dead whale), or a continuous resource (e.g. dissolved nutrients).
* `graphics/` : Output directory for plots. Not tracked in git.
  
## How to run the analyses

0. Ensure that Julia (v1.6 or later) and R (v 4.0 or later) are available. They can be downloaded from https://julialang.org and https://r-project.org.

1. Install required packages. For Julia, open a Julia prompt in this directory. Type `]` to enter package-manager mode, then enter:  

    ```
    pkg> activate .
    pkg> instantiate
    ```  

    to install required packages at the versions used for this analysis. Press backspace to exit the package manager. For R, run:

    ```
    > install.packages(c("ggplot2", "tidyr", "dplyr", "stringr", "rgl", "BiocManager", "circlize", "scales"))
    > BiocManager::install("ComplexHeatmap")
    ```

2. Check and convert all units to base SI using the Julia script. From the Julia command line in this directory, run:  

    ```
    julia> include("consolidate_units.jl")
    ```

3. Run the analyses and produce plots. From the R prompt in this directory, run:

    ```
    > source("basic_plots.R")
    > source("clustering.R")
    ```
    
    All figures will be written to the `graphics` directory.