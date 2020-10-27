
# medicareR <img src="man/figures/logo.png" width = "175" height = "200" align="right" /> 
A package that create analysis ready datasets from Medicare data. The code were designed specifically for UofM IHPI Medicare data format. however, it can also be generalized to other medicare data format. This package is used at MEDLab at the department of surgery, University of Michigan

<img src="man/figures/MEDlab.png" /> 

## Summary

The goal of medicareR is to make standardized medicare datasets across years including: 

- MBSF
- MedPAR
- Carrier Line 
- Carrier Claim

## Installation

You can install medicareR from GitHub with devtools:

``` r
devtools::install_github("https://github.com/UMCSTaR/medicareR")
```

## Example
```r
# filter procedures
define_proc_by_cpt <- readr::read_csv(paste0("your-data-path1", "cpt_ecs_map.csv"))
  
analytic_cpt = medicareR::procedure_selection(
  std_data_root = "your-data-path2",
  prof_codes_folder = "prof_clm",
  cpt_map = define_proc_by_cpt
)
```
