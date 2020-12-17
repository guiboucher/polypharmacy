
<!-- README.md is generated from README.Rmd. Please edit that file -->

# polypharmacy IN DEVELOPMENT <img src="man/figures/sticker/stiker2.png" width = "150" align="right" />

2020-12-17

This package analyse prescription drugs deliveries to calculate several
indicators of polypharmacy corresponding to the various definitions
found in the literature.

It is essential to know the concepts used to calculate the various
polypharmacy indicators to adequately use this package.  
The core of the package is the `data_process()` function that creates
the `data.table` of pharmacists drug deliveries by restructuring the
drug delivery records (usually extracted from a pharmacy or a health
insurance information system) into continuous periods of drug
availability, applying user-defined arguments such as the grace periods
between renewals or the longest treatment duration that an individual
may accumulate through the successive renewals.

Then, each polypharmacy indicator can be computed using the
corresponding function (`ind_simult()`, `ind_stdcumul()`,
`ind_wcumul()`, `ind_stdcontinuous()`, `ind_ucontinuous()`) or using the
overall function `indicators()` and select all the desired indicator(s)
to be calculated at once.

Prior to running `data_process()` the user may need to pre-process the
table of original drug delivery records to break down combination drug
into their individual components (`drugs_bkdn()`) and/or to overwrite
the treatment duration of specified drugs with constant time periods
(`cst_trt_dur()`).

## Installation

<!-- You can install the released version of polypharmacy from [CRAN](https://CRAN.R-project.org) with: -->

<!-- ``` r -->

<!-- install.packages("polypharmacy") -->

<!-- ``` -->

<!-- And the development version from [GitHub](https://github.com/guiboucher/polypharmacy) with: -->

``` r
# install.packages("remotes")
remotes::install_github("guiboucher/polypharmacy")
```

## References
