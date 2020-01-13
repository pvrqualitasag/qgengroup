
# qgengroup

<!-- badges: start -->
<!-- badges: end -->

The goal of qgengroup is to have a set of tools to create genetic groups for all types of analyses done at Qualitas AG. 

## Installation

You can install the latest version of qgengroup from [GitHub](https://github.com/pvrqualitasag/qgengroup) with:

``` r
# if (! require(remotes)) install.packages("remotes")
remotes::install_github("pvrqualitasag/qgengroup")
```

## Example

This is a basic example which shows you how to create genetic groups without considering countries:

``` r
library(qgengroup)
s_input_path <- system.file("extdata", "statGenGroupOutFile", package = "qgengroup")
s_output_fname_woc <- "genGrDef_functionMain_Default_withoutCountry.csv"
n_min_group_size <- 1500
n_number_of_years <- 5
create_GG_withoutCountry(psInputFile = s_input_path,
            pminGroupSize = n_min_group_size,
            pnumberOfYears = n_number_of_years,
            psOutputFile = s_output_fname_woc)
```

