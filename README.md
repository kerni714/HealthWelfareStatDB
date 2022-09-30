<!-- badges: start -->
[![R-CMD-check](https://github.com/kerni714/InpatientDiagnosisSdb/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/kerni714/InpatientDiagnosisSdb/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# HealtWelfareStatDB
R client for the Statistical Database web API at the Swedish National Board of 
Health and Welfare (Socialstyrelsen).

The Statistical Database at the Swedish National Board of Health and Welfare 
contains information on health, utilization of care, social services and 
personnel in health care. The statistical database makes it possible to produce 
tables, diagrams and maps. For more information and documentation, see
https://www.socialstyrelsen.se/statistik-och-data/statistik/statistikdatabasen.

The HealthWelfareStatDB package contains functions for connecting to the 
database web API and download data directly in R. The package has been developed
to work for version 1 (v1) of the API. Documentation of the API can be found at https://sdb.socialstyrelsen.se/sdbapi.aspx. 


**Note**: there seem to be a limitation in the API in that it cannot handle
queries for multiple measure categories. For all other variables, it seems to
be possible to query multiple categories in the same query.

**Disclaimer**: the R code in this package has been written to be general and work
for all parts of the API. However, formal testing has only been carried out for
language: English and topic: In-Patient Care Diagnoses. 

## Installation

You can install the development version of this package from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("https://github.com/kerni714/HealthWelfareStatDB", build_vignettes = TRUE)
```