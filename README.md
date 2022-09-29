<!-- badges: start -->
[![R-CMD-check](https://github.com/kerni714/InpatientDiagnosisSdb/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/kerni714/InpatientDiagnosisSdb/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# InpatientDiagnosisSdb
R client for the Statistical Database, In-patient Care Diagnoses API from 
Swedish National Board of Health and Welfare

The HealthWelfareStatDB package contains functions for connecting to the web API 
for the Statistical Database at the Swedish National Board of Health and Welfare
(Socialstyrelsen). The documentation for the API can be found at: 
https://sdb.socialstyrelsen.se/sdbapi.aspx. This package has been developed 
to work for version 1 of the API.

Note: there seem to be a limitation in the API in that it cannot handle
queries for multiple measure categories. For all other variables, it seems to
be possible to query multiple categories in one query.

Disclaimer: the R code in this package has been written to be general and work
for all parts of the API. However, formal testing has only been carried out for
language: English and topic: In-Patient Care Diagnoses. 