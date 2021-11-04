
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rbackupr

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of rbackupr is to facilitate backups to Google Drive from R.

## Installation

You can install the development version of rbackupr from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("giocomai/rbackupr")
```

## Core concepts

By default, when you use the `googledrive` package you give access to
all of your Google Drive. This has a number of shortcomings, including
the fact that one may have a very large number of files there, as well
the feeling that one may be giving too much access to scripts.

While you can give the kind of access to Google Drive you wish, this
package is developed based on the expectation that it will have only
access to the “drive.file” scope, i.e. that it will have access only to
folders created with this app. In order to reduce the impact of some of
the issues that come with this choice, this app tries to cache locally
some metadata about remote folders and files in order to speed up
processing.

## How to use

In order to show how this package works, we’ll first create some files
with random data in the temporary folder. So this is how our base folder
looks like: a main folder, with some subfolders, and some files within
each of them.

``` r
library("rbackupr")
base_temp_folder <- fs::dir_create(fs::path(tempdir(), "rbackupr_testing"))

subfolder_names <- stringr::str_c("data_", sample(x = 10:100, size = 3, replace = FALSE))

purrr::walk(
  .x = subfolder_names,
  .f = function(x) {
    current_folder <- fs::path(base_temp_folder, x)
    fs::dir_create(path = current_folder)
    current_number <- stringr::str_extract(string = x, pattern = "[[:digit:]]+") %>% 
      as.numeric()
    
    purrr::walk(.x = rep(NA, sample(x = 2:5, size = 1)), .f = function(x){
          readr::write_csv(x = tibble::tibble(numbers = rnorm(n = 10, mean = current_number)), 
                     file = fs::file_temp(pattern = stringr::str_c("spreadsheet_", current_number, "_"),
                                          tmp_dir = current_folder,
                                          ext = "csv"))
    })
  })


fs::dir_tree(base_temp_folder)
#> /tmp/RtmposzxgC/rbackupr_testing
#> ├── data_34
#> │   ├── spreadsheet_34_308d406454e4ac.csv
#> │   └── spreadsheet_34_308d4072d6f699.csv
#> ├── data_58
#> │   ├── spreadsheet_58_308d402d16eafd.csv
#> │   ├── spreadsheet_58_308d404286c9bf.csv
#> │   └── spreadsheet_58_308d40597c2b20.csv
#> └── data_74
#>     ├── spreadsheet_74_308d4015050e07.csv
#>     ├── spreadsheet_74_308d403d9f96fc.csv
#>     └── spreadsheet_74_308d406e742b69.csv
```
