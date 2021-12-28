
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rbackupr

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of rbackupr is to facilitate online backups to from R. At the
current stage, it facilitates backups only to Google Drive, but may
support other services in the future.

## Core motivation

By default, when you use the `googledrive` package you give access to
all of your Google Drive. This has a number of shortcomings, in
particular if you use Google Drive for different reasons and you have
many files and folders stored there:

-   going thrhough a lot of files, `googledrive` is slow in finding what
    you need
-   in particular if you use `googledrive` from a remote server, you may
    feel unconfortable in potentially leaving an open door to *all* of
    the files you have stored there
-   even if you use it locally, you may be concerned of the mistakes you
    could make by giving full access to all of your files to scripts
    written by you, or to packages published by a random GitHub user
    (such as the author of this package)

While you can give full access to Google Drive if you so wish,
`rbackupr` is developed based on the expectation that it will have only
access to the [“drive.file” scope of Google
Drive](https://developers.google.com/drive/api/v2/about-auth), i.e. that
it have “access to files created or opened by the app”. In brief, of
course you still need to trust the scripts you are running, but at least
you can be sure that it will not mess with completely unrelated files
you keep on Google Drive.

In order to reduce the impact of some of the issues that come with this
choice, this app caches locally metadata about remote folders and files
in order to speed up processing.

## Installation

You can install the development version of `rbackupr` from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("giocomai/rbackupr")
```

## How to use

In order to show how this package works, we will first create some files
with random data in the temporary folder. So this is how our base folder
looks like: a main folder, with some sub-folders, and some files within
each of them.

``` r
library("rbackupr")
base_temp_folder <- fs::dir_create(fs::path(tempdir(),
                                            "rbackupr_testing"))

subfolder_names <- stringr::str_c("data_",
                                  sample(x = 10:100, size = 3, replace = FALSE))

purrr::walk(
  .x = subfolder_names,
  .f = function(x) {
    current_folder <- fs::path(base_temp_folder, x)
    fs::dir_create(path = current_folder)
    current_number <- stringr::str_extract(string = x, pattern = "[[:digit:]]+") %>% 
      as.numeric()
    
    purrr::walk(
      .x = rep(NA, sample(x = 2:5, size = 1)),
      .f = function(x){
        readr::write_csv(x = tibble::tibble(numbers = rnorm(n = 10, mean = current_number)), 
                         file = fs::file_temp(pattern = stringr::str_c("spreadsheet_", current_number, "_"),
                                              tmp_dir = current_folder,
                                              ext = "csv"))
      })
  })


fs::dir_tree(base_temp_folder)
#> /tmp/RtmpSnjBZe/rbackupr_testing
#> ├── data_34
#> │   ├── spreadsheet_34_37a825275244.csv
#> │   └── spreadsheet_34_37a83e2eeda0.csv
#> ├── data_58
#> │   ├── spreadsheet_58_37a8294aa524.csv
#> │   ├── spreadsheet_58_37a83f83b781.csv
#> │   └── spreadsheet_58_37a84ad09f72.csv
#> └── data_74
#>     ├── spreadsheet_74_37a81f155b88.csv
#>     ├── spreadsheet_74_37a84b9a03ef.csv
#>     └── spreadsheet_74_37a86a250ed9.csv
```

By defaukt, `rbackupr` does not cache metadata files and folders it
stores on Google Drive, but you are strongly encouraged to do so. You
can do so with `rb_enable_cache()`. By default, metadata will be cached
in the current active directory, but if you use `rbackupr` with
different projects, you may not want to leave files with cached metadata
scattered around your local drive, but rather keep them in place where
they can be accessed. Something like this could typically be included at
the beginning of `rbackupr` backup scripts.

``` r
library("rbackupr")
rb_enable_cache()

rb_set_cache_folder(path = fs::path(fs::path_home_r(),
                                    "R",
                                    "rb_data"))

rb_create_cache_folder(ask = FALSE)
```

``` r
rb_get_cache_folder()
```
