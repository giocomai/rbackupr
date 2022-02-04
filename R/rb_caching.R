#' Creates the base cache folder where `rbackupr` caches data.
#'
#' @param ask Logical, defaults to TRUE. If FALSE, and cache folder does not exist, it just creates it without asking (useful for non-interactive sessions).
#'
#' @return Nothing, used for its side effects.
#' @export
#'
#' @examples
#' \donttest{
#' if (interactive()) {
#'   rb_create_cache_folder()
#' }
#' }
rb_create_cache_folder <- function(ask = TRUE) {
  if (fs::file_exists(rbackupr::rb_get_cache_folder()) == FALSE) {
    if (ask == FALSE) {
      fs::dir_create(path = rbackupr::rb_get_cache_folder(), recurse = TRUE)
    } else {
      usethis::ui_info(glue::glue("The cache folder {{usethis::ui_path(rb_get_cache_folder())}} does not exist. If you prefer to cache files elsewhere, reply negatively and set your preferred cache folder with `rb_set_cache_folder()`"))
      check <- usethis::ui_yeah(glue::glue("Do you want to create {{usethis::ui_path(rb_get_cache_folder())}} for caching data?"))
      if (check == TRUE) {
        fs::dir_create(path = rbackupr::rb_get_cache_folder(), recurse = TRUE)
      }
    }
    if (fs::file_exists(rbackupr::rb_get_cache_folder()) == FALSE) {
      usethis::ui_stop("This function requires a valid cache folder.")
    }
  }
}


#' Set folder for caching data
#'
#' Consider using a folder out of your current project directory, e.g. `rb_set_cache_folder("~/R/rbackupr_data/")`: you will be able to use the same cache in different projects, and prevent cached files from being sync-ed if you use services such as Nextcloud or Dropbox.
#'
#' @param path A path to a location used for caching data. If the folder does not exist, it will be created.
#'
#' @return The path to the caching folder, if previously set; the same path as given to the function; or the default, `rbackupr_data` is none is given.
#' @export

#' @examples
#' \donttest{
#' if (interactive()) {
#'   rb_set_cache_folder(fs::path(fs::path_home_r(), "R", "rbackupr_data"))
#' }
#' }
rb_set_cache_folder <- function(path = NULL) {
  if (is.null(path)) {
    path <- Sys.getenv("rbackupr_cache_folder")
  } else {
    Sys.setenv(rbackupr_cache_folder = path)
  }
  if (path == "") {
    path <- fs::path("rbackupr_data")
  }
  path
}

#' @rdname rb_set_cache_folder
#' @examples
#' rb_get_cache_folder()
#' @export
rb_get_cache_folder <- rb_set_cache_folder


#' Enable caching for the current session
#'
#' @return Nothing, used for its side effects.
#' @export

#' @examples
#' \donttest{
#' if (interactive()) {
#'   rb_enable_cache()
#' }
#' }
rb_enable_cache <- function() {
  Sys.setenv(rbackupr_cache = TRUE)
}


#' Disable caching for the current session
#'
#' @return Nothing, used for its side effects.
#' @export

#' @examples
#' \donttest{
#' if (interactive()) {
#'   rb_disable_cache()
#' }
#' }
rb_disable_cache <- function() {
  Sys.setenv(rbackupr_cache = FALSE)
}

#' Check caching status in the current session, and override it upon request
#'
#' Mostly used internally in functions, exported for reference.
#'
#' @param cache Defaults to NULL. If NULL, checks current cache settings. If given, returns given value, ignoring cache.
#'
#' @return Either TRUE or FALSE, depending on current cache settings.
#' @export

#' @examples
#' \donttest{
#' if (interactive()) {
#'   rb_check_cache()
#' }
#' }
rb_check_cache <- function(cache = NULL) {
  if (is.null(cache) == FALSE) {
    return(as.logical(cache))
  }
  current_cache <- Sys.getenv("rbackupr_cache")
  if (current_cache == "") {
    as.logical(FALSE)
  } else {
    as.logical(current_cache)
  }
}

#' Checks if cache folder exists, if not returns an informative message
#'
#' @return If the cache folder exists, returns TRUE. Otherwise throws an error.
#' @export
#'
#' @examples
#'
#' # If cache folder does not exist, it throws an error
#' tryCatch(rb_check_cache_folder(),
#'   error = function(e) {
#'     return(e)
#'   }
#' )
#'
#' # Create cache folder
#' rb_set_cache_folder(path = fs::path(
#'   tempdir(),
#'   "rb_cache_folder"
#' ))
#' rb_create_cache_folder(ask = FALSE)
#'
#' rb_check_cache_folder()
rb_check_cache_folder <- function() {
  if (fs::file_exists(rb_get_cache_folder()) == FALSE) {
    usethis::ui_stop(paste(
      "Cache folder does not exist. Set it with",
      usethis::ui_code("rb_get_cache_folder()"),
      "and create it with",
      usethis::ui_code("rb_create_cache_folder()")
    ))
  }
  TRUE
}


#' Set (or get) name of project for the current session.
#'
#' @param project Defaults to NULL. If given, it must be a character vector of
#'   length one. Name of a project. It will be used as the root folder for your
#'   current project, and located under the `base_folder` on your Google Drive.
#'
#' @return The project name, if previously set; the same as input if not NULL;
#'   or the default, `rbackupr_data` is none is given.
#' @export
#' @examples
#' rb_set_project(project = "weather_csv_files")
rb_set_project <- function(project = NULL) {
  if (is.null(path)) {
    path <- Sys.getenv("rbackupr_cache_folder")
  } else {
    Sys.setenv(rbackupr_cache_folder = path)
  }
  if (path == "") {
    path <- fs::path("rbackupr_data")
  }
  path
}

#' @rdname rb_set_project
#' @examples
#' rb_get_project()
#' @export
rb_get_project <- rb_set_project