#' Create rbackupr project
#'
#' @param project Defaults to NULL. Can be set once per session with `rb_get_project_name()`. If given, must be a character vector of length one: name of the project.
#' @param create Logical, defaults to TRUE. Creates folder if not existing.
#'
#' @return A dribble corresponding to the project folder.
#' @export
#'
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   rb_drive_find_project(project = "example")
#' }
#' }
#'
rb_drive_find_project <- function(project = NULL,
                                  base_folder = "rbackupr",
                                  create = TRUE,
                                  cache = TRUE) {
  project <- rb_get_project_name(project = project)

  if (rb_check_cache(cache = cache)) {
    db_connection <- RSQLite::dbConnect(
      drv = RSQLite::SQLite(),
      rb_get_cache_file()
    )
    db_table_exists_v <- RSQLite::dbExistsTable(
      conn = db_connection,
      name = rb_get_cache_table_name(type = "projects")
    )
    if (db_table_exists_v) {
      project_folder_df <- RSQLite::dbReadTable(
        conn = db_connection,
        name = rb_get_cache_table_name(type = "projects")
      ) %>%
        dplyr::filter(name == project) %>%
        dplyr::collect() %>%
        tibble::as_tibble() %>%
        dplyr::mutate(id = googledrive:::as_id.character(x = .data$id))

      if (nrow(project_folder_df) > 0) {
        RSQLite::dbDisconnect(conn = db_connection)
        return(project_folder_df)
      }
    }
  }

  base_folder_dribble_id <- rb_drive_find_base_folder(
    base_folder = base_folder,
    cache = cache
  ) %>%
    dplyr::pull(id)

  project_dribble <- googledrive::drive_ls(
    path = googledrive::as_id(base_folder_dribble_id),
    recursive = FALSE,
    type = "folder"
  ) %>%
    dplyr::filter(name == project)

  if (nrow(project_dribble) == 1) {
    # do nothing, good to go
  } else if (nrow(project_dribble) == 0) {
    if (create == TRUE) {
      project_dribble <- googledrive::drive_mkdir(
        name = project,
        path = base_folder_dribble_id
      )
    } else {
      stop("Base project folder does not exist. Set 'create' to TRUE to create it")
    }
  } else if (nrow(project_dribble) > 1) {
    stop("Something went wrong: you have more than one folder corresponding with the project name in the base folder. This needs to be fixed manually.")
  }

  project_df <- project_dribble %>%
    dplyr::select(.data$name, .data$id) %>%
    dplyr::mutate(parent_id = base_folder_dribble_id)


  if (rb_check_cache(cache = cache)) {
    if (db_table_exists_v == FALSE) {
      RSQLite::dbWriteTable(
        conn = db_connection,
        name = rb_get_cache_table_name(type = "projects"),
        value = project_df
      )
    } else {
      RSQLite::dbAppendTable(
        conn = db_connection,
        name = rb_get_cache_table_name(type = "projects"),
        value = project_df
      )
    }
    RSQLite::dbDisconnect(conn = db_connection)
  }
  project_df %>%
    dplyr::mutate(
      id = googledrive:::as_id.character(x = .data$id),
      parent_id = googledrive:::as_id.character(x = .data$parent_id)
    )
}


#' @rdname rb_drive_find_project
#' @export
#' @examples 
#' \dontrun{
#' if (interactive()) {
#' rb_drive_create_project(project = "example")
#' }
#' }
rb_drive_create_project <- rb_drive_find_project

#' @rdname rb_drive_find_project
#' @examples rb_get_project(project = "example")
#' @export
rb_get_project <- rb_drive_find_project

#' Create or get base folder
#'
#' @param base_folder Name of base folder, defaults to "rbackupr".
#' @param create Defaults to FALSE. If set to TRUE, the folder is create if not found.
#' @param cache Logical, defaults to TRUE. Can be se to NULL, and managed with `rb_enable_cache()` / `rb_disable_cache()`
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   rb_drive_find_base_folder()
#' }
#' }
rb_drive_find_base_folder <- function(base_folder = "rbackupr",
                                      create = FALSE,
                                      cache = TRUE) {
  if (rb_check_cache(cache = cache)) {
    db_connection <- RSQLite::dbConnect(
      drv = RSQLite::SQLite(),
      rb_get_cache_file()
    )
    db_table_exists_v <- RSQLite::dbExistsTable(
      conn = db_connection,
      name = rb_get_cache_table_name(type = "base_folder")
    )
    if (db_table_exists_v) {
      base_folder_df <- RSQLite::dbReadTable(
        conn = db_connection,
        name = rb_get_cache_table_name(type = "base_folder")
      ) %>%
        dplyr::filter(name == base_folder) %>%
        dplyr::collect() %>%
        tibble::as_tibble() %>%
        dplyr::mutate(id = googledrive:::as_id.character(x = .data$id))

      if (nrow(base_folder_df) > 0) {
        RSQLite::dbDisconnect(conn = db_connection)
        return(base_folder_df)
      }
    }
  }

  rbackupr_root_ls <- googledrive::drive_ls() %>%
    dplyr::filter(name == base_folder)
  if (nrow(rbackupr_root_ls) == 0) {
    params <- list()
    params[["mimeType"]] <- "application/vnd.google-apps.folder"
    params[["name"]] <- base_folder
    request <- googledrive::request_generate(
      endpoint = "drive.files.create",
      params = params
    )
    response <- googledrive::request_make(
      x = request,
      encode = "json"
    )
    proc_res <- gargle::response_process(response)
    rbackupr_base_dribble <- googledrive::as_dribble(x = list(proc_res))
    base_folder_df <- rbackupr_base_dribble %>%
      dplyr::select(.data$name, .data$id)
  } else {
    base_folder_df <- rbackupr_root_ls %>%
      dplyr::select(.data$name, .data$id)
  }

  if (rb_check_cache(cache = cache)) {
    if (db_table_exists_v == FALSE) {
      RSQLite::dbWriteTable(
        conn = db_connection,
        name = rb_get_cache_table_name(type = "base_folder"),
        value = base_folder_df
      )
    } else {
      RSQLite::dbAppendTable(
        conn = db_connection,
        name = rb_get_cache_table_name(type = "base_folder"),
        value = base_folder_df
      )
    }
    RSQLite::dbDisconnect(conn = db_connection)
  }
  base_folder_df
}
