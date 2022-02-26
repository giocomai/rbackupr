#' Gets cached folder with given parent, or update them from Google Drive upon request
#'
#' @param dribble_id The dribble identifier of a folder on Google Drive.
#' @param update Logical, defaults to FALSE. If TRUE, checks on Google Drive for newly updated folders.
#' @param project Defaults to NULL. Can be set once per session with
#'   `rb_get_project()`. If given, must be a character vector of length one:
#'   name of the project.
#' @param cache Logical, defaults to TRUE. Stores locally cached information
#'   about base and project folder.
#'
#' @return A data frame with three columns: name (of the folder), id (of the
#'   dribble of the folder), parent_id (dribble id of the parent folder)
#' @export
#'
#' @examples
#'
#' if (interactive()) {
#'   rb_get_folder_cache(rb_drive_find_project())
#' }
rb_get_folder_cache <- function(dribble_id,
                                update = FALSE,
                                project = NULL,
                                cache = TRUE) {
  if (cache == FALSE) {
    return(NULL)
  }

  if (is.data.frame(dribble_id) == TRUE) {
    if ("id" %in% colnames(dribble_id)) {
      dribble_id <- dribble_id %>%
        dplyr::pull(.data$id)
    }
  }

  project <- rb_get_project(project = project)

  table_name <- rb_get_cache_table_name(type = stringr::str_c("folders_", project))

  db_connection <- RSQLite::dbConnect(
    drv = RSQLite::SQLite(),
    rb_get_cache_file()
  )
  db_table_exists_v <- RSQLite::dbExistsTable(
    conn = db_connection,
    name = table_name
  )
  if (db_table_exists_v) {
    previous_folders_df <- RSQLite::dbReadTable(
      conn = db_connection,
      name = table_name
    ) %>%
      dplyr::filter(dribble_id %in% .data$parent_id) %>%
      dplyr::collect() %>%
      tibble::as_tibble() %>%
      dplyr::mutate(
        id = googledrive:::as_id.character(x = .data$id),
        parent_id = googledrive:::as_id.character(x = .data$parent_id)
      )
  } else {
    previous_folders_df <- tibble::tibble(
      name = as.character(NA),
      id = googledrive::as_id("x"),
      parent_id = googledrive:::as_id.character(dribble_id)
    ) %>%
      dplyr::slice(0)
  }

  if (update == FALSE) {
    return(previous_folders_df)
  }

  current_folders_df <- googledrive::drive_ls(
    path = googledrive::as_id(dribble_id),
    recursive = FALSE,
    type = "folder"
  ) %>%
    dplyr::select(.data$name, .data$id) %>%
    dplyr::mutate(parent_id = googledrive:::as_id.character(dribble_id))

  new_folders_df <- current_folders_df %>%
    dplyr::anti_join(
      y = previous_folders_df,
      by = "id"
    )

  if (nrow(new_folders_df) > 0) {
    if (db_table_exists_v == FALSE) {
      RSQLite::dbWriteTable(
        conn = db_connection,
        name = table_name,
        value = new_folders_df
      )
    } else {
      RSQLite::dbAppendTable(
        conn = db_connection,
        name = table_name,
        value = new_folders_df
      )
    }
  }

  RSQLite::dbDisconnect(conn = db_connection)

  return(current_folders_df)
}




#' Check if new files appeared inside an online folder
#'
#' @param dribble_id The dribble identifier of a folder on Google Drive.
#' @param update Logical, defaults to FALSE. If TRUE, checks on Google Drive for newly updated folders.
#' @param project Defaults to NULL. Can be set once per session with
#'   `rb_get_project()`. If given, must be a character vector of length one:
#'   name of the project.
#' @param cache Logical, defaults to TRUE. Stores locally cached information
#'   about base and project folder.
#'
#' @return A data frame with three columns: name (of the folder), id (of the
#'   dribble of the folder), parent_id (dribble id of the parent folder)
#' @export
#'
#' @examples
#'
#' if (interactive()) {
#'   rb_get_file_cache(rb_drive_find_project())
#' }
rb_get_file_cache <- function(dribble_id,
                              update = FALSE,
                              project = NULL,
                              cache = TRUE) {
  if (cache == FALSE) {
    return(NULL)
  }

  project <- rb_get_project(project = project)

  table_name <- rb_get_cache_table_name(type = stringr::str_c(
    "files_",
    project
  ))

  db_connection <- RSQLite::dbConnect(
    drv = RSQLite::SQLite(),
    rb_get_cache_file()
  )
  db_table_exists_v <- RSQLite::dbExistsTable(
    conn = db_connection,
    name = table_name
  )
  if (db_table_exists_v) {
    previous_files_df <- RSQLite::dbReadTable(
      conn = db_connection,
      name = table_name
    ) %>%
      dplyr::filter(dribble_id %in% .data$parent_id) %>%
      dplyr::collect() %>%
      tibble::as_tibble() %>%
      dplyr::mutate(
        id = googledrive:::as_id.character(x = .data$id),
        parent_id = googledrive:::as_id.character(x = .data$id)
      )
  } else {
    previous_files_df <- tibble::tibble(
      name = as.character(NA),
      id = googledrive::as_id("x"),
      parent_id = googledrive:::as_id.character(dribble_id)
    ) %>%
      dplyr::slice(0)
  }

  if (update == FALSE) {
    return(previous_files_df)
  }

  current_files_df <- googledrive::drive_ls(
    path = googledrive::as_id(dribble_id),
    recursive = FALSE
  ) %>%
    tidyr::unnest(drive_resource) %>%
    dplyr::select(.data$name, .data$id) %>%
    dplyr::mutate(parent_id = googledrive:::as_id.character(dribble_id))

  new_files_df <- current_files_df %>%
    dplyr::anti_join(
      y = previous_files_df,
      by = "id"
    )

  if (nrow(new_files_df) > 0) {
    if (db_table_exists_v == FALSE) {
      RSQLite::dbWriteTable(
        conn = db_connection,
        name = table_name,
        value = new_files_df
      )
    } else {
      RSQLite::dbAppendTable(
        conn = db_connection,
        name = table_name,
        value = new_files_df
      )
    }
  }

  RSQLite::dbDisconnect(conn = db_connection)

  current_files_df
}
