#' Add file to cache
#'
#' @param dribble A dribble.
#' @param parent_id Identifier of the parent folder
#' @param project Defaults to NULL. Can be set once per session with
#'   `rb_get_project_name()`. If given, must be a character vector of length
#'   one: name of the project.
#'
#' @return
#' @export
#'
#' @examples
rb_add_file_to_cache <- function(dribble,
                                 parent_id,
                                 project = NULL) {
  project <- rb_get_project_name(project = project)

  table_name <- rb_get_cache_table_name(type = stringr::str_c("files_", project))

  db_connection <- RSQLite::dbConnect(
    drv = RSQLite::SQLite(),
    rb_get_cache_file()
  )

  db_table_exists_v <- RSQLite::dbExistsTable(
    conn = db_connection,
    name = table_name
  )

  new_file_for_cache_df <- dribble %>%
    dplyr::mutate(mimeType = purrr::map_chr(.data$drive_resource, function(x) purrr::pluck(x, "mimeType"))) %>%
    dplyr::filter(.data$mimeType != "application/vnd.google-apps.folder") %>%
    dplyr::mutate(
      mimeType = purrr::map_chr(.data$drive_resource, function(x) purrr::pluck(x, "mimeType")),
      createdTime = purrr::map_chr(.data$drive_resource, function(x) purrr::pluck(x, "createdTime")),
      modifiedTime = purrr::map_chr(.data$drive_resource, function(x) purrr::pluck(x, "modifiedTime")),
      originalFilename = purrr::map_chr(.data$drive_resource, function(x) purrr::pluck(x, "originalFilename")),
      fullFileExtension = purrr::map_chr(.data$drive_resource, function(x) purrr::pluck(x, "fullFileExtension")),
      size = purrr::map_chr(.data$drive_resource, function(x) purrr::pluck(x, "size")),
      md5Checksum = purrr::map_chr(.data$drive_resource, function(x) purrr::pluck(x, "md5Checksum"))
    ) %>%
    dplyr::select(-.data$drive_resource) %>%
    dplyr::mutate(
      parent_id = googledrive:::as_id.character(parent_id),
      rbackupr_cache_time = Sys.time()
    )

  if (nrow(new_file_for_cache_df) > 0) {
    if (db_table_exists_v == FALSE) {
      RSQLite::dbWriteTable(
        conn = db_connection,
        name = table_name,
        value = new_file_for_cache_df
      )
    } else {
      RSQLite::dbAppendTable(
        conn = db_connection,
        name = table_name,
        value = new_file_for_cache_df
      )
    }
  }

  RSQLite::dbDisconnect(conn = db_connection)

  new_file_for_cache_df
}
