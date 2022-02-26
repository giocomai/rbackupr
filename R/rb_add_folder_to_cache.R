#' Add folder to cache
#'
#' @param dribble A dribble.
#' @param parent_id Identifier of the parent folder.
#' @param project Defaults to NULL. Can be set once per session with
#'   `rb_get_project_name()`. If given, must be a character vector of length
#'   one: name of the project.
#'
#' @return
#' @export
#'
#' @examples
rb_add_folder_to_cache <- function(dribble, 
                                   parent_id,
                                   project = NULL) {
  
  project <- rb_get_project_name(project = project)
  
  table_name <- rb_get_cache_table_name(type = stringr::str_c("folders_", project))
  
  db_connection <- RSQLite::dbConnect(
    drv = RSQLite::SQLite(),
    rb_get_cache_file()
  )
  
  db_table_exists_v <- RSQLite::dbExistsTable(
    conn = db_connection,
    name = table_name
  )
  
  new_folder_for_cache_df <- dribble %>%
    dplyr::select(.data$name, .data$id) %>%
    dplyr::mutate(parent_id = parent_id)
  
  if (nrow(new_folders_df) > 0) {
    if (db_table_exists_v == FALSE) {
      RSQLite::dbWriteTable(
        conn = db_connection,
        name = table_name,
        value = new_folder_for_cache_df
      )
    } else {
      RSQLite::dbAppendTable(
        conn = db_connection,
        name = table_name,
        value = new_folder_for_cache_df
      )
    }
  }
  
  RSQLite::dbDisconnect(conn = db_connection)
  
  new_folder_for_cache_df
}