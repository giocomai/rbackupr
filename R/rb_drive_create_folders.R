#' Create missing folders and get data frame with identifiers of all given folders
#'
#' @param folders A character vector of folder names.
#' @param parent_id Identifier of parent folder, where the new folders are to be created.
#' @param project Defaults to NULL. Can be set once per session with
#'   `rb_get_project_name()`. If given, must be a character vector of length
#'   one: name of the project.
#' @param update Logical, defaults to FALSE. If TRUE, checks on Google Drive for
#'   newly updated files or folders, otherwise it assumes that only files and
#'   folders listed in cache exist online.
#'
#' @return
#' @export
#'
#' @examples
#'
#' if (interactive()) {
#'   rb_drive_create_folders(
#'     folders = c("folder_a", "folder_b"),
#'     parent_id = rb_get_project()
#'   )
#' }
rb_drive_create_folders <- function(folders,
                                    parent_id,
                                    relative_path = NULL,
                                    project = NULL,
                                    update = FALSE,
                                    base_folder = "rbackupr",
                                    cache = TRUE) {
  if (is.data.frame(parent_id) == TRUE) {
    if ("id" %in% colnames(parent_id)) {
      parent_id <- parent_id %>%
        dplyr::pull(.data$id)
    }
  }

  folders_on_drive <- rb_get_folders(
    dribble_id = parent_id,
    project = project,
    update = update,
    base_folder = base_folder,
    cache = cache
  )

  new_folder_names <- folders[(folders %in% folders_on_drive$name) == FALSE]

  if (is.null(relative_path) == FALSE) {
    relative_path <- relative_path[(folders %in% folders_on_drive$name) == FALSE]
  }


  # create folders if they do not exist

  new_folders_df <- purrr::map2_dfr(
    .x = new_folder_names,
    .y = relative_path,
    .f = function(x, y) {
      new_folder_dribble <- googledrive::drive_mkdir(
        name = x,
        path = googledrive::as_id(parent_id)
      ) %>%
        dplyr::select(.data$name, .data$id) %>%
        dplyr::mutate(parent_id = googledrive:::as_id.character(parent_id))

      if (is.null(y)) {
        y <- rb_get_relative_path(
          dribble_id = new_folder_dribble$id,
          project = project
        )
      }

      new_folder_for_cache_df <- rb_add_folder_to_cache(
        dribble = new_folder_dribble,
        parent_id = parent_id,
        relative_path = y,
        project = project,
        cache = cache
      )

      new_folder_for_cache_df
    }
  )

  folders_on_drive_df <- rb_get_folders(
    dribble_id = parent_id,
    project = project,
    update = FALSE,
    base_folder = base_folder,
    cache = cache
  )


  folders_on_drive_df %>%
    dplyr::filter(.data$name %in% folders)
}
