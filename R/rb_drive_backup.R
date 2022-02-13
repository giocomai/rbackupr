#' Backup files
#'
#' @param path Local path where files to backup are stored.
#' @param project Name of backup project
#' @param first_level_folders Defaults to NULL. If included, clarifies which folders within the path should be uploaded, keeping the folder structure.
#' @param glob Defaults to NULL. Can be used to filter type of files to upload, e.g. "*.jpg"
#' @param recurse Defaults to TRUE. Recurse up to one level.
#' @param create Logical, defaults to TRUE. Create folders if missing.
#' @param cache Logical, defaults to TRUE. Stores locally cached information about base and project folder.
#' @param base_folder Name of base folder, defaults to `rbackupr`
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' if (interactive()) {
#' rb_backup()
#' }
#' }
rb_backup <- function(path,
                      project,
                      first_level_folders = NULL,
                      glob = NULL,
                      recurse = TRUE,
                      create = TRUE,
                      cache = TRUE,
                      base_folder = "rbackupr") {
  if (is.null(first_level_folders)) {
    first_level_folders <- fs::dir_ls(path = path, recurse = FALSE, type = "directory") %>%
      fs::path_file()
  }

  project_folder_dribble <- rb_drive_find_project(
    project = project,
    base_folder = base_folder,
    create = create,
    cache = cache
  )

  project_folder_ls <- googledrive::drive_ls(path = project_folder_dribble)

  purrr::walk(
    .x = first_level_folders,
    .f = function(x) {
      current_folder_dribble <- project_folder_ls %>%
        dplyr::filter(name == x)

      if (nrow(current_folder_dribble) == 0) {
        current_folder_dribble <- googledrive::drive_mkdir(
          name = x,
          path = project_folder_dribble
        )
      }

      current_folder_dribble_ls <- googledrive::drive_ls(path = current_folder_dribble)

      current_local_media <- fs::dir_ls(
        path = fs::path(path, x),
        recurse = FALSE,
        type = "file",
        glob = glob
      ) %>%
        fs::path_file()

      current_media_to_upload <- tibble::tibble(name = current_local_media) %>%
        dplyr::anti_join(y = current_folder_dribble_ls, by = "name") %>%
        dplyr::pull(name)

      purrr::walk(
        .x = fs::path(path, x, current_media_to_upload),
        .f = function(current_media) {
          googledrive::drive_upload(
            media = current_media,
            path = current_folder_dribble
          )
        }
      )

      if (recurse == TRUE) {
        subfolder <- fs::dir_ls(path = fs::path(path, x), recurse = FALSE, type = "directory")

        if (length(subfolder) > 0) {
          current_folder_dribble_ls_folders <- googledrive::drive_ls(
            path = current_folder_dribble,
            type = "folder"
          )


          purrr::walk(
            .x = subfolder %>%
              fs::path_file(),
            .f = function(current_subfolder) {
              current_subfolder_dribble <- current_folder_dribble_ls_folders %>%
                dplyr::filter(name == current_subfolder)

              if (nrow(current_subfolder_dribble) == 0) {
                current_subfolder_dribble <- googledrive::drive_mkdir(
                  name = current_subfolder,
                  path = current_folder_dribble
                )
              }

              current_subfolder_dribble_ls <- googledrive::drive_ls(path = current_subfolder_dribble)

              current_local_media_subfolder <- fs::dir_ls(
                path = fs::path(path, x, current_subfolder),
                recurse = FALSE,
                type = "file",
                glob = glob
              ) %>%
                fs::path_file()

              current_media_subfolder_to_upload <- tibble::tibble(name = current_local_media_subfolder) %>%
                dplyr::anti_join(y = current_subfolder_dribble_ls, by = "name") %>%
                dplyr::pull(name)

              purrr::walk(
                .x = fs::path(path, x, current_subfolder, current_media_subfolder_to_upload),
                .f = function(current_media_subfolder) {
                  googledrive::drive_upload(
                    media = current_media_subfolder,
                    path = current_subfolder_dribble
                  )
                }
              )
            }
          )
        }
      }
    }
  )
}
