#' Backup files
#'
#' @param path Local path where files to backup are stored.
#' @param project Defaults to NULL. Can be set once per session with
#'   `rb_get_project_name()`. If given, must be a character vector of length
#'   one: name of the project.
#' @param first_level_folders Defaults to NULL. If given, clarifies which
#'   folders within the path should be uploaded, keeping the folder structure.
#' @param first_level_files Logical, defaults to TRUE. If FALSE, first level
#'   files (files that are directly under the project folder, rather than a
#'   subfolder) are not included in the backup.
#' @param max_level Defaults to 100. Maximum level of sub-folders to backup.
#'   Default means it will go 100 times deep into sub-folders. Used also to
#'   prevent infinite loops.
#' @param glob Defaults to NULL. Can be used to filter type of files to upload,
#'   e.g. "*.jpg"
#' @param recurse Defaults to TRUE. Recurse up to one level.
#' @param create Logical, defaults to TRUE. Create folders if missing.
#' @param update Logical, defaults to FALSE. If TRUE, checks on Google Drive for
#'   newly updated files or folders, otherwise it assumes that only files and
#'   folders listed in cache exist online.
#' @param cache Logical, defaults to TRUE. Stores locally cached information
#'   about base and project folder.
#' @param base_folder Name of base folder, defaults to `rbackupr`
#'
#' @return
#' @export
#' 
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   rb_backup(path = "folder_to_backup", project = "test_project")
#' }
#' }
rb_backup <- function(path,
                      project = NULL,
                      first_level_folders = NULL,
                      first_level_files = TRUE,
                      max_level = 100,
                      recurse = TRUE,
                      glob = NULL,
                      create = TRUE,
                      update = FALSE,
                      cache = TRUE,
                      base_folder = "rbackupr") {
  
  project <- rb_get_project_name(project = project)
  
  if (recurse = FALSE) {
    max_level <- 1
  }

  project_folder_df <- rb_get_project(
    project = project,
    base_folder = base_folder,
    create = create,
    cache = cache
  )

  # check local files in top level folder
  
  if (first_level_files==TRUE) {
    
    local_files_top <- fs::dir_ls(path = path, recurse = FALSE, type = "file", glob = glob) %>% 
      fs::path_file()
    
    if (length(local_files_top)>0) {
      previous_first_level_files <- rb_get_files(project_folder_df)
      
      warning("Missing functionality: Top level files bakcup not yet implemented")
      # TODO actually introduce upload
      
    } else {
     # do nothing: if there are not files, just move ahead 
    }
  }

  ## check if folders exist in cache
  local_first_level_folders <- fs::dir_ls(
    path = path,
    recurse = FALSE,
    type = "directory"
  ) %>%
    fs::path_file()
  
  if (is.null(first_level_folders)==FALSE) {
    # first, check if given first_level_folders exist locally
    
    if (Reduce(`|`, first_level_folders %in% local_first_level_folders)==FALSE) {
      missing_local <- first_level_folders[first_level_folders %in% local_first_level_folders==FALSE]
      first_level_folders <- first_level_folders[first_level_folders %in% local_first_level_folders==TRUE]
      warning(stringr::str_c("The following folders do not exist locally: ",stringr::str_c(missing_local, collapse = ";")))
    }

  } else {
    first_level_folders <- local_first_level_folders
  }
  
  folders_df <- rb_drive_create_folders(folders = first_level_folders, 
                                        parent_id = project_folder_df,
                                        project = project,
                                        update = update)
  
  
  
  ### now check folders one by one, recursively
  
  
  
  ###################
 
  ###################

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
