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
#' @param create Logical, defaults to TRUE. Create folders if missing. Set to
#'   FALSE if you are sure there are no new folders to raise an error if
#'   something unexpected happens.
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

  if (recurse == FALSE) {
    max_level <- 1
  }

  project_folder_df <- rb_get_project(
    project = project,
    base_folder = base_folder,
    create = create,
    cache = cache
  )

  # check local files in top level folder

  if (first_level_files == TRUE) {
    local_files_top <- fs::dir_ls(path = path,
                                  recurse = FALSE,
                                  type = "file",
                                  glob = glob) %>%
      fs::path_file()

    if (length(local_files_top) > 0) {
      previous_first_level_files <- rb_get_files(project_folder_df)

      files_to_upload <- local_files_top[(local_files_top %in% previous_first_level_files$name)==FALSE]
      
      remote_parent_id <- project_folder_df$id
      
      remote_parent_dribble <- googledrive::as_dribble(x = googledrive::as_id(remote_parent_id))
      
      purrr::walk(
        .x = fs::path(path, files_to_upload),
        .f = function(current_file) {
          new_upload_dribble <- googledrive::drive_upload(
            media = current_file,
            path = remote_parent_dribble
          )
          
          rb_add_file_to_cache(
            dribble = new_upload_dribble,
            parent_id = remote_parent_id,
            project = project
          )
        }
      )
      
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

  if (is.null(first_level_folders) == FALSE) {
    # first, check if given first_level_folders exist locally

    if (Reduce(`|`, first_level_folders %in% local_first_level_folders) == FALSE) {
      missing_local <- first_level_folders[first_level_folders %in% local_first_level_folders == FALSE]
      first_level_folders <- first_level_folders[first_level_folders %in% local_first_level_folders == TRUE]
      warning(stringr::str_c("The following folders do not exist locally: ", stringr::str_c(missing_local, collapse = ";")))
    }
  } else {
    first_level_folders <- local_first_level_folders
  }

  all_sub_folders <- fs::dir_ls(
    path = fs::path(path, first_level_folders),
    recurse = TRUE,
    type = "directory"
  )

  folders_to_process <- tibble::tibble(full_path = c(fs::path(path, first_level_folders), all_sub_folders)) %>%
    dplyr::mutate(relative_path = stringr::str_remove(
      string = .data$full_path,
      pattern = stringr::str_c(path, "/")
    )) %>%
    dplyr::mutate(
      level = .data$relative_path %>%
        stringr::str_count(pattern = "/"),
      processed_folder = FALSE,
      processed_files = FALSE
    ) %>%
    dplyr::filter(.data$level <= max_level) %>%
    dplyr::arrange(.data$level) %>%
    dplyr::mutate(folder_name = fs::path_file(.data$relative_path)) %>%
    dplyr::mutate(folder_id = as.character(NA)) %>%
    dplyr::mutate(parent_folder_path = fs::path_dir(.data$relative_path)) %>%
    dplyr::mutate(parent_folder_id = as.character(NA))


  for (i in seq_along(unique(folders_to_process$level))) {
    current_level <- i - 1
    current_folders_to_process <- folders_to_process %>%
      dplyr::filter(.data$level == current_level)

    current_folders_to_process$parent_folder_id[current_folders_to_process$parent_folder_path == "."] <- rb_get_project(project = project, base_folder = base_folder) %>% dplyr::pull(id)
    current_folders_to_process$parent_folder_path[current_folders_to_process$parent_folder_path == "."] <- project


    for (j in seq_along(current_folders_to_process$full_path)) {
      new_folder_df <- rb_drive_create_folders(
        folders = current_folders_to_process$folder_name[j],
        parent_id = current_folders_to_process$parent_folder_id[j],
        relative_path = current_folders_to_process$relative_path[j],
        project = project,
        update = update
      )

      selected_row <- which(folders_to_process$full_path == current_folders_to_process$full_path[j])

      folders_to_process$folder_id[selected_row] <- new_folder_df$id
      folders_to_process$parent_folder_id[selected_row] <- new_folder_df$parent_id
      folders_to_process$parent_folder_path[folders_to_process$parent_folder_path == "."] <- project
      folders_to_process$parent_folder_id[folders_to_process$parent_folder_path == folders_to_process$relative_path[selected_row]] <- new_folder_df$id
      folders_to_process$processed_folder[selected_row] <- TRUE
    }
  }


  for (i in seq_along(folders_to_process$full_path)) {
    message(stringr::str_c("Now processing ", i, " of ", nrow(folders_to_process), ": ", folders_to_process$full_path[i]))


    local_parent_relative <- folders_to_process$relative_path[i]
    local_parent_full <- folders_to_process$full_path[i]

    remote_parent_id <- folders_to_process$folder_id[i]

    ## upload local files to current folder


    local_files <- fs::dir_ls(
      path = local_parent_full,
      recurse = FALSE,
      type = "file",
      glob = glob
    )

    remote_files_df <- rb_get_files(
      dribble_id = remote_parent_id,
      update = update,
      project = project
    )


    if (nrow(remote_files_df) == 0) {
      files_to_upload <- local_files
    } else {
      files_to_upload <- local_files[(fs::path_file(local_files) %in% remote_files_df$name) == FALSE]
    }

    remote_parent_dribble <- googledrive::as_dribble(x = googledrive::as_id(remote_parent_id))

    purrr::walk(
      .x = files_to_upload,
      .f = function(current_file) {
        new_upload_dribble <- googledrive::drive_upload(
          media = current_file,
          path = remote_parent_dribble
        )

        rb_add_file_to_cache(
          dribble = new_upload_dribble,
          parent_id = remote_parent_id,
          project = project
        )
      }
    )

    folders_to_process$processed_files[i] <- TRUE
  }
  folders_to_process
}
