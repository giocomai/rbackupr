#' Create rbackupr project
#'
#' @param project A character vector of length one: name of the project.
#' @param create Logical, defaults to TRUE. Creates folder if not existing.
#'
#' @return A dribble corresponding to the project folder.
#' @export
#'
#' @examples
#' \dontrun {
#' rb_drive_create_project(project = "example")
#' }
#'
rb_drive_find_project <- function(project,
                                  base_folder = "rbackupr",
                                  create = TRUE,
                                  cache = TRUE) {
  if (cache == TRUE) {
    drive_project_folder_path <- fs::path("rbackupr_cache",
                                          stringr::str_c(base_folder, "-drive_project_folder.rds"))
    if (fs::file_exists(drive_project_folder_path)==TRUE) {
      return(readr::read_rds(path = drive_project_folder_path))
    }
  }
  googledrive::drive_auth_configure(app = rbackupr::rbackupr_google_app)
  googledrive::drive_auth(scopes = "https://www.googleapis.com/auth/drive.file")
  base_folder_dribble <- rb_drive_find_base_folder(base_folder = base_folder,
                                                   cache = cache)
  base_folder_ls <- googledrive::drive_ls(path = base_folder_dribble)
  project_dribble <- base_folder_ls %>%
    dplyr::filter(name == project)
  if (nrow(project_dribble)==0) {
    if (create==TRUE) {
      project_dribble <- googledrive::drive_mkdir(name = project,
                                                  path = base_folder_dribble)
    } else {
      stop("Base project folder does not exist. Set 'create' to TRUE to create it")
    }
  } else if (nrow(project_dribble)>1) {
    stop("Something went wrong: you have more than one folder corresponding with the project name in the base folder. This needs to be fixed manually.")
  }
  if (cache == TRUE) {
    fs::dir_create(path = "rbackupr_cache")
    readr::write_rds(x = project_dribble, path = drive_project_folder_path)
  }
  project_dribble
}


#' Create or get base folder
#'
#' @param base_folder Name of base folder, defaults to "rbackupr".
#' @param cache Logical, defaults to TRUE.
#'
#' @return
#' @export
#'
#' @examples
#'
#' \dontrun {
#' rb_drive_find_base_folder()
#' }
#'

rb_drive_find_base_folder <- function(base_folder = "rbackupr",
                                      cache = TRUE) {
  if (cache == TRUE) {
    drive_base_folder_path <- fs::path("rbackupr_cache",
                                       stringr::str_c(base_folder, "-drive_base_folder.rds"))
    if (fs::file_exists(drive_base_folder_path)==TRUE) {
      return(readr::read_rds(path = drive_base_folder_path))
    }
  }
  googledrive::drive_auth_configure(app = rbackupr::rbackupr_google_app)
  googledrive::drive_auth(scopes = "https://www.googleapis.com/auth/drive.file")
  rbackupr_root_ls <- googledrive::drive_ls() %>%
    dplyr::filter(name == base_folder)
  if (nrow(rbackupr_root_ls)==1) {
    rbackupr_base_dribble <- rbackupr_root_ls
  } else if (nrow(rbackupr_root_ls)==0) {
    params <- list()
    params[["mimeType"]] <- "application/vnd.google-apps.folder"
    params[["name"]] <- base_folder
    request <- googledrive::request_generate(endpoint = "drive.files.create",
                                             params = params)
    response <- googledrive::request_make(request, encode = "json")
    proc_res <- gargle::response_process(response)
    rbackupr_base_dribble <- googledrive::as_dribble(list(proc_res))
  }
  if (cache == TRUE) {
    fs::dir_create(path = "rbackupr_cache")
    readr::write_rds(x = rbackupr_base_dribble, path = drive_base_folder_path)
  }
  return(rbackupr_base_dribble)
}


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

  project_folder_dribble <- rb_drive_find_project(project = project,
                                                  base_folder = base_folder,
                                                  create = create,
                                                  cache = cache)

  project_folder_ls <- googledrive::drive_ls(path = project_folder_dribble)

  purrr::walk(.x = first_level_folders,
              .f = function(x) {
                current_folder_dribble <- project_folder_ls %>%
                  dplyr::filter(name == x)

                if (nrow(current_folder_dribble)==0) {
                  current_folder_dribble <- googledrive::drive_mkdir(name = x,
                                                                     path = project_folder_dribble)
                }

                current_folder_dribble_ls <- googledrive::drive_ls(path = current_folder_dribble)

                current_local_media <- fs::dir_ls(path = fs::path(path, x),
                                                  recurse = FALSE,
                                                  type = "file",
                                                  glob = glob) %>%
                  fs::path_file()

                current_media_to_upload <- tibble::tibble(name = current_local_media) %>%
                  dplyr::anti_join(y = current_folder_dribble_ls, by = "name") %>%
                  dplyr::pull(name)

                purrr::walk(.x = fs::path(path, x, current_media_to_upload),
                            .f = function(current_media) {
                              googledrive::drive_upload(media = current_media,
                                                        path = current_folder_dribble)
                            }
                )

                if (recurse == TRUE) {

                  subfolder <- fs::dir_ls(path = fs::path(path, x), recurse = FALSE, type = "directory")

                  if (length(subfolder)>0) {
                    current_folder_dribble_ls_folders <- googledrive::drive_ls(path = current_folder_dribble,
                                                                               type = "folder")


                    purrr::walk(.x = subfolder %>%
                                  fs::path_file(),
                                .f = function(current_subfolder) {
                                  current_subfolder_dribble <- current_folder_dribble_ls_folders %>%
                                    dplyr::filter(name == current_subfolder)

                                  if (nrow(current_subfolder_dribble)==0) {
                                    current_subfolder_dribble <- googledrive::drive_mkdir(name = current_subfolder,
                                                                                          path = current_folder_dribble)
                                  }

                                  current_subfolder_dribble_ls <- googledrive::drive_ls(path = current_subfolder_dribble)

                                  current_local_media_subfolder <- fs::dir_ls(path = fs::path(path, x, current_subfolder),
                                                                              recurse = FALSE,
                                                                              type = "file",
                                                                              glob = glob) %>%
                                    fs::path_file()

                                  current_media_subfolder_to_upload <- tibble::tibble(name = current_local_media_subfolder) %>%
                                    dplyr::anti_join(y = current_subfolder_dribble_ls, by = "name") %>%
                                    dplyr::pull(name)

                                  purrr::walk(.x = fs::path(path, x, current_subfolder, current_media_subfolder_to_upload),
                                              .f = function(current_media_subfolder) {
                                                googledrive::drive_upload(media = current_media_subfolder,
                                                                          path = current_subfolder_dribble)
                                              }
                                  )
                                }
                    )
                  }

                }

              })


}
