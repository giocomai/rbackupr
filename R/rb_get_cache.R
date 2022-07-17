#' Gets cached folder with given parent, or update them from Google Drive upon
#' request
#'
#' @param dribble_id The dribble identifier of a folder on Google Drive.
#' @param update Logical, defaults to FALSE. If TRUE, checks on Google Drive for
#'   newly updated folders.
#' @param project Defaults to NULL. Can be set once per session with
#'   `rb_get_project_name()`. If given, must be a character vector of length
#'   one: name of the project.
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
#'   rb_get_folders(rb_drive_find_project())
#' }
rb_get_folders <- function(dribble_id,
                           update = FALSE,
                           project = NULL,
                           base_folder = "rbackupr",
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
  if (db_table_exists_v) {
    previous_folders_df <- RSQLite::dbReadTable(
      conn = db_connection,
      name = table_name
    ) %>%
      dplyr::filter(.data$parent_id %in% dribble_id) %>%
      dplyr::collect() %>%
      tibble::as_tibble() %>%
      dplyr::mutate(
        id = googledrive:::as_id.character(x = .data$id),
        parent_id = googledrive:::as_id.character(x = .data$parent_id),
        relative_path = fs::path(relative_path)
      )
  } else {
    previous_folders_df <- tibble::tibble(
      name = as.character(NA),
      id = googledrive::as_id("x"),
      parent_id = googledrive:::as_id.character(dribble_id),
      relative_path = fs::path(NA)
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
    dplyr::mutate(
      parent_id = googledrive:::as_id.character(dribble_id),
      relative_path = rb_get_relative_path(
        dribble_id = .data$id,
        project = project,
        base_folder = base_folder
      )
    )

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
#'   `rb_get_project_name()`. If given, must be a character vector of length one:
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
#'   rb_get_files(rb_drive_find_project())
#' }
rb_get_files <- function(dribble_id,
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

  project <- rb_get_project_name(project = project)

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
      dplyr::filter(.data$parent_id %in% dribble_id) %>%
      dplyr::collect() %>%
      tibble::as_tibble() %>%
      dplyr::mutate(
        id = googledrive:::as_id.character(x = .data$id),
        parent_id = googledrive:::as_id.character(x = .data$parent_id)
      )
  } else {
    previous_files_df <- tibble::tibble(
      name = as.character(NA),
      id = googledrive:::as_id.character("x"),
      mimeType = as.character(NA),
      createdTime = as.character(NA),
      modifiedTime = as.character(NA),
      originalFilename = as.character(NA),
      fullFileExtension = as.character(NA),
      size = as.character(NA),
      md5Checksum = as.character(NA),
      parent_id = googledrive:::as_id.character(dribble_id),
      rbackupr_cache_time = as.POSIXct(NA)
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
      parent_id = googledrive:::as_id.character(dribble_id),
      rbackupr_cache_time = Sys.time()
    )

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






#' Gets parent id of a given folder
#'
#' @param dribble_id The dribble identifier of a folder on Google Drive.
#' @param project Defaults to NULL. Can be set once per session with
#'   `rb_get_project_name()`. If given, must be a character vector of length
#'   one: name of the project.
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
#'   rb_get_parent_folder(rb_drive_find_project())
#' }
rb_get_parent_folder <- function(dribble_id,
                                 project = NULL,
                                 base_folder = "rbackupr") {
  if (is.data.frame(dribble_id) == TRUE) {
    if ("id" %in% colnames(dribble_id)) {
      dribble_id <- dribble_id %>%
        dplyr::pull(.data$id)
    }
  }

  project <- rb_get_project_name(project = project)


  project_df <- rb_get_project(
    project = project,
    base_folder = base_folder,
    create = FALSE,
    cache = TRUE
  )

  if (dribble_id == project_df$id) {
    return(project_df$parent_id)
  } else if (dribble_id == project_df$parent_id) {
    return(NULL)
  }

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
    parent_folder_id <- RSQLite::dbReadTable(
      conn = db_connection,
      name = table_name
    ) %>%
      dplyr::filter(.data$id %in% dribble_id) %>%
      dplyr::pull(.data$parent_id)
  } else {
    stop("Folder table for the current project does not exist")
  }
  parent_folder_id
}


#' Gets folder name based on id
#'
#' @param dribble_id The dribble identifier of a folder on Google Drive.
#' @param project Defaults to NULL. Can be set once per session with
#'   `rb_get_project_name()`. If given, must be a character vector of length
#'   one: name of the project.
#' @param cache Logical, defaults to TRUE. Stores locally cached information
#'   about base and project folder.
#' @param base_folder Name of base folder, defaults to `rbackupr`
#'
#' @return A data frame with three columns: name (of the folder), id (of the
#'   dribble of the folder), parent_id (dribble id of the parent folder)
#' @export
#'
#' @examples
#'
#' if (interactive()) {
#'   rb_get_folder_name(rb_drive_find_project())
#' }
rb_get_folder_name <- function(dribble_id,
                               project = NULL,
                               base_folder = "rbackupr") {
  if (is.data.frame(dribble_id) == TRUE) {
    if ("id" %in% colnames(dribble_id)) {
      dribble_id <- dribble_id %>%
        dplyr::pull(.data$id)
    }
  }

  project <- rb_get_project_name(project = project)

  project_df <- rb_get_project(
    project = project,
    base_folder = base_folder,
    create = FALSE,
    cache = TRUE
  )

  if (dribble_id == project_df$id) {
    return(project_df$name)
  } else if (dribble_id == project_df$parent_id) {
    return(base_folder)
  }


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
    folder_name <- RSQLite::dbReadTable(
      conn = db_connection,
      name = table_name
    ) %>%
      dplyr::filter(.data$id %in% dribble_id) %>%
      dplyr::pull(.data$name)
  } else {
    stop("Folder table for the current project does not exist")
  }
  folder_name
}



#' Get relative path in local folder from identifier
#'
#' Based only on cached data-
#'
#' @param dribble_id The dribble identifier of a folder on Google Drive.
#' @param project Defaults to NULL. Can be set once per session with
#'   `rb_get_project_name()`. If given, must be a character vector of length
#'   one: name of the project.
#' @param max_level Defaults to 100. Maximum level of sub-folders to backup.
#'   Default means it will go 100 times deep into sub-folders. Used also to
#'   prevent infinite loops.
#' @param base_folder Name of base folder, defaults to `rbackupr`
#'
#' @return
#' @export
#'
#' @examples
rb_get_relative_path <- function(dribble_id,
                                 project = NULL,
                                 max_level = 100,
                                 base_folder = "rbackupr") {
  if (is.null(dribble_id)) {
    return(fs::path(NA))
  }

  path_l <- list()

  level <- 1

  while (level < max_level & is.null(dribble_id) == FALSE) {
    path_l[[level]] <- rb_get_folder_name(
      dribble_id = dribble_id,
      project = project,
      base_folder = base_folder
    )

    dribble_id <- rb_get_parent_folder(
      dribble_id = dribble_id,
      project = project,
      base_folder = base_folder
    )

    level <- level + 1
  }

  fs::path_join(rev(unlist(path_l[1:(length(path_l) - 2)])))
}
