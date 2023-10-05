#' Set up app and scope for the current session
#'
#' @param client A Google client. See example, and dedicated gargle documentation.
#' @param scopes Defaults to `https://www.googleapis.com/auth/drive.file`. See Google api documentation for details
#'
#' @inheritParams googledrive::drive_auth
#'
#' @return Nothing, used for its side effects, i.e. logging in your Google account.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   google_client <- httr::oauth_app(
#'     "my-very-own-google-app",
#'     key = "123456789.apps.googleusercontent.com",
#'     secret = "abcdefghijklmnopqrstuvwxyz"
#'   )
#'   rb_drive_auth(google_client)
#' }
rb_drive_auth <- function(client = NULL,
                          scopes = "https://www.googleapis.com/auth/drive.file",
                          email = gargle::gargle_oauth_email(),
                          cache = gargle::gargle_oauth_cache(),
                          use_oob = gargle::gargle_oob_default(),
                          path = NULL,
                          token = NULL) {
  if (is.null(client) == FALSE) {
    googledrive::drive_auth_configure(client = client)
  } else {
    googledrive::drive_auth_configure(client = rbackupr_client)
  }
  googledrive::drive_auth(scopes = scopes, 
                          path = path,
                          token = token,
                          email = email,
                          cache = cache,
                          use_oob = use_oob)
}
