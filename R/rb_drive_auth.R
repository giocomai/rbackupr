#' Set up app and scope for the current session
#'
#' @param app A Google app. See example, and dedicated gargle documentation.
#' @param scopes Defaults to `https://www.googleapis.com/auth/drive.file`. See Google api documentation for details
#'
#' @inheritParams googledrive::drive_auth
#'
#' @return Nothing, used for its side effects, i.e. logging in your Google account.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   google_app <- httr::oauth_app(
#'     "my-very-own-google-app",
#'     key = "123456789.apps.googleusercontent.com",
#'     secret = "abcdefghijklmnopqrstuvwxyz"
#'   )
#'   rb_drive_auth(app = google_app)
#' }
rb_drive_auth <- function(app = NULL,
                          scopes = "https://www.googleapis.com/auth/drive.file",
                          path = NULL,
                          token = NULL) {
  if (is.null(app) == FALSE) {
    googledrive::drive_auth_configure(app = app)
  } else {
    googledrive::drive_auth_configure(app = rbackupr::rbackupr_google_app)
  }
  googledrive::drive_auth(scopes = scopes, 
                          path = path,
                          token = token)
}
