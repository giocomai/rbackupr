rb_drive_auth <- function() {
  googledrive::drive_auth_configure(app = rbackupr::rbackupr_google_app)
  googledrive::drive_auth(scopes = "https://www.googleapis.com/auth/drive.file")
  # googledrive::drive_oauth_app()
}
