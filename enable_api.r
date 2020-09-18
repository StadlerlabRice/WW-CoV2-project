# bring your own app via client id (aka key) and secret
google_app <- httr::oauth_app(
  "wrapper",
  key = "598444958176-ot4u688cad3ufp5mtpsn3p451ao9h5l8.apps.googleusercontent.com",
  secret = "VlZkiFjPNRtlxe_jBujBFomd"
)
google_key <- "AIzaSyC2pC8Uotu765_uXEyu_diICSEEzapYVCY"
sheets_auth_configure(app = google_app, api_key = google_key)

# confirm the changes
sheets_oauth_app()
sheets_api_key()