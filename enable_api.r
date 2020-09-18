library(googlesheets4)
# bring your own app via client id (aka key) and secret
google_app <- httr::oauth_app(
  "wrapper",
  key = "keygoeshere",
  secret = "secretgoeshere"
)
google_key <- "apikeygoeshere"
gs4_auth_configure(app = google_app, api_key = google_key)
