# Author: David Zong. Date: 18/Sep/20
# Purpose - to get googlesheets4 to use a google cloud account to increase quote limits

if(!exists('key_this')) key_this <- "keygoeshere"
if(!exists('secret_this')) secret_this <- "secretgoeshere"

library(googlesheets4)
# bring your own app via client id (aka key) and secret
google_app <- httr::oauth_app(
  "wrapper",
  key = key_this,
  secret = secret_this
)
google_key <- "apikeygoeshere"
gs4_auth_configure(app = google_app, api_key = google_key)