library(rsconnect)

# Read credentials from environment variables
rsconnect::setAccountInfo(
  name = Sys.getenv("SHINYAPPS_ACCOUNT"),
  token = Sys.getenv("SHINYAPPS_TOKEN"),
  secret = Sys.getenv("SHINYAPPS_SECRET")
)

# Check if credentials are loaded
if(Sys.getenv("SHINYAPPS_ACCOUNT") == "") {
  stop("Credentials not found. Create .Renviron file with:\n",
       "SHINYAPPS_ACCOUNT=your_account\n",
       "SHINYAPPS_TOKEN=your_token\n",
       "SHINYAPPS_SECRET=your_secret")
}

rsconnect::deployApp(
  appName = "shiny-meta",
  account = Sys.getenv("SHINYAPPS_ACCOUNT"),
  server = "shinyapps.io",
  forceUpdate = TRUE
)

# Check rsconnect accounts
rsconnect::accountInfo()
