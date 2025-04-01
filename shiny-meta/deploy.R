library(rsconnect)

rsconnect::setAccountInfo(
  name = "meta-oxytocin",
  token = "5407112F282F857A8E250D7B886D4D3D",  
  secret = "+C9pcd5UszwYCh9vqSvxUJmxZOwM4/qq8UE97fOh"  
)

rsconnect::deployApp(
  appName = "shiny-meta",
  account = "meta-oxytocin",
  server = "shinyapps.io",
  forceUpdate = TRUE
)

# Check rsconnect accounts
rsconnect::accountInfo()


