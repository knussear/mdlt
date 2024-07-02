library(shinyloadtest)
record_session(
  target_app_url,
  host = "127.0.0.1",
  port = 8600,
  output_file = "recording.log",
  open_browser = TRUE,
  connect_api_key = NULL
)
