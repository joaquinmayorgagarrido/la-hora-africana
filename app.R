
# Clear workspace
rm(list = ls())

# Packages
#install.packages('rsconnect')

#install.packages(c("shiny", "httr", "jsonlite", "dplyr"))
library(httr)
library(jsonlite)
library(shiny)
library(DT)
library('rsconnect')

# Create dataframe with links to Youtube videos 
video_urls <- c(
  "https://www.youtube.com/watch?v=cjyQlmrMPSo", # First 
  "https://www.youtube.com/watch?v=GqD2SOtowtY", # Second 
  "https://www.youtube.com/watch?v=8uGo6-ZSMJI", # Third
  "https://www.youtube.com/watch?v=iEIpzJNYXKM" # Cuarto 
) 
 
video_ids <- sub(".*v=", "", video_urls)
df_videos <- data.frame(url=video_urls, id=video_ids)
print(df_videos)

# API key, and base URL 
api_key <- "AIzaSyCqRPgYBYi29Pi8bHRlWAouSGLDAdEDAcA"
base_url <- "https://www.googleapis.com/youtube/v3/videos"

# Function to get video details 
get_video_details <- function(video_id, api_key) {
  response <- GET(url = base_url, query = list(part = "snippet,statistics", id = video_id, key = api_key))
  content_as_text <- content(response, "text", encoding = "UTF-8")
  content_as_json <- fromJSON(content_as_text)
  title_raw <- content_as_json$items$snippet$title
  title_clean <- sub("\\|.*", "", title_raw)
  print(title_clean)
  video_url <- sprintf("https://www.youtube.com/watch?v=%s", video_id)
  title <- sprintf('<a href="%s" target="_blank">%s</a>', video_url, title_clean)
  date <- content_as_json$items$snippet$publishedAt
  date <- substr(date, 1, 10)
  view_count <- content_as_json$items$statistics$viewCount
  return(data.frame(Título = title, Fecha = date, Vistas = view_count, stringsAsFactors = FALSE))
}

# Function to fetch details for multiple video IDs
get_videos_details <- function(video_ids, api_key) {
  details_list <- lapply(video_ids, get_video_details, api_key = api_key)
  details_df <- do.call(rbind, details_list)
  return(details_df)
}

# Run function 
video_details <- get_videos_details(video_ids, api_key)
video_details <- cbind(Episodio = seq_len(nrow(video_details)), video_details)
print(video_details)

# Define UI
ui <- fluidPage(
  titlePanel("LA HORA AFRICANA con IVÁN SCHARGRODSKY | HAY ALGO AHÍ | BLENDER"),
  fluidRow(
    column(12,
           HTML("<h3 style='margin-top: 0;'>Estadísticas de YouTube</h3>"))
  ),
  fluidRow(
    column(12,
           textOutput("lastRefresh"))
  ),
  DTOutput("table")
)

# Define server logic
server <- function(input, output) {
  # Last refresh 
  lastRefresh <- reactiveVal()
  observe({
    lastRefresh(Sys.time())
  })
  output$lastRefresh <- renderText({
    paste("Última actualización:", format(lastRefresh(), "%Y-%m-%d %H:%M:%S"), "GMT")
  })
  # Table
  output$table <- renderDT({
    datatable(video_details, options = list(
      dom = 'ft',
      searching = FALSE,
      language = list(url = "//cdn.datatables.net/plug-ins/1.10.19/i18n/Spanish.json"),
      autoWidth = TRUE,
      ordering = TRUE, # Enable column ordering,
      order = list(list(2, 'desc'))#, # Order by Date column, ascending
      #columnDefs = list(list(targets = "_all", className = "dt-center")) # Center align all columns
    ), 
    rownames=FALSE,
    escape=FALSE)
  }, server = FALSE) # Set server = FALSE for processing (like sorting) on the client side
}

# Run the application
shinyApp(ui = ui, server = server)


