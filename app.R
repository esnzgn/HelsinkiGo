library(shiny)
library(readxl)
library(dplyr)
library(leaflet)
library(stringr)

# Load the data
places <- read_excel("data/helsinki_places_to_visit_with_modes.xlsx")

# UI
ui <- fluidPage(
  titlePanel("📍 HelsinkiGo: Where To Go & What To Do"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("time", "🕐 Total time you have (min):", min = 15, max = 240, value = 60, step = 15),
      
      selectInput("type", "🎯 Activity Type:",
                  choices = c("All", sort(unique(unlist(strsplit(paste(places$type, collapse = ","), ",\\s*"))))),
                  selected = "All"),
      
      selectInput("mode", "🚗 Transport Mode:",
                  choices = c("walk", "bike", "car", "public"),
                  selected = "walk"),
      
      actionButton("explore", "🔍 Find Places")
    ),
    
    mainPanel(
      leafletOutput("map", height = 400),
      br(),
      uiOutput("cards")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  filtered_places <- eventReactive(input$explore, {
    selected_type <- input$type
    selected_mode <- input$mode
    time_limit <- input$time
    
    travel_col <- paste0(selected_mode, "_travel_min")
    
    df <- places %>%
      mutate(total_time = .data[[travel_col]] + visit_time_min) %>%
      filter(total_time <= time_limit)
    
    if (selected_type != "All") {
      df <- df %>% filter(str_detect(type, fixed(selected_type, ignore_case = TRUE)))
    }
    
    if (nrow(df) == 0) return(NULL)
    df
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 24.941, lat = 60.171, zoom = 12)
  })
  
  observeEvent(filtered_places(), {
    df <- filtered_places()
    req(df)
    
    leafletProxy("map") %>%
      clearMarkers() %>%
      addMarkers(
        lng = df$lon,
        lat = df$lat,
        popup = paste0("<b>", df$name, "</b><br>", df$description)
      )
  })
  
  output$cards <- renderUI({
    df <- filtered_places()
    mode <- input$mode
    
    if (is.null(df)) {
      return(tags$p("❌ No places match your filters. Try increasing time or changing type."))
    }
    
    tagList(
      lapply(seq_len(nrow(df)), function(i) {
        travel_time <- df[[paste0(mode, "_travel_min")]][i]
        total_time <- travel_time + df$visit_time_min[i]
        
        tags$div(style = "margin-bottom: 20px; padding: 15px; border: 1px solid #ccc; border-radius: 10px;",
                 tags$h4(df$name[i]),
                 tags$img(src = df$image_url[i], width = "100%", 
                          style = "border-radius: 10px; max-height: 200px; object-fit: cover; margin-bottom: 10px;"),
                 tags$p(tags$strong("Type:"), df$type[i]),
                 tags$p(tags$strong("Transport mode:"), mode),
                 tags$p(tags$strong("Travel time:"), paste0(travel_time, " min")),
                 tags$p(tags$strong("Visit time:"), paste0(df$visit_time_min[i], " min")),
                 tags$p(tags$strong("Total time needed:"), paste0(total_time, " min")),
                 tags$p(tags$strong("Description:"), df$description[i])
        )
      })
    )
  })
}

# Run the app
shinyApp(ui = ui, server = server)
