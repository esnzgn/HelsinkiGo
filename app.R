library(shiny)
library(readxl)
library(dplyr)
library(leaflet)
library(stringr)
library(openrouteservice)
library(googlePolylines)

# Set ORS API key
ors_api_key("5b3ce3597851110001cf624813a05e1055784127a02ee3fd14568ce4")

# Load data
places <- read_excel("data/helsinki_places_to_visit_with_modes.xlsx")

ui <- fluidPage(
  titlePanel("\U0001F4CD HelsinkiGo: Real Route Explorer"),
  sidebarLayout(
    sidebarPanel(
      p("\U0001F5F1 Click a starting location on the map."),
      sliderInput("time", "\U0001F551 Available time (min):", min = 15, max = 240, value = 60, step = 15),
      selectInput("type", "\U0001F3AF Activity Type:",
                  choices = c("All", sort(unique(places$type))),
                  selected = "All"),
      selectInput("mode", "\U0001F697 Transport Mode:",
                  choices = c("walk", "bike", "car", "public"),
                  selected = "walk"),
      actionButton("explore", "\U0001F50D Find Places")
    ),
    mainPanel(
      leafletOutput("map", height = 500),
      br(),
      uiOutput("cards")
    )
  )
)

server <- function(input, output, session) {
  starting_point <- reactiveVal(c(lat = 60.171, lon = 24.941))
  
  observeEvent(input$map_click, {
    starting_point(c(lat = input$map_click$lat, lon = input$map_click$lng))
  })
  
  filtered_places <- eventReactive(input$explore, {
    sp <- starting_point()
    profile_map <- list(
      walk = "foot-walking",
      bike = "cycling-regular",
      car = "driving-car",
      public = "foot-walking"
    )
    profile <- profile_map[[input$mode]]
    selected_type <- input$type
    time_limit <- input$time
    
    result <- list()
    
    for (i in 1:nrow(places)) {
      dest <- places[i, ]
      coords <- list(
        c(sp["lon"], sp["lat"]),
        c(dest$lon, dest$lat)
      )
      
      cat("\U0001F50D Checking route from", coords[[1]], "to", coords[[2]], "\n")
      
      tryCatch({
        route <- ors_directions(
          coordinates = coords,
          profile = profile,
          output = "json"
        )
        
        geometry <- route$routes[[1]]$geometry
        decoded <- googlePolylines::decode(geometry, arg = "text")[[1]]
        
        if (!is.data.frame(decoded)) {
          warning("❌ Decoded geometry is not a data frame.")
          next
        }
        
        coords_matrix <- as.matrix(decoded[, c("lon", "lat")])
        travel_time <- round(route$routes[[1]]$summary$duration / 60)
        directions <- route$routes[[1]]$segments[[1]]$steps
        
        direction_text <- paste(sapply(directions, function(step) {
          paste0("- ", step$instruction)
        }), collapse = "\n")
        
        total_time <- travel_time + dest$visit_time_min
        
        if (total_time <= time_limit && (selected_type == "All" || str_detect(dest$type, fixed(selected_type, ignore_case = TRUE)))) {
          dest$route_coords <- list(coords_matrix)
          dest$travel_time <- travel_time
          dest$total_time <- total_time
          dest$directions <- direction_text
          result[[length(result) + 1]] <- dest
        }
      }, error = function(e) {
        message("\u26A0\uFE0F ORS error: ", e$message)
      })
    }
    
    if (length(result) == 0) return(NULL)
    bind_rows(result)
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 24.941, lat = 60.171, zoom = 12)
  })
  
  observeEvent(filtered_places(), {
    df <- filtered_places()
    req(df)
    sp <- starting_point()
    
    mode_colors <- list(
      walk = "green",
      bike = "orange",
      car = "blue",
      public = "purple"
    )
    color <- mode_colors[[input$mode]]
    
    map <- leafletProxy("map") %>%
      clearMarkers() %>%
      clearShapes() %>%
      addAwesomeMarkers(
        lng = sp["lon"],
        lat = sp["lat"],
        icon = awesomeIcons(icon = "star", markerColor = "green", iconColor = "white"),
        popup = "Start Point"
      )
    
    for (i in 1:nrow(df)) {
      dest <- df[i, ]
      coords <- dest$route_coords[[1]]
      popup_text <- paste0("<b>", dest$name, "</b><br><b>Travel:</b> ", dest$travel_time, " min<br><b>Directions:</b><br><pre>", dest$directions, "</pre>")
      map <- map %>%
        addMarkers(lng = dest$lon, lat = dest$lat, popup = popup_text) %>%
        addPolylines(lng = coords[, "lon"], lat = coords[, "lat"], color = color, weight = 4, opacity = 0.8)
    }
  })
  
  output$cards <- renderUI({
    df <- filtered_places()
    if (is.null(df)) {
      return(tags$p("\u274C No places found. Try a different starting point, mode, or time limit."))
    }
    
    tagList(
      lapply(1:nrow(df), function(i) {
        tags$div(style = "margin-bottom: 20px; padding: 15px; border: 1px solid #ccc; border-radius: 10px;",
                 tags$h4(df$name[i]),
                 tags$img(src = df$image_url[i], width = "100%",
                          style = "border-radius: 10px; max-height: 200px; object-fit: cover; margin-bottom: 10px;"),
                 tags$p(tags$strong("Type:"), df$type[i]),
                 tags$p(tags$strong("Travel time:"), paste0(df$travel_time[i], " min")),
                 tags$p(tags$strong("Visit time:"), paste0(df$visit_time_min[i], " min")),
                 tags$p(tags$strong("Total time needed:"), paste0(df$total_time[i], " min")),
                 tags$p(tags$strong("Description:"), df$description[i])
        )
      })
    )
  })
}

shinyApp(ui = ui, server = server)