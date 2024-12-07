# Load necessary libraries
library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(RColorBrewer)

# Load your precomputed data
load("future_data_b.RData")        # Data for income prediction
load("future_data_combined.RData") # Combined data for industries

# Load the US state shapefile
us_states <- st_read("ne_10m_admin_1_states_provinces.shp")

# Filter for U.S. states
us_states <- us_states[us_states$admin == "United States of America", ]

# Ensure geometries are valid
us_states <- st_make_valid(us_states)

# Define UI
ui <- navbarPage(
  title = "Income Prediction Dashboard",
  
  # Page 1: Mean Quarterly Income
  tabPanel(
    "Mean Quarterly Income",
    sidebarLayout(
      sidebarPanel(
        selectInput("year", "Select Year:", choices = unique(future_data_b$year)),
        selectInput("sex", "Select Sex:", choices = unique(future_data_b$sex)),
        actionButton("filter", "Get Predicted Income")
      ),
      mainPanel(
        h3("Predicted Mean Quarterly Income"),
        verbatimTextOutput("prediction")
      )
    )
  ),
  
  # Page 2: 12 Month Percent Change in Employment
  tabPanel(
    "12 Month Percent Change in Employment",
    sidebarLayout(
      sidebarPanel(
        selectInput("year_quarter", "Select Time Period (Year_Quarter):", choices = unique(future_data_combined$year_quarter)),
        selectInput("industry", "Select Industry:", choices = unique(future_data_combined$industry)),
        actionButton("update_map", "Update Map")
      ),
      mainPanel(
        h3("12 Month Percent Change in Employment"),
        leafletOutput("map", height = "600px")
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Income Prediction Logic
  observeEvent(input$filter, {
    req(input$year, input$sex)
    
    # Filter the precomputed data
    filtered_data <- future_data_b %>%
      filter(year == as.numeric(input$year), sex == input$sex)
    
    # Display the result
    output$prediction <- renderText({
      if (nrow(filtered_data) > 0 && is.numeric(filtered_data$predicted_value)) {
        paste("Predicted mean quarterly income:", round(mean(filtered_data$predicted_value), 2))
      } else {
        "No data available for the selected inputs."
      }
    })
  })
  
  # Render the Interactive Map
  observeEvent(input$update_map, {
    req(input$year_quarter, input$industry)
    
    # Filter data for the selected time period and industry
    map_data <- future_data_combined %>%
      filter(year_quarter == input$year_quarter, industry == input$industry)
    
    # Join map data with the state geometry
    map_data <- us_states %>%
      left_join(map_data, by = c("name" = "state"))  # Adjust 'name' and 'state' as needed
    
    # Define bins
    bins <- c(-7, -3, 0, 1.5, 2.5, 4, 6, 9, 13)
    
    # Define a custom palette with hex color codes
    palette <- colorBin(
      palette = c("#08306B", "#2171B5", "#6BAED6", "#FD8D3C", "#FC4E2A", "#E31A1C", "#BD0026", "#800026"),
      domain = map_data$predicted_value,
      bins = bins
    )
    
    # Render the map
    output$map <- renderLeaflet({
      leaflet(map_data) %>%
        addTiles() %>%
        addPolygons(
          fillColor = ~palette(predicted_value),  # Replace 'predicted_value' with your data column
          color = "white",
          weight = 2,
          opacity = 1,
          fillOpacity = 0.7,
          highlight = highlightOptions(weight = 3, color = "#666", bringToFront = TRUE),
          label = ~paste(name, ": ", predicted_value),  # Replace 'name' and 'predicted_value' with your columns
          labelOptions = labelOptions(direction = "auto")
        ) %>%
        addLegend(
          pal = palette,
          values = map_data$predicted_value,
          position = "bottomright",
          title = "Predicted % Change",
          labFormat = labelFormat(suffix = ""),
          opacity = 1
        )
    })
  })
}

# Run the App
shinyApp(ui = ui, server = server)
