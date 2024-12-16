# Load necessary libraries
library(shiny)
library(leaflet)
library(dplyr)
library(sf)

# Load your precomputed data
load("future_data_b.RData") # year and sex
load("future_data.RData")   # year, sex, and race
load("future_data_combined.RData") # Combined data for industries

# Load the US state shapefile
us_states <- st_read("ne_10m_admin_1_states_provinces.shp")

# Filter for U.S. states
us_states <- us_states[us_states$admin == "United States of America", ]

# Ensure geometries are valid
us_states <- st_make_valid(us_states)

# Define UI
ui <- navbarPage(
  title = "Income and Employment Prediction Dashboard",
  
  # Page 1: Average Weekly Income
  tabPanel(
    "Predicted Average Weekly Income",
    sidebarLayout(
      sidebarPanel(
        tabsetPanel(
          id = "model_selection",
          tabPanel(
            "Model 1",
            selectInput("year", "Select Year:", choices = unique(future_data_b$year)),
            selectInput("sex", "Select Sex:", choices = unique(future_data_b$sex)),
            actionButton("filter", "Get Predicted Income")
          ),
          tabPanel(
            "Model 2",
            selectInput("year_2", "Select Year:", choices = unique(future_data$year)),
            selectInput("sex_2", "Select Sex:", choices = unique(future_data$sex)),
            selectInput("race_2", "Select Race:", choices = unique(future_data$race)),
            actionButton("filter_2", "Get Predicted Income")
          )
        )
      ),
      mainPanel(
        h3("Predicted Average Weekly Income"),
        verbatimTextOutput("prediction_combined"),
        div(style = "color: gray; font-size: 12px; margin-top: 15px;",
            "Note: These models are based on population-level data from the U.S. Bureau of Labor Statistics.")
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
        leafletOutput("map", height = "600px"),
        div(style = "color: gray; font-size: 12px; margin-top: 15px;",
            "Note: This map is based on population-level data from the U.S. Bureau of Labor Statistics.")
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Income Prediction Logic for Model 1
  observeEvent(input$filter, {
    req(input$year, input$sex)
    
    # Filter the precomputed data
    filtered_data <- future_data_b %>%
      filter(year == as.numeric(input$year), sex == input$sex)
    
    # Display the result
    output$prediction_combined <- renderText({
      if (nrow(filtered_data) > 0 && is.numeric(filtered_data$predicted_value)) {
        paste("Model 1 - Predicted average weekly income is", "$", round(mean(filtered_data$predicted_value), 2))
      } else {
        "Model 1 - No data available for the selected inputs."
      }
    })
  })
  
  # Income Prediction Logic for Model 2
  observeEvent(input$filter_2, {
    req(input$year_2, input$sex_2, input$race_2)
    
    # Filter the precomputed data
    filtered_data_2 <- future_data %>%
      filter(year == as.numeric(input$year_2), sex == input$sex_2, race == input$race_2)
    
    # Display the result
    output$prediction_combined <- renderText({
      if (nrow(filtered_data_2) > 0 && is.numeric(filtered_data_2$predicted_value)) {
        paste("Model 2 - Predicted average weekly income is", "$", round(mean(filtered_data_2$predicted_value), 2))
      } else {
        "Model 2 - No data available for the selected inputs."
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
      palette = c("#800026", "#BD0026", "#E6F5FF", "#B3DDF2", "#73B3D8", "#3A88C5", "#1764A9", "#08306B"),
      domain = map_data$predicted_value,
      bins = bins
    )
    
    # Render the map
    output$map <- renderLeaflet({
      leaflet(map_data) %>%
        addTiles() %>%
        addPolygons(
          fillColor = ~palette(predicted_value),
          color = "white",
          weight = 2,
          opacity = 1,
          fillOpacity = 0.7,
          highlight = highlightOptions(weight = 3, color = "#666", bringToFront = TRUE),
          label = ~paste(name, ": ", predicted_value),
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
