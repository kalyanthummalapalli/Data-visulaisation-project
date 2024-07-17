library(shiny)
library(sf)
library(leaflet)
library(dplyr)
library(htmlwidgets)

# Read and prepare the data (adjust the paths as needed)
poly_data <- st_read("state village boundary polygon data in .json format")
point_data <- read.csv("MGNAREGA points geospatial data in village wise ")

# Convert point data to an sf object
point_data <- st_as_sf(point_data, coords = c("longitude", "latitude"), crs = st_crs(poly_data))

# Calculate intersection
inter <- st_intersects(poly_data, point_data)

# Initialize columns for counts and categories
poly_data$total_count <- lengths(inter)
poly_data$unique_categories <- vector("list", length(poly_data$total_count))
poly_data$num_categories <- numeric(length(poly_data$total_count))
poly_data$category_counts <- vector("list", length(poly_data$total_count))

# Aggregate counts by category
for (i in seq_along(inter)) {
  if (length(inter[[i]]) > 0) {
    points_in_poly <- point_data[inter[[i]], ]
    
    # Unique categories and their counts
    categories <- unique(points_in_poly$category)
    poly_data$unique_categories[[i]] <- categories
    poly_data$num_categories[i] <- length(categories)
    
    # Count points in each category
    category_counts <- points_in_poly %>%
      group_by(category) %>%
      summarise(count = n())
    poly_data$category_counts[[i]] <- category_counts
  }
}

# Create labels including the counts, number of categories, and village names
poly_data$labels <- sapply(seq_along(poly_data$total_count), function(i) {
  categories_formatted <- paste(poly_data$category_counts[[i]]$category, poly_data$category_counts[[i]]$count, sep = ": ", collapse = "<br>")
  label <- paste("Village Name:", poly_data$NAME[i], "<br>",
                 "Total points count:", poly_data$total_count[i], "<br>",
                 "Number of categories:", poly_data$num_categories[i], "<br>",
                 "Categories and counts:<br>", categories_formatted)
  label
})

# Define colors and breaks based on number of categories
max_categories <- max(poly_data$num_categories)
colors <- c("blue", "green", "yellow", "red", "purple", "orange")
breaks <- unique(c(0, 1, 2, 4, 6, 8, max_categories + 1))  # Ensure breaks are unique

color_palette <- colorBin(palette = colors, bins = breaks, domain = poly_data$num_categories, na.color = "transparent")

# Define UI for the application
ui <- fluidPage(
  titlePanel("Interactive Leaflet Map with Categories"),
  sidebarLayout(
    sidebarPanel(
      selectInput("category_select", "Select Category:", c("All", unique(unlist(poly_data$unique_categories)))),
      uiOutput("download")
    ),
    mainPanel(
      leafletOutput("map", height = 800, width = "100%")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive expression to filter data based on category selection
  filtered_data <- reactive({
    data <- poly_data
    if (input$category_select != "All") {
      # Determine which polygons contain the selected category
      data$highlight <- sapply(data$unique_categories, function(categories) input$category_select %in% categories)
    } else {
      data$highlight <- TRUE
    }
    return(data)
  })
  
  # Create the leaflet map
  output$map <- renderLeaflet({
    leaflet(data = filtered_data()) %>%
      addTiles() %>%
      addPolygons(fillColor = ~ifelse(highlight, color_palette(num_categories), "transparent"), 
                  fillOpacity = ~ifelse(highlight, 0.7, 0.1),
                  color = "black", 
                  weight = 1,
                  label = lapply(filtered_data()$labels, htmltools::HTML),
                  highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
                  popup = ~labels) %>%
      addLegend("bottomright", 
                pal = color_palette, 
                values = filtered_data()$num_categories, 
                title = "Number of Categories", 
                labFormat = labelFormat(suffix = " categories"),
                opacity = 1)
  })
  
  # Create the download button UI
  output$download <- renderUI({
    if (!is.null(poly_data) && !is.null(point_data)) {
      downloadButton("downloadData", "Download Map")
    }
  })
  
  # Download handler for the map as an HTML file
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("map", Sys.Date(), ".html", sep = "")
    },
    content = function(file) {
      saveWidget(widget = leafletProxy("map"), file = file, selfcontained = TRUE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
