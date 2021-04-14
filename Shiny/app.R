library(shiny)
library(dplyr)
library(readr)
library(leaflet)
library(leaflet.extras)
library(rabm)

source("C:/Users/rnussba1/Documents/GitHub/rabm/r/pull_single_specie_location.R")
source("C:/Users/rnussba1/Documents/GitHub/rabm/r/pull_single_specie.R")
source("C:/Users/rnussba1/Documents/GitHub/rabm/r/find_pentad_coordinates.R")
source("C:/Users/rnussba1/Documents/GitHub/rabm/r/find_pentad.R")
source("C:/Users/rnussba1/Documents/GitHub/rabm/r/find_pentad_within.R")
source("C:/Users/rnussba1/Documents/GitHub/rabm/r/extract_species.R")


species_list <- read_csv('C:/Users/rnussba1/Documents/GitHub/rabm/Shiny/species_list.csv')



ui <- fluidPage(
  leafletOutput("leafmap"),
  #selectInput("country", "Choose Country:",),
  selectInput("specie", "Choose Specie:",
              setNames(as.character(species_list$Spp), species_list$name)
  ),
  p(),
  actionButton("recalc", "New points")
)

server <- function(input, output, session) {

  output$leafmap <- renderLeaflet({
    map = leaflet() %>% addTiles() %>% setView( 38, 0.32, zoom = 6) %>% addDrawToolbar(
      targetGroup = "draw",
      polylineOptions = FALSE,
      markerOptions = FALSE,
      circleMarkerOptions = FALSE,
      circleOptions = FALSE,
      editOptions = editToolbarOptions(),
      singleFeature=TRUE)
  })


  points <- observeEvent(input$recalc, {
   # req(input$leafmap_draw_stop)

    if (!is.null(input$leafmap_draw_new_feature$geometry$coordinates)){
      coordinates <- do.call(rbind,
                             lapply(
        input$leafmap_draw_new_feature$geometry$coordinates[[1]],
        function(x){
          c(x[[1]][1],x[[2]][1])
          }
        )
        )
      coordinates <- data.frame(lat = coordinates[,1], lon = coordinates[,2])
      pentads <- find_pentad_within(type = "coordinates",
        coordinates = coordinates)
      print(pentads)

      list <- extract_species(
         species_ids = input$specie,
         start_date = '2019-01-01',
         end_date = '2019-02-01',
         region_type = 'pentad',
         region_ids = pentads,
         return_type = 'data'
        )

      print(list)

    }

  }, ignoreNULL = FALSE)


}

shinyApp(ui, server)
