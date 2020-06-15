library(shiny)
library(shinydashboard)
library(httr)
library(ggplot2)
library(dplyr)
library(leaflet)
library(bikeR6pkg)

# Create dashboard page UI
ui <- dashboardPage(skin = "red",
                    dashboardHeader(title = "Capitol Bikeshare Bikes"),
                    dashboardSidebar(disable = TRUE),
                    dashboardBody(
                      box(
                        title = "Station Map",
                        leafletOutput(
                          "map"
                        ),
                        actionButton(
                          "refresh",
                          "Refresh",
                          icon = icon('refresh')),
                        verbatimTextOutput(
                          "last_updated"
                        ),
                        width = 12
                      )
                    )
)

server <- function(input, output, session) {
  api_client <- BikeClient$new()

  #----- Data Gathering + Plot Data Formatting -----
  dat <- reactive({
    input$refresh

    api_client$update_status()
    list(
      last_updated = api_client$last_updated,
      status = api_client$status
    )
  })

  output$last_updated <- renderText(dat()$last_updated)

  plot_dat <- reactive({
    req(dat)
    bikeR6pkg::make_plot_dat(api_client$stations, dat()$status)
  })

  #----- Format Outputs -----
  output$map <- renderLeaflet({
    req(plot_dat)

    plot_dat() %>%
      leaflet() %>%
      addProviderTiles(
        providers$CartoDB.Positron
      ) %>%
      setView(
        lng = median(plot_dat()$lon),
        lat = median(plot_dat()$lat),
        zoom = 14) %>%
      addAwesomeMarkers(
        lng = ~lon,
        lat = ~lat,
        icon = awesomeIcons(
          "bicycle",
          library = "fa",
          iconColor = "white",
          markerColor = "red"
        ),
        popup = ~paste0(plot_val)
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
