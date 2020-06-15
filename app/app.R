library(shiny)
library(shinydashboard)
library(httr)
library(ggplot2)
library(dplyr)
library(leaflet)

source(here::here("app/R6_class.R"))

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
                        shiny::actionButton(
                          "refresh",
                          label = "Refresh",
                          icon("refresh")
                        ),
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
  dat <- eventReactive(input$refresh, {
    api_client$update_status()
    list(
      last_updated = api_client$last_updated,
      status = api_client$status
    )
  })

  plot_dat <- reactive({
    req(dat)
    make_plot_dat(api_client$stations, dat()$status)
  })

  #----- Format Outputs -----
  output$last_updated <- renderText({
    req(dat)
    dat()$last_updated
  })

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
