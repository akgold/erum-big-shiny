library(shiny)
library(shinydashboard)
library(httr)
library(ggplot2)
library(dplyr)
library(leaflet)

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

  #----- Data Gathering-----
  my_api <- "https://colorado.rstudio.com/rsc/erum-bikes/"

  # Get URLS of feeds
  feeds <- file.path(my_api, "feeds") %>%
    httr::GET() %>%
    httr::content() %>%
    purrr::map_df(tibble::as.tibble)

  # Get URL of station feed
  station_url <- feeds %>%
    dplyr::filter(name == "station_information") %>%
    dplyr::pull(url)

  # Get stations (name, lat, long)
  stations <- file.path(my_api, "feed_dat") %>%
    httr::GET(
      query = list(
        feed_url = station_url
      )
    ) %>%
    httr::content() %>%
    magrittr::extract2("dat") %>%
    purrr::map_df(tibble::as.tibble)

  # Get URL of current status feed
  status_url <- feeds %>%
    dplyr::filter(name == "station_status") %>%
    dplyr::pull(url)

  # Get current status (bikes/docks per station)
  status_dat <- reactive({
    input$refresh

    status <- file.path(my_api, "feed_dat") %>%
      httr::GET(
        query = list(
          feed_url = status_url
        )
      ) %>%
      httr::content()

    status$dat <- status$dat %>%
      purrr::map_df(tibble::as.tibble)
    status$last_updated <- status$last_updated[[1]]

    status
  })

  #----- Create Plot Data -----
  plot_dat <- reactive({
    inner_join(stations, status_dat()$dat) %>%
      mutate(
        plot_val = glue::glue(
        paste0(
          "<b>{name}</b><br>",
          "Bikes: {num_bikes_available}<br>",
          "Empty Docks: {num_docks_available}"
        ))
      )
  })

  #----- Format Outputs -----
  output$last_updated <- renderText({
    req(plot_dat)
    glue::glue("Last Updated: {status_dat()$last_updated}")
  })

  output$map <- renderLeaflet({
    req(plot_dat)

    plot_dat() %>%
      leaflet() %>%
      addProviderTiles(
        providers$CartoDB.Positron
      ) %>%
      setView(
        lng = median(stations$lon),
        lat = median(stations$lat),
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
