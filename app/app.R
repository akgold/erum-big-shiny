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
  # Get URLS of feeds
  feeds <- httr::GET("https://gbfs.capitalbikeshare.com/gbfs/gbfs.json") %>%
    httr::content() %>%
    magrittr::extract2("data") %>%
    magrittr::extract2("en") %>%
    magrittr::extract2("feeds") %>%
    purrr::map_df(tibble::as.tibble)

  # Get URL of station feed
  station_url <- feeds %>%
    dplyr::filter(name == "station_information") %>%
    dplyr::pull(url)

  # Get stations (name, lat, long)
  stations <- station_url %>%
    httr::GET() %>%
    httr::content() %>%
    magrittr::extract2("data") %>%
    magrittr::extract2("stations") %>%
    purrr::map_df(function(x) {
      tibble::as_tibble(
        x[!names(x) %in% c("eightd_station_services",
                           "rental_uris",
                           "rental_methods")]
      )
    })

  # Get URL of current status feed
  status_url <- feeds %>%
    dplyr::filter(name == "station_status") %>%
    dplyr::pull(url)

  # Get current status (bikes/docks per station)
  status_dat <- reactive({
    input$refresh

    status_return <- status_url %>%
      httr::GET() %>%
      httr::content()

    last_updated <- as.POSIXct(
      status_return$last_updated,
      origin = "1970-01-01 00:00:00 UTC"
    )

    list(
      dat = status_return %>%
        magrittr::extract2("data")  %>%
        magrittr::extract2("stations") %>%
        purrr::map_df(tibble::as.tibble),
      last_updated = last_updated
    )
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
        zoom = 14
      ) %>%
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
