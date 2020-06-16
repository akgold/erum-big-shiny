#' R6 class Representing a connection to the bike data client.
#'
#' @export
BikeClient <- R6::R6Class(
  classname = "BikeClient",

  public = list(
    #' @field stations df of stations
    stations = NULL,
    #' @field status df of statuses
    status = NULL,
    #' @field last_updated character, when last updated
    last_updated = NULL,
    #' @description
    #' Create a new BikeClient
    initialize = function() {
      private$url <- "https://colorado.rstudio.com/rsc/erum-bikes/"
      private$feeds <- file.path(private$url, "feeds") %>%
        httr::GET() %>%
        httr::content() %>%
        purrr::map_df(tibble::as.tibble)
      private$update("stations", "station_information")
      self$update_status()

      invisible(self)
    },
    #' @description
    #' Update current statuses.
    update_status = function() {
      private$update("status", "station_status")
    }
  ),
  private = list(
    # @field URL of API
    url = NULL,
    # @field list of feeds + urls
    feeds = NULL,
    # @description
    # Update a public attribute
    # @param attr an attribute of this class
    # @param feed name of a feed from feeds
    update = function(attr, feed) {
      dat <- httr::GET(
        file.path(private$url, "feed_dat"),
        query = list(
          feed_url = private$feed_url(feed)
        )
      ) %>%
        httr::content()
      self[[attr]] <- dat$dat %>%
        purrr::map_df(tibble::as.tibble)
      self$last_updated <- dat$last_updated[[1]]
    },
    # @description
    # Get the url of a feed
    # @inheritParams update
    feed_url = function(feed) {
      stopifnot(feed %in% private$feeds$name)

      dplyr::filter(private$feeds, name == feed) %>%
        dplyr::pull(url)
    }
  )
)
