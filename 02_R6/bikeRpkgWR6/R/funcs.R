#' Make Plot Data from Station and Status
#'
#' @param station station df
#' @param status status df
#'
#' @return combined plot data
#' @export
#'
#' @examples
#' make_plot_dat(get_feed_dat("station_information")$dat, get_feed_dat("station_status")$dat)
make_plot_dat <- function(station, status) {
  dplyr::inner_join(station, status) %>%
    dplyr::mutate(
      plot_val = glue::glue(
        paste0(
          "<b>{name}</b><br>",
          "Bikes: {num_bikes_available}<br>",
          "Empty Docks: {num_docks_available}"
        ))
    )
}
