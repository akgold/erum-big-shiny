library(plumber)
library(magrittr)
library(dplyr)

#* @apiTitle Bike Data Access API

#* Get the feed list
#* @param lang which language? One of "en", "es", "fr", defaults to "en"
#* @get /feeds
function(lang = "en") {
  stopifnot(lang %in% c("en", "es", "fr"))

  httr::GET("https://gbfs.capitalbikeshare.com/gbfs/gbfs.json") %>%
    httr::content() %>%
    magrittr::extract2("data") %>%
    magrittr::extract2(lang) %>%
    magrittr::extract2("feeds") %>%
    purrr::map_df(tibble::as.tibble)
}

#* Get the feed list
#* @param feed_url url of a bikeshare feed
#* @get /feed_dat
function(feed_url) {
  dat <- httr::GET(feed_url) %>%
    httr::content()

  list(dat = dat %>%
         magrittr::extract2("data") %>%
         magrittr::extract2("stations") %>%
         purrr::map_df(function(x) {
           tibble::as_tibble(
             x[!names(x) %in% c("eightd_station_services",
                                "rental_uris",
                                "rental_methods")]
           )
         }),
       last_updated = as.POSIXct(
         dat$last_updated,
         origin = "1970-01-01 00:00:00 UTC"
       )
  )
}
