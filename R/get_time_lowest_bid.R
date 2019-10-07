get_time_lowest_bid <- function(x, time_var, lowest_bid = NULL) {

  if (is.null(lowest_bid)) lowest_bid <- find_lowest(x)

  map_dbl(.x = 1:length(x),
          .f = ~ time_var[min(which(lowest_bid[.x] == x[1:.x]))]
  ) %>%
    unlist() %>%
    as.POSIXct(origin = "1970-01-01", tz = "UTC")
}
