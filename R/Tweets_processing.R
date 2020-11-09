#' Filter tweets by date
#'
#' This function filter tweets file between given dates.
#'
#' @param tweets Dataframe with tweets data
#' @param start start date in format:"yyyy-mm-dd"
#' @param end end date in format:"yyyy-mm-dd"
#' @return tweets between given dates
#' @export
filter_by_date <- function(tweets, start, end) {
  x <- dplyr::filter(dplyr::between(as.Date(created_at), as.Date(start), as.Date(end)))
  return(x)
}
