#' Opens a GTD event in the browser
#'
#' @param id A GTD event id
#'
#' @return Opens browser window
#' @export
browse_gtd <- function(id) {
  url = paste0('https://www.start.umd.edu/gtd/search/IncidentSummary.aspx?gtdid=', id)
  browseURL(url)
}

#' Opens a Guardian article in the browser
#'
#' @param id A Guardian article id
#'
#' @return Opens browser window
#' @export
browse_guardian <- function(id) {
  url = paste0('https://www.theguardian.com/', id)
  browseURL(url)
}
