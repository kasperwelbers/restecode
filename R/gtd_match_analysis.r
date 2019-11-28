#' Extract GTD events with news article annotations
#'
#' @param g
#' @param weight_thres
#' @param use_geomatch
#'
#' @return
#' @export
#'
#' @examples
gtd_event_coverage <- function(g, weight_thres, use_geomatch) {
  g$d$.use = g$d$weight > weight_thres
  if (use_geomatch) g$d$use = g$d$.use & g$d$geomatch

  meta = subset(g$from_meta, .complete_window)
  e = subset(g$d, .use)
  e = merge(e, g$to_meta[,c('document_id')], by.x='to', by.y='document_id', all.x=T)
  e = e[, list(N_news=length(weight), has_news=T), by='from']

  #meta$geo_distance = london_dist_km(meta)
  e = merge(meta, e, by.x='document_id', by.y='from', all=T)
  e$N_news[is.na(e$N_news)] = 0
  e$has_news[is.na(e$has_news)] = 0

  e$killed[is.na(e$killed)] = 0
  e$wounded[is.na(e$wounded)] = 0
  e
}
