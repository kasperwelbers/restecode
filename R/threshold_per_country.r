
function() {
  ## this seems hopeless

  min_events = 200
  countries = unique(g$from_meta$country)
  thres = rep(NA, nrow(g$d))

  agg_countries = c()
  for (country in countries) {
    ids = g$from_meta$document_id[g$from_meta$country == country]
    if (length(ids) < min_events) {
      agg_countries = c(agg_countries, country)
      next
    }
    res = estimate_validity(g, from_sample = g$from_meta$country == country, do_plot = F)
    c_thres = res$threshold[which.max(res$F1)]
    thres[g$d$from %in% ids] = c_thres
  }
  ## for countries with too few events, calc weight together
  ids = g$from_meta$document_id[g$from_meta$country %in% agg_countries]



}
