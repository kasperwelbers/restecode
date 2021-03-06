#' Terrorism terms
#'
#' A set of query terms for finding news items that potentially mention terrorist events.
#' The list is intentially very broad. The goal is to have high recall (i.e. find all events
#' that mention terrorist events) and we are not that concerned about precision (i.e. finding only
#' events that mention terrorist events) in the data collection phase.
#'
#' @docType data
#' @usage data(terrorism_terms)
#' @format A character vector
#' @examples
#' terrorism_terms
"terrorism_terms"

#' Gold standard for GTD event matches
#'
#' Gold standard for GTD events matches in 500 random articles from the Guardian between
#' 2000 and 2015. Articles were manually coded, by looking whether a new terrorist event
#' was reported in the headline and lead (i.e. was the main topic of the news). If so, all
#' GTD events within 7 days before the article publication date and in the same country as
#' mentioned in the article were manually checked for a match. All matches are reported in
#' the $matches slot, which contains pairs of guardian article ids and GTD event ids."
#'
#' @docType data
#' @usage data(gold_matches)
#' @format A list with three slots: $description, $articles and $matches
#' @examples
#' gold_matches$description
#' head(gold_matches$articles)
#' head(gold_matches$matches)
"gold_matches"
