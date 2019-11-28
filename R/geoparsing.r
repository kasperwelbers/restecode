geo_dfm <- function(txt, ids, with_model=F) {
  ## with_model is not yet implemented

  month <- c('January', 'February', 'March', 'April', 'May', 'June',
             'July', 'August', 'September', 'October', 'November', 'December')
  day <- c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday')
  agency <- c('AP', 'AFP', 'Reuters')

  toks = quanteda::corpus(txt, docnames=ids)
  toks <- quanteda::tokens(toks)
  toks <- quanteda::tokens_remove(toks, stopwords('english'), valuetype = 'fixed', padding = TRUE)
  toks <- quanteda::tokens_remove(toks, c(month, day, agency), valuetype = 'fixed', padding = TRUE)
  label_toks <- quanteda::tokens_lookup(toks, newsmap::data_dictionary_newsmap_en,
                              levels = 3, nested_scope = "dictionary")

  if (with_model) label_toks = newsmap_model(toks, label_toks)

  quanteda::dfm(label_toks)
}

newsmap_model <- function(toks, label_toks) {
  feat_dfm <- dfm(toks, tolower = FALSE)
  feat_dfm <- dfm_select(feat_dfm, selection = "keep", '^[A-Z][A-Za-z1-2]+', valuetype = 'regex', case_insensitive = FALSE) # include only proper nouns to model
  feat_dfm <- dfm_trim(feat_dfm, min_count = 10)
  model <- textmodel_newsmap(feat_dfm, label_toks)
  browser()
}

geo_tags_batch <- function(txt, min_freq=NA, top_n=NA, with_model=F) {
  #geo = NULL ## for using geo in data.table aggregate

  ids = 1:length(txt)
  gdfm = geo_dfm(txt, ids, with_model=with_model)
  m = as(gdfm, 'dgTMatrix')
  d = data.table::data.table(id=rownames(m)[m@i+1], geo=colnames(m)[m@j+1], x=m@x)
  if (!is.na(min_freq)) d = d[d$x > min_freq,]

  if (!is.na(top_n)) {
    data.table::setorder(d, -x)
    d = d[,list(tag=paste(head(get('geo'), top_n), collapse=',')),by='id']
  } else {
    d = d[,list(tag=paste(get('geo'), collapse=',')), by='id']
  }
  d$tag[match(1:length(txt), d$id)]
}

#' Create geo tags
#'
#' Uses the newsmap package to create geo tags. Currenly only simple dictionary matching is used.
#'
#' @param txt       A character vector with texts
#' @param min_freq  Minimum frequency of matches in the geo dictionary
#' @param top_n     If used, return only the top n tags
#' @param batchsize Calculation is performed in batches
#'
#' @return A character vector of length txt with geo tags, concatenated with a komma delimiter
#' @export
#'
#' @examples
#' geo_tags('Paris Germany Berlin Leiden')
geo_tags <- function(txt, min_freq=NA, top_n=NA, batchsize=10000, with_model=F) {
  txt = as.character(txt)
  batch_i = get_batch_i(length(txt), batchsize=batchsize, return_list=T)

  tags = rep('', length(txt))
  for (i in seq_along(batch_i)) {
    if (i > 1 && i == length(batch_i) && with_model) {
      ## if with_model is used, fill up final batch for better approximation
      batch = batch_i[[i]]
      n = length(batch)
      if (n < batchsize) {
        addtobatch = batchsize - n
        batch = c(batch, batch_i[[i-1]][1:addtobatch])
      }
      tags[batch_i[[i]]] = geo_tags_batch(txt[batch], min_freq = min_freq, top_n = top_n, with_model=T)[1:n]
    } else {
      tags[batch_i[[i]]] = geo_tags_batch(txt[batch_i[[i]]], min_freq = min_freq, top_n = top_n)
    }
  }
  tags
}


#' Match geo code
#'
#' If the DTMs used in the compare_documents function contained docvars with geo codes,
#' it can be used to match documents based on their geo code. This can be a single code (e.g., "en")
#' or multiple codes concatenated with a comma (e.g., "en,de") or another seperator defined in the 'sep' argument.
#' A match is TRUE if at least one of the codes in the from document is the same as at least one of the codes in the to document.
#'
#' The function returns a logical vector indicating if each match in the edge list has a geo match.
#'
#' @param g             An edgelist, as created with newsflow.compare
#' @param from_var      The name of the geo code column in from_dtm
#' @param to_var        The name of the geo code column in to_dtm
#'
#' @return A logical vector
#' @export
#'
#' @examples
match_geo <- function(g, from_var='geo', to_var='geo', sep=',') {
  from_i = g$from_meta[list(g$d$from),on='document_id',which=T]
  to_i = g$to_meta[list(g$d$to),on='document_id',which=T]
  geo_lookup(g$from_meta[[from_var]][from_i],
             g$to_meta[[to_var]][to_i])
}


geo_lookup <- function(x, y, sep=',') {
  n = length(x)

  x = data.table::tstrsplit(x, split=sep)
  y = data.table::tstrsplit(y, split=sep)

  eq = rep(F, n)
  for (i in seq_along(x)) {
    for (j in seq_along(y)) {
      eq = eq | (x[[i]] == y[[j]] & !is.na(x[[i]]) & !is.na(y[[j]]))
    }
  }
  eq
}
