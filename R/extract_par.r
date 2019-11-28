#' Extract paragraphs that mention islam and terrorism from a text
#'
#' @param corp           A quanteda corpus
#' @param islam_regex    A regular expression for findings texts that mention islam
#' @param terror_regex   A regular expression for finding texts that mention terrorism
#' @param ignore.case    Passed to grepl
#' @param ...            Additional arguments passed to grepl
#'
#' @return               A quanteda corpus
#' @export
#'
#' @examples
#' corp = quanteda::corpus(data.frame(stringsAsFactors = F,
#' id = c(1,2),
#' date = c('2010-01-01','2010-02-01'),
#' text = c('terror islam.\n\ntest.\n\nterrorism muslim', 'test.\n\nterrorism islam')
#' ))
#'
#' corp2 = suspect_comm_paragraphs(corp)
#' corp2$documents
suspect_comm_paragraphs <- function(text, islam_regex = 'islam|muslim', terror_regex='terror', ignore.case=T, ...) {
  corp = quanteda::corpus(text)
  par = quanteda::corpus_reshape(corp, 'paragraphs')

  islam = grepl(islam_regex, quanteda::texts(par), ignore.case = ignore.case, ...)
  terror = grepl(terror_regex, quanteda::texts(par), ignore.case = ignore.case, ...)
  x = quanteda::corpus_subset(par, subset = islam & terror)
  x = quanteda::texts(x)
  paste(as.character(x), collapse='\n\n')
}
