#' Function that wraps tqueries for extracting quotes
#'
#' @param tokens          A tokenindex
#' @param verbs           A character vector with verbs (as lemma) that indicate speech acts
#' @param exclude_verbs   A character vector with verbs to explicitly exclude as speech acts
#'
#' @return                A tokenindex
#' @export
corenlp_quotes <- function(tokens, verbs, exclude_verbs=NULL) {
  if (!require('rsyntax')) stop('Using this function requires the rsyntax package (not yet on CRAN)')

  direct = tquery(lemma = verbs, NOT(lemma = exclude_verbs), label='verb', fill=F,
                  children(req=F, relation = c('npadvmod'), block=T),
                  children(relation=c('su', 'nsubj', 'agent', 'nmod:agent'), label='source'),
                  children(label='quote', NOT(relation=c('mark','prep','cc')),
                           rsyntax::fill(NOT(relation='mark'))))

  nosrc = tquery(pos='VERB*',
                 children(relation= c('su', 'nsubj', 'agent', 'nmod:agent'), label='source'),
                 children(lemma = verbs, NOT(lemma = exclude_verbs), relation='xcomp', label='verb',
                          children(relation=c("ccomp", "dep", "parataxis", "dobj", "nsubjpass", "advcl"), label='quote')))

  according = tquery(label='quote',
                     children(relation='nmod:according_to', label='source',
                              children(label='verb')))

  tokens = rsyntax::annotate(tokens, 'quote', dir=direct, nos=nosrc, acc=according)

  span1 = tquery(pos = 'VERB*', lemma = verbs,
                 children(relation=c('su', 'nsubj', 'agent', 'nmod:agent'), label='source'))
  span2 = tquery(pos = 'VERB*',
                 children(relation=c('su', 'nsubj', 'agent', 'nmod:agent'), label='source'))
  tokens = add_span_quotes(tokens, 'token', quote_col = 'quote', source_val = 'source', quote_val = 'quote', tqueries=list(span1,span2))

  tokens
}

#' Function that wraps tqueries for extracting clauses
#'
#' @param tokens          A tokenindex
#' @param verbs           A character vector with verbs (as lemma) that indicate clauses
#' @param exclude_verbs   A character vector with verbs to explicitly exclude as clause
#'
#' @return                A tokenindex
#' @export
corenlp_clauses <- function(tokens, verbs=NULL, exclude_verbs=NULL) {
  if (!require('rsyntax')) stop('Using this function requires the rsyntax package (not yet on CRAN)')

  passive = tquery(pos = 'VERB*', lemma = verbs, NOT(lemma = exclude_verbs), label='predicate', fill=F,
                   children(relation = c('agent'), label = 'subject'),
                   children(NOT(relation = 'mark'), label='predicate'))

  direct = tquery(pos = 'VERB*', lemma = verbs, NOT(lemma = exclude_verbs), label='verb', fill=F,
                  children(relation = c('nsubj', 'nsubjpass'), label='subject'),
                  children(NOT(relation = c('auxpass','mark')), label='clause'))


  copula = tquery(pos = 'VERB*', lemma = verbs, NOT(lemma = exclude_verbs), label='clause',
                  parents(label='verb', NOT(lemma = exclude_verbs),
                          children(relation = c('su', 'nsubj', 'agent'), label='subject')))

  rsyntax::annotate(tokens, 'clause', pas=passive, dir=direct, cop=copula)
}

#' Simplify the dependency tree by unpacking conjunctions and relative clauses
#'
#' @param tokens   A tokenindex (as_tokenindex)
#'
#' @return  A tokenindex
#' @export
corenlp_simplify <- function(tokens) {
  if (!require('rsyntax')) stop('Using this function requires the rsyntax package (not yet on CRAN)')
  tokens = corenlp_relcl(tokens, pron=NULL)
  tokens = corenlp_conjunctions(tokens)
  tokens
}


corenlp_conjunctions <- function(tokens) {
  no_fill = c('compound*','case', 'relcl')
  tq = tquery(label='target', NOT(relation = 'conj'),
              rsyntax::fill(NOT(relation = no_fill), window = c(Inf,0)),
              children(relation = 'conj', label='origin',
                       rsyntax::fill(NOT(relation = no_fill), window=c(0,Inf))))
  tokens = rsyntax:::climb_tree(tokens, tq)
  chop(tokens, relation = 'cc')
}


corenlp_relcl <- function(tokens, pron=NULL) {
  if (!is.null(pron)) {
    tq = tquery(relation = c('acl:relcl', 'relcl'), label='relcl',
                children(lemma =  pron, label='reference'),
                parents(label = "parent", pos=c('PROPN','NOUN')))

    tokens = select_nodes(tokens, tq)
    tokens = copy_nodes(tokens, 'parent', new = 'parent_copy', copy_fill = T)
    tokens = mutate_nodes(tokens, 'parent_copy', parent = reference$parent, relation = reference$relation)
    tokens = remove_nodes(tokens, 'reference', with_fill = T)
  }
  tq = tquery(relation = c('acl:relcl', 'relcl'), label='relcl',
              parents(label = "parent"))
  tokens = select_nodes(tokens, tq)
  tokens = mutate_nodes(tokens, 'relcl', parent = NA, relation = 'ROOT')
  #tokens = mutate_nodes(tokens, 'relcl', parent = parent$parent, relation = 'iso_rel')
  tokens
}
