#' Function that wraps tqueries for extracting links
#'
#' Links of subjects to descriptions. This can be with the linking verb "be" ("Bob is sick"),
#' but also with appositional modifiers (Bob, the director, argued that...") or ajectival clauses ("A fish called Wanda")
#'
#' @param tokens          A tokenindex
#'
#' @return                A tokenindex
#' @export
spacy_links <- function(tokens) {
  mod = tquery(label='subject',
               fill(relation=c('compound*','flat')),
               children(relation=c('appos','amod'), label='predicate'))

  spacy_verb_clauses(tokens, column = 'link', verbs='be', mod=mod)
}

#' Function that wraps tqueries for extracting possive relations
#'
#' Links of subjects to possesives. This can be with the linking verb "have" ("Bob has cookies"),
#' but also with the poss relation.
#'
#' @param tokens          A tokenindex
#'
#' @return                A tokenindex
#' @export
spacy_poss <- function(tokens) {
  poss = tquery(label='subject',
                children(relation='poss', label='predicate'))
  spacy_verb_clauses(tokens, column = 'poss', verbs='have', poss=poss)
}

#' Function that wraps tqueries for extracting actions
#'
#' Actions
#'
#' @param tokens          A tokenindex
#'
#' @return                A tokenindex
#' @export
spacy_actions <- function(tokens) {
  say_verbs = c("tell", "show", "acknowledge", "admit", "affirm", "allege", "argue", "announce", "assert", "attest", "avow", "call", "claim", "comment", "concede", "confirm", "declare", "deny", "describe","exclaim", "express", "insist", "mention", "note", "post","predict", "proclaim", "promise", "reply", "refer", "remark", "report", "say","show", "speak", "state", "suggest", "talk", "tell", "think","warn","write", "add")
  link_verbs = c("be")
  spacy_verb_clauses(tokens, 'action', exclude_verbs=c(say_verbs, link_verbs))
}


#' Function that wraps tqueries for extracting quotes
#'
#' @param tokens          A tokenindex
#'
#' @return                A tokenindex
#' @export
spacy_quotes <- function(tokens) {
  if (!require('rsyntax')) stop('Using this function requires the rsyntax package (not yet on CRAN)')
  for (dropcol in c('quote','quote_id','quote_fill')) if (dropcol %in% colnames(tokens)) tokens[[dropcol]] = NULL

  say_verbs = c("tell", "show", "acknowledge", "admit", "affirm", "allege", "argue", "announce", "assert", "attest", "avow", "call", "claim", "comment", "concede", "confirm", "declare", "deny", "describe", "exclaim", "express", "insist", "mention", "note", "post","predict", "proclaim", "promise", "reply", "refer", "remark", "report", "say","show", "speak", "state", "suggest", "talk", "tell", "think","warn","write", "add")

  parataxis = tquery(label='quote',
                     children(relation='parataxis', lemma=say_verbs, label='verb',
                              children(relation= c('su', 'nsubj', 'agent', 'nmod:agent'), label='source')))

  relcl = tquery(relation = 'relcl', lemma=say_verbs, fill=F, label='verb',
                 parents(pos=c('PROPN','PRON'), label='source',
                         fill(NOT(relation=c('conj','relcl','acl','acl:relcl')), connected=T)),
                 children(NOT(relation=c('su', 'nsubj', 'agent', 'nmod:agent')), label='quote'))

  direct = tquery(lemma = say_verbs, label='verb', fill=F,
                  children(req=F, relation = c('npadvmod'), block=T),
                  children(relation=c('su', 'nsubj', 'agent', 'nmod:agent'), label='source'),
                  children(label='quote', NOT(relation=c('mark')),
                           rsyntax::fill(NOT(relation='mark'))))

  nosrc = tquery(pos='VERB*',
                 children(relation= c('su', 'nsubj', 'agent', 'nmod:agent'), label='source'),
                 children(lemma = say_verbs, relation='xcomp', label='verb',
                          children(relation=c("ccomp", "dep", "parataxis", "dobj", "nsubjpass", "advcl"), label='quote')))

  according = tquery(label='quote',
                     children(relation='nmod:according_to', label='source',
                              children(label='verb')))

  advcl = tquery(relation = 'advcl', lemma=say_verbs, label='verb',
                 parents(pos='VERB',
                         children(relation=c('su', 'nsubj', 'agent', 'nmod:agent'), label='source')),
                 children(NOT(relation=c('su', 'nsubj', 'agent', 'nmod:agent')), label='quote'))


  tokens = rsyntax::annotate(tokens, 'quote', par=parataxis, relcl=relcl, dir=direct, nos=nosrc, acc=according, advcl=advcl)

  span1 = tquery(pos = 'VERB*', lemma = say_verbs,
                 children(relation=c('su', 'nsubj', 'agent', 'nmod:agent'), pos=c('PRON','PROPN'), label='source'))
  span2 = tquery(pos = 'VERB*',
                 children(relation=c('su', 'nsubj', 'agent', 'nmod:agent'), pos=c('PRON','PROPN'), label='source'))

  tokens = add_span_quotes(tokens, 'token', quote_col = 'quote', source_val = 'source',
                           quote_val = 'quote', tqueries=list(span1,span2),
                           add_quote_symbols="'‘’", quote_subset = pos != "PART")
  print(sum(!is.na(tokens$quote_id)))
  tokens$token

  tokens
}

#' Function that wraps tqueries for extracting verb clauses
#'
#' @param tokens          A tokenindex
#' @param column          The name of the column with the annotations
#' @param verbs           Optionally, a character vector to specify specific verb lemma
#' @param exclude_verbs   Optionally, a character vector to specify verb lemma to exclude
#' @param ...             additional tqueries to annotate
#'
#' @return                A tokenindex
#' @export
spacy_verb_clauses <- function(tokens, column, verbs=NULL, exclude_verbs=NULL, ...) {
  if (!require('rsyntax')) stop('Using this function requires the rsyntax package (not yet on CRAN)')
  for (dropcol in paste0(column, c('','_id','_fill'))) if (dropcol %in% colnames(tokens)) tokens[[dropcol]] = NULL


  relcl = tquery(relation = 'relcl', lemma=verbs, NOT(lemma = exclude_verbs), fill=F, label='verb',
                 parents(relation=c('su', 'nsubj', 'agent', 'nmod:agent','dobj','nsubjpass'), label='subject',
                         fill(NOT(relation=c('conj','relcl','acl','acl:relcl')), connected=T)),
                 children(NOT(relation=c('su', 'nsubj', 'agent', 'nmod:agent')), label='predicate'))

  passive = tquery(pos = 'VERB*', lemma = verbs, NOT(lemma = exclude_verbs), label='predicate', fill=F,
                   children(relation = c('agent'), label = 'subject'),
                   children(NOT(relation = 'mark'), label='predicate'))

  direct = tquery(pos = 'VERB*', lemma = verbs, NOT(lemma = exclude_verbs), label='verb', fill=F,
                  children(relation = c('nsubj', 'nsubjpass'), label='subject'),
                  children(NOT(relation = c('auxpass','mark')), label='clause'))


  copula = tquery(pos = 'VERB*', lemma = verbs, NOT(lemma = exclude_verbs), label='clause',
                  parents(label='verb', NOT(lemma = exclude_verbs),
                          children(relation = c('su', 'nsubj', 'agent'), label='subject')))

  acl = tquery(label='clause',
               children(label='verb', relation='acl', lemma=verbs, NOT(lemma = exclude_verbs),
                        children(relation = c('su', 'nsubj', 'agent'), label='subject')))


  rsyntax::annotate(tokens, column, relcl=relcl, pas=passive, dir=direct, cop=copula, acl=acl, ...)
}












function() {
  library(rsyntax)
  library(spacyr)
  spacy_initialize('en_coref_lg')


  spacy_parse('Bob, vice-president, went home', dependency=T, coref=T) %>%
    as_tokenindex() %>%
    plot_tree(token, lemma, pos)

  spacy_parse('Bob stays a terrorist', dependency=T, coref=T) %>%
    as_tokenindex() %>%
    plot_tree(token, lemma, pos)



  spacy_parse('Bob, vice-president, went home', dependency=T, coref=T) %>%
    as_tokenindex() %>%
    spacy_links() %>%
    plot_tree(token, lemma, pos, annotation = 'link')

}
