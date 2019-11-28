#' Get a vector of english verbs that indicate speech
#'
#' @param add_verbs  add verbs
#' @param rm_verbs   remove verbs
#'
#' @return A character vector
#' @export
#'
#' @examples
#' sayverbs()
sayverbs <- function(add_verbs=NULL, rm_verbs=NULL) {
  sv = c("tell", "show", "acknowledge", "admit", "affirm", "allege", "announce", "assert", "attest", "avow", "call", "claim", "comment", "concede", "confirm", "declare", "deny", "exclaim", "insist", "mention", "note", "post","predict", "proclaim", "promise", "reply", "remark", "report", "say", "speak", "state", "suggest", "talk", "tell", "think","warn","write", "add")
  sv = union(sv, add_verbs)
  sv = setdiff(sv, rm_verbs)
  sv
}

#' Function that wraps tqueries for extracting quotes
#'
#' @param tokens          A tokenindex
#' @param verbs           A character vector with verbs (as lemma) that indicate speech acts
#' @param exclude_verbs   A character vector with verbs to explicitly exclude as speech acts
#'
#' @return                A tokenindex
#' @export
corenlp_quotes <- function(tokens, verbs=sayverbs(), exclude_verbs=NULL) {
  if (!require('rsyntax')) stop('Using this function requires the rsyntax package (not yet on CRAN)')

  direct = tquery(lemma = verbs, NOT(lemma = exclude_verbs), label='verb', fill=F,
                  children(req=F, relation = c('npadvmod'), block=T),
                  children(relation=c('su', 'nsubj', 'agent', 'nmod:agent'), label='source'),
                  children(label='quote', NOT(relation=c('mark','prep','cc')),
                           rsyntax::fill(NOT(relation='mark'))))

  nosrc = tquery(upos='VERB*',
                 children(relation= c('su', 'nsubj', 'agent', 'nmod:agent'), label='source'),
                 children(lemma = verbs, NOT(lemma = exclude_verbs), relation='xcomp', label='verb',
                          children(relation=c("ccomp", "dep", "parataxis", "dobj", "nsubjpass", "advcl"), label='quote')))

  according = tquery(label='quote',
                     children(relation='nmod:according_to', label='source',
                              children(label='verb')))

  tokens = rsyntax::annotate(tokens, 'quote', dir=direct, nos=nosrc, acc=according)

  span1 = tquery(upos = 'VERB*', lemma = verbs,
                 children(relation=c('su', 'nsubj', 'agent', 'nmod:agent'), label='source'))
  span2 = tquery(upos = 'VERB*',
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
corenlp_clauses <- function(tokens, verbs=NULL, exclude_verbs=sayverbs()) {
  if (!require('rsyntax')) stop('Using this function requires the rsyntax package (not yet on CRAN)')

  passive = tquery(upos = 'VERB*', lemma = verbs, NOT(lemma = exclude_verbs), label='predicate', fill=F,
                   children(relation = c('agent'), label = 'subject'),
                   children(NOT(relation = 'mark'), label='predicate'))

  direct = tquery(upos = 'VERB*', lemma = verbs, NOT(lemma = exclude_verbs), label='verb', fill=F,
                  children(relation = c('nsubj', 'nsubjpass'), label='subject'),
                  children(NOT(relation = c('auxpass','mark')), label='clause'))


  copula = tquery(upos = 'VERB*', lemma = verbs, NOT(lemma = exclude_verbs), label='clause',
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
  tokens = split_conjunctions(tokens)
  tokens
}



split_tree <- function(tokens, rel='conj', parent_rel=NULL, parent_pos=NULL, no_fill=NULL, min_dist=0, max_dist=Inf) {
  tq = tquery(label='target', NOT(relation = rel), relation=parent_rel, upos=parent_pos,
              fill(NOT(relation = no_fill), max_window = c(Inf,0), connected=T),
              children(relation = rel, label='origin', min_window=c(min_dist,min_dist), max_window = c(max_dist,max_dist),
                       fill(NOT(relation = no_fill), max_window=c(0,Inf), connected=T)))
  tokens = climb_tree(tokens, tq)
}


resolve_siblings <- function(tokens, rel, no_fill=NULL) {
  sib = tokens[rel, on='relation']
  sib = sib[duplicated(sib[,c('doc_id','sentence','parent')])]
  tq = tquery(label='target',
              fill(NOT(relation=union(rel,no_fill)), connected=T),
              children(label='origin', g_id=sib[,c('doc_id','sentence','token_id')]))

  tokens = select_nodes(tokens, tq)
  tokens = copy_nodes(tokens, 'target', new = 'target_copy', copy_fill = T)
  tokens = mutate_nodes(tokens, 'origin', parent = target_copy$token_id)
  tokens
}

split_conjunctions <- function(tokens, rel='conj', parent_rel=NULL, parent_pos=NULL) {
  tokens = split_tree(tokens, rel=rel, parent_rel=parent_rel, parent_pos=parent_pos, no_fill=c('compound*','relcl', 'cop', 'advmod','advcl','xcomp','obl','ccomp','aux','det'), min_dist = 3)
  tokens = split_tree(tokens, rel=rel, parent_rel=parent_rel, parent_pos=parent_pos, no_fill=c('compound*','relcl', 'cop'))
  #tokens = resolve_siblings(tokens, rel=c('xcomp','nsubj'))
  chop(tokens, relation='cc')
}

extract_ann <- function(tokens, annotation, regex, ...) {
  m = grep(regex, tokens$token, ...)
  print(tokens[m,])
  ann_col = paste(annotation, 'id', sep='_')
  ids = unique(tokens[[ann_col]][m])
  tokens[tokens[[ann_col]] %in% ids,]
}



function() {
  library(rsyntax)
  library(magrittr)
  library(data.table)
  tokens = readRDS('Suspect_communities/backup.rds')
  tokens = as_tokenindex(tokens)


  tokens[ids[i,], on=c('doc_id','sentence')] %>%
    split_conjunctions() %>%
    corenlp_clauses() %>%
    extract_ann('clause', 'islam', ignore.case=T) %>%
    plot_tree(token, annotation='clause')



  ids = unique(tokens[grep('islam|muslim',tokens$token,ignore.case=T), c('doc_id','sentence')])

  i = 1
  paste(tokens[ids[i,], on=c('doc_id','sentence')]$token, collapse=' ')
  tokens[ids[i,], on=c('doc_id','sentence')] %>%
    plot_tree(token, upos)
  tokens[ids[i,], on=c('doc_id','sentence')] %>%
    split_conjunctions() %>%
    corenlp_clauses() %>%
    plot_tree(token, upos, annotation = 'clause')



  corenlp_quotes() %>%
  corenlp_clauses()

  tokens2 = corenlp_simplify(tokens)


  tokens[list(doc_id='world/2000/dec/08/terrorism', sentence=11)] %>%
    corenlp_simplify() %>%
    resolve_siblings('nsubj') %>%
    plot_tree(token, pdf_viewer = T)




}
