#' Simplify the dependency tree
#'
#' @param tokens   A tokenindex (as_tokenindex)
#'
#' @return  A tokenindex
#' @export
spacy_simplify <- function(tokens) {
  if (!require('rsyntax')) stop('Using this function requires the rsyntax package (not yet on CRAN)')
  tokens = isolate_branch(tokens, c('appos','acl','acl:relcl','relcl','advcl'))
  #tokens = isolate_modifier(tokens, c('appos','acl'))
  #tokens = isolate_modifier(tokens, c('acl:relcl', 'relcl','advcl'))
  #tokens = isolate_noun_modifier(tokens, mods=c('appos'))
  #tokens = isolate_verb_modifier(tokens, mods=c('acl:relcl', 'relcl','advcl'))
  tokens = split_conjunctions(tokens)
  tokens
}

#isolate_noun_modifier <- function(tokens, mods=c('acl','appos')) {
#  ## Isolate and copy the noun parent
#  tq = tquery(label='noun',
#              fill(relation=c('compound*','flat')),
#              children(relation = mods, label='mod'))
#  tokens = select_nodes(tokens, tq)
#  tokens = copy_nodes(tokens, 'noun', new = 'noun_copy', copy_fill=T)
#  tokens = mutate_nodes(tokens, 'mod', parent = noun_copy$token_id)
#  tokens = mutate_nodes(tokens, 'noun_copy', parent = NA, relation='ROOT')
#  tokens
#}

#isolate_verb_modifier <- function(tokens, mods=c('acl:relcl', 'relcl','advcl')) {
#  ## relcl is relative clause.
#  ## simply isolate. Pronouns in the clause ("Bob, <who> is...") should be dealt with with coreference resolution
#  tq = tquery(relation = mods, label='mod',
#              parents(label = "parent"))
#  tokens = select_nodes(tokens, tq)
#  tokens = mutate_nodes(tokens, 'mod', parent = NA, relation = 'ROOT')
#  tokens
#}

#isolate_modifier <- function(tokens, mods, copy_parent=F, parent_fill=NULL) {
#  ## Isolate and copy the noun parent
#  tq = tquery(label='parent',
#              parent_fill,
#              children(relation = mods, label='mod'))
#  tokens = select_nodes(tokens, tq)
#  if (copy_parent){
#    tokens = copy_nodes(tokens, 'parent', new = 'parent_copy', copy_fill=T)
#    tokens = mutate_nodes(tokens, 'mod', parent = parent_copy$token_id)
#    tokens = mutate_nodes(tokens, 'parent_copy', parent = NA, relation='ROOT')
#  } else {
#    tokens = mutate_nodes(tokens, 'mod', parent = NA, relation = 'ROOT')
#  }
#  tokens
#}

isolate_branch <- function(tokens, rel) {
  ## Isolate and copy the noun parent
  tq = tquery(label='parent',
              children(relation = rel, label='branch'))
  tokens = select_nodes(tokens, tq)
  tokens = mutate_nodes(tokens, 'branch', parent = NA, relation = 'ROOT', branch_parent=parent$token_id)
  tokens
}


split_conjunctions <- function(tokens) {
  ## ignore most fill nodes for distant conjunctions (>= 3 words)
  tokens = split_tree(tokens, rel='conj', no_fill=c('acl:relcl','acl','appos','relcl', 'conj', 'cop', 'advmod','advcl','xcomp','obl','ccomp','aux','det'), min_dist = 3)
  ## copy most fill nodes for close conjunctions
  tokens = split_tree(tokens, rel='conj', no_fill=c('acl:relcl','relcl', 'conj', 'cop'))
  chop(tokens, relation='cc')
}

split_tree <- function(tokens, rel='conj', no_fill=NULL, min_dist=0, max_dist=Inf) {
  tq = tquery(label='target', NOT(relation = rel),
              children(relation = c('compound*', 'flat'), label='ignore', req=F),
              fill(NOT(relation = no_fill), max_window = c(Inf,0), connected=T),
              children(relation = rel, label='origin', min_window=c(min_dist,min_dist), max_window = c(max_dist,max_dist),
                       fill(NOT(relation = no_fill), max_window=c(0,Inf), connected=T)))
  tokens = climb_tree(tokens, tq)
}



function() {
  library(rsyntax)
  library(magrittr)
  #library(spacyr)
  #spacy_initialize('en_coref_lg')

  tokens = readRDS('Suspect_communities/backup.rds')
  tokens = as_tokenindex(tokens)

  islam = unique(tokens[grep('islam|muslim', tokens$token, ignore.case = T),c('doc_id','sentence')])
  terror = unique(tokens[grep('terror', tokens$token, ignore.case = T),c('doc_id','sentence')])
  both = islam[terror, on=c('doc_id','sentence')]

  tokens = tokens[both, on=c('doc_id','sentence') ,]

  tokens[both[1,c('doc_id','sentence')], on=c('doc_id','sentence')] %>%
    spacy_simplify() %>%
    plot_tree(token, lemma, pos)


  spacy_parse('a red, blue and green orb', dependency=T, coref=T) %>%
    as_tokenindex() %>%
    plot_tree()

  tokens %>%
    spacy_simplify()

}


