#' Plot Precision and Recall scores
#'
#' Uses the gold standard in the gold_matches data to calculate Precision and Recall scores.
#' Results are calculated separately for the article level (does the article have at least one GTD match) and
#' match level (each individual match of an article to a GTD event). Results are also calculated for different
#' weight thresholds, which will be plotted on the x-axis.
#'
#' The results are also returned as a list with data.frames.
#' See gold_matches$description for details on the gold standard.
#'
#' @param g               An edgelist, as created with newsflow.compare
#' @param hourdiff_range  Filter on hour difference
#' @param weight_range    The range of weight to plot on the x axis
#' @param steps           The number of ticks on the x axis
#' @param weights         Optionally, provide a vector of weights. This overrides the weight_range and steps arguments
#'
#' @return A plot an a list with data.frames
#' @export
#'
#' @examples
gtd_pr <- function(g, hourdiff_range=c(0,7*24), weight_range=c(1,30), steps=30, weights=NULL) {
  a = gold_matches$articles
  m = gold_matches$matches
  m$real = T
  d = g[g$hourdiff >= hourdiff_range[1] & g$hourdiff < hourdiff_range[2],]
  d = d[d$to %in% a$guardian_id,]

  if (is.null(weights)) {
    minw = weight_range[1]
    maxw = weight_range[2]
    weights = seq(minw, maxw, length.out = steps)
  }

  art_res = data.frame(weight=weights, Precision=NA, Recall=NA)
  match_res = data.frame(weight=weights, Precision=NA, Recall=NA)

  for (i in seq_along(weights)) {
    e = d[d$weight >= weights[i],]
    e = merge(e,m, by.x=c('from','to'), by.y=c('gtd_id','guardian_id'), all=T)
    e$real[is.na(e$real)] = F
    e$hit = !is.na(e$weight)
    e$hit_f = factor(e$hit, levels=c(F,T))
    e$real_f = factor(e$real, levels=c(F,T))

    tab = table(hit=e$hit_f, real=e$real_f)
    p = round(tab[2,2] / sum(tab[2,]),3) * 100
    r = round(tab[2,2] / sum(tab[,2]),3) * 100
    match_res$Precision[i] = p
    match_res$Recall[i] = r
    match_res$F1[i] = round(2*(r * p) / (r + p),1)


    e2 = e[order(-e$real),]
    e2 = e2[!duplicated(e2$to),]
    tab = table(hit=e2$hit_f, real=e2$real_f)
    p = round(tab[2,2] / sum(tab[2,]),3) * 100
    r = round(tab[2,2] / sum(tab[,2]),3) * 100
    art_res$Precision[i] = p
    art_res$Recall[i] = r
    art_res$F1[i] = round(2*(r * p) / (r + p),1)

  }

  par(mfrow=c(1,1))

  minx = min(c(art_res$Precision, match_res$Precision))
  maxx = max(c(art_res$Precision, match_res$Precision))
  miny = min(c(art_res$Recall, match_res$Recall))
  maxy = max(c(art_res$Recall, match_res$Recall))

  #par(mfrow=c(1,1))
  #plot(art_res$Precision, art_res$Recall, type='l',
  #     xlab='Precision', ylab='Recall',
  #     xlim=c(0,100), ylim=c(0,100), bty='l')
  #lines(match_res$Precision, match_res$Recall, type='l', lty=2)

  m <- matrix(c(1,2,3,3), nrow = 2,ncol = 2,byrow = TRUE)

  layout(mat = m,heights = c(0.8,0.2))
  par(mar=c(4,3,4,2), xpd=F)
  plot(art_res$weight, art_res$Precision, type='l', lty=1, lwd=2,
       xlab='Weight threshold', ylab='',
       ylim=c(0,100), bty='l', main='Article level')
  lines(art_res$weight, art_res$Recall, type='l', lty=2, lwd=2)
  lines(art_res$weight, art_res$F1, type='l', lty=3, lwd=3, col='darkgrey')
  top = which.max(art_res$F1)
  graphics::abline(v=art_res$weight[top], lty=2)
  graphics::text(x=art_res$weight[top], y=2, labels=paste(' weight =',round(art_res$weight[top],2)), adj=0, font=3, cex=0.9, family='mono')


  plot(match_res$weight, match_res$Precision, type='l', lty=1, lwd=2,
       xlab='Weight threshold', ylab='',
       ylim=c(0,100), bty='l', main='Match level')
  lines(match_res$weight, match_res$Recall, type='l', lty=2, lwd=2)
  lines(match_res$weight, match_res$F1, type='l', lty=3, lwd=3, col='darkgrey')
  top = which.max(match_res$F1)
  graphics::abline(v=match_res$weight[top], lty=2)
  graphics::text(x=match_res$weight[top], y=2, labels=paste(' weight =',round(match_res$weight[top],2)), adj=0, font=3, cex=0.9, family='mono')


  par(mar=c(0,0,1,0), xpd=T)
  plot(1, type = "n", axes=FALSE, xlab="", ylab="")
  legend('top', legend=c('Precision','Recall','F1'), lty=c(1,2,3), lwd=c(2,2,3), col=c('black','black','darkgrey'),
         horiz=T, bty='n', cex=1.2)

  par(mar=c(4,4,4,4), mfrow=c(1,1))
  invisible(list(articles=art_res, matches=match_res))

}


#' Compare results of gtd_pr
#'
#' Calculates the difference of the precision and recall scores in pr1 and pr2.
#' This is calculated as pr2 minus pr1. So if precision in pr2 is 20, and precision in pr1 is 15,
#' the difference indicates an improvement of 5.
#'
#' @param pr1 Results of gtd_pr
#' @param pr2 Results of gtd_pr
#'
#' @return A plot an a list with data.frames
#' @export
#'
#' @examples
gtd_compare_pr <- function(pr1, pr2) {
  if (!identical(pr1$articles$weight, pr2$articles$weight)) stop('compare_pr can only be used if weight columns are identical')

  art_res = data.frame(weight = pr1$matches$weight,
                       Precision = pr2$articles$Precision - pr1$articles$Precision,
                       Recall = pr2$articles$Recall - pr1$articles$Recall,
                       F1 = pr2$articles$F1 - pr1$articles$F1)
  match_res = data.frame(weight = pr1$matches$weight,
                       Precision = pr2$matches$Precision - pr1$matches$Precision,
                       Recall = pr2$matches$Recall - pr1$matches$Recall,
                       F1 = pr2$matches$F1 - pr1$matches$F1)

  m <- matrix(c(1,2,3,3), nrow = 2,ncol = 2,byrow = TRUE)

  layout(mat = m,heights = c(0.8,0.2))
  par(mar=c(4,3,4,2), xpd=T)
  plot(art_res$weight, art_res$Precision, type='l', lty=1, lwd=2,
       xlab='Weight threshold', ylab='',
       ylim=c(min(c(art_res$Precision,art_res$Recall,art_res$F1)),max(c(art_res$Precision,art_res$Recall,art_res$F1))),
       bty='l', main='Article level P/R')
  lines(art_res$weight, art_res$Recall, type='l', lty=2, lwd=2)
  lines(art_res$weight, art_res$F1, type='l', lty=3, lwd=3, col='darkgrey')

  plot(match_res$weight, match_res$Precision, type='l', lty=1, lwd=2,
       xlab='Weight threshold', ylab='',
       ylim=c(min(c(match_res$Precision,match_res$Recall,match_res$F1)),max(c(match_res$Precision,match_res$Recall,match_res$F1))),
       bty='l', main='Match level P/R')
  lines(match_res$weight, match_res$Recall, type='l', lty=2, lwd=2)
  lines(match_res$weight, match_res$F1, type='l', lty=3, lwd=3, col='darkgrey')

  par(mar=c(0,0,1,0))
  plot(1, type = "n", axes=FALSE, xlab="", ylab="")
  legend('top', legend=c('Precision difference','Recall difference', 'F1 difference'), lty=c(1,2,3), lwd=c(2,2,3), col=c('black','black','darkgrey'),
         horiz=T, bty='n', cex=1.2)

  par(mar=c(4,4,4,4), mfrow=c(1,1))
  invisible(list(articles=art_res, matches=match_res))

}

