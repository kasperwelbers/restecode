#' Estimate weight threshold by estimating precision and recall
#'
#' This function estimates a weight threshold for an RNewsflow document comparison network that serves as an "event matching" task.
#' The "from" documents in the edgelist need to be events, or other types of documents of which you can be sure that the date of the "to" documents cannot precede them.
#'
#' For the estimation to work best, the following settings should be considered in the newsflow.compare function (that creates the g input).
#' \itemize{
#'   \item The similarity threshold should be low enough that you are confident that all true positives are in the data
#'   \item The right hand side of the comparison window should be the time frame in which 'real' matches are most likely to occur. This
#'         estimation was designed for the task of finding out if an event is covered in the news while the event is still news.
#'         We cannot guarantee that it also works for long term matching.
#'   \item The left side of the comparison window can be longer than the right side to get good false positive estimates
#'         (see details), but should not be too long. Twice the size of the right side of the window is sufficient in our experience.
#'   \item Note (!!) that this function requires the direct output from the newsflow.compare function. If the data is manipulated in between
#'         (e.g., increasing the weight threshold, changing the time window) the estimation won't work.
#' }
#'
#' See details for more information on how the estimation works.
#'
#' @param g         The edgelist output of newsflow.compare (use the argument: return_as = "edgelist").
#' @param min_weight Optionally, set a minimum weight only for this calculation
#' @param do_plot   IF set to FALSE, do not plot results (results are also returned as a data.frame)
#' @param from_subset Optionally, a logical vector of the same length as nrow(g$from_meta) to look only as specific cases
#' @param n_sample  Draw a random sample of events. Overrides from_subset
#'
#' @details
#' We define a true positive as a match between a news document and event document where the news document indeed covers the event.
#' Accordingly, without actually looking at the news coverage, we can be sure that if the news document was published before the actual occurence of the event,
#' it is a false positive. We can use this information to get an estimate of the precision, recall and F1 scores. While the exact values of theses scores
#' will not be accurate, they can be used to see if certain differences in preparing or comparing the data (in particular, using different weight thresholds)
#' improves the results.
#'
#' To calculate these estimates we make the assumption that the probability of false positives in the matches for a given event
#' is the same before and after the event. We can then calculate the probability of false positives as the number of matches
#' divided by the number of news articles before the event to which the event has been compared (for the edgelist output of newsflow.compare,
#' the total number of comparisons is included in the "from_meta" attribute). We can then estimate the number of true positives as the observed
#' number of matches after the event minus the expected number of matches. To estimate the number of false negatives, we assume that the number of
#' true positives for the lowest available similarity threshold (i.e. the threshold used in input g) is the best estimate of the real number of true positives.
#'
#' This estimation works best if the input g has a deliberately low threshold, to get high recall with low precision. The right hand side
#' of the comparison window (i.e. matches after the event) should be limited to a time frame in which you expect true positives to occur. The left side of the comparison window
#' (matches before the event) should be long enough too get a good estimate of the probability of false positives. However, beware that taking a very long period makes it more likely to violate the assumption that this probabilty is constant over time. This is especially the case if
#' events are correlated with broader issues, such as terrorist attacks are often correlated with the political situation in a country.
#'
#' @return A plot of the estimation, and the data.frame with estimates can be assigned
#' @export
estimate_validity <- function(g, min_weight=NA, do_plot=T, from_sample=NULL, n_sample=NA) {
  if (!is.na(n_sample)) {
    from_sample = rep(F, nrow(g$from_meta))
    if (n_sample > length(from_sample)) stop('n_sample must be smaller than number of events')
    from_sample[sample(1:length(from_sample), size = n_sample)] = T
  }
  if (!is.null(from_sample)) {
    g$from_meta = subset(g$from_meta, from_sample)
    g$d = subset(g$d, from %in% g$from_meta$document_id)
  }

  before_n = sum(g$from_meta$lag_n, na.rm=T)
  after_n = sum(g$from_meta$from_n - g$from_meta$lag_n, na.rm=T)

  thresholds = seq(min(g$d$weight), 50, length.out=99)
  if (!is.na(min_weight)) thresholds = thresholds[thresholds > min_weight]
  res = data.frame(threshold=thresholds, N_after=NA, M_after=NA, FP_prob=NA)

  before_event = g$d$hourdiff < 0
  for (i in 1:length(thresholds)) {
    above_thres = g$d$weight >= thresholds[i]
    if (sum(above_thres) == 0) break

    res$N_after[i] = after_n
    res$M_after[i] = sum(above_thres & !before_event)
    res$FP_prob[i] = sum(above_thres & before_event) / before_n

    ## or use upper conf int (but takes long to compute)
    #bt = binom.test(sum(above_thres & before_event), before_n)
    #res$FP_prob[i] = bt$conf.int[2]
  }
  res = res[!is.na(res$FP_prob) &! res$N_after == 0,]

  res$FP = res$FP_prob * res$N_after
  res$TP = res$M_after - res$FP
  res$P = ifelse(res$N_after == 0, NA, res$TP / res$M_after)
  res$R = ifelse(res$TP < 0, NA, res$TP / max(res$TP))
  res$F1 = (2 * res$P * res$R) / (res$P + res$R)

  res$P = res$P * 100
  res$R = res$R * 100
  res$F1 = res$F1 * 100

  if (do_plot) {
    par(mar=c(4,3,4,2), mfrow=c(1,2), xpd=F)
    plot(res$threshold, res$P, type='l', lty=1, lwd=2,
         xlab='Weight threshold', ylab='',
         ylim=c(0,100), bty='l', main='Match level P/R estimation')
    lines(res$threshold, res$R, type='l', lty=2, lwd=2)
    lines(res$threshold, res$F1, type='l', lty=1, lwd=3, col='darkgrey')

    top = which.max(res$F1)
    graphics::abline(v=res$threshold[top], lty=2)
    graphics::text(x=res$threshold[top], y=95, labels=paste(' weight =',round(res$threshold[top],2)), adj=0, font=3, cex=0.9, family='mono')
    graphics::legend('right', legend = c(expression(hat("P")), expression(hat("R")), expression(hat("F1"))), bty='n', lty=c(1,2,1), col=c('black','black','darkgrey'), lwd=2)

    hist(g$d$hourdiff[g$d$weight >= res$threshold[top]], xlab='Hour difference', main='Matches per hour', right=F)
  }

  invisible(res)
}

moving_weight <- function(g, min_ratio=2, min_weight=NA, report_pr=T) {
  d = data.table::data.table(from=g$d$from,
                 to=g$d$to,
                 weight=g$d$weight,
                 orig_i=1:nrow(g$d))
  d[, before := as.numeric(g$d$hourdiff < 0)]
  d[, after := as.numeric(g$d$hourdiff >= 0)]
  d[, before_all := as.numeric(g$d$hourdiff < 0)]
  d[, after_all := as.numeric(g$d$hourdiff >= 0)]

  ## sort on -weight to calculate total number of matches for each weight value
  data.table::setorderv(d, cols = c('weight'), order = c(-1))
  d[,before_all := cumsum(before)]
  d[,after_all := cumsum(after)]
  d[,before := cumsum(before), by='from']
  d[,after := cumsum(after), by='from']

  ## prepare margin scores and index
  m =data.table::data.table(from=g$from_meta$document_id,
                 before_n = g$from_meta$lag_n,
                 after_n = g$from_meta$from_n - g$from_meta$lag_n)
  mi = m[list(d$from), on = 'from', which=T]

  ## calculate odds ratio
  #odds_before = (d$before / (m$before_n[mi] - d$before))
  #odds_after = (d$after / (m$after_n[mi] - d$after))
  #d[, odds_ratio := odds_after / odds_before]
  ## note that it is impossible that both before_n and after_n are zero
  ## so the outcome is never NaN (only 0 or Inf)

  ## alternative, use all matches for any event with this weight to calculate probability of false positive
  ## but use after current event for actual matches
  odds_before = (d$before_all + 0.5 / (sum(m$before_n, na.rm = T) - d$before_all + 0.5))
  odds_after = (d$after + 0.5 / (m$after_n[mi] - d$after + 0.5))
  d[, odds_ratio := odds_after / odds_before]

  ## probablity of fp is postives that occured before event,
  ## plus positives that occured after event times fp probability
  ## divided by n
  d$fp_before = d$before_all
  d$total_before = sum(m$before_n, na.rm=T)
  d$total_after = sum(m$after_n, na.rm=T)
  d$false_rate_before = d$fp_before / d$total_before
  d$fp_after = d$false_rate_before * d$total_after
  d$tp = d$after_all - d$fp_after
  d$p = d$tp / 1:nrow(d)
  d$prob = 1 - ((1-d$p)^d$after)

  ## we'll also add the cumsum of the n_before

  ## now sort on +weight, so that we can find the first match for from
  ## of which the odds_ratio > min_ratio
  data.table::setorderv(d, 'orig_i')
  return(d$p)

  data.table::setorderv(d, cols = c('from','weight'))
  ## find where threshold should stop
  d[, stop_threshold := odds_ratio > min_ratio]
  if (!is.na(min_weight)) d[weight < min_weight, stop_threshold := F]
  ## use cumsum > 0 so that all weights after stop are also TRUE
  ever_since <- function(x) cumsum(x) > 0
  d[, stop_threshold := ever_since(stop_threshold), by='from']

  d


  rbeta(10, 10,1000000)

  ## return logical vector in original order
  data.table::setorderv(d, 'orig_i')

  mean(d$odds_ratio > 680 & d$weight < 11)
  mean(d$odds_ratio < 680 & d$weight >= 11)

  d[d$odds_ratio > 680,]

  d

  sum(d$odds_ratio == Inf)
  sum(d$odds_ratio >= 2)


  pr = calculate_gtd_pr(g$d[d$stop_threshold & g$d$hourdiff >= 0,])
  message(sprintf('Matches:\nPrecision: %s\nRecall:  %s\nF1:    %s', pr$Pm, pr$Rm, pr$F1m))
  message(sprintf('\n\nArticles:\nPrecision: %s\nRecall:  %s\nF1:    %s', pr$Pa, pr$Ra, pr$F1a))

  d$stop_threshold
}
