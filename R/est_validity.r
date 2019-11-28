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
#' @param weight_range   A vector of length 2, with the min and max weight threshold
#' @param steps   The number of observations for which to calculate the weight threshold
#' @param min_weight Optionally, set a minimum weight only for this calculation
#' @param do_plot   IF set to FALSE, do not plot results (results are also returned as a data.frame)
#' @param from_subset Optionally, a logical vector of the same length as nrow(g$from_meta) to look only as specific cases
#' @param weight_col the name of the column with the weight scores
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
estimate_validity <- function(g, weight_range, steps, min_weight=NA, do_plot=T, from_sample=NULL, weight_col='weight', n_sample=NA) {
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

  thresholds = seq(from=weight_range[1], to = weight_range[2], length.out=steps)
  if (!is.na(min_weight)) thresholds = thresholds[thresholds > min_weight]
  res = data.frame(threshold=thresholds, N_after=NA, M_after=NA, FP_prob=NA)

  before_event = g$d$hourdiff < 0
  for (i in 1:length(thresholds)) {
    above_thres = g$d[[weight_col]] >= thresholds[i]
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

    g$d$daydiff = floor(g$d$hourdiff / 24)
    hist(g$d$daydiff[g$d[[weight_col]] >= res$threshold[top]], xlab='Day difference', main='Matches over time', right=F, breaks=20)
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

  d = merge(d, subset(g$from_meta, select=c('document_id','country')),
                      by.x='from', by.y='document_id')

  d[,before_c := cumsum(before), by='country']
  d[,after_c := cumsum(after), by='country']
  d[,before := cumsum(before), by='from']
  d[,after := cumsum(after), by='from']

  ## prepare margin scores and index
  m = data.table::data.table(from=g$from_meta$document_id,
                             country = g$from_meta$country,
                             before_n = g$from_meta$lag_n,
                             after_n = g$from_meta$from_n - g$from_meta$lag_n)
  mi = m[list(d$from), on = 'from', which=T]

  country = m[, list(before_n = sum(before_n, na.rm=T),
                     after_n = sum(after_n, na.rm=T)), by='country']
  countryi = country[list(d$country), on='country', which=T]

  ## calculate odds ratio
  #odds_before = (d$before / (m$before_n[mi] - d$before))
  #odds_after = (d$after / (m$after_n[mi] - d$after))
  #d[, odds_ratio := odds_after / odds_before]
  ## note that it is impossible that both before_n and after_n are zero
  ## so the outcome is never NaN (only 0 or Inf)

  ## pr total
  total_pr = calc_PR(n_before = sum(m$before_n, na.rm=T),
               n_after = sum(m$after_n, na.rm=T),
               m_before = d$before_all,
               m_after = d$after_all)

  #d$fp_before = d$before_all
  #d$total_before = sum(m$before_n, na.rm=T)
  #d$total_after = sum(m$after_n, na.rm=T)
  #d$false_rate_before = d$fp_before / d$total_before
  #d$fp_after = d$false_rate_before * d$total_after
  #d$TP = d$after_all - d$fp_after
  #d$P = d$TP / d$after_all
  #d$R = ifelse(d$TP < 0, NA, d$TP / max(d$TP, na.rm = T))
  #d$F1 = (2 * d$P * d$R) / (d$P + d$R)


  ## pr country
  country_pr = calc_PR(n_before = country$before_n[countryi],
               n_after = country$after_n[countryi],
               m_before = d$before_c,
               m_after = d$after_c)

  #d$c_before_prob = ((d$before_c + 0.1) / (country$before_n[countryi] + country$before_n[countryi]*0.1))
  #d$c_after_prob = ((d$after_c + 0.1) / (country$after_n[countryi] + country$after_n[countryi]*0.1))
  #d$Pc = 1 - (d$c_before_prob / d$c_after_prob)
  #d$Pc[d$Pc < 0] = 0



  ## pr event
  event_pr = calc_PR(n_before = m$before_n[mi],
               n_after = m$after_n[mi],
               m_before = d$before,
               m_after = d$after)

  #d$e_before_prob = ((d$before + 0.1) / (m$before_n[mi] + m$before_n[mi]*0.1))
  #d$e_after_prob = ((d$after + 0.1) / (m$after_n[mi] + m$after_n[mi]*0.1))
  #d$e_odds = d$e_after_prob / d$e_before_prob
  #d$Pe = 1 - (d$e_before_prob / d$e_after_prob)
  #d$Pe[d$Pe < 0] = 0

  #d$chi = calc_chi2(d$before,
  #                  d$after,
  #                  m$before_n[mi] - d$before,
  #                  m$after_n[mi] - d$after)
  #d$chi = ifelse(d$e_after_prob < d$e_before_prob, 0, d$chi)
  #d$chi_p = pchisq(d$chi, df=1)


  #d$Pe = 1 - (e_before_prob / e_after_prob)
  #d$Pe[d$Pe < 0] = 0
  #e_fp_after = e_false_rate_before * d$after
  #e_TP = d$after - e_fp_after
  #d$Pe = e_TP / d$after
  #prob_after = ((d$after + 0.5) / (m$after_n[mi] + m$after_n[mi]*0.5))
  #d$odds = prob_after / prob_before
  #d$prob = d$odds / (1 + d$odds)


  d = cbind(from=d$from, to=d$to, weight=d$weight, country=d$country, i = d$orig_i,
            t=total_pr, c=country_pr, e=event_pr)


  ## carry highest odds over to higher weights
  data.table::setorderv(d, cols = c('weight'))

  d[is.na(d)] = 0

  d[, t_F1s := cummax(t.F1)]
  d[, c_F1s := cummax(c.F1), by='country']
  d[, e_F1s := cummax(e.F1), by='from']

  is_max <- function(x) x == max(x, na.rm=T)
  d[, t_select := is_max(t_F1s)]
  d[, c_select := is_max(c_F1s), by='country']
  d[, e_select := is_max(e_F1s), by='from']


  data.table::setorderv(d, cols = c('i'))
  return(d)
}

#' Vectorized computation of chi^2 statistic for a 2x2 crosstab containing the values
#' [a, b]
#' [c, d]
#'
#' @param a topleft value of the table
#' @param b topright value
#' @param c bottomleft value
#' @param d bottomright value
#' @param correct if TRUE, use yates correction. Can be a vector of length a (i.e. the number of tables)
calc_chi2 <- function(a,b,c,d, correct=T){
  n = a+b+c+d
  sums = cbind(c1 = a+c, c2 = b+d, r1 = a+b, r2 = c+d)
  yates_correction = if (correct) rep(T, nrow(sums)) else rep(F, nrow(sums))

  x = as.numeric(a)*as.numeric(d) - as.numeric(b)*as.numeric(c) ## as.numeric to prevent integer overflow
  x = ifelse(yates_correction, abs(x) - n/2, x)
  chi = n*x^2 / (as.numeric(sums[,'c1']) * as.numeric(sums[,'c2']) * as.numeric(sums[,'r1']) * as.numeric(sums[,'r2']))
  chi = ifelse(is.na(chi), 0, chi)
  chi[chi == Inf] = 0
  chi
}


calc_PR <- function(n_before, n_after, m_before, m_after) {
  false_rate = m_before / n_before
  fp_after = false_rate * n_after
  TP = m_after - fp_after
  TP[TP < 0] = 0
  P = TP / m_after
  R = TP / max(TP, na.rm = T)
  F1 = (2 * P * R) / (P + R)
  data.table::data.table(P=P, R=R, F1=F1)
}
