#' Inspect effects of thresholds on matches over time
#'
#' If it can be assumed that matches should only occur within a given time range (e.g., event data should match news items after the event occured)
#' a low effort validation can be obtained by looking at whether the matches only occur within this time range.
#' This function plots the percentage of matches within a given time range (hourdiff) for different thresholds of the weight column.
#' This can be used to determine a good threshold.s
#'
#' @param g                 The edgelist output of newsflow.compare (use the argument: return_as = "edgelist"). Has to come directly from newsflow.compare (i.e. no intermediate operations performed such as subsetting),
#'                          because the current function requires certain attributes that are removed if g is changed. Also, the margin_attr argument in newsflow.compare has to be TRUE (as is the default)
#' @param total_hourdiff    The range of the hourdiff value in g. This should be the same as the hour.window in newsflow.compare (if g has not been subsetted afterwards).
#' @param expected_hourdiff A vector of length 2, that indicates the range (including endpoints) in which you expect matches to occur based on reasonable assumptions about the data.
#'                          For matching events to news articles, a very reasonable assumption is that we expect matches to occur 'after' the event took place,
#'                          and a reasonable second assumption is that we expect matches to occur 'within a limited amount of time' after the event.
#' @param min_weight        Filter out all matches below the given weight
#' @param breaks            The number of breaks for the weight threshold
#' @param hist_breaks       the number of breaks on the histogram
#'
#' @return A plot, and the plot data can be assigned
#' @export
time_based_validity <- function(g, total_hourdiff, expected_hourdiff, min_weight=NA, lambda=log(2)/24, breaks=100, hist_breaks=NA) {
  if (expected_hourdiff[1] < total_hourdiff[1]) stop('expected hourdiff should be within the total hourdiff')
  if (expected_hourdiff[2] > total_hourdiff[2]) stop('expected hourdiff should be within the total hourdiff')

  if (is.null(attr(g, 'from_meta'))) stop('g does not have the "from_meta" attribute. Probably, g has been changed after being created with newsflow.compare (see documentation for argument g)')
  meta = attr(g, 'from_meta')
  if (!all(c('lag_n','from_n') %in% colnames(meta))) stop("g does not have margin attributes. Make sure the margin_attr argument in newsflow.compare is TRUE.")

  g = g[g$hourdiff >= total_hourdiff[1] & g$hourdiff <= total_hourdiff[2],]
  if (is.na(min_weight)) min_weight = min(weight)

  ef = expected_filter(total_hourdiff, expected_hourdiff, lambda=lambda, do_plot=F)
  expected_coef = ef$y[match(floor(hourdiff), ef$t)]

  #correct_hourdiff = expected_hourdiff[2] - expected_hourdiff[1]
  exp_window = sum(ef$y)
  unexp_window = sum(1-ef$y)

  if (is.na(hist_breaks)) {
    hist_breaks = (total_hourdiff[2] - total_hourdiff[1]) / (expected_hourdiff[2] - expected_hourdiff[1])
    hist_breaks = hist_breaks*3
  }

  if (exp_window == 0 || unexp_window == 0) stop('the total_hourdiff with expected_hourdiff settings are invalid')

  thresholds = seq(min(weight), max(weight), length.out=breaks)
  res = data.frame(threshold=thresholds, n=NA, exp_ph=NA, unexp_ph=NA)
  #is_expected = hourdiff >= expected_hourdiff[1] & hourdiff <= expected_hourdiff[2]


  #browser()

  for (i in 1:length(thresholds)) {
    is_match = weight > thresholds[i]
    #res$n[i] = sum(is_expected & is_match)
    exp_n = sum(is_match * expected_coef)
    unexp_n = sum(is_match * (1-expected_coef))
    if (exp_n == 0 || unexp_n == 0) break
    res$n[i] = sum(is_match)
    res$exp_ph[i] = exp_n / exp_window
    res$unexp_ph[i] = unexp_n / unexp_window
    res$ratio[i] = (res$exp_ph[i] - res$unexp_ph[i])
  }
  res = res[!is.na(res$n),]
  #res$ratio = res$exp_ph / (res$unexp_ph + 0.0000001)
  #res$exp_P = res$exp_ph / (res$unexp_ph + res$exp_ph)
  #res$unexp_P = res$exp_ph / (res$unexp_ph + res$exp_ph)
  #res$est_FP = (res$unexp_P * res$n) + 0.00001
  #res$est_TP = (res$exp_P * res$n) + 0.00001
  #res$ratio_by_n = (res$exp_ph / res$unexp_ph)
  #exp_P_per_n = res$exp_P / res$n


  #print(res)
  graphics::par(mar = c(3,3,3,3), mfrow=c(1,1), xpd=F)
  graphics::layout(matrix(c(1,2), ncol = 1, nrow=2), heights = c(0.6,0.4))
  graphics::plot(as.numeric(res$threshold), res$ratio, type='l', xlab='weight threshold', ylab='pct in range / est noice',
                 #ylim=c(min(c(res$exp_ph,res$unexp_ph)),max(c(res$exp_ph,res$unexp_ph))),
                 #ylim  = c(min(res$est_TP), max(res$est_TP)),
                 log='y',
                 main=sprintf('Matches within hour range %s to %s', expected_hourdiff[1], expected_hourdiff[2]))
  #graphics::lines(as.numeric(res$threshold), res$unexp_ph, type='l', lty=2, xlab='weight threshold', ylab='pct in range / est noice')
  #maxv = max(res$est_TP)
  #axis(2, c(1 %o% 10^(1:ceiling(log10(maxv)))))  # draw y axis with required labels

  #graphics::par(new = T)
  #graphics::plot(as.numeric(res$threshold), res$n, pch=10, axes=F, xlab=NA, ylab=NA, cex=0.5, log='y')
  #graphics::axis(side = 4)
  #graphics::mtext(side = 4, line = 3, 'N in range')

  is_match = weight > min_weight
  #is_expected = hourdiff >= expected_hourdiff[1] & hourdiff <= expected_hourdiff[2]
  n = sum(is_match)
  exp_ph = sum(is_match * expected_coef) / exp_window
  unexp_ph = sum(is_match * expected_coef) / unexp_window
  exp_P = exp_ph / (unexp_ph + exp_ph)
  est_TP = exp_P*n
  exp_P_per_n = exp_P / n

  graphics::abline(v=min_weight, lty=2)
  print(res)
  print(est_TP / n)
  graphics::text(x=min_weight+(0.2*min_weight), y=exp(log(max(res$est_TP))*0.85), labels=sprintf('weight = %s\nN      = %s\nTP     = %s', round(min_weight,3), n, round(est_TP)), adj=0, font=3, cex=0.8, family='mono')
  graphics::legend("topright",
                   legend=c("Expected / hour", "Unexpected / hour"),
                   lty=c(1,2), col=c("black", "black"))


  graphics::hist(hourdiff[weight > min_weight],
                 right=F, main=sprintf('Matches over time with threshold %s', round(min_weight, 3)), breaks=hist_breaks, xlab='hour difference')
  invisible(res)
}

#' Inspect effects of thresholds on matches over time
#'
#' If it can be assumed that matches should only occur within a given time range (e.g., event data should match news items after the event occured)
#' a low effort validation can be obtained by looking at whether the matches only occur within this time range.
#' This function plots the percentage of matches within a given time range (hourdiff) for different thresholds of the weight column.
#' This can be used to determine a good threshold.
#'
#' @param g                 The output of newsflow.compare (either as "igraph" or "edgelist")
#' @param total_hourdiff    The range of the hourdiff value in g. This should be the same as the hour.window in newsflow.compare (if g has not been subsetted afterwards).
#' @param expected_hourdiff A vector of length 2, that indicates the range (including endpoints) in which you expect matches to occur based on reasonable assumptions about the data.
#'                          For matching events to news articles, a very reasonable assumption is that we expect matches to occur 'after' the event took place,
#'                          and a reasonable second assumption is that we expect matches to occur 'within a limited amount of time' after the event.
#' @param min_weight        Filter out all matches below the given weight
#' @param breaks            The number of breaks for the weight threshold
#' @param hist_breaks       the number of breaks on the histogram
#'
#' @return A plot, and the plot data can be assigned
#' @export
expected_hourdiff_validity <- function(g, total_hourdiff, expected_hourdiff, min_weight=NA, breaks=100, hist_breaks=NA) {
  if (expected_hourdiff[1] < total_hourdiff[1]) stop('expected hourdiff should be within the total hourdiff')
  if (expected_hourdiff[2] > total_hourdiff[2]) stop('expected hourdiff should be within the total hourdiff')

  weight = if(methods::is(g,'igraph')) E(g)$weight else g$weight
  hourdiff = if(methods::is(g,'igraph')) E(g)$hourdiff else g$hourdiff
  weight = weight[hourdiff >= total_hourdiff[1] & hourdiff <= total_hourdiff[2]]
  hourdiff = hourdiff[hourdiff >= total_hourdiff[1] & hourdiff <= total_hourdiff[2]]
  if (is.na(min_weight)) min_weight = min(weight)


  correct_hourdiff = expected_hourdiff[2] - expected_hourdiff[1]
  exp_window = correct_hourdiff / (total_hourdiff[2] - total_hourdiff[1])
  unexp_window = 1 - exp_window

  if (is.na(hist_breaks)) {
    hist_breaks = (total_hourdiff[2] - total_hourdiff[1]) / (expected_hourdiff[2] - expected_hourdiff[1])
    hist_breaks = hist_breaks*3
  }

  if (exp_window == 0 || unexp_window == 0) stop('the total_hourdiff with expected_hourdiff settings are invalid')

  thresholds = seq(min(weight), max(weight), length.out=breaks)
  res = data.frame(threshold=thresholds, n=NA, exp_ph=NA, unexp_ph=NA)


  #is_expected = hourdiff >= expected_hourdiff[1] & hourdiff <= expected_hourdiff[2]

  is_expected = hourdiff >= 0

  #browser()

  for (i in 1:length(thresholds)) {
    is_match = weight > thresholds[i]
    #res$n[i] = sum(is_expected & is_match)
    exp_n = sum(is_match & is_expected)
    unexp_n = sum(is_match &! is_expected)
    if (exp_n == 0 || unexp_n == 0) break
    res$n[i] = sum(is_match)
    res$exp_ph[i] = exp_n / exp_window
    res$unexp_ph[i] = unexp_n / unexp_window
    res$TPP[i] = exp_n - unexp_n
    res$ratioP[i] = ((exp_n / unexp_n) * sqrt(res$n[i]))

  }
  res = res[!is.na(res$n),]
  print(res)
  res$ratio = res$exp_ph / (res$unexp_ph + 0.0000001)
  res$ratio_TP = res$ratio * res$n


  #res$exp_P = res$exp_ph / (res$unexp_ph + res$exp_ph)
  #res$unexp_P = res$exp_ph / (res$unexp_ph + res$exp_ph)
  #res$est_FP = (res$unexp_P * res$n) + 0.00001
  #res$est_TP = (res$exp_P * res$n) + 0.00001
  #res$ratio_by_n = (res$exp_ph / res$unexp_ph)
  #exp_P_per_n = res$exp_P / res$n


  #print(res)
  graphics::par(mar = c(3,3,3,3), mfrow=c(1,1), xpd=F)
  graphics::layout(matrix(c(1,2), ncol = 1, nrow=2), heights = c(0.6,0.4))
  graphics::plot(as.numeric(res$threshold), res$ratioP, type='l', xlab='weight threshold', ylab='pct in range / est noice',
                 #ylim=c(min(c(res$exp_ph,res$unexp_ph)),max(c(res$exp_ph,res$unexp_ph))),
                 #ylim  = c(min(res$est_TP), max(res$est_TP)),
                 log='y',
                 main=sprintf('Matches within hour range %s to %s', expected_hourdiff[1], expected_hourdiff[2]))
  #graphics::lines(as.numeric(res$threshold), res$unexp_ph, type='l', lty=2, xlab='weight threshold', ylab='pct in range / est noice')
  #maxv = max(res$est_TP)
  #axis(2, c(1 %o% 10^(1:ceiling(log10(maxv)))))  # draw y axis with required labels

  graphics::par(new = T)
  graphics::plot(as.numeric(res$threshold), res$n, pch=10, axes=F, xlab=NA, ylab=NA, cex=0.5, log='y')
  graphics::axis(side = 4)
  graphics::mtext(side = 4, line = 3, 'N in range')

  is_match = weight > min_weight
  is_expected = hourdiff >= expected_hourdiff[1] & hourdiff <= expected_hourdiff[2]
  n = sum(is_match)
  exp_ph = sum(is_match & is_expected) / exp_window
  unexp_ph = sum(is_match &! is_expected) / unexp_window
  exp_P = exp_ph / (unexp_ph + exp_ph)
  est_TP = exp_P*n
  exp_P_per_n = exp_P / n

  graphics::abline(v=min_weight, lty=2)
  print(res)
  print(est_TP / n)
  graphics::text(x=min_weight+(0.2*min_weight), y=exp(log(max(res$est_TP))*0.85), labels=sprintf('weight = %s\nN      = %s\nTP     = %s', round(min_weight,3), n, round(est_TP)), adj=0, font=3, cex=0.8, family='mono')
  graphics::legend("topright",
                   legend=c("Expected / hour", "Unexpected / hour"),
                   lty=c(1,2), col=c("black", "black"))


  graphics::hist(hourdiff[weight > min_weight],
                 right=F, main=sprintf('Matches over time with threshold %s', round(min_weight, 3)), breaks=hist_breaks, xlab='hour difference')
  invisible(res)
}

#' Inspect effects of thresholds on matches over time
#'
#' If it can be assumed that matches should only occur within a given time range (e.g., event data should match news items after the event occured)
#' a low effort validation can be obtained by looking at whether the matches only occur within this time range.
#' This function plots the percentage of matches within a given time range (hourdiff) for different thresholds of the weight column.
#' This can be used to determine a good threshold.s
#'
#' @param g                 The edgelist output of newsflow.compare (use the argument: return_as = "edgelist"). Has to come directly from newsflow.compare (i.e. no intermediate operations performed such as subsetting),
#'                          because the current function requires certain attributes that are removed if g is changed. Also, the margin_attr argument in newsflow.compare has to be TRUE (as is the default)
#' @param total_hourdiff    The range of the hourdiff value in g. This should be the same as the hour.window in newsflow.compare (if g has not been subsetted afterwards).
#' @param expected_hourdiff A vector of length 2, that indicates the range (including endpoints) in which you expect matches to occur based on reasonable assumptions about the data.
#'                          For matching events to news articles, a very reasonable assumption is that we expect matches to occur 'after' the event took place,
#'                          and a reasonable second assumption is that we expect matches to occur 'within a limited amount of time' after the event.
#' @param min_weight        Filter out all matches below the given weight
#' @param breaks            The number of breaks for the weight threshold
#' @param hist_breaks       the number of breaks on the histogram
#'
#' @return A plot, and the plot data can be assigned
#' @export
expectation_validity <- function(g, total_hourdiff, expected_hourdiff, min_weight=NA, lambda=log(2)/24, breaks=100, hist_breaks=NA) {
  if (expected_hourdiff[1] < total_hourdiff[1]) stop('expected hourdiff should be within the total hourdiff')
  if (expected_hourdiff[2] > total_hourdiff[2]) stop('expected hourdiff should be within the total hourdiff')

  if (is.null(attr(g, 'from_meta'))) stop('g does not have the "from_meta" attribute. Probably, g has been changed after being created with newsflow.compare (see documentation for argument g)')
  meta = attr(g, 'from_meta')
  if (!all(c('lag_n','from_n') %in% colnames(meta))) stop("g does not have margin attributes. Make sure the margin_attr argument in newsflow.compare is TRUE.")

  g = g[g$hourdiff >= total_hourdiff[1] & g$hourdiff <= total_hourdiff[2],]
  if (is.na(min_weight)) min_weight = min(weight)

  ef = expected_filter(total_hourdiff, expected_hourdiff, lambda=lambda, do_plot=F)
  expected_coef = ef$y[match(floor(hourdiff), ef$t)]

  #correct_hourdiff = expected_hourdiff[2] - expected_hourdiff[1]
  exp_window = sum(ef$y)
  unexp_window = sum(1-ef$y)

  if (is.na(hist_breaks)) {
    hist_breaks = (total_hourdiff[2] - total_hourdiff[1]) / (expected_hourdiff[2] - expected_hourdiff[1])
    hist_breaks = hist_breaks*3
  }

  if (exp_window == 0 || unexp_window == 0) stop('the total_hourdiff with expected_hourdiff settings are invalid')

  thresholds = seq(min(weight), max(weight), length.out=breaks)
  res = data.frame(threshold=thresholds, n=NA, exp_ph=NA, unexp_ph=NA)
  #is_expected = hourdiff >= expected_hourdiff[1] & hourdiff <= expected_hourdiff[2]


  #browser()

  for (i in 1:length(thresholds)) {
    is_match = weight > thresholds[i]
    #res$n[i] = sum(is_expected & is_match)
    exp_n = sum(is_match * expected_coef)
    unexp_n = sum(is_match * (1-expected_coef))
    if (exp_n == 0 || unexp_n == 0) break
    res$n[i] = sum(is_match)
    res$exp_ph[i] = exp_n / exp_window
    res$unexp_ph[i] = unexp_n / unexp_window
    res$ratio[i] = (res$exp_ph[i] - res$unexp_ph[i])
  }
  res = res[!is.na(res$n),]
  #res$ratio = res$exp_ph / (res$unexp_ph + 0.0000001)
  #res$exp_P = res$exp_ph / (res$unexp_ph + res$exp_ph)
  #res$unexp_P = res$exp_ph / (res$unexp_ph + res$exp_ph)
  #res$est_FP = (res$unexp_P * res$n) + 0.00001
  #res$est_TP = (res$exp_P * res$n) + 0.00001
  #res$ratio_by_n = (res$exp_ph / res$unexp_ph)
  #exp_P_per_n = res$exp_P / res$n


  #print(res)
  graphics::par(mar = c(3,3,3,3), mfrow=c(1,1), xpd=F)
  graphics::layout(matrix(c(1,2), ncol = 1, nrow=2), heights = c(0.6,0.4))
  graphics::plot(as.numeric(res$threshold), res$ratio, type='l', xlab='weight threshold', ylab='pct in range / est noice',
                 #ylim=c(min(c(res$exp_ph,res$unexp_ph)),max(c(res$exp_ph,res$unexp_ph))),
                 #ylim  = c(min(res$est_TP), max(res$est_TP)),
                 log='y',
                 main=sprintf('Matches within hour range %s to %s', expected_hourdiff[1], expected_hourdiff[2]))
  #graphics::lines(as.numeric(res$threshold), res$unexp_ph, type='l', lty=2, xlab='weight threshold', ylab='pct in range / est noice')
  #maxv = max(res$est_TP)
  #axis(2, c(1 %o% 10^(1:ceiling(log10(maxv)))))  # draw y axis with required labels

  #graphics::par(new = T)
  #graphics::plot(as.numeric(res$threshold), res$n, pch=10, axes=F, xlab=NA, ylab=NA, cex=0.5, log='y')
  #graphics::axis(side = 4)
  #graphics::mtext(side = 4, line = 3, 'N in range')

  is_match = weight > min_weight
  #is_expected = hourdiff >= expected_hourdiff[1] & hourdiff <= expected_hourdiff[2]
  n = sum(is_match)
  exp_ph = sum(is_match * expected_coef) / exp_window
  unexp_ph = sum(is_match * expected_coef) / unexp_window
  exp_P = exp_ph / (unexp_ph + exp_ph)
  est_TP = exp_P*n
  exp_P_per_n = exp_P / n

  graphics::abline(v=min_weight, lty=2)
  print(res)
  print(est_TP / n)
  graphics::text(x=min_weight+(0.2*min_weight), y=exp(log(max(res$est_TP))*0.85), labels=sprintf('weight = %s\nN      = %s\nTP     = %s', round(min_weight,3), n, round(est_TP)), adj=0, font=3, cex=0.8, family='mono')
  graphics::legend("topright",
                   legend=c("Expected / hour", "Unexpected / hour"),
                   lty=c(1,2), col=c("black", "black"))


  graphics::hist(hourdiff[weight > min_weight],
                 right=F, main=sprintf('Matches over time with threshold %s', round(min_weight, 3)), breaks=hist_breaks, xlab='hour difference')
  invisible(res)
}

expected_filter <- function(total_hourdiff, expected_hourdiff, lambda=log(2)/24, do_plot=T) {
  t = seq(total_hourdiff[1], total_hourdiff[2], by=1)
  expected = t >= expected_hourdiff[1] & t < expected_hourdiff[2]

  exp_decay <- function(t, lambda, N=1) N*exp(1)^(-lambda*t)

  y = rep(0, length(t))
  y[expected] = exp_decay(1:sum(expected), lambda)
  #y = y/sum(y)
  if (do_plot) plot(t,y, type='l')
  data.frame(t=t, y=y)
}

function(){
  x=expected_filter(total_hourdiff=   c(-14*24, 14*24),
                  expected_hourdiff=c(     0,  7*24),
                  lambda=log(2)/24)

  expectation_validity(g, total_hourdiff=   c(-14*24, 14*24),
                                                  expected_hourdiff=c(     0,  7*24),
                                                  min_weight = 15)
}

