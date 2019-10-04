get_batch_i <- function(n, n.batches=NA, batchsize=NA, return_list=F, return_vector=F, for_meta=F){
  if (!is.na(batchsize)) n.batches = ceiling(n / batchsize)

  if (!n.batches == 1){
    splits = split((1:n), cut(seq_along(1:n), n.batches, labels = FALSE))
    splits = sapply(splits, FUN=function(x) cbind(min(x), max(x)))
    batch_i = data.frame(start=splits[1,], end=splits[2,])
  } else batch_i = data.frame(start=1, end=n)

  if (return_list | return_vector) batch_i = mapply(seq, from=batch_i$start, to=batch_i$end, SIMPLIFY=F)
  if (return_vector) batch_i = rep(1:length(batch_i), sapply(batch_i, length))
  batch_i
}
