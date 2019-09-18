general.merger <- function(merge.edges, subs, plot){
  zeros <- rep(F,length(merge.edges))
  for (merge.nr in 1:length(merge.edges)) {
    if (merge.edges[merge.nr] ==1) {
      merge.nr2 <- if_else(merge.nr == length(merge.edges), true = 1, false = merge.nr+1)
      merge.nr3 <- if_else(merge.nr2 == length(merge.edges), true = 1, false = merge.nr2+1)
      temp <- merger(subs[[merge.nr]],subs[[merge.nr2]])
      if (any(extension.vertices(temp)$extend)) {
        next
      }
      subs[[merge.nr]] <- temp
      rownames(subs[[merge.nr]]) <- NULL
      subs[[merge.nr2]] <- subs[[merge.nr]]
      t <- merger(subs[[merge.nr]],subs[[merge.nr3]])
      rownames(t) = NULL
      t <- t %>% extension.vertices()
      
      zeros[merge.nr] <- T
    }
  }
  for (i in rev(which(zeros))) {
    subs[[i]] <- NULL
  }
  if(plot){
    lapply(subs, lines.figure)
  }
  return(subs)
}