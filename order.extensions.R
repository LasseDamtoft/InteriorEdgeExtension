order.extensions <- function(extended.lines){
  test <- extended.lines
  test4 <- do.call(rbind, test)
  test2 <- do.call(rbind, test)
  test3 <- test2[which(!(test2$edge %in% as.numeric(as.matrix(edges[edges$t == 'P',2:3])))),]
  num <- test3$edge %>% unique() %>% length
  ver <- rownames(df[df$t == 'P',][ df[df$t == 'P',]$x %in%test3$x &  df[df$t == 'P',]$y %in%test3$y,]) %>% as.numeric()
  ver <- ver- seq(1,0,length.out = ( num +2))[-c(1,(num+2))]
  
  for (i in 1:length(ver)) {
    test3[((i-1)*3+1):((i-1)*3+3),]$edge <- ver[i]
  }
  
  test2[which(!(test2$edge %in% as.numeric(as.matrix(edges[edges$t == 'P',2:3])))),] <- test3
  test4 <- test4[order(test2$length),]
  test2 <- test2[order(test2$length),]
  test4 <- test4[order(test2$edge),]
  test <- lapply(seq(3,36, by = 3), function(x){
    temp <- test4[(x-2):x,]
    rownames(temp) <- c('point1', 'point2', 'crosspoint')
    temp
  })
  return(test)
}