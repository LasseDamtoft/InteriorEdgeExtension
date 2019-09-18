width.function <- function(df, plot){
  e.s <- edge.function(df)
  edge.points <- lapply(as.numeric(rownames(e.s)), function(edge.num){
    e <- e.s[edge.num,2:3] %>% as.numeric()
    ee <- df[e,1:2]
    rownames(df) <- NULL
    altitude <- max(df[-e,1:2] %>% dist.point.line(as.numeric(ee[1,]), as.numeric(ee[2,])))
  })
  altitudes <- do.call(rbind, edge.points)
  edge.min <- which.min(altitudes)
  # edge.min2 <- which(altitudes-altitudes[edge.min]<0.00001)
  # e.min <- e.s[edge.min2,2:3] %>% as.matrix() %>% as.numeric()
  # e2 <- e.min[e.min %>% duplicated()]
  # if (length(e2)>0) {
  #   e.min <- e.s[edge.min2,2:3][e.s[edge.min2,]$origin == e2 | e.s[edge.min2,]$end == e2,]
  #   e.min <- e.min[e.min != e2]
  # }else{
  #   e.min <- e.s[edge.min,2:3] %>% as.matrix() %>% as.numeric()
  # }
  e.min <- e.s[edge.min,2:3] %>% as.matrix() %>% as.numeric()
  ver.min <- df[e.min,1:2]
  if (plot) {
    lines(ver.min$x, ver.min$y, lwd = 7, col = 'gold', type = 'l')
    vec <- ver.min[2,]-ver.min[1,]
    vec2 <- data.frame(x= (-1) *vec$y, y =vec$x)
    vec2 <- vec2/(sqrt(sum(vec2^2)))
    arrows(mean(ver.min$x), mean(ver.min$y),mean(ver.min$x)+ vec2$x, mean(ver.min$y)+vec2$y)
  }
  return(list(edge.min, altitudes[edge.min], ver.min[1,],ver.min[2,]))
}

general.width.function <- function(subs, plot){
  if (plot) {
    lapply(subs, lines.figure)
  }
  widths <- lapply(subs, function(sub){
    width.function(sub, plot)[[2]]
  })
  widths <- do.call(rbind, widths)
  return(sum(widths))
}