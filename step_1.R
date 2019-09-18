source('angle.R')
step_1 <- function(df, plot = T){
  if (plot) {
    plot.figure(df) 
  }
  extensions <- extension.lines(df, plot = F)
  edges <- edge.function(df)
  linesegments <- lapply(edges$v, function(lin){
    vec <- rbind(point = df[edges[lin,]$origin,1:2], vector = df[edges[lin,]$end,1:2]-df[edges[lin,]$origin,1:2])
    colnames(vec) <- c('x','y')
    vec
  })
  
  linesegments2 <- lapply(extensions, function(ext){
    vec <- rbind(point = ext[1,], vector = ext[2,]-ext[1,]) %>% as.data.frame()
    colnames(vec) <-  c('x','y')
    vec
    
    vec2 <- rbind(point = ext[1,], vector = ext[3,]-ext[1,]) %>% as.data.frame()
    colnames(vec2) <-  c('x','y')
    vec2
    list(vec,vec2)
  }) %>%   unlist(recursive = F)
  
  
  t <- lapply(linesegments2, function(lin){
    inter <- lapply(c(linesegments), function(x){
      if (any(!(round(lin[1,],5) == round(x[1,],5)))) {
        cross = lineline.intersect(lin,x)
        if (all(!is.na(cross))){
          length = sqrt(sum((lin[1,]-cross)^2))
        }else{
          NA
        }
      }else{
        NA
      }
    })
    min.length <- do.call(rbind, inter) %>% which.min() 
    cross <- min.length %>% linesegments[[.]] %>% lineline.intersect(lin,.) 
    if (plot) {
      lines(x=c(cross$x, lin[1,1]), y = c(cross$y, lin[1,2]))
    }
    edge2 = edges[(df[edges$origin,1:2]$x == linesegments[[min.length]][1,]$x &
                     df[edges$origin,1:2]$y == linesegments[[min.length]][1,]$y),]
    s <- rbind(point1 = linesegments[[min.length]][1,], point2 = lin[1,], crosspoint = cross
    ) %>% cbind(edge = edge2$v, length =sqrt(sum((linesegments[[min.length]][1,]-cross)^2))  ) %>%
      as.data.frame()
  })
  
  linesegments3 <- lapply(t, function(x){
    x[3,1:2]-x[2,1:2]
    rbind(x[2,1:2],x[3,1:2]-x[2,1:2])
  })
  t2 <- lapply(1:length(linesegments2), function(lin){
    inter <- lapply(1:length(linesegments3), function(x){
      if (any(!(round(linesegments2[[lin]][1,],5) == round(linesegments3[[x]][1,],5)))) {
        cross = lineline.intersect(linesegments2[[lin]],linesegments3[[x]])
        if (all(!is.na(cross))){
          l.cross <- sqrt(sum((cross-t[[lin]][2,1:2])^2)) 
          if (l.cross< sqrt(sum((t[[lin]][3,1:2]-t[[lin]][2,1:2])^2)) ) {
            t[[lin]] %>% rbind(point3= c(cross, edge=max(edges$v)+1, length = l.cross))
          }else{
            t[[lin]]
          }
        }else{
          t[[lin]]
        }
      }else{
        t[[lin]]
      }
    })
    inter <- do.call(rbind, inter) 
    wh <- !duplicated2(inter)
    wh[1:3] <- T
    inter[wh,]
  })
  return(list(t,t2))
}