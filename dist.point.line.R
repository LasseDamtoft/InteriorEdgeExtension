dist.point.line <- function(point,line.end1,line.end2) {
  dd <- lapply(1:dim(point)[1], function(x){
    v1 <- line.end1 - line.end2
    v2 <- as.numeric(point[x,]) - line.end1
    m <- cbind(v1,v2)
    d <- abs(det(m))/sqrt(sum(v1*v1))
  })
  dd <- do.call(rbind, dd)
  return(dd)
} 