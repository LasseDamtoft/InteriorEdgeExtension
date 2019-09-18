angle <- function(from,to){
  s <- atan2(from[2],from[1]) - atan2(to[2],to[1]) 
  if (s<0.0001) {
    s <- s+2*pi
  }
  return(s)
}
angle2 <- function(from,to){
  atan2(from[2],from[1]) - atan2(to[2],to[1]) 
}
`%~%` <- function(x,y) sapply(x, function(.x) {
  any(sapply(y, function(.y) isTRUE(all.equal(.x, .y, tolerance=0.00001))))
})

duplicated2 <- function(x) x %>% round(digits = 3) %>% duplicated()

angle.points <- function(x,y){
  
  if (all(y$y %~% x$y & y$x %~% x$x)) {
    return(NA)
  }
  start <- y[y$y %~% x$y & y$x %~% x$x,]
  a <- (y[!(y$y %~% x$y & y$x %~% x$x),]-start) %>% as.numeric()
  b <- (x[!(x$y %~% y$y & x$x %~% y$x),]-start) %>% as.numeric()
  return(angle(a,b))
}