inter.point <- function(initial.point, vector, vertex.start, vertex.end){
  vector2 <- vertex.start %>% as.numeric()-vertex.end%>% as.numeric()
  mat <- cbind(vector, vector2=-vector2)
  constant <- vertex.start-initial.point
  return(solve(mat, constant)[1]*vector+initial.point)
}


lineline.intersect <- function(line, perimiter){
  mat <- cbind(as.numeric(line[2,]), -as.numeric(perimiter[2,]))
  constant <- as.numeric(perimiter[1,])-as.numeric(line[1,])
  if ((angle(mat[,1], mat[,2]) < 0.01 & angle(mat[,1], mat[,2]) > -0.01) |
      (angle(mat[,1], mat[,2]) < (pi+0.01) & angle(mat[,1], mat[,2]) > (pi-0.01))|
      (angle(mat[,1], mat[,2]) < (2*pi+0.01) & angle(mat[,1], mat[,2]) > (2*pi-0.01)))
  {
    return(NA)
  }
  cross <- solve(mat, constant)
  if (cross[2]>-0.00001 & cross[2]<1.00001 & cross[1]>0) {
    return(cross[1]*line[2,]+line[1,])
  }else{
    return(NA)
  }
}
