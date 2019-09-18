source('inter.point.R')
source('edge.function.R')
source('extension.vertices.R')
extension.lines <- function(df, plot = T){
  df.e <- extension.vertices(df)
  df.e <- df.e[df.e$extend== T,]
  
  vertices <- rownames(df.e) %>% as.numeric()
  dfe <- edge.function(df)
  extensions <- lapply(vertices, function(vertex){
    origin.df <- dfe[dfe$origin == vertex ,]
    end.df <- dfe[dfe$end == vertex ,]
    a <- as.numeric(df[origin.df[1,]$end,][1:2])-as.numeric(df[origin.df[1,]$origin,][1:2])
    b <- as.numeric(df[end.df[1,]$origin,][1:2])-as.numeric(df[end.df[1,]$end,][1:2])
    ext.1 <- a * (-1)
    ext.2 <- b * (-1)
    ext.1 <- ext.1/sqrt(sum(ext.1^2))
    ext.2 <- ext.2/sqrt(sum(ext.2^2))
    vertex2 = df.e[rownames(df.e) == as.character(vertex),][1:2] %>% as.numeric()
    edge.point1 <- vertex2 + ext.1
    edge.point2 <- vertex2 + ext.2
    if (plot) {
      lin1 <- rbind(edge.point1, vertex2)
      lines(lin1[,1],lin1[,2])
      lin2 <- rbind(edge.point2, vertex2)
      lines(lin2[,1],lin2[,2])
    }
    
    rbind(vertex = vertex2,edge.point1, edge.point2,c(vertex,vertex))
  })
  return(extensions)
}
