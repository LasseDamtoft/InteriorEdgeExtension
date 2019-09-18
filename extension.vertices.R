source('edge.function.R')

extension.vertices <- function(df){
  edge <- edge.function(df)
  vertices <- rownames(df) %>% as.numeric()
  v <- lapply(vertices, function(vertex){
    origin.df <- edge[edge$origin == vertex ,]
    end.df <- edge[edge$end == vertex ,]
    a <- as.numeric(df[origin.df[1,]$end,][1:2])-as.numeric(df[origin.df[1,]$origin,][1:2])
    b <- as.numeric(df[end.df[1,]$origin,][1:2])-as.numeric(df[end.df[1,]$end,][1:2])
    angle(b,a)>(pi +0.001)
  })
  if(length(v) > 1){
    dfe <- do.call("rbind", v)
  }else{
    dfe <- v %>% data.frame()
  }
  return(cbind(df,extend = dfe))
}

remove.straight.vertices <- function(df){
  edge <- edge.function(df)
  vertices <- rownames(df) %>% as.numeric()
  v <- lapply(vertices, function(vertex){
    origin.df <- edge[edge$origin == vertex ,]
    end.df <- edge[edge$end == vertex ,]
    a <- as.numeric(df[origin.df[1,]$end,][1:2])-as.numeric(df[origin.df[1,]$origin,][1:2])
    b <- as.numeric(df[end.df[1,]$origin,][1:2])-as.numeric(df[end.df[1,]$end,][1:2])
    angle(b,a)
  })
  if(length(v) > 1){
    dfe <- do.call("rbind", v)
  }else{
    dfe <- v %>% data.frame()
  }
  
  return(df[!(as.vector(round(dfe-pi,4) == 0)),])
}
