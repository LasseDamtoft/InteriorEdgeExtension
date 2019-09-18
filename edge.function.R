source('angle.R')
edge.function = function(df){
  filters <- df$t %>% unique() %>% as.character()
  v <- lapply(filters, function(filter){
    new.df <- df[df$t == filter,]
    edge.df <- data.frame(v = rownames(new.df) %>% as.numeric())
    edge.df$origin = edge.df$v %>% as.numeric()
    edge.df$end <- edge.df$v %>% as.numeric() + 1  
    edge.df$end[which.max(edge.df$end)] = edge.df$origin[1]
    edge.df$t <- filter
    edge.df
  })
  if(length(v) > 1){
    dfe <- do.call("rbind", v)
  }else{
    dfe <- v %>% data.frame()
  }
  return(dfe)
}
