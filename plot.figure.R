plot.figure <- function(df){
  filters <- df$t %>% unique() %>% as.character()
  v <- lapply(filters, function(filter){
    # new_df <- df %>% filter(t == m.filter)
    new.df <- df[df$t == filter,]%>% rbind(df[df$t== filter,][1,])
    if (filter == filters[1]) {
      plot(new.df[,1:2], type = 'l', lwd = 2, col = 'blue')
    }else{
      lines(new.df[,1:2], type = 'l', lwd = 2, col = 'red')
      obs.x <- new.df[,1] %>% as.matrix() %>% as.numeric()
      obs.y <- new.df[,2] %>% as.matrix() %>% as.numeric()
      polygon(obs.x, obs.y, col = "grey70", border = NA)
    }
  })
}

lines.figure <- function(df){
  filters <- df$t %>% unique() %>% as.character()
  v <- lapply(filters, function(filter){
    # new_df <- df %>% filter(t == m.filter)
    new.df <- df[df$t == filter,]%>% rbind(df[df$t== filter,][1,])
    obs.x <- new.df[,1] %>% as.matrix() %>% as.numeric()
    obs.y <- new.df[,2] %>% as.matrix() %>% as.numeric()
    polygon(obs.x, obs.y, col = "chocolate", border = NA)
    lines(new.df[,1:2], type = 'l', lwd = 2, col = 'black')
  })
}
