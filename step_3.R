source('merge.options.R')
step_3 <- function(df, step_2_res){
  subs <- step_2_res[[1]]
  adjacent <- step_2_res[[2]] %>% do.call(rbind,.)
  
  adjacent <- adjacent[adjacent$Line != 0, ]
  adjacent <- adjacent[order(adjacent$Line),2:1]
  
  adjacent.list <- lapply(1:length(subs), function(x){
    lins <- adjacent[adjacent$Polygon== x,1]
    adjacent[adjacent$Line %in% lins & adjacent$Polygon != x,2]
  })
  subs.numbers <- subs.vertices <- subs.widths <- list()
  for (n in 1:length(subs)) {
    if (n == 1) {
      subs.vertices[[1]] <- subs
      subs.numbers[[1]] <- 1:length(subs) %>% as.list()
      subs.widths[[1]] <- lapply(subs, function(x){width.function(x,F)[[2]]})
    }else{
      result.vertices <- list()
      result.numbers <- list()
      
      for (m in floor(n/2)) {
        # print(c(round(n/length(subs),4), m))
        if (m > length(subs.numbers) | (n-m)> length(subs.numbers)) {
          next
        }
        if (is.null(subs.numbers[[n-m]])) {
          next
        }
        if (is.null(subs.numbers[[m]])) {
          next
        }
        temp <- merge.options(m,n-m,subs.numbers, subs.vertices, adjacent.list)
        result.vertices <- list(result.vertices, temp[[1]]) %>% unlist(recursive = F)
        result.numbers[[m]] <- temp[[2]]
      }
      if (is.null(result.numbers)| length(result.numbers) == 0) {
        subs.vertices[[n]] <- NULL
        subs.numbers[[n]] <- NULL
        subs.widths[[n]] <- NULL
      }else{
        result.numbers <- result.numbers %>% do.call(rbind,.)
        result.numbers <- result.numbers %>% apply(1, sort) %>% t()
        result.vertices <- result.vertices[!(result.numbers %>% duplicated())]
        result.numbers <- result.numbers[!(result.numbers %>% duplicated()),]
        if (is.null(nrow(result.numbers))) {
          subs.vertices[[n]] <- result.vertices
          subs.numbers[[n]] <- list(result.numbers)
          subs.widths[[n]] <- lapply(result.vertices, function(x){width.function(x,F)[[2]]})
        }else{
          result.numbers <- result.numbers %>% t() %>% as.data.frame(optional = T) %>% as.list.data.frame()
          subs.vertices[[n]] <- result.vertices
          subs.numbers[[n]] <- result.numbers
          subs.widths[[n]] <- lapply(result.vertices, function(x){width.function(x,F)[[2]]})
        }
      }
    }
  }
  return(list(subs.vertices, subs.numbers, subs.widths))
}





