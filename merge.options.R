merge.options <- function(first.sub.amount,second.sub.amount, subs.numbers, subs.vertices, adjacent.list){
  temp <- lapply(1:length(subs.numbers[[first.sub.amount]]), function(merge.number){
    currenct.sub <- subs.numbers[[first.sub.amount]][[merge.number]]
    adjacancies <- adjacent.list[currenct.sub] %>% unlist()
    adjacancies <- adjacancies[!(round(adjacancies,2) %in% round(currenct.sub,2))]
    lapply(which(apply(matrix((subs.numbers[[second.sub.amount]] %>% do.call(rbind,.)) %in% 
                                adjacancies, ncol = second.sub.amount),1,any) & 
                   !apply( matrix((subs.numbers[[second.sub.amount]] %>% do.call(rbind,.)) %in%
                                    currenct.sub, ncol = second.sub.amount),1,any)), function(y){
                                      merg3 <- merger(subs.vertices[[first.sub.amount]][[merge.number]], 
                                                      subs.vertices[[second.sub.amount]][[y]])
                                      if(is.na(merg3[1,1])){
                                        NULL
                                      }else{
                                        if (any(extension.vertices(merg3)$extend)) {
                                          NULL
                                        }else{
                                          list(c(currenct.sub,subs.numbers[[second.sub.amount]][[y]]), merg3)
                                        }
                                      }
                                    }) 
  })  %>% unlist(recursive = F)
  temp <- Filter(Negate(is.null), temp)
  temp <- temp %>% unlist(recursive = F)
  if (is.null(temp)) {
    return(list(NULL, NULL))
  }
  result.numbers <- temp[seq(1,length(temp), by = 2)] %>% do.call(rbind,.)
  result.vertices <- temp[seq(2,length(temp), by = 2)]
  
  return(list(result.vertices, result.numbers))
}