step_2 <- function(df, plot){
  test <- step_1(df, plot = F)
  test2 <- lapply(test[[2]], function(x){
    l <- list()
    x <- x[order(x$length),]
    x <- x[order(x$edge),]
    if ((dim(x)[1]-3)>0) {
      l[[1]] <- rbind(c(x[2,1:2],edge=0), c(x[4,1:2],edge=0))
      if ((dim(x)[1]-4)>0) {
        for (i in 2:(dim(x)[1]-2)) {
          l[[i]] <- rbind(c(x[2+i, 1:2],edge=0), c(x[3+i,1:2], edge=0))
          
        }
      }
      l[[dim(x)[1]-2]] <- rbind(c(x[dim(x)[1],1:2],edge=0), x[3,1:3])
    }else{
      l[[dim(x)[1]-2]] <- rbind(c(x[2,1:2],edge=0), x[3,1:3])
    }
    
    t <- do.call(rbind,l) %>% unlist %>% matrix(nrow = ((dim(x)[1]-2)*2)) %>% as.data.frame()
    colnames(t) <- c('x','y','edge')
    t
  }) %>% do.call(rbind, .) 
  innerlines<- lapply(1:(dim(test2)[1]/2), function(x){
    if (all(test2[(2*x-1),1:2] == test2[(2*x),1:2])) {
      NULL
    }else{
      s <- cbind(test2[(2*x-1):(2*x),], line = x)
      rownames(s) <- NULL
      s
    }
  })
  innerlines <- Filter(Negate(is.null), innerlines)
  
  innerline.new <- lapply(innerlines, function(lin){
    rbind(cbind(lin[1,c(1,2)],lin[2,c(1,2,4)]),cbind(lin[2,c(1,2)],lin[1,c(1,2,4)]))
  }) %>% do.call(rbind,.)
  innerline.new <- innerline.new[!(duplicated2(innerline.new[,1:4])),]
  
  test5 <- test[[1]] %>% do.call(rbind,.)
  test5 <- test5[order(test5$length),]
  test5 <- test5[order(test5$edge),]
  edges <- edge.function(df)
  per.lines <- lapply(edges$v, function(i){
    count <- 1
    ver.start <- df[edges[i,2],1:2]
    ver.end <- df[edges[i,3],1:2]
    ver.in <- matrix(0,nrow=sum(test5$edge == i)/3, ncol = 2) %>% as.data.frame()
    while (count <= sum(test5$edge == i)/3) {
      ver.in[count,1:2] <- test5[test5$edge == i,1:2][count*3,]
      count <- count+1
    }
    colnames(ver.in) <- c('x','y')
    vers <- rbind(ver.start, ver.in, ver.end) %>% unique()
    lapply(1:(dim(vers)[1]-1), function(point){
      vers[point:(point+1),]
    })
  }) %>% unlist(recursive = F)
  
  
  per.lines.new <- lapply(per.lines, function(lin){
    rbind(cbind(lin[1,],lin[2,]))
  }) %>% do.call(rbind,.)
  per.lines.new <- per.lines.new %>% cbind(0)
  
  all.1 <- per.lines.new[,1:4] %>% rbind(per.lines.new[,c(3:4,1:2)])
  all.1 <- rbind(all.1, innerline.new[,1:4])
  innerline.new <- innerline.new[!duplicated2(all.1)%>% tail(nrow(innerline.new[,1:4])),]
  innerline.new$line <- ceiling(1:dim(innerline.new)[1]/2)
  
  
  rownames(per.lines.new) <- NULL
  rownames(innerline.new) <- NULL
  colnames(per.lines.new) <- c('x','y','x1','y1', 'line')
  colnames(innerline.new) <- c('x','y','x1','y1', 'line')
  
  all.lines <- rbind(per.lines.new, innerline.new)
  all.lines <- round(all.lines,5)
  
  subs <-  adjacent <- list()
  i <- 0
  while (dim(all.lines)[1]>0) {
    i <- i+1
    current.sub <- currentline <- new.line <- all.lines[1,]
    while(!( isTRUE(all.equal(as.numeric(current.sub[1,1:2]), 
                              as.numeric(currentline[,3:4]), tolerance=0.00000001)))){
      
      currentline <- new.line
      currentline <- round(currentline,5)
      
      
      all.lines <- all.lines[!(rbind(currentline, all.lines) %>% duplicated2() %>% tail(nrow(all.lines))),]
      
      
      rownames(all.lines) <- NULL
      current.sub <- rbind(current.sub,currentline)
      if (dim(all.lines)[1] < 1) {
        break
      }
      inners <- all.lines[,1] %in% currentline[,3] & all.lines[,2] %in% currentline[,4]
      
      if (sum(inners)>1) {
        angles <- lapply(which(inners), function(y){
          yy <- all.lines[y,3:4]-all.lines[y,1:2]
          angle =angle(as.numeric(currentline[1:2]-currentline[3:4]),as.numeric(yy))
          angle = ifelse(angle < 0.0001, yes = NA, no = angle)
          cbind(which =y, angle)
        }) %>% do.call(rbind,.)
        new.line <- all.lines[which(inners)[which.min(angles[,2])],]
      }else{
        new.line <- all.lines[inners,]
      }
    }
    rownames(current.sub) <- NULL
    subs[[i]] <- cbind(current.sub[!(current.sub[,1:2] %>% duplicated2()),1:2], t = 'I')
    rownames(subs[[i]]) <- NULL
    adjacent[[i]] <- data.frame(Polygon = i, Line = current.sub[-1,5])
    rownames(adjacent[[i]]) <- NULL
  }
  if (plot) {
    
    plot.figure(df)
    
    for (j in 1:length(subs)) {
      polygon(x=subs[[j]][,1], y= subs[[j]][,2])
      
    }
  }
  return(list(subs, adjacent))
}
