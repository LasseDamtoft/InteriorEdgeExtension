merger <- function(sub1,sub2){
  rownames(sub1) <- NULL
  rownames(sub2) <- NULL
  if (length(which(rbind(sub1[,1:2],sub2[,1:2]) %>% duplicated2()))!=2) {
    # warning('sub polygons does not share one and only one edge')
    return(rbind(NA,NA))
  }
  if (length(which(rbind(sub1[,1:2],sub2[,1:2]) %>% duplicated2())) == dim(sub1)[1] |
      length(which(rbind(sub1[,1:2],sub2[,1:2]) %>% duplicated2())) == dim(sub2)[1]) {
    if (dim(sub2)[1] > dim(sub1)[1]) {
      return(sub2)
    }else{
      return(sub1)
    }
  }
  if (all(which((rbind(sub2[,1:2],sub1[,1:2]) %>% duplicated2() %>% tail(nrow(sub1)))) ==c(1,dim(sub1)[1]))) {
    temp1 <- sub1[(rbind(sub2[,1:2],sub1[,1:2]) %>% duplicated2() %>% tail(nrow(sub1))) ,][2,]
  }else{
    temp1 <- sub1[(rbind(sub2[,1:2],sub1[,1:2]) %>% duplicated2() %>% tail(nrow(sub1))) ,][1,]
  }
  ss <- which((rbind(temp1[,1:2],sub2[,1:2]) %>% duplicated2() %>% tail(nrow(sub2))))
  if (length(ss)==0) {
    return(rbind(NA,NA))
  }
  
  if (ss== 1) {
    sss <- sub2[(ss):(dim(sub2)[1]),]
  }else{
    sss <- sub2[c((ss):dim(sub2)[1], 1:(ss-1)),]
    
  }
  sss <- sss[2:(dim(sss)[1]-1),]
  if (as.numeric(rownames(temp1)) == dim(sub1)[1]) {
    res <- rbind(sub1[1:as.numeric(rownames(temp1)),],sss) %>% 
      unique()
  }else{
    res <- rbind(sub1[1:(as.numeric(rownames(temp1))),],sss,
                 sub1[(as.numeric(rownames(temp1))):dim(sub1)[1],]) %>% 
      unique()
  }
  
  rownames(res) <- NULL
  res <- remove.straight.vertices(res)
  rownames(res) <- NULL
  return(res)
}