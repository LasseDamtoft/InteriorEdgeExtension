source('source.R')

# paper example
x <- c(0,0.47,1.56,2.61,2.92,2.49,1.09,0.35,0.62,1.18,1.69,2.33,2.02,1.42,0.84)
y <- c(0.83,0.17,0,0.27,0.95,1.71,1.81,1.47,1.11,0.94,1.26,1.14,0.67,0.71,0.59)
t <- c("P","P","P","P","P","P","P","P","O","O","O","O","O","O","O")

## huang
# x <- c(0,1,1,1.5,4,4,1.5,0,1,2,3,2)
# y <- c(0,0,1.5,2,2,4,4,2.5,2.5,3.5,3.5,2.5 )
# t <- c("P","P","P","P","P","P","P","P","O","O","O","O")


df <- data.frame(x,y,t, stringsAsFactors = F)
plot.figure(df)


step_1_res <- step_1(df)

step_2_res <- step_2(df, plot = T)
step_3_res <- step_3(df, step_2_res = step_2_res)

weight <- (step_3_res[[3]] %>% unlist(recursive = F) %>% do.call(rbind,.))
sub.list <- step_3_res[[2]] %>% unlist(recursive = F)
a <- lapply(sub.list, function(x){
  a <- rep(0,length(step_3_res[[1]][[1]]))
  a[x] <- 1
  a
}) %>% do.call(rbind,.)

## Step 4
library(gurobi)

model <- list()

model$A          <- a %>% as.matrix() %>% t()
model$obj        <- weight %>% as.vector()
model$modelsense <- 'min'
model$rhs        <- rep(1,ncol(a))
model$sense      <- rep('=',ncol(a))
model$vtype      <- 'B'

params <- list(OutputFlag=0, PoolSearchMode=2)

result <- gurobi(model, params)

print('Solution:')
print(result$objval)
print(result$x)

