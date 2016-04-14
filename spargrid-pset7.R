# Problem Set 7 Joseph Ludmir
#Initial SparseGrid function
require("SparseGrid")
# Lower is lower bound of integration, floor is the minimum of it, upper is the 
# higher bound with the ceiling being the maximum.
spargrid_int <- function(g,..., lower, upper, parallelCores = TRUE){
  lower <- floor(lower)
  upper <- ceiling(upper)
# error is lower is greater than upper value
if(any(lower >= upper)){
  stop("The lower values must be smaller than the upper values.")
}
# Creates a grid across dimensions to add each unit of the integration,
grids <- as.matrix(expand.grid(seq(lower[1],upper[1]-1,by=1), 
  seq(lower[2],upper[2]-1,by=1)))
sp.grid <- createIntegrationGrid("KPU", dimension = 2, k = 5)
nodes <- grids[1,] + sp.grid$nodes
weights <- sp.grid$weights
for(i in 2:nrow(grids)){
  nodes <- rbind(nodes, grids[i,] + sp.grid$nodes)
  weights <- c(weights, sp.grid$weights)
}
# Calculates the total integral of function.
gx.sp <- aaply(nodes, 2, f, ..., .parallel=parallel)

val.sp <- (gx.sp %*% weights)
return(val.sp)
}
# For when dimension and parallel are different:
# dim = dimensions, parallel running is now an option
spargrid_int_dim_par <- function(g,..., lower, upper, dim, parallelCores = FALSE){
# if parallel is true, run 6 cores on the computer
if(parallel = TRUE){
    require("doParallel")
    registerDoParallel(cores = 6)
  }
  lower <- floor(lower)
  upper <- ceiling(upper)
# still throw an error if lower values are greater than upper ones
if(any(lower >= upper)){
    stop("The lower values must be smaller than the upper values.")
}
# Same formula as the function above, except now you are applying the integration
# to each dimension at a time through a sequence
  grid_eachdim <- function(i){
    seq(lower[i], upper[i]-1, by=1)
  }
  grids <- as.matrix(expand.grid(llply(1:dim, gridFun, .parallel=parallel)))
  
  sp.grid <- createIntegrationGrid( 'KPU', dimension=dim, k=5 )
  nodes<-grids[1,]+sp.grid$nodes
  weights<-sp.grid$weights
  for (i in 2:nrow(grids)){
    nodes<-rbind(nodes,grids[i,]+sp.grid$nodes)  
    weights<-c(weights,sp.grid$weights)
  }
  
  gx.sp <- aaply(nodes, 2, f, ..., .parallel=parallel)
  
  val.sp <- (gx.sp%*%weights)
  return(val.sp)
}

# Measure Gains In Speed
require("microbenchmark")
ex_1 <- function(x){
  return(2x)
}
# Essentially here we are comparing the speeds of the two integration techniques with
# different dimensions and running parallel or no parallel.
microbenchmark("no parallel"=spargrid_int_dim_par(ex_1,lower=lower,upper=upper,dim=1), 
               "parallel"=spargrid_int_dim_par(ex_1,lower=lower,upper=upper, dim=1, parallel=TRUE),
               times=10)
microbenchmark(spargrid_int_dim_par(ex_1,lower=lower,upper=upper, dim=2), 
               spargrid_int_dim_par(f,lower=lower,upper=upper, dim=2, parallel=TRUE),
               times=10)
# Now using cubature and mvtnorm
require("cubature")
require("mvtnorm")

# Create function with 3 dimensions to test
ex_1 <- function(x){
  dmvnorm(x, mean=rep(0, 3), sig=diag(rep(1, 3)))
}
# True integration value is below
true_ans <- as.numeric(pmvnorm(upper=rep(1, 3), mean=rep(0, 3), sig=diag(rep(1, 3))))
# Checking the answer using adaptIntegrate
adaptIntegrate(ex_1, lowerLimit=rep(-1000, 3), upperLimit=rep(1, 3))$integral - true_ans
# Checking the error
spargrid_int_dim_par(ex_1, lower=rep(-1, 3), upper=rep(1, 3), dim=3) - true_ans
# Checking the speeds
microbenchmark("adapt"=adaptIntegrate(ex_1, lowerLimit=rep(-1000, 3), upperLimit=rep(1, 3))$integral,
               "sparse"=spargrid_int_dim_par(ex_1, lower=rep(-1, 3), upper=rep(1, 3), dim=3),
               times=10)
microbenchmark("adapt"=adaptIntegrate(ex_1, lowerLimit=rep(-1000, 3), upperLimit=rep(1, 3))$integral,
               "sparse"=spargrid_int_dim_par(ex_1, lower=rep(-1, 3), upper=rep(1, 3)),
               times=10)
