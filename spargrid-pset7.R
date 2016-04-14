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

