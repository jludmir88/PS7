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

