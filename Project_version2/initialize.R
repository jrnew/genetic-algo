#' Initialize first generation of chromosomes.
#' 
#' Initialize first generation of chromosomes completely randomly.
#' 
#' @param pop_size Non-negative integer; Number of chromosomes in population.
#' @param num_vars Non-negative integer; Number of variables in model under consideration/
#' number of genes in each chromosome. 
#' @return A matrix of size \code{pop_size} x \code{num_vars} with 1's and 0's.
initialize <- function(
  pop_size,
  num_vars
) {
  pop <- matrix(rep(0,pop_size*num_vars),nrow=pop_size, ncol=num_vars)
  check <- length(which(rowSums(pop)==0))
  
  #To aviod rows with all zeros
  while( check != 0)
  {
  pop <- matrix(sample(c(0, 1), size = pop_size*num_vars,
                       replace = TRUE),
                nrow = pop_size,
                ncol = num_vars)
  check <- length(which(rowSums(pop)==0))
  }
  
  return(pop)
}

# Testing
k <- initialize(pop_size = 100,
           num_vars = 10) #population size has to be preditermined 
