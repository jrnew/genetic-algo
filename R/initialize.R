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
  pop <- matrix(sample(c(0, 1), size = pop_size*num_vars,
                       replace = TRUE),
                nrow = pop_size,
                ncol = num_vars)
  return(pop)
}

# Testing
# initialize(pop_size = 1000,
#            num_vars = 10)
