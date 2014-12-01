#' Mutate genes in the population.
#' 
#' Mutate each gene in the population at a pre-defined rate.
#' 
#' @param pop Matrix of population of chromosomes
#' @param mutation_rate Numeric, between 0 and 1; Default is 0.01.
#' @return A matrix of population of chromosomes that have undergone mutation.
mutate <- function(
  pop,
  mutation_rate = 0.01
) {
  num_genes_total <- prod(dim(pop))
  # Probabilty of getting a 1 is mutation_rate if gene is currently 0 and vice versa
  pop_mutate <- matrix(rbinom(num_genes_total, rep(1, num_genes_total), 
                              ifelse(c(pop) == 0, mutation_rate, 1-mutation_rate)))
  return(pop_mutate)
}



