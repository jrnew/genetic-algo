#' Recombine.
#' 
#' Carry out crossover of parent chromosomes.
#' 
#' @param pop Matrix of population of chromosomes.
#' @param method String; One of "single", "double", "all"
recombine <- function(
  pop,
  method = "single"
) {
# pick a pair of strings, with prob pc recombine 
# crossover at single random point
# crossover at all genes
  if ("single") {
    breakpoint <- ceiling(runif(1, 0, 10))
    child <- c(parent1[1:breakpoint], parent2[(breakpoint + 1):num_genes])
  } else if ("all") {
    child <- ifelse(rbinom(num_genes, 1, 0.5) == 1, parent1, parent2)
  }
}