recombine_once <-
function(
  parent1,
  parent2,
  method = "onepoint"
) {
  if (all(parent1 == parent2)) {
    # Crossover not necessary (has no effect) if both parents are identical
    child <- parent1
  } else {
    num_genes <- length(parent1)
    if (method == "onepoint") {
      breakpoint <- sample(1:num_genes, size = 1)
      if (breakpoint == num_genes) {
        child <- parent1
      } else {
        child <- c(parent1[1:breakpoint], parent2[(breakpoint + 1):num_genes])
      }
    } else if (method == "twopoint") {
      breakpoints <- sort(sample(1:num_genes, size = 2))
      if (breakpoints[2] == num_genes) {
        child <- c(parent1[1:breakpoints[1]], parent2[(breakpoints[1] + 1):breakpoints[2]])
      } else {
        child <- c(parent1[1:breakpoints[1]], parent2[(breakpoints[1] + 1):breakpoints[2]], 
                   parent1[(breakpoints[2] + 1):num_genes])
      }
    } else if (method == "uniform") {
      child <- ifelse(rbinom(num_genes, 1, 0.5) == 1, parent1, parent2)
    }
  }
  return(child)
}
