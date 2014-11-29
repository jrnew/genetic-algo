#' Calculate fitness of chromosomes.
#' 
#' Calculate fitness of chromosomes.
#' 
#' @param chromosomes Matrix; Chromosomes
#' @param method String; "default", "rank", "sampling"
fitness <- function(
  criterion,
  method = ""
) {
  criterion/mean(criterion)
}

# Ways to calculate fitness:
# - criterion/mean(criterion)
# - assigned based on rank of chromosome in population
# - sampling methods, e.g. tournament selection