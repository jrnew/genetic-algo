#' Wrapper function for reproduction stage.
#' 
#' Wrapper function for reproduction stage.
#' 
#' @param data Data frame
#' @param yvar Character; Name of column containing response variable
#' @param xvars Character vector; Default is all column names that are not yvar;
#' Name(s) of column(s) containing set of explanatory variables to select on.
#' @param model; Character; "lm" (default) or "glm"; Linear model or 
#' generalized linear model.
#' @param criterion; "AIC" (default) or "BIC"; AIC or BIC.
#' @param pop_size Integer; Default is 1000; Number of chromosomes per generation.
#' @param num_crossover_breaks Integer; Default is 1; Number of breaks in each 
#' parent chromosome when doing crossover.
#' @param mutation_rate Numeric, between 0 and 1; Default is 0.01.
#' @param elitism_rate Numeric, between 0 and 1; Default is 0.2; 
#' Proportion of chromosomes that are replaced at iteration.
#' @param num_max_iterations Non-negative integer; Default is 1000; 
#' Maximum number of iterations before algorithm is stopped.
#' @param seed Non-negative integer; Default is 123; Random seed 
#' for reproducibility.
reproduce <- function(
) {
  select()
  evaluate()
  fitness()
  recombine()
  mutate()
  # then replace some proportion of current generation with offspring chromosomes
}