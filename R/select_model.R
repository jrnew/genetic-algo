#' Carry out model selection with a genetic algorithm.
#' 
#' Main function for carrying out model selection with a genetic algorithm.
#' 
#' @param data Data frame
#' @param yvar Character; Name of column containing response variable
#' @param xvars Character vector; Default is all column names that are not yvar;
#' Name(s) of column(s) containing set of explanatory variables to select on.
#' @param model; Character; "lm" (default) or "glm"; Linear model or 
#' generalized linear model.
#' @param criterion; "AIC" (default) or "BIC"; Criterion to be minimized.
#' @param pop_size Integer; Default is 100; Number of chromosomes per generation.
#' @param method_select String; "rank" (linear rank selection) (default) or
#' "tournament"; Method to select chromosomes for inclusion in mating pool.
#' @param method_recombine String; "onepoint" (default), "twopoint", "uniform"; 
#' Type of crossover, at one point, at two points or uniformly (at all possible points).
#' @param prob_recombine Numeric, between 0 and 1; Default is 0.6; 
#' Probability of recombination.
#' @param prob_mutate Numeric, between 0 and 1; Default is 0.01; 
#' Probability of mutation.
#' @param num_max_iterations Non-negative integer; Default is 100; 
#' Maximum number of iterations before algorithm is stopped.
#' @param seed Non-negative integer; Default is 123; Random seed 
#' for reproducibility.
#' @param do_parallel Logical; Default is \code{FALSE}; Do in parallel?
select_model <- function(
  data,
  yvar,
  xvars = NULL,
  model = "lm",
  criterion = "AIC",
  pop_size = 100L,
  method_select = "rank",
  method_recombine = "onepoint",
  prob_recombine = 0.6,
  prob_mutate = 0.01,
  num_max_iterations = 100L,
  seed = 123,
  do_parallel = FALSE
) {
  stopifnot(is.data.frame(data))
  stopifnot(model %in% c("lm", "glm"))
  stopifnot(criterion %in% c("AIC", "BIC"))
  stopifnot(is.integer(pop_size))
  stopifnot(method_select %in% c("rank", "tournament"))
  stopifnot(method_recombine %in% c("onepoint", "twopoint", "uniform"))
  stopifnot(is.numeric(prob_mutate))
  stopifnot(prob_mutate >= 0 & prob_mutate <= 1)
  stopifnot(is.numeric(prob_recombine))
  stopifnot(prob_recombine >= 0 & prob_recombine <= 1)
  stopifnot(is.integer(num_max_iterations))
  stopifnot(num_max_iterations >= 10)
  stopifnot(is.logical(do_parallel))
  
  set.seed(seed)
  if (do_parallel)
    registerDoParallel(cores = detectCores())
  settings <- list(model = model,
                   criterion = criterion,
                   pop_size = pop_size,
                   method_select = method_select,
                   method_recombine = method_recombine,
                   prob_recombine = prob_recombine,
                   prob_mutate = prob_mutate,
                   num_max_iterations = num_max_iterations,
                   seed = seed)
  model_data <- process_data(data = data, yvar = yvar, xvars = xvars)
  cat(paste0("Initializing population...\n"))
  pop <- initialize(pop_size = pop_size, num_vars = model_data$num_vars)
  log <- list(models = matrix(NA, nrow = num_max_iterations, 
                              ncol = model_data$num_vars),
              evaluation_best = rep(NA, num_max_iterations),
              evaluation_mean = rep(NA, num_max_iterations))
  evaluation <- evaluate(pop = pop,
                         model_data = model_data,
                         model = settings$model,
                         criterion = settings$criterion,
                         do_parallel = do_parallel)
  ga <- list(settings = settings,
             model_data = model_data,
             pop = pop,
             evaluation = evaluation,
             log = log)
  class(ga) <- "ga"
  iteration <- 1L
  while (iteration <= num_max_iterations) {
    cat(paste0("Producing generation ", iteration, "...\n"))
    ga <- reproduce(ga = ga, iteration = iteration, do_parallel = do_parallel)
    iteration <- iteration + 1L
  }
  cat("Model selection complete!\n")
  return(ga)
}