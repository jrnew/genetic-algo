#' Initialize a list that contains information about the observed data, current 
#' generation, best fit for pervious generations and settings
#' 
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
#' @return A list that contains 
ga <- function(data, yvar, xvars = NULL, model = "lm", criterion = "AIC",
               pop_size = 1000, num_crossover_breaks = 1, mutation_rate = 0.01,
               elitism_rate = 0.2, num_max_iterations = 1000, seed = 123) 
{
  # Pending:
  # Checks for validity of all function arguments.
  set.seed(seed)
  settings <- list(model = model,
                   criterion = criterion,
                   num_crossover_breaks = num_crossover_breaks,
                   mutation_rate = mutation_rate,
                   elitism_rate = elitism_rate,
                   num_max_iterations = num_max_iterations,
                   seed = seed)
  model_data <- process_data(data = data, yvar = yvar, xvars = xvars)
  pop <- initialize(pop_size = pop_size, num_vars = model_data$num_vars)
  log <- list(models = matrix(NA, nrow = num_max_iterations, 
                              ncol = model_data$num_vars),
              criterion_best = numeric(num_max_iterations),
              criterion_mean = numeric(num_max_iterations))
  Fitness <- fitness(pop, model_data, criterion)
  #This ga contains everything needed, including fitness score for the first 
  #generation
  ga <- list(settings = settings,
             model_data = model_data,
             pop = pop, # the current population
             Fitness = Fitness,
             #copies = copies, #dont need it
             #record the fittest sample in each generation matrix
             log = log)
 

  class(ga) <- "ga"
  
  #while(i <= num_max_iterations) {
   # ga <- reproduce(ga = ga, i = i)
    #i <- i + 1 
  #}
  return(ga)
  }

