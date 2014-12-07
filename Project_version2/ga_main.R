ga_main <- function(data, yvar, xvars = NULL, model = "lm", criterion = "AIC",
                    pop_size = pop_size, num_crossover_breaks = 1, mutation_rate = 0.01,
                    elitism_rate = 0.2, num_max_iterations = 10, seed = 123)
{
  data <- read.csv("airquality.csv", header = TRUE)
  yvar <- "y"
  xvars <- names(data)[1:length(data)-1]
  model_data <- process_data(data = data, yvar = yvar)
  
  pop_size <- max(nrow(data)*2, 200)

  #initial list  
  ga_list <- ga (data, yvar, xvars = NULL, model = "lm", criterion = "AIC",
                 pop_size = pop_size, num_crossover_breaks = 1, mutation_rate = 0.01,
                 elitism_rate = 0.2, num_max_iterations = 10, seed = 123)
  
  #start produce till the num_max_iterations
  iteration = 1
  while(iteration <= num_max_iterations) 
    {
    ga_list <- reproduce(ga_list,model_data,iteration)
    iteration <- iteration + 1 
  }
}