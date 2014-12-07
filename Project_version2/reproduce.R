reproduce <- function(ga = ga, generation)
{ 
  selected_data <- selection(ga) # may want to add seed for the argument 
  crossover_data <- crossover(selected_data)
  mutation_data <- mutation(crossover_data)
  ga["pop"] <- mutation_data #update the model_data to the current population 
  return(ga)
}