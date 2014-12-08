#' Wrapper function for reproduction stage.
#' 
#' Wrapper function for reproduction stage.
#' 
#' @param ga Object of class \code{ga}.
#' @param iteration Iteration number.
#' @return Updated \code{ga} list object.
reproduce <- function(
  ga,
  iteration,
  do_parallel = FALSE
) {
  # Create mating pool
  pop_mating <- select(pop = ga$pop, 
                       evaluation = ga$evaluation,
                       method = ga$settings$method_select,
                       do_parallel = do_parallel)
  # Do recombination/crossover
  pop_recombined <- recombine(pop_mating = pop_mating,
                              pop_size = ga$settings$pop_size,
                              method = ga$settings$method_recombine,
                              prob_recombine = ga$settings$prob_recombine,
                              do_parallel = do_parallel)
  # Carry out mutation
  pop_mutated <- mutate(pop = pop_recombined, 
                        prob_mutate = ga$settings$prob_mutate)
  #----------------------------------------------------------------------
#   # Replace parent chromosomes with child chromosomes
#   pop_child <- pop_mutated ############## combine with parents or replace?
#   #Elitism
#   elitism_num <- ceiling(nrow(parents)*ga_list$settings$elitism_rate)
#   sample_parents <- parents[1:elitism_num,]
#   index <- sample(1:nrow(children), size = (pop_size-elitism_num), replace = TRUE)
#   sample_children <- children[index,]  
#   
#   #update current list
#   #pop
#   ga_list$pop <- rbind(sample_parents,sample_children)
#   ga$pop <- pop_child
  #----------------------------------------------------------------------
  # Replace entire generation with child chromosomes
  ga$pop <- pop_mutated
  # Save results of current iteration
  evaluation_child <- evaluate(pop = ga$pop,
                               model_data = ga$model_data,
                               model = ga$settings$model,
                               criterion = ga$settings$criterion,
                               do_parallel = do_parallel)
  ga$evaluation <- evaluation_child
  # Save only the first "best" model as there may be multiple "best" models
  ga$log$models[iteration, ] <- ga$pop[which(ga$evaluation == min(ga$evaluation))[1], ] 
  ga$log$evaluation_best[iteration] <- min(ga$evaluation)[1]
  ga$log$evaluation_mean[iteration] <- mean(ga$evaluation)
  return(ga)
}