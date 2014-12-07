#' Carry out reproduction and updating the ga list.
#' 
#' @param ga_list; List;
#' @param model_data; Object of class \code{model_data}.
#' @param iteration; Numeric; Number of iteration the user would like to examine. 
#' @return List; Updated ga list. 
reproduce <- function(ga_list,model_data,iteration)
{
  #selection
  copies <- copy(ga_list$Fitness, seed = 123)
  parents <- selection(ga_list,copies)
  
  #crossover
  children <- crossover(ga_list, parents)
  
  #mutation
  mutated_child <- mutate(children, ga_list$settings$mutation_rate)
  
  #Elitism
  elitism_num <- ceiling(nrow(parents)*ga_list$settings$elitism_rate)
  sample_parents <- parents[1:elitism_num,]
  index <- sample(1:nrow(children), size = (pop_size-elitism_num), replace = TRUE)
  sample_children <- children[index,]  
  
  #updata current list
  #pop
  ga_list$pop <- rbind(sample_parents,sample_children)
  #Fitness
  ga_list$Fitness <- fitness(ga_list$pop, model_data, criterion="AIC")
  #log$models
  score <- rep(0,nrow(ga_list$pop))
  for (i in 1:nrow(ga_list$pop))
  { 
   score[i] <- evaluate(model_data, xvars_select=as.logical(ga_list$pop[i,]), model = "lm", criterion = criterion)
  }
  ga_list$log$models[iteration,] <- ga_list$pop[which(score == max(score))[1],] #just take the first one since there might be multiple "best ones"
  ga_list$log$criterion_best[iteration] <- max(score)
  ga_list$log$criterion_mean[iteration] <- mean(score)
  
  return(ga_list)
}

#Test
new_list <- reproduce(ga_list,model_data,iteration)
