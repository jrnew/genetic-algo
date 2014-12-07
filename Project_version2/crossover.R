#' Function for processing chromosome crossover
#' 
#' @param ga_list; List.
#' @param parents; Matrix; Chromosomes to produce the next generation.
#' @return Matrix; New chromosomes. 


crossover <- function(ga_list, parents)
{
  pop_size <- nrow(ga_list$pop)
  num_vars <- ga_list$model_data$num_vars
  #two parents will give birth to two children 
  children <- matrix(rep(0,2*pop_size*num_vars), nrow = 2*pop_size, ncol = num_vars)
  
  count = 1
  while(count < nrow(children))
  {
   pair_num <- sample(1:nrow(parents),size = 2, replace = FALSE)
   parent1 <- parents[pair_num[1],]
   parent2 <- parents[pair_num[2],]
   cross_point <- sample(1:num_vars, size = 1)
   
   #Check if the cross_point is at the first or the last position
   if (cross_point == 1 || cross_point == num_vars)
   {
    children[count,] <- parent1 
    children[count+1,] <- parent2
   }
   else
   {
   children[count,] <- c(parent1[1:cross_point], parent2[(cross_point+1):num_vars])
   children[count+1,] <- c(parent2[1:cross_point], parent1[(cross_point+1):num_vars])
   }
   count = count+2
  }
  
  children <- children[which(rowSums(children) != 0),]
  
  #Pending: may or may not need to sort since sorting reduces diversity of the chromosome
  #order the child chromosome by fitness 
  #Fitness <- fitness(children, model_data, criterion="AIC")
  #order_pop <- children[order(Fitness, decreasing=TRUE),]
  #order_fit <- sort(Fitness, decreasing=TRUE)
  return(children)
}

#Testing
children <- crossover(ga_list, parents)

