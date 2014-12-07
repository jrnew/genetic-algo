#' Function for computing copies score for one generation. 
#' 
#' @param Fitness Numberic Vector: Containning fitness score for each chromosome 
#' @param Seed Non-negative integer; Default is 123; Random seed 
#' @return A vector contains the number of copies for each chromosome

copy <- function(Fitness, seed = 123) 
{
  set.seed(seed)
  random <- runif(length(Fitness),min=0,max=1)
  copies <- rep(0,length(Fitness))
  
  remainder <- Fitness - floor(Fitness)
  for (i in 1:pop_size)
  { 
    if (remainder [i] <= random[i])
    {copies[i] <- floor(Fitness[i])}
    else
    {copies[i] <- ceiling(Fitness[i])}
  }
  return(copies)
}

#Test
Fitness <- ga_list$Fitness
copies <- copy(Fitness, seed = 123)
