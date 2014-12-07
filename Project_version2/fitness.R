#' Carry out model selection with a genetic algorithm.
#' 
#' Function for computing fitness score for one generation. 
#' 
#' @param model_data List; Containing data, names of x variables and y values 
#' @param pop Matrix; The current population, containing only 1's and 0's to 
#' represent current population of chromosomes.
#' @param criterion character; "AIC" (default) or "BIC"; AIC or BIC.
#' @return A vector contains fitness score for each chromosome.
#' 
fitness <- function(pop, model_data, criterion)
{ 
  f <- rep(0,nrow(pop))
  for (i in 1:nrow(pop))
  {
    f[i] <- evaluate(model_data, xvars_select=as.logical(pop[i,]), model = "lm", criterion = criterion)    
  }
  
  fit <- f/mean(f)
  return(fit)
}

#Test
fitness(pop, model_data, criterion="AIC")

