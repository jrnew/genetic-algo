#' Select chromosomes for recombination.
#' 
#' Select chromosomes for recombination based on fitness.
#' 
#' @param pop Matrix; Population of chromosomes.
#' @param evaluation Numeric vector; Evaluation values of all chromosomes in population.
#' @param method String; "LRS" (linear rank selection) (default), 
#' "RWS" (roulette wheel selection), "DS" (deterministic sampling), 
#' "SRS" (stochastic remainder sampling) or "SUS" (stochastic universal selection);
#' Method to select chromosomes for inclusion in mating pool.
#' @param do_parallel Logical; Default \code{FALSE}; Do in parallel?
#' @return Matrix of population of chromosomes that form the mating pool.
select <- function(
  pop,
  evaluation,
  method = "LRS",
  do_parallel = FALSE
) {
  mating_pool_size <- nrow(pop)
  indices_select <- rep(NA, mating_pool_size)
  if (method %in% c("LRS", "RWS")) {
    # Select chromosomes randomly with probability proportional to 
    # their relative rank/fitness
   if (method == "LRS") {
      # Assign rank 1 to chromosome with highest (worst) evaluation criterion
      # and vice versa
      rank <- order(evaluation, decreasing = TRUE)
      fitness <- rank/sum(rank)
    } else if (method == "RWS") {
      # Subtract from 1 because evaluation criterion is minimized
      fitness <- 1 - evaluation/sum(evaluation)
    }
    cum_fitness <- cumsum(fitness)
    # Generate a random number rand in range [0, 1]. 
    # If all elements of cum_fitness < rand, select last index.
    # Else select first index of cum_fitness with element >= rand 
    if (do_parallel) {
      indices_select <- foreach (i = 1:mating_pool_size, .combine = c) %dopar% {
        rand <- runif(1)
        ifelse(all(cum_fitness < rand), length(cum_fitness), 
               which(cum_fitness >= rand)[1])
      }
    } else {
      for (i in 1:mating_pool_size) {
        rand <- runif(1)
        indices_select[i] <- ifelse(all(cum_fitness < rand), 
                                    length(cum_fitness), 
                                    which(cum_fitness >= rand)[1])
      }
    }
  } else if (method %in% c("DS", "SRS")) {
    ### Fix for minimization/negative criterion
    fitness <- evaluation/mean(evaluation)
    # Calculate integer and fraction portions of fitness for each chromosome
    fitness_integer <- floor(fitness)
    fitness_fraction <- fitness - fitness_integer
    num_slots_filled <- sum(fitness_integer)
    num_slots_remaining <- mating_pool_size - num_slots_filled
    # Select each chromosome fitness_integer times for mating pool
    select_fitness_integer <- fitness_integer > 0
    indices_select[1:num_slots_filled] <- rep(which(select_fitness_integer), 
                                              fitness_integer[select_fitness_integer])
    if (method == "DS") {
      # Fill remaining slots in mating pool with chromosomes with
      # largest fitness_fraction values
      fitness_fraction_order <- order(fitness_fraction, decreasing = TRUE)
      indices_select[(num_slots_filled + 1):mating_pool_size] <- 
        which(fitness_fraction_order <= num_slots_remaining)
    } else if (method == "SRS") {
      ### Fix for minimization/negative criterion
      # Fill remaining slots in mating pool with chromosomes based on
      # roulette wheel selection
      cum_fitness_fraction <- cumsum(fitness_fraction/sum(fitness_fraction))
      if (do_parallel) {
        indices_select[(num_slots_filled + 1):mating_pool_size] <- 
          foreach (i = 1:num_slots_remaining, .combine = c) %dopar% {
            rand <- runif(1)
            ifelse(all(cum_fitness_fraction < rand), 
                   length(cum_fitness_fraction), 
                   which(cum_fitness_fraction >= rand)[1])
          }
      } else {
        for (i in 1:num_slots_remaining) {
          rand <- runif(1)
          indices_select[num_slots_filled + i] <- 
            ifelse(all(cum_fitness_fraction < rand), 
                   length(cum_fitness_fraction), 
                   which(cum_fitness_fraction >= rand)[1])
        }
      }
    }
  } else if (method == "SUS") {
    fitness <- 1 - evaluation/sum(evaluation)
    cum_fitness <- cumsum(fitness)
    rand <- runif(1)
    seq_temp <- seq(from = rand, by = 1/mating_pool_size, 
                    length.out = mating_pool_size)
    seq <- sort(ifelse(seq_temp > 1, seq_temp - 1, seq_temp))
    if (do_parallel) {
      indices_select <- foreach (i = 1:mating_pool_size, .combine = c) %dopar% {
        ifelse(all(cum_fitness < seq[i]), length(cum_fitness), 
               which(cum_fitness >= seq[i])[1])
      }
    } else {
      for (i in 1:mating_pool_size) {
        indices_select[i] <- ifelse(all(cum_fitness < seq[i]), 
                                    length(cum_fitness), 
                                    which(cum_fitness >= seq[i])[1])
      }
    }
  }
  pop_mating <- pop[indices_select, ]
  return(pop_mating)
}
