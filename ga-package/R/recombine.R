recombine <-
function(
  pop_mating,
  pop_size,
  method = "onepoint",
  prob_recombine = 0.6,
  do_parallel = FALSE
) {
  stopifnot(is.matrix(pop_mating))
  stopifnot(all(c(pop_mating) %in% c(0, 1)))
  stopifnot(is.integer(pop_size))
  stopifnot(method %in% c("onepoint", "twopoint", "uniform"))
  stopifnot(is.logical(do_parallel))
  stopifnot(is.numeric(prob_recombine))
  stopifnot(prob_recombine >= 0 & prob_recombine <= 1)
  
  do_recombine <- as.logical(rbinom(pop_size, 1, prob_recombine))
  if (do_parallel) {
    pop_new <- foreach (i = 1:pop_size, .combine = rbind) %dopar% {
      indices_parents <- sample(1:nrow(pop_mating), size = 2)
      if (do_recombine[i]) {
        recombine_once(parent1 = pop_mating[indices_parents[1], ], 
                       parent2 = pop_mating[indices_parents[2], ], 
                       method = method)
      } else {
        pop_mating[indices_parents[1], ]
      }
    }
  } else {
    pop_new <- matrix(NA, pop_size, ncol(pop_mating))
    for (i in 1:pop_size) {
      indices_parents <- sample(1:nrow(pop_mating), size = 2)
      if (do_recombine[i]) {
        pop_new[i, ] <- recombine_once(parent1 = pop_mating[indices_parents[1], ], 
                                       parent2 = pop_mating[indices_parents[2], ], 
                                       method = method)
      } else {
        pop_new[i, ] <- pop_mating[indices_parents[1], ]
      }
    }
  }
  return(pop_new)
}
