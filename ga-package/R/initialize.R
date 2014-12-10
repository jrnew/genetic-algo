initialize <-
function(
  pop_size,
  num_vars
) {
  stopifnot(is.integer(pop_size))
  stopifnot(pop_size > 0)
  stopifnot(is.integer(num_vars))
  stopifnot(num_vars > 0)
  
  pop <- matrix(sample(c(0, 1), size = pop_size*num_vars,
                       replace = TRUE),
                nrow = pop_size,
                ncol = num_vars)
  return(pop)
}
