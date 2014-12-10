evaluate <-
function(
  pop,
  model_data,
  model = "lm",
  criterion = "AIC",
  do_parallel = FALSE
) {
  stopifnot(is.matrix(pop))
  stopifnot(all(c(pop) %in% c(0, 1)))
  stopifnot(is.list(model_data))
  stopifnot(model %in% c("lm", "glm"))
  stopifnot(criterion %in% c("AIC", "BIC"))
  stopifnot(is.logical(do_parallel))
  
  if (do_parallel) {
    evaluation <- foreach (i = 1:nrow(pop), .combine = c) %dopar%
      evaluate_once(model_data = model_data, 
                    xvars_select = as.logical(pop[i, ]),
                    model = model, criterion = criterion)
  } else {
    evaluation <- rep(NA, nrow(pop))
    for (i in 1:nrow(pop))
      evaluation[i] <- evaluate_once(model_data = model_data, 
                                     xvars_select = as.logical(pop[i, ]),
                                     model = model, criterion = criterion)
  }  
  return(evaluation)
}
