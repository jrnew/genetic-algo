#' Do evaluation.
#' 
#' Do evaluation for chromosomes in population by calculating model selection criterion.
#' 
#' @param pop Matrix of population of chromosomes.
#' @param model_data; Object of class \code{model_data}.
#' @param model; Character; "lm" (default) or "glm"; Linear model or 
#' generalized linear model.
#' @param criterion; "AIC" (default) or "BIC"; Criterion to be minimized.
#' @param do_parallel Logical; Default \code{FALSE}; Do in parallel?
#' @return Numeric vector; Evaluation values for all chromosomes 
#' in the current generation.
evaluate <- function(
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

#' Do evaluation once.
#' 
#' Do evaluation for a chromosome by calculating model selection criterion.
#' 
#' @param model_data; Object of class \code{model_data}.
#' @param xvars_select; Logical vector; 
#' @param model; Character; "lm" (default) or "glm"; Linear model or generalized linear model.
#' @param criterion; "AIC" (default) or "BIC"; AIC or BIC.
#' @return Numeric; Value of criterion.
evaluate_once <- function(
  model_data,
  xvars_select,
  model = "lm",
  criterion = "AIC"
) {
  stopifnot(model %in% c("lm", "glm"))
  stopifnot(criterion %in% c("AIC", "BIC"))
  
  mod_formula <- as.formula(paste(
    model_data$yvar, "~", 
    ifelse(sum(xvars_select) == 0, 1,
           paste(model_data$xvars[xvars_select], collapse = " + "))))
  if (model == "lm") {
    mod <- lm(mod_formula, data = model_data$data)
  } else if (model == "glm") {
    mod <- glm(mod_formula, family = "gaussian", data = model_data$data)
  }
  if (criterion == "AIC") {
    result <- AIC(mod)
  } else if (criterion == "BIC") {
    result <- BIC(mod)
  }
  return(result)
}

# Testing
# for (model in c("lm", "glm")) 
#   for (criterion in c("AIC", "BIC"))
#     print(evaluate_once(model_data = model_data, 
#                         xvars_select = as.logical(c(1, 0, 1, 0, 1, 1)),
#                         model = model, criterion = criterion))

