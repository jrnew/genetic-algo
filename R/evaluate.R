#' Do evaluation.
#' 
#' Do evaluation for chromosomes in population by calculating model selection criterion.
#' 
#' @param pop Matrix of population of chromosomes.
#' @param model_data Object of class model_data.
#' @param model Character; "lm" (default) or "glm"; Linear model or 
#' generalized linear model.
#' @param glm_family Character if model is "glm", NULL otherwise; 
#' "binomial", "gaussian" (default), "Gamma", "inverse.gaussian", "poisson", "quasi",
#' "quasibinomial", "quasipoisson"; A family function that gives the error 
#' distribution and link function to be used in the model.
#' @param criterion "AIC" (default) or "BIC"; Criterion to be minimized.
#' @param do_parallel Logical; Default FALSE; Do in parallel?
#' @return Numeric vector; Evaluation values for all chromosomes 
#' in the current generation.
evaluate <- function(
  pop,
  model_data,
  model = "lm",
  glm_family = NULL,
  criterion = "AIC",
  do_parallel = FALSE
) {
  stopifnot(is.matrix(pop))
  stopifnot(all(c(pop) %in% c(0, 1)))
  stopifnot(is.list(model_data))
  stopifnot(model %in% c("lm", "glm"))
  stopifnot(criterion %in% c("AIC", "BIC"))
  stopifnot(is.logical(do_parallel))
  if (model == "glm") {
    stopifnot(!is.null(glm_family))
    stopifnot(is.character(glm_family))
    stopifnot(glm_family %in%
                c("binomial", "gaussian", "Gamma", "inverse.gaussian",
                  "poisson", "quasi", "quasibinomial", "quasipoisson"))
  }
  
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
#' @param model_data; Object of class model_data.
#' @param xvars_select; Logical vector; 
#' @param model; Character; "lm" (default) or "glm"; Linear model or generalized linear model.
#' @param glm_family Character if model is "glm", NULL otherwise; 
#' "binomial", "gaussian" (default), "Gamma", "inverse.gaussian", "poisson", "quasi",
#' "quasibinomial", "quasipoisson"; A family function that gives the error 
#' distribution and link function to be used in the model.
#' @param criterion; "AIC" (default) or "BIC"; AIC or BIC.
#' @return Numeric; Value of criterion.
evaluate_once <- function(
  model_data,
  xvars_select,
  model = "lm",
  glm_family = NULL,
  criterion = "AIC"
) {
  stopifnot(model %in% c("lm", "glm"))
  stopifnot(criterion %in% c("AIC", "BIC"))
  if (model == "glm") {
    stopifnot(!is.null(glm_family))
    stopifnot(is.character(glm_family))
    stopifnot(glm_family %in%
                c("binomial", "gaussian", "Gamma", "inverse.gaussian",
                  "poisson", "quasi", "quasibinomial", "quasipoisson"))
  }
  
  mod_formula <- as.formula(paste(
    model_data$yvar, "~", 
    ifelse(sum(xvars_select) == 0, 1,
           paste(model_data$xvars[xvars_select], collapse = " + "))))
  if (model == "lm") {
    mod <- lm(mod_formula, data = model_data$data)
  } else if (model == "glm") {
    mod <- glm(mod_formula, family = glm_family, data = model_data$data)
  }
  if (criterion == "AIC") {
    result <- AIC(mod)
  } else if (criterion == "BIC") {
    result <- BIC(mod)
  }
  return(result)
}
