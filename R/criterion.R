#' Do evaluation by calculating model selection criterion.
#' 
#' Do evaluation by calculating model selection criterion.
#' 
#' @param model_data; Object of class \code{model_data}.
#' @param xvars_select; Logical vector; 
#' @param model; Character; "lm" (default) or "glm"; Linear model or generalized linear model.
#' @param criterion; "AIC" (default) or "BIC"; AIC or BIC.
#' @return Numeric; Value of criterion.
evaluate <- function(
  model_data,
  xvars_select,
  model = "lm",
  criterion = "AIC"
) {
  # Pending:
  # Check that model and criterion input values are valid!
  
  mod_formula <- as.formula(paste(
    model_data$yvar, "~", 
    paste(model_data$xvars[xvars_select], collapse = "+")))
  if (model == "lm") {
    mod <- lm(mod_formula, data = model_data$data)
  } else {
    mod <- glm(mod_formula, family = "gaussian", data = model_data$data)
  }
  if (criterion == "AIC") {
    result <- AIC(mod)
  } else {
    result <- BIC(mod)
  }
  return(result)
}

# Testing
for (model in c("lm", "glm")) 
  for (criterion in c("AIC", "BIC"))
    print(evaluate(model_data = model_data, 
                   xvars_select = as.logical(c(1, 0, 1, 0, 1, 1)),
                   model = model, criterion = criterion))
