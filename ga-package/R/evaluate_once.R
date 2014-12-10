evaluate_once <-
function(
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
