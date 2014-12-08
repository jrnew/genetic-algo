#' Process data for input into genetic algorithm.
#' 
#' Process data for input into genetic algorithm.
#' 
#' @param data Data frame
#' @param yvar Character; Name of column containing response variable.
#' @param xvars Character vector; Default is all column names that are not yvar;
#' Name(s) of column(s) containing set of explanatory variables to select on.
#' @return A list object named \code{model_data} containing:
#' \describe{
#'   \item{\code{data}}{Data frame; Processed data with only relevant columns.}
#'   \item{\code{yvar}}{Character; Name of column containing response variable.}
#'   \item{\code{xvars}}{Character vector; Name(s) of column(s) containing 
#'   set of explanatory variables to select on.}
#'   \item{\code{num_vars}}{Integer; Length of xvars.}   
#' } 
process_data <- function(
  data,
  yvar,
  xvars = NULL
) {
  vars_all <- colnames(data)
  vars <- vars_all[!(vars_all %in% yvar)]
  # If xvars is NULL, set xvars = vars, else check that all
  # xvars can be found in column names of data.
  if (is.null(xvars)) {
    xvars <- vars
  } else if (any(!(xvars %in% vars))) {
    stop(paste("The following xvars do not correspond to", 
               "any column names in data:",
               xvars[!(xvars %in% vars)], collapse = ", "))
  }
  data_proc <- subset(data, select = c(yvar, xvars))
  model_data <- list(data = data_proc,
                     yvar = yvar,
                     xvars = xvars,
                     num_vars = length(xvars))
  return(model_data)
}

# Testing
# data <- read.csv("data/airquality.csv", header = TRUE)
# yvar <- "y"
# model_data <- process_data(data = data, yvar = yvar)
