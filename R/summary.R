#' Display summary of results from the genetic algorithim.
#' 
#' Outputs the top models selected from the genetic algorithm.
#' 
#' @param ga Object of class \code{ga}.
#' @param num_view Number of top models to display.
#' @return Prints summary of top models and associated value of 
#' model selection criterion.
summary_ga <- function(
  ga, 
  num_view = 5
) {
  eval <- unique(ga$log$models)
  eval_score <- unique(ga$log$evaluation_best)
 
  if (nrow(eval) < num_view){
    cat ("No enough to print, try use a smaller num_view value")
  }
  else if (nrow(eval) == 1){
    x <- ga$model_data$xvars[which(eval == 1)]
    y <- ga$model_data$yvar
    xvars <- paste(x, collapse = " + ")
    cat("Model","1", ":\n",
        paste(c(y,xvars), collapse = "="), "\n",
        ga$settings$criterion, "=", eval_score[1], "\n")
  }
  else{
   rank <- order(eval_score)
   top <- eval[rank, ][1:num_view, ]
   score <- sort(eval_score)[1:num_view]

   for (i in 1:num_view) {
     x <- ga$model_data$xvars[which(top[i, ] == 1)]
     y <- ga$model_data$yvar
     xvars <- paste(x, collapse = " + ")
     cat("Model", i, ":\n",
        paste(c(y, xvars), collapse = " = "), "\n",
        ga$settings$criterion, "=", score[i], "\n",
        "--------------------------------------------------\n")
   }
  }
  return(invisible())
}

#Testing
data <- as.data.frame(trancSFO)
yvar <- "ArrDelay"
pop_size <- min(nrow(data)*2, 200)
xvars <- c( "Month", "DayofMonth", "DayofWeek", "DepTime", 
            "CRSDepTime", "ArrTime", "CRSArrTime", 
            "FlightNum", "TailNum", "ActualElapsedTime", "CRSElaspsedTime", 
            "AirTime", "DepDelay", "Origin", "Distane","TaxiIn",
            "TaxiOut", "NASDelay")
ga <- select_model(data,yvar = yvar, xvars = xvars ,num_max_iterations = 10L, seed = 100)
summary_ga(ga)
summary_ga(ga,num_view = 12)

data <- berkhousing
yvar <- "price"
xvars <- c("tax","sqft","built")
pop_size <- min(nrow(data)*2, 200)
ga <- select_model(data,yvar = yvar, xvars = xvars, num_max_iterations = 10L)
summary_ga(ga)
