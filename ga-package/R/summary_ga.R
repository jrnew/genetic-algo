summary_ga <-
function(
  ga, 
  num_view = 5
) {
  rank <- order(ga$log$evaluation_best)
  top <- ga$log$models[rank, ][1:num_view, ]
  score <- sort(ga$log$evaluation_best)[1:num_view]
  
  for (i in 1:num_view) {
    x <- ga$model_data$xvars[which(top[i, ] == 1)]
    y <- ga$model_data$yvar
    xvars <- paste(x, collapse = " + ")
    cat("Model", i, ":\n",
        paste(c(y, xvars), collapse = " = "), "\n",
        ga$settings$criterion, "=", score[i], "\n",
        "--------------------------------------------------\n")
  }
  return(invisible())
}
