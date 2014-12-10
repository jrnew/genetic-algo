#' Plots results from the genetic algorithim.
#' 
#' Plots the best model evaluation criterion in each generation
#' against the generation iteration.
#' 
#' @param ga Object of class ga.
#' @param num_view Number of top models to display.
#' @return Prints summary of top models and associated value of 
#' model selection criterion.
plot.ga <- function(
  ga,
  num_view = 3
) {
  stopifnot(class(ga) == "ga")
  stopifnot(is.numeric(num_view))
  
  generation <- 1:ga$settings$num_max_iterations
  outliers <- boxplot(ga$log$evaluation_best)$out
  range_evaluation <- range(ga$log$evaluation_best)
#   range_evaluation <- range(ga$log$evaluation_best[
#     !(ga$log$evaluation_best %in% outliers)])
#   if (range_evaluation[1] < 0) {
#     range_evaluation[1] <- range_evaluation[2]*1.1
#   } else {
#     range_evaluation[2] <- range_evaluation[2]*0.9
#   }
#   if (range_evaluation[2] < 0) {
#     range_evaluation[2] <- range_evaluation[2]*0.9
#   } else {
#     range_evaluation[2] <- range_evaluation[2]*1.1
#   }
  plot(ga$log$evaluation_best ~ generation,
       main = "Evolution of best model through generations",
       xlab = "Generation", 
       ylab = paste0("Best ", ga$settings$criterion),
       ylim = range_evaluation, type = "l")
  return(invisible())
}
