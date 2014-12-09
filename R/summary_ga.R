#' Function to summerize the result from the genetic algorithim
#' 
#' Provides the top 5 fit for the linear model.
#' @param ga Object of class \code{ga}.
#' @return Dataframe containning the linear combinations with small AIC
#' and their corresponding AIC 

summary.ga <- function(ga_result)
{
  rank <- order(ga_result$log$evaluation_best)
  top5 <- ga_result$log$models[rank,][1:5,]
  score <- sort(ga_result$log$evaluation_best)[1:5]

  for (i in 1:5){
  x <- ga_result$model_data$xvars[which(top5[i,]==1)]
  y <- ga_result$model_data$yvar
  right <- paste(x,collapse = "+")
  cat("Model", i,":", "\n",
      paste(c(y,right), collapse = "="), "\n",
      "AIC", "=", score[i], "\n")
  }
  
}
#Test
#ga_result <- ga(data,yvar = yvar, xvars = xvars, num_max_iterations = 10L, method_select = "DS")
#summary.ga(ga_result)

