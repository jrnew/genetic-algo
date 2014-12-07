#' Selection of chromosomes based on fitness. 
#' 
#' Function used to select parents chromosome, which will later be called when using GA function. 
#' 
#' @param ga_list Data Frame;
#' @param copies Numeric Vector; Containning number of copies each chromosome makes
#' @return Matrix; Contains selected ordered chromosomes based on fitness. 

selection <- function(ga_list, copies) 
{
  order_pop <- ga_list$pop[order(ga_list$Fitness, decreasing=TRUE),]
  order_fit <- sort(ga_list$Fitness, decreasing=TRUE)
  
  #making copies of the chromosome 
  copies <- copy(order_fit, seed = 123)
  rows <- rep(1:nrow(ga_list$pop) , copies)
  sel_pop <- order_pop[rows , ]

  return(sel_pop) #may want just update the ga list here
}

#Test
selection(ga_list,copies)
