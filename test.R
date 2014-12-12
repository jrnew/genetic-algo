test <- function() {
  test_dir("testing")
  data <- read.table("data/video.txt", header = TRUE, quote = "\"")
  ga <- select(data, 
                     yvar = "grade",
                     pop_size = nrow(data)*2,
                     num_max_iterations = 50, 
                     model = "glm",
                     glm_family = "gaussian")
  res <- summary(ga)
  plot(ga)
  mod <- glm(grade ~ ., data = data) 
  res_step <- stepAIC(mod)
  cat("Testing done!\n")
  return(invisible())
}
