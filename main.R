rm(list = ls())
setwd("~/Copy/Berkeley/genetic-algo")
# Load all libraries
library(doParallel)
library(foreach)

# Load all functions in R folder
Rfiles <- list.files(file.path(getwd(), "R"))
Rfiles <- Rfiles[grepl(".R", Rfiles)]
sapply(paste0("R/", Rfiles), source)

# Test data
n <- 100
x1 <- runif(n, 2, 3)
x2 <- runif(n, -5, -4)
x4 <- runif(n, 0.8, 1)
data <- data.frame(x1 = x1, x2 = x2, x3 = rnorm(n), x4 = x4, x5 = rnorm(n), 
                   y = x1 + 0.5*x2 + 1.7*x4)

# Examples
# With a user-defined model evaluation criterion function
rsquared <- function(lm) {
  mod <- summary(lm)
  return(-mod$r.squared)
}
ga <- select_model(data = airquality,
                   yvar = "Ozone",
                   model = "lm",
                   criterion = "rsquared",
                   criterion_function = rsquared)

# Testing
system.time({
  ga <- select_model(data = data,
                     yvar = "y",
                     xvars = NULL,
                     model = "lm",
                     glm_family = NULL,
                     criterion = "AIC",
                     criterion_function = NULL,
                     pop_size = 100L,
                     method_select = "rank",
                     method_recombine = "onepoint",
                     prob_recombine = 0.6,
                     prob_mutate = 0.01,
                     num_max_iterations = 100L,
                     seed = 123,
                     do_parallel = FALSE)
  # Check
  ga$log$models[ga$log$evaluation_best == min(ga$log$evaluation_best), ]
})

# summary
summary(ga)
# plot
plot(ga)

# Check using stepAIC
# mod <- lm(y ~ ., data)
# library(MASS)
# stepAIC(mod)
