\name{GA-package}
\alias{GA-package}
\alias{GA-package}
\docType{package}
\title{
Genetic algorithm for model variable selection
}
\description{
Final project for Statistics 243. An R package that implements a genetic algorithm for variable selection in linear and GLM problems.
}
\details{
\tabular{ll}{
Package: \tab GA-package\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-12-13\cr
}
}
\author{
Eddie Buehler, Yang Hu, JR New
}
\references{
G. Givens and J. Hoeting. \strong{Computational Statistics, 2nd ed.} (2012).
}

\keyword{ package }
\seealso{
https://github.com/jrnew/genetic-algo
}
\examples{
# Select regression variables for airquality data using lm model and AIC criterion
select_model(
  data = airquality,
  yvar = "Ozone",
  xvars = NULL,
  model = "lm",
  criterion = "AIC",
  pop_size = 100L,
  method_select = "rank",
  method_recombine = "onepoint",
  prob_recombine = 0.6,
  prob_mutate = 0.01,
  num_max_iterations = 100L,
  seed = 123,
  do_parallel = FALSE
)

# With a user-defined model evaluation criterion function
rsquared <- function(lm) {
  mod <- summary(lm)
  return(-mod\\$r.squared)
}
ga <- select_model(data = airquality,
                   yvar = "Ozone",
                   model = "lm",
                   criterion = "rsquared",
                   criterion_function = rsquared)
}
