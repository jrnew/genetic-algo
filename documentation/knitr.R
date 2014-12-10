rm(list = ls())
library(knitr)
setwd("~/Copy/Berkeley/genetic-algo/documentation")
file <- "manual_yang.Rtex"
knit2pdf(file)