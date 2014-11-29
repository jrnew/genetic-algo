setwd("~/Copy/Berkeley/genetic-algo")
# Test data
# airquality$y <- airquality$Wind + 5*airquality$Temp + rnorm(nrow(airquality))
# write.csv(airquality, "data/airquality.csv", row.names = F)
airquality <- read.csv("data/airquality.csv")


# summary
# plot