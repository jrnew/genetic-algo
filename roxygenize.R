library(tools)
devtools::document() 

dir.create("texforfunctions")
rm(list = ls())
Rfiles <- list.files(file.path(getwd(), "R"))
Rfiles <- Rfiles[grepl(".R", Rfiles)]
sapply(paste0("R/", Rfiles), source)
rm(Rfiles)
functionnames <- ls()
for (functionname in functionnames)
  Rd2latex(paste0("man/", functionname, ".Rd"), 
           out = paste0("texforfunctions/", functionname, ".tex"), 
           outputEncoding = "UTF-8")

# Get tex file that inputs all functions
cat(paste0("\\input{texforfunctions/", functionnames, "} \n"), 
    file = paste("overviewallfunctions.tex"))

