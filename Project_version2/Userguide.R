#process_data: function to define the observed data
#ga: i         initializing ga_list, which contains all information about the algorithm
#initialize:   initiation of the the first generation
#fitness:      calculate fitness score
#evaluation:   calulate the criterion score, the default is AIC
#copy:         making copies of chromosome based on fitness scores
#selection:    select fittest parent chromsomes
#crossover:    produce child chromsomes
#mutate:       chromsome mutation in the child generation
#reproduction: updating the ga_list so it contains all information about the child generation


#The following is an example to run the codes 


data <- read.csv("airquality.csv", header = TRUE)
yvar <- "y"
xvars <- names(data)[1:length(data)-1]
model_data <- process_data(data = data, yvar = yvar) #a list containning information about the observation
pop_size <- max(nrow(data)*2, 200)

ga_list <- ga (data, yvar, xvars = NULL, model = "lm", criterion = "AIC",
               pop_size = pop_size, num_crossover_breaks = 1, mutation_rate = 0.01,
               elitism_rate = 0.2, num_max_iterations = 10, seed = 123) 

iteration = 1
while(iteration <= ga_list$settings$num_max_iterations) 
{
  ga_list <- reproduce(ga_list,model_data,iteration)
  iteration <- iteration + 1 
}
 