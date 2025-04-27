source(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), "init.R"))


###############################
#
#   Load raw files
#
###############################

load(file = "./02 RData/analysis.RData")

############################
#
# Does AR, overall, perform worse?
#
############################

t.test(
  task.df %>% 
    subset(task == "3")          %>%  # this is the task for hypothesis testing
    subset(task.type == "s")     %>%  # condition in hypothesis
    subset(AR == FALSE)          %>%  # condition in hypothesis
    pull(value),
  task.df %>% 
    subset(task == "3")          %>%  # this is the task for hypothesis testing
    subset(task.type == "s")     %>%  # condition in hypothesis
    subset(AR == TRUE)          %>%  # condition in hypothesis
    pull(value)
)

# --> weakly worse in the simple task

t.test(
  task.df %>% 
    subset(task == "3")          %>%  # this is the task for hypothesis testing
    subset(task.type == "c")     %>%  # condition in hypothesis
    subset(AR == FALSE)          %>%  # condition in hypothesis
    pull(value),
  task.df %>% 
    subset(task == "3")          %>%  # this is the task for hypothesis testing
    subset(task.type == "c")     %>%  # condition in hypothesis
    subset(AR == TRUE)          %>%  # condition in hypothesis
    pull(value)
)

# --> weakly worse in the complex task

############################
#
# When choosing the WRONG approach for onsite (i.e., random for simple, dedicated for complex), do the AR perform worse? In other words, is the performance difference due to lacking specific benefits?
#
############################

t.test(
  task.df %>% 
    subset(task == "3")          %>%  # this is the task for hypothesis testing
    subset(task.type == "s")     %>%  # condition in hypothesis
    subset(AR == FALSE)          %>%  # condition in hypothesis
    subset(random.dyad == TRUE) %>%  # select first group for hypothesis
    pull(value),
  task.df %>% 
    subset(task == "3")          %>%  # this is the task for hypothesis testing
    subset(task.type == "s")     %>%  # condition in hypothesis
    subset(AR == TRUE)          %>%  # condition in hypothesis
    subset(random.dyad == TRUE) %>%  # select first group for hypothesis
    pull(value)
)

# --> n.s.: without the dedication-benefits, AR does not yield worse performance in simple tasks!


t.test(
  task.df %>% 
    subset(task == "3")          %>%  # this is the task for hypothesis testing
    subset(task.type == "c")     %>%  # condition in hypothesis
    subset(AR == FALSE)          %>%  # condition in hypothesis
    subset(random.dyad == FALSE) %>%  # select first group for hypothesis
    pull(value),
  task.df %>% 
    subset(task == "3")          %>%  # this is the task for hypothesis testing
    subset(task.type == "c")     %>%  # condition in hypothesis
    subset(AR == TRUE)          %>%  # condition in hypothesis
    subset(random.dyad == FALSE) %>%  # select first group for hypothesis
    pull(value)
)

# --> n.s.: without the random-benefits, AR does not yield worse performance in complex tasks!


############################
#
# When choosing the best suitbale approach for onsite (i.e., dedicated for simple, random for complex), do the AR perform worse?
#
############################

t.test(
  task.df %>% 
    subset(task == "3")          %>%  # this is the task for hypothesis testing
    subset(task.type == "s")     %>%  # condition in hypothesis
    subset(AR == FALSE)          %>%  # condition in hypothesis
    subset(random.dyad == FALSE) %>%  # select first group for hypothesis
    pull(value),
  task.df %>% 
    subset(task == "3")          %>%  # this is the task for hypothesis testing
    subset(task.type == "s")     %>%  # condition in hypothesis
    subset(AR == TRUE)          %>%  # condition in hypothesis
    subset(random.dyad == FALSE) %>%  # select first group for hypothesis
    pull(value),
)

# --> for simple tasks: YES, clearly!


t.test(
  task.df %>% 
    subset(task == "3")          %>%  # this is the task for hypothesis testing
    subset(task.type == "c")     %>%  # condition in hypothesis
    subset(AR == FALSE)          %>%  # condition in hypothesis
    subset(random.dyad == TRUE) %>%  # select first group for hypothesis
    pull(value),
  task.df %>% 
    subset(task == "3")          %>%  # this is the task for hypothesis testing
    subset(task.type == "c")     %>%  # condition in hypothesis
    subset(AR == TRUE)          %>%  # condition in hypothesis
    subset(random.dyad == TRUE) %>%  # select first group for hypothesis
    pull(value),
)

# --> for complex task, clearly yes


