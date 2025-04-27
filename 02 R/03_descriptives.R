source(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), "init.R"))


###############################
#
#   Load raw files
#
###############################

load(file = "./02 RData/analysis.RData")


# let's take a quick look what we have
Hmisc::describe(study1.df)


###### 
# 
# Factor variables - Table 1
#

print.table.1<-function(){
  output <- createTable(list(study1.df$age.range,
                             study1.df$gender,
                             study1.df$en.native,
                             study1.df$learning.disorders,
                             study1.df$medications.pills),
              c("age","gender","native english speaker","learning disorder","medication pills"),
              list(study1.df$experience.nreal,
                   study1.df$experience.gpt,
                   study1.df$bmi.category),
              c("AR experience","GPT experience","bmi category"))
  clipr::write_clip(output)
}

print.table.1()


###### 
# 
# Cont. variables - Table 2
#


print.string = paste(
  print.tex.effectVar(task.df %>% subset(task == 1 & task.type == "s") %>% pull(value),"repair time (simple task 1)"),
  print.tex.effectVar(task.df %>% subset(task == 2 & task.type == "s") %>% pull(value),"repair time (simple task 2)"),
  print.tex.effectVar(task.df %>% subset(task == 3 & task.type == "s") %>% pull(value),"repair time (simple task 3)"),
  print.tex.effectVar(task.df %>% subset(task == 1 & task.type == "c") %>% pull(value),"repair time (complex task 1)"),
  print.tex.effectVar(task.df %>% subset(task == 2 & task.type == "c") %>% pull(value),"repair time (complex task 2)"),
  print.tex.effectVar(task.df %>% subset(task == 3 & task.type == "c") %>% pull(value),"repair time (complex task 3)"),
  print.tex.effectVar(subject.df$same.preference,"preference for fixed dyad"),
  print.tex.effectVar(subject.df$varying.preference,"preference for random dyad"),
  print.tex.effectVar(subject.df$same.important.for.complex,"importance of fixed dyad for complex task"),
  print.tex.effectVar(subject.df$varying.important.for.complex,"importance of random dyad for complex task"),
  print.tex.effectVar(subject.df$fast.repair.onsite,"benefits of onsite collaboration"),
  print.tex.effectVar(subject.df$team.relationships,"team relationship quality"),
  print.tex.effectVar(subject.df$problem.solving,"team ability to solve problem"),
  print.tex.effectVar(subject.df$skills.and.learning,"team ability to acquire skills and learn"),
  sep=""
)

cat(print.string)
clipr::write_clip(print.string)


# functions
se <- function(x) sd(x)/sqrt(length(x))


###### 
# 
# Correlation Table -- Table 3 (Panel A)
#
#######


corr.matrix.experiment <- subject.df %>%
  select(c(
          "AR",
          "random.dyad",
          "technician",
          "task.s.1",
          "task.s.2",
          "task.s.3",
          "task.c.1",
          "task.c.2",
          "task.c.3")) %>% 
  rename_with(~ gsub("AR", "AR (1=yes)", .)) %>%
  rename_with(~ gsub("random.dyad", "random dyad (1=yes)", .)) %>%
  rename_with(~ gsub("technician", "technician (1=yes)", .)) %>%
  rename_with(~ gsub("task.s.1", "repair time (simple task 1)", .))  %>%
  rename_with(~ gsub("task.s.2", "repair time (simple task 2)", .))  %>%
  rename_with(~ gsub("task.s.3", "repair time (simple task 3)", .))  %>%
  rename_with(~ gsub("task.c.1", "repair time (complex task 1)", .))  %>%
  rename_with(~ gsub("task.c.2", "repair time (complex task 2)", .))  %>%
  rename_with(~ gsub("task.c.3", "repair time (complex task 3)", .))  %>% 
  as.matrix %>%
  apply(., 2., as.numeric) %>%
  Hmisc::rcorr(., type = "pearson")

corr.matrix.experiment %>% print.corr.matrix

######################### 
# 
# Correlation Table -- Table 3 (Panel B)
#
#########################

corr.matrix.survey <- subject.df %>%
  select(c(
          "AR",
          "random.dyad",
          "technician",
          "same.preference",
          "varying.preference",
          "same.important.for.complex",
          "varying.important.for.complex",
          "fast.repair.onsite",
          "team.relationships",
          "problem.solving",
          "skills.and.learning")) %>% 
  rename_with(~ gsub("AR", "AR (1=yes)", .)) %>%
  rename_with(~ gsub("random.dyad", "random dyad (1=yes)", .)) %>%
  rename_with(~ gsub("technician", "technician (1=yes)", .)) %>%
  rename_with(~ gsub("same.preference", "preference for fixed dyad", .)) %>%
  rename_with(~ gsub("varying.preference", "preference for random dyad", .)) %>%
  rename_with(~ gsub("same.important.for.complex", "importance of fixed dyad for complex task", .)) %>%
  rename_with(~ gsub("varying.important.for.complex", "importance of random dyad for complex task", .)) %>%
  rename_with(~ gsub("fast.repair.onsite", "benefits of onsite collaboration", .)) %>%
  rename_with(~ gsub("team.relationships", "team relationship quality", .)) %>%
  rename_with(~ gsub("problem.solving", "team ability to solve problem", .))  %>%
  rename_with(~ gsub("skills.and.learning", "team ability to acquire skills and learn", .))  %>% 
  as.matrix %>%
  apply(., 2., as.numeric) %>%
  Hmisc::rcorr(., type = "pearson")



corr.matrix.survey %>% print.corr.matrix

######################### 
# 
# Mean Table -- Table 4
#
#########################

print.latex.mean.row(
  task.df %>% subset(task == 3 & task.type == "s" & AR == FALSE),
  proximity.label = "ONSITE",
  complexity.label = "simple"
)

print.latex.mean.row(
  task.df %>% subset(task == 3 & task.type == "c" & AR == FALSE),
  proximity.label = "ONSITE",
  complexity.label = "complex"
)

print.latex.mean.row(
  task.df %>% subset(task == 3 & task.type == "s" & AR == TRUE),
  proximity.label = "AR",
  complexity.label = "simple"
)

print.latex.mean.row(
  task.df %>% subset(task == 3 & task.type == "c" & AR == TRUE),
  proximity.label = "AR",
  complexity.label = "complex"
)


############################
#
# Randomization checks
#
############################

# todo: make group-wise comparisons with chi^2 tests and t.tests

# Function to perform chi-square tests. Chi square test more applicable as the data is non numeric.
run.chisq.test <- function(var1, var2, var1.label, var2.label) {
  test.result <- chisq.test(table(var1, var2))
  data.frame(
    Variable.1 = var1.label,
    Variable.2 = var2.label,
    Chi.Square = round(test.result$statistic, 2),
    df = test.result$parameter,
    p_value = format.pval(test.result$p.value, digits = 3, eps = 0.001)
  )
}

# Run all chi-square tests and store results
randomization.results <- bind_rows(
  run.chisq.test(subject.df$AR, subject.df$edu.factor, "AR", "education"),
  run.chisq.test(subject.df$random.dyad, subject.df$edu.factor, "Random Dyad", "education"),
  run.chisq.test(subject.df$AR, subject.df$major.factor, "AR", "major"),
  run.chisq.test(subject.df$random.dyad, subject.df$major.factor, "Random Dyad", "major"),
  run.chisq.test(subject.df$AR, subject.df$gender.factor, "AR", "gender"),
  run.chisq.test(subject.df$random.dyad, subject.df$gender.factor, "Random Dyad", "gender"),
  run.chisq.test(subject.df$AR, subject.df$years.of.work.ex.factor, "AR", "year of work experience"),
  run.chisq.test(subject.df$random.dyad, subject.df$years.of.work.ex.factor, "Random Dyad", "year of work experience"),
  run.chisq.test(subject.df$AR, subject.df$ar.experience.factor, "AR", "AR experience"),
  run.chisq.test(subject.df$random.dyad, subject.df$ar.experience.factor, "Random Dyad", "AR experience"),
  run.chisq.test(subject.df$AR, subject.df$electricals.experience.factor, "AR", "electrical equipment experience"),
  run.chisq.test(subject.df$random.dyad, subject.df$electricals.experience.factor, "Random Dyad", "electrical equipment experience"),
  run.chisq.test(subject.df$AR, subject.df$open.to.new.technologies.factor, "AR", "open to new technology"),
  run.chisq.test(subject.df$random.dyad, subject.df$open.to.new.technologies.factor, "Random Dyad", "open to new technology")
)



#chisq_NoAR_gender <- subject.df %>%
#  filter(AR == FALSE) %>%  # Filter for AR = FALSE
#  with(chisq.test(table(random.dyad, gender.factor), simulate.p.value = TRUE)) %>% 
#  print # Run Chi-square test

#chisq_AR_gender <- subject.df %>%
#  filter(AR == TRUE) %>%
#  mutate(gender.factor = droplevels(gender.factor)) %>%  # Remove empty levels
#  with(chisq.test(table(random.dyad, gender.factor), simulate.p.value = TRUE))

# Chi-square test for electricals experience distribution by dyad type
#chisq_NoAR_electricexp <- subject.df %>%
#  filter(AR == FALSE) %>%  # Filter for AR = FALSE
#  mutate(electricals.experience.factor = droplevels(electricals.experience.factor)) %>%  # Remove empty levels
#  with(chisq.test(table(random.dyad, electricals.experience.factor), simulate.p.value = TRUE))  # Run Chi-square test

#chisq_AR_electricexp <- subject.df %>%
#  filter(AR == TRUE) %>%
#  mutate(electricals.experience.factor = droplevels(electricals.experience.factor)) %>%  # Remove empty levels
#  with(chisq.test(table(random.dyad, electricals.experience.factor), simulate.p.value = TRUE))

# Chi-square test for AR experience distribution by dyad type
#chisq_AR_ARexp <- subject.df %>%
#  filter(AR == TRUE) %>%
#  mutate(ar.experience.factor = droplevels(ar.experience.factor)) %>%  # Remove empty levels
#  with(chisq.test(table(random.dyad, ar.experience.factor), simulate.p.value = TRUE))

# Chi-square test for open.to.new.technologies distribution by dyad type
#chisq_NoAR_techopen <- subject.df %>%
#  filter(AR == FALSE) %>%  # Filter for AR = FALSE
#  mutate(open.to.new.technologies.factor = droplevels(open.to.new.technologies.factor)) %>%  # Remove empty levels
#  with(chisq.test(table(random.dyad, open.to.new.technologies.factor), simulate.p.value = TRUE))  # Run Chi-square test

#chisq_AR_techopen <- subject.df %>%
#  filter(AR == TRUE) %>%
#  mutate(open.to.new.technologies.factor = droplevels(open.to.new.technologies.factor)) %>%  # Remove empty levels
#  with(chisq.test(table(random.dyad, open.to.new.technologies.factor), simulate.p.value = TRUE))
