# clear workspace
rm(list = ls())

# load packages
# Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_202')



library(readxl)
library(dplyr)

df <- read_excel("B:\\Downloads\\Dyad Formation - Post Experiment Survey.xlsx", sheet="timing")


time <- df %>% 
  as.matrix %>% 
  rowSums %>% # if you do not outcomment this one, it gives you the sum, i.e., total time per dyad
  print %>%
  as.numeric() %>% 
  as.data.frame() %>% 
  plyr::rename(c("." = "time")) %>% 
  mutate(time = 300 * 6 - time) %>% # multiple with 6 if row sums (!)
  .$time


time %>%
  hist



rnorm(240,mean=mean(time), sd=sd(time)) %>% hist

# c(rnorm(120, mean=1), rnorm(120, mean=5)) %>% hist
