source(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), "init.R"))

study1.raw.df <- readxl::read_excel("./01 raw data/data.xlsx",sheet="study1") 
study2.raw.df <- readxl::read_excel("./01 raw data/data.xlsx",sheet="study2") 
study3.raw.df <- readxl::read_excel("./01 raw data/data.xlsx",sheet="study3") 

save(list = c("study1.raw.df", "study2.raw.df","study3.raw.df"), file = "./02 RData/raw.RData")