source(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), "init.R"))

study2.raw.df <- readxl::read_excel("./01 raw data/data.xlsx",sheet="study2") 
study3.raw.df <- readxl::read_excel("./01 raw data/data.xlsx",sheet="study3") 
study4.raw.df <- readxl::read_excel("./01 raw data/data.xlsx",sheet="study4") 

save(list = c("study2.raw.df", "study3.raw.df","study4.raw.df"), file = "./02 RData/raw.RData")
