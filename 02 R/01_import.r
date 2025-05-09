source(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), "init.R"))

load(file = "./01 raw data/study1.RData")

study1.raw.df <- stress.data # Rename to match naming convention
study2.raw.df <- readxl::read_excel("./01 raw data/data.xlsx",sheet="study2") 
study3.raw.df <- readxl::read_excel("./01 raw data/data.xlsx",sheet="study3") 
study4.raw.df <- readxl::read_excel("./01 raw data/data.xlsx",sheet="study4") 

save(list = c("study1.raw.df", "study2.raw.df", "study3.raw.df","study4.raw.df"), file = "./02 RData/raw.RData")
