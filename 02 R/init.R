rm(list = ls())

mrunal.wd = "H:\\TUM-PC\\Dokumente\\DATA\\01 AR Stress"
michela.wd = "C:\\Users\\michela.carraro\\Documents\\Research\\AR Stress\\R Analyses"


if(dir.exists(file.path(mrunal.wd, "."))){
    setwd(mrunal.wd)
    github.dir = "C:\\Users\\ge45tab\\GIT\\ARStress\\02 R\\" 
} 

if(dir.exists(file.path(michela.wd, "."))){
    setwd(david.wd)
    github.dir = " "
}

artwork.dir = paste(github.dir,"\\..\\01 latex\\pictures",sep="")
models.dir = paste(github.dir,"02 Models\\",sep="")
scripts.dir = paste(github.dir,"\\",sep="")

inArtDir <- function(file.name) return(paste(artwork.dir,file.name,sep=""))
inModelsDir <- function(file.name) return(paste(models.dir,file.name,sep=""))
inScriptsDir <- function(file.name) return(paste(scripts.dir,file.name,sep=""))

required_packages <- c(
  "hms", "pastecs", "Hmisc", "robustHD", "ggpubr", "cowplot", "ggsignif",
  "readxl", "ggplot2", "afex", "emmeans", "dplyr", "tidyr", "xtable",
  "DescTools", "rmarkdown", "knitr", "lme4", "sjPlot", "car", "tidyverse"
)

installed_packages <- rownames(installed.packages())
for(p in required_packages) {
  if(!(p %in% installed_packages)) install.packages(p)
}
lapply(required_packages, library, character.only = TRUE)

source("00_loadFonts.R" %>% inScriptsDir())
source("00_latex-functions.R" %>% inScriptsDir())
source("00_scf-functions.R" %>% inScriptsDir())