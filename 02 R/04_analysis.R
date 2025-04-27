source(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), "init.R"))


###############################
#
#   Load data files
#
###############################

load(file = "./02 RData/analysis.RData")

############################
#
# Hypotheses tests - Study 1
#
############################

# Collect important t-test p-values
pvals.study1 <- list()

############################
#
# H1a: The gain of digital resources reduces stress.
#
############################
#cortisol (supported)
pvals.study1$h1a.cortisol <- t.test(
  study1.final.df %>% 
    subset(resource == "gain")      %>%  # this is the task for hypothesis testing
    subset(condition == "Digital")     %>%  # condition in hypothesis
    pull(AUCi.cortisol),
  study1.final.df %>% 
    subset(resource == "lack")      %>%  # this is the task for hypothesis testing
    subset(condition == "Digital")     %>%  # condition in hypothesis
    pull(AUCi.cortisol)
)$p.value #%>%
  #print.t.test

#perceived (supported)
pvals.study1$h1a.perceived <- t.test(
  study1.final.df %>% 
    subset(resource == "gain")      %>%  # this is the task for hypothesis testing
    subset(condition == "Digital")     %>%  # condition in hypothesis
    pull(AUCi.perceived),
  study1.final.df %>% 
    subset(resource == "lack")      %>%  # this is the task for hypothesis testing
    subset(condition == "Digital")     %>%  # condition in hypothesis
    pull(AUCi.perceived)
)$p.value # %>%
  #print.t.test

############################
#
# H1b: the loss of digital resources increases stress.
#
############################
#Cortisol (supported)
pvals.study1$h1b.cortisol <- t.test(
  study1.final.df %>% 
    subset(resource == "gain")      %>%  # this is the task for hypothesis testing
    subset(condition == "Digital")     %>%  # condition in hypothesis
    pull(AUCi.cortisol),
  study1.final.df %>% 
    subset(resource == "loss")      %>%  # this is the task for hypothesis testing
    subset(condition == "Digital")     %>%  # condition in hypothesis
    pull(AUCi.cortisol)
)$p.value #%>%
  #print.t.test

#Perceived (not supported)
pvals.study1$h1b.perceived <- t.test(
  study1.final.df %>% 
    subset(resource == "gain")      %>%  # this is the task for hypothesis testing
    subset(condition == "Digital")     %>%  # condition in hypothesis
    pull(AUCi.perceived),
  study1.final.df %>% 
    subset(resource == "loss")      %>%  # this is the task for hypothesis testing
    subset(condition == "Digital")     %>%  # condition in hypothesis
    pull(AUCi.perceived)
)$p.value #%>%
  #print.t.test

############################
#
# H1c: The decrease in stress due to the gain of digital resources is smaller than the increase in stress due to the loss of digital resources. 
#
############################
#cortisol (supported)
pvals.study1$h1c.cortisol <- t.test(
  study1.final.df %>% 
    subset(resource == "lack")      %>%  # 
    subset(condition == "Digital")     %>%  # condition in hypothesis
    pull(AUCi.cortisol)-
    study1.final.df %>% 
    subset(resource == "gain")      %>%  #
    subset(condition == "Digital")     %>%  # condition in hypothesis
    pull(AUCi.cortisol)
  ,
  study1.final.df %>% 
    subset(resource == "loss")      %>%  # Since gain is already -ve, that is why loss-(-gain)
    subset(condition == "Digital")     %>%  # condition in hypothesis
    pull(AUCi.cortisol)-
    study1.final.df %>% 
    subset(resource == "gain")      %>%  #
    subset(condition == "Digital")     %>%  # condition in hypothesis
    pull(AUCi.cortisol)
  ,
  alternate = "less"
)$p.value #%>%
  #print.t.test

#perceived (supported)
pvals.study1$h1c.perceived <- t.test(
  study1.final.df %>% 
    subset(resource == "lack")      %>%  # 
    subset(condition == "Digital")     %>%  # condition in hypothesis
    pull(AUCi.perceived)-
    study1.final.df %>% 
    subset(resource == "gain")      %>%  #
    subset(condition == "Digital")     %>%  # condition in hypothesis
    pull(AUCi.perceived)
  ,
  study1.final.df %>% 
    subset(resource == "loss")      %>%  #
    subset(condition == "Digital")     %>%  # condition in hypothesis
    pull(AUCi.perceived)-
    study1.final.df %>% 
    subset(resource == "gain")      %>%  #
    subset(condition == "Digital")     %>%  # condition in hypothesis
    pull(AUCi.perceived)
  ,
  alternate = "less"
)$p.value #%>%
  #print.t.test


############################
#
# H2a: The gain of traditional resources increases stress. 
#
############################
#cortisol (supported)
pvals.study1$h2a.cortisol <- t.test(
  study1.final.df %>% 
    subset(resource == "gain")      %>%  # this is the task for hypothesis testing
    subset(condition == "Traditional")     %>%  # condition in hypothesis
    pull(AUCi.cortisol),
  study1.final.df %>% 
    subset(resource == "lack")      %>%  # this is the task for hypothesis testing
    subset(condition == "Traditional")     %>%  # condition in hypothesis
    pull(AUCi.cortisol)
)$p.value #%>%
  #print.t.test

#perceived (supported)
pvals.study1$h2a.perceived <- t.test(
  study1.final.df %>% 
    subset(resource == "gain")      %>%  # this is the task for hypothesis testing
    subset(condition == "Traditional")     %>%  # condition in hypothesis
    pull(AUCi.perceived),
  study1.final.df %>% 
    subset(resource == "lack")      %>%  # this is the task for hypothesis testing
    subset(condition == "Traditional")     %>%  # condition in hypothesis
    pull(AUCi.perceived)
)$p.value #%>%
  #print.t.test

############################
#
# H2b: The loss of traditional resources increases stress. 
#
############################
#Cortisol (supported)
pvals.study1$h2b.cortisol <- t.test(
  study1.final.df %>% 
    subset(resource == "gain")      %>%  # this is the task for hypothesis testing
    subset(condition == "Traditional")     %>%  # condition in hypothesis
    pull(AUCi.cortisol),
  study1.final.df %>% 
    subset(resource == "loss")      %>%  # this is the task for hypothesis testing
    subset(condition == "Traditional")     %>%  # condition in hypothesis
    pull(AUCi.cortisol)
)$p.value #%>%
  #print.t.test

#perceived (not supported)
pvals.study1$h2b.perceived <- t.test(
  study1.final.df %>% 
    subset(resource == "gain")      %>%  # this is the task for hypothesis testing
    subset(condition == "Traditional")     %>%  # condition in hypothesis
    pull(AUCi.perceived),
  study1.final.df %>% 
    subset(resource == "loss")      %>%  # this is the task for hypothesis testing
    subset(condition == "Traditional")     %>%  # condition in hypothesis
    pull(AUCi.perceived)
)$p.value #%>%
  #print.t.test

############################
#
# H2c: The increase in stress due to the gain of traditional resources is smaller than the increase in stress due to the loss of traditional resources. 
#
############################
#cortisol (supported)
pvals.study1$h2c.cortisol <- t.test(
  study1.final.df %>% 
    subset(resource == "lack")      %>%  # 
    subset(condition == "Traditional")     %>%  # condition in hypothesis
    pull(AUCi.cortisol)-
    study1.final.df %>% 
    subset(resource == "gain")      %>%  #
    subset(condition == "Traditional")     %>%  # condition in hypothesis
    pull(AUCi.cortisol)
  ,
  study1.final.df %>% 
    subset(resource == "loss")      %>%  # Since gain is already -ve, that is why loss-(-gain)
    subset(condition == "Traditional")     %>%  # condition in hypothesis
    pull(AUCi.cortisol)-
    study1.final.df %>% 
    subset(resource == "gain")      %>%  #
    subset(condition == "Traditional")     %>%  # condition in hypothesis
    pull(AUCi.cortisol)
  ,
  alternate = "less"
)$p.value #%>%
  #print.t.test

#perceived (supported)
pvals.study1$h2c.perceived <- t.test(
  study1.final.df %>% 
    subset(resource == "lack")      %>%  # 
    subset(condition == "Traditional")     %>%  # condition in hypothesis
    pull(AUCi.perceived)-
    study1.final.df %>% 
    subset(resource == "gain")      %>%  #
    subset(condition == "Traditional")     %>%  # condition in hypothesis
    pull(AUCi.perceived)
  ,
  study1.final.df %>% 
    subset(resource == "loss")      %>%  #
    subset(condition == "Traditional")     %>%  # condition in hypothesis
    pull(AUCi.perceived) -
    study1.final.df %>% 
    subset(resource == "gain")      %>%  #
    subset(condition == "Traditional")     %>%  # condition in hypothesis
    pull(AUCi.perceived)
  ,
  alternate ="less"
)$p.value #%>%
  #print.t.test


############################
#
# H3: The stress reduction associated with the gain of digital resources is smaller than the stress increase associated with the gain of traditional resources.
#
############################
#cortisol (not supported)
pvals.study1$h3.cortisol <- t.test(
  study1.final.df %>% 
    subset(resource == "lack")      %>%  # 
    subset(condition == "Digital")     %>%  # condition in hypothesis
    pull(AUCi.cortisol)-
    study1.final.df %>% 
    subset(resource == "gain")      %>%  #
    subset(condition == "Digital")     %>%  # condition in hypothesis
    pull(AUCi.cortisol)
  ,
  study1.final.df %>% 
    subset(resource == "lack")      %>%  # Since gain is already -ve, that is why loss-(-gain)
    subset(condition == "Traditional")     %>%  # condition in hypothesis
    pull(AUCi.cortisol)-
    study1.final.df %>% 
    subset(resource == "gain")      %>%  #
    subset(condition == "Traditional")     %>%  # condition in hypothesis
    pull(AUCi.cortisol)
  ,
  alternate ="less"
)$p.value #%>%
  #print.t.test

#perceived (not supported)
pvals.study1$h3.perceived <- t.test(
  study1.final.df %>% 
    subset(resource == "lack")      %>%  # 
    subset(condition == "Digital")     %>%  # condition in hypothesis
    pull(AUCi.perceived)-
    study1.final.df %>% 
    subset(resource == "gain")      %>%  #
    subset(condition == "Digital")     %>%  # condition in hypothesis
    pull(AUCi.perceived)
  ,
  study1.final.df %>% 
    subset(resource == "lack")      %>%  #
    subset(condition == "Traditional")     %>%  # condition in hypothesis
    pull(AUCi.perceived) -
    study1.final.df %>% 
    subset(resource == "gain")      %>%  #
    subset(condition == "Traditional")     %>%  # condition in hypothesis
    pull(AUCi.perceived)  
  ,
  alternate ="less"
)$p.value #%>%
  #print.t.test

############################
#
# H4: The stress increase associated with the loss of digital resources is larger than the stress increase associated with the loss of traditional resources. 
# 
############################
#cortisol (not supported)
pvals.study1$h4.cortisol <- t.test(
  study1.final.df %>% 
    subset(resource == "loss")      %>%  # 
    subset(condition == "Digital")     %>%  # condition in hypothesis
    pull(AUCi.cortisol)-
    study1.final.df %>% 
    subset(resource == "gain")      %>%  #
    subset(condition == "Digital")     %>%  # condition in hypothesis
    pull(AUCi.cortisol)
  ,
  study1.final.df %>% 
    subset(resource == "loss")      %>%  # Since gain is already -ve, that is why loss-(-gain)
    subset(condition == "Traditional")     %>%  # condition in hypothesis
    pull(AUCi.cortisol)-
    study1.final.df %>% 
    subset(resource == "gain")      %>%  #
    subset(condition == "Traditional")     %>%  # condition in hypothesis
    pull(AUCi.cortisol)
  ,
  alternate ="greater"
)$p.value #%>%
  #print.t.test

#perceived (not supported)
pvals.study1$h4.perceived <- t.test(
  study1.final.df %>% 
    subset(resource == "loss")      %>%  # 
    subset(condition == "Digital")     %>%  # condition in hypothesis
    pull(AUCi.perceived)-
    study1.final.df %>% 
    subset(resource == "gain")      %>%  #
    subset(condition == "Digital")     %>%  # condition in hypothesis
    pull(AUCi.perceived)
  ,
  study1.final.df %>% 
    subset(resource == "loss")      %>%  #
    subset(condition == "Traditional")     %>%  # condition in hypothesis
    pull(AUCi.perceived) -
    study1.final.df %>% 
    subset(resource == "gain")      %>%  #
    subset(condition == "Traditional")     %>%  # condition in hypothesis
    pull(AUCi.perceived)  
  ,
  alternate ="greater"
)$p.value #%>%
  #print.t.test



# Save the p-values to a file
save(pvals.study1, file = "./02 RData/pvals_study1.RData")
