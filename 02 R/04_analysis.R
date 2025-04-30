source(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), "init.R"))


###############################
#
#   Load data files
#
###############################

load(file = "./02 RData/analysis.RData")

# Collect important t-test p-values
pvals.study2 <- list()
pvals.study3 <- list()
pvals.study4 <- list()


############################
#
# Hypotheses tests - Study 2 (We do two sided t-tests to check if there is in general differences in means of two groups. We do not check here is one mean is less/greater than the other.)
# We have already arranged the dataframe as per participant ID: so, no need for arrange(participant.id)
# 
############################

############################
#
# H1a: The gain of digital resources reduces stress.
#
############################
#cortisol (supported)
pvals.study2$h1a.cortisol <- t.test(
  study2.final.df %>% 
    subset(resource == "gain")      %>%  # this is the task for hypothesis testing
    subset(condition == "Digital")     %>%  # condition in hypothesis
    pull(AUCi.cortisol),
  study2.final.df %>% 
    subset(resource == "lack")      %>%  # this is the task for hypothesis testing
    subset(condition == "Digital")     %>%  # condition in hypothesis
    pull(AUCi.cortisol),
  paired = TRUE
)$p.value #%>%
  #print.t.test

#perceived (supported)
pvals.study2$h1a.perceived <- t.test(
  study2.final.df %>% 
    subset(resource == "gain")      %>%  # this is the task for hypothesis testing
    subset(condition == "Digital")     %>%  # condition in hypothesis
    pull(AUCi.perceived),
  study2.final.df %>% 
    subset(resource == "lack")      %>%  # this is the task for hypothesis testing
    subset(condition == "Digital")     %>%  # condition in hypothesis
    pull(AUCi.perceived),
  paired = TRUE
)$p.value # %>%
  #print.t.test

############################
#
# H1b: the loss of digital resources increases stress.
#
############################
#Cortisol (supported)
pvals.study2$h1b.cortisol <- t.test(
  study2.final.df %>% 
    subset(resource == "loss")      %>%  # this is the task for hypothesis testing
    subset(condition == "Digital")     %>%  # condition in hypothesis
    pull(AUCi.cortisol),
  study2.final.df %>% 
    subset(resource == "gain")      %>%  # this is the task for hypothesis testing
    subset(condition == "Digital")     %>%  # condition in hypothesis
    pull(AUCi.cortisol),
  paired = TRUE
)$p.value #%>%
  #print.t.test

#Perceived (not supported)
pvals.study2$h1b.perceived <- t.test(
  study2.final.df %>% 
    subset(resource == "loss")      %>%  # this is the task for hypothesis testing
    subset(condition == "Digital")     %>%  # condition in hypothesis
    pull(AUCi.perceived),
  study2.final.df %>% 
    subset(resource == "gain")      %>%  # this is the task for hypothesis testing
    subset(condition == "Digital")     %>%  # condition in hypothesis
    pull(AUCi.perceived),
  paired = TRUE
)$p.value #%>%
  #print.t.test

############################
#
# H1c: The decrease in stress due to the gain of digital resources is smaller than the increase in stress due to the loss of digital resources. 
#
############################
#cortisol (supported)
pvals.study2$h1c.cortisol <- t.test(
  study2.final.df %>% 
    subset(resource == "lack")      %>%  # 
    subset(condition == "Digital")     %>%  # condition in hypothesis
    pull(AUCi.cortisol)-
    study2.final.df %>% 
    subset(resource == "gain")      %>%  #
    subset(condition == "Digital")     %>%  # condition in hypothesis
    pull(AUCi.cortisol)
  ,
  study2.final.df %>% 
    subset(resource == "loss")      %>%  # Since gain is already -ve, that is why loss-(-gain)
    subset(condition == "Digital")     %>%  # condition in hypothesis
    pull(AUCi.cortisol)-
    study2.final.df %>% 
    subset(resource == "gain")      %>%  #
    subset(condition == "Digital")     %>%  # condition in hypothesis
    pull(AUCi.cortisol),
  paired = TRUE
)$p.value #%>%
  #print.t.test

#perceived (supported)
pvals.study2$h1c.perceived <- t.test(
  study2.final.df %>% 
    subset(resource == "lack")      %>%  # 
    subset(condition == "Digital")     %>%  # condition in hypothesis
    pull(AUCi.perceived)-
    study2.final.df %>% 
    subset(resource == "gain")      %>%  #
    subset(condition == "Digital")     %>%  # condition in hypothesis
    pull(AUCi.perceived)
  ,
  study2.final.df %>% 
    subset(resource == "loss")      %>%  #
    subset(condition == "Digital")     %>%  # condition in hypothesis
    pull(AUCi.perceived)-
    study2.final.df %>% 
    subset(resource == "gain")      %>%  #
    subset(condition == "Digital")     %>%  # condition in hypothesis
    pull(AUCi.perceived),
  paired = TRUE
)$p.value #%>%
  #print.t.test

############################
#
# H2a: The gain of traditional resources increases stress. 
#
############################
#cortisol (supported)
pvals.study2$h2a.cortisol <- t.test(
  study2.final.df %>% 
    subset(resource == "gain")      %>%  # this is the task for hypothesis testing
    subset(condition == "Traditional")     %>%  # condition in hypothesis
    pull(AUCi.cortisol),
  study2.final.df %>% 
    subset(resource == "lack")      %>%  # this is the task for hypothesis testing
    subset(condition == "Traditional")     %>%  # condition in hypothesis
    pull(AUCi.cortisol),
  paired = TRUE
)$p.value #%>%
  #print.t.test

#perceived (supported)
pvals.study2$h2a.perceived <- t.test(
  study2.final.df %>% 
    subset(resource == "gain")      %>%  # this is the task for hypothesis testing
    subset(condition == "Traditional")     %>%  # condition in hypothesis
    pull(AUCi.perceived),
  study2.final.df %>% 
    subset(resource == "lack")      %>%  # this is the task for hypothesis testing
    subset(condition == "Traditional")     %>%  # condition in hypothesis
    pull(AUCi.perceived),
  paired = TRUE
)$p.value #%>%
  #print.t.test

############################
#
# H2b: The loss of traditional resources increases stress. 
#
############################
#Cortisol (supported)
pvals.study2$h2b.cortisol <- t.test(
  study2.final.df %>% 
    subset(resource == "gain")      %>%  # this is the task for hypothesis testing
    subset(condition == "Traditional")     %>%  # condition in hypothesis
    pull(AUCi.cortisol),
  study2.final.df %>% 
    subset(resource == "loss")      %>%  # this is the task for hypothesis testing
    subset(condition == "Traditional")     %>%  # condition in hypothesis
    pull(AUCi.cortisol),
  paired = TRUE
)$p.value #%>%
  #print.t.test

#perceived (not supported)
pvals.study2$h2b.perceived <- t.test(
  study2.final.df %>% 
    subset(resource == "gain")      %>%  # this is the task for hypothesis testing
    subset(condition == "Traditional")     %>%  # condition in hypothesis
    pull(AUCi.perceived),
  study2.final.df %>% 
    subset(resource == "loss")      %>%  # this is the task for hypothesis testing
    subset(condition == "Traditional")     %>%  # condition in hypothesis
    pull(AUCi.perceived),
  paired = TRUE
)$p.value #%>%
  #print.t.test

############################
#
# H2c: The increase in stress due to the gain of traditional resources is smaller than the increase in stress due to the loss of traditional resources. 
#
############################
#cortisol (marginally supported)
pvals.study2$h2c.cortisol <- t.test(
  study2.final.df %>% 
    subset(resource == "lack")      %>%  # 
    subset(condition == "Traditional")     %>%  # condition in hypothesis
    pull(AUCi.cortisol)-
    study2.final.df %>% 
    subset(resource == "gain")      %>%  #
    subset(condition == "Traditional")     %>%  # condition in hypothesis
    pull(AUCi.cortisol)
  ,
  study2.final.df %>% 
    subset(resource == "loss")      %>%  # Since gain is already -ve, that is why loss-(-gain)
    subset(condition == "Traditional")     %>%  # condition in hypothesis
    pull(AUCi.cortisol)-
    study2.final.df %>% 
    subset(resource == "gain")      %>%  #
    subset(condition == "Traditional")     %>%  # condition in hypothesis
    pull(AUCi.cortisol),
  paired = TRUE
)$p.value #%>%
  #print.t.test

#perceived (supported)
pvals.study2$h2c.perceived <- t.test(
  study2.final.df %>% 
    subset(resource == "lack")      %>%  # 
    subset(condition == "Traditional")     %>%  # condition in hypothesis
    pull(AUCi.perceived)-
    study2.final.df %>% 
    subset(resource == "gain")      %>%  #
    subset(condition == "Traditional")     %>%  # condition in hypothesis
    pull(AUCi.perceived)
  ,
  study2.final.df %>% 
    subset(resource == "loss")      %>%  #
    subset(condition == "Traditional")     %>%  # condition in hypothesis
    pull(AUCi.perceived) -
    study2.final.df %>% 
    subset(resource == "gain")      %>%  #
    subset(condition == "Traditional")     %>%  # condition in hypothesis
    pull(AUCi.perceived),
  paired = TRUE
)$p.value #%>%
  #print.t.test


############################
#
# H3: The stress reduction associated with the gain of digital resources is smaller than the stress increase associated with the gain of traditional resources.
#
############################
#cortisol (not supported)
pvals.study2$h3.cortisol <- t.test(
  study2.final.df %>% 
    subset(resource == "lack")      %>%  # 
    subset(condition == "Digital")     %>%  # condition in hypothesis
    pull(AUCi.cortisol)-
    study2.final.df %>% 
    subset(resource == "gain")      %>%  #
    subset(condition == "Digital")     %>%  # condition in hypothesis
    pull(AUCi.cortisol)
  ,
  study2.final.df %>% 
    subset(resource == "lack")      %>%  # Since gain is already -ve, that is why loss-(-gain)
    subset(condition == "Traditional")     %>%  # condition in hypothesis
    pull(AUCi.cortisol)-
    study2.final.df %>% 
    subset(resource == "gain")      %>%  #
    subset(condition == "Traditional")     %>%  # condition in hypothesis
    pull(AUCi.cortisol)
)$p.value #%>%                          # we do not need paired t-test here
  #print.t.test

#perceived (not supported)
pvals.study2$h3.perceived <- t.test(
  study2.final.df %>% 
    subset(resource == "lack")      %>%  # 
    subset(condition == "Digital")     %>%  # condition in hypothesis
    pull(AUCi.perceived)-
    study2.final.df %>% 
    subset(resource == "gain")      %>%  #
    subset(condition == "Digital")     %>%  # condition in hypothesis
    pull(AUCi.perceived)
  ,
  study2.final.df %>% 
    subset(resource == "lack")      %>%  #
    subset(condition == "Traditional")     %>%  # condition in hypothesis
    pull(AUCi.perceived) -
    study2.final.df %>% 
    subset(resource == "gain")      %>%  #
    subset(condition == "Traditional")     %>%  # condition in hypothesis
    pull(AUCi.perceived)
)$p.value #%>%
  #print.t.test

############################
#
# H4: The stress increase associated with the loss of digital resources is larger than the stress increase associated with the loss of traditional resources. 
# 
############################
#cortisol (not supported)
pvals.study2$h4.cortisol <- t.test(
  study2.final.df %>% 
    subset(resource == "loss")      %>%  # 
    subset(condition == "Digital")     %>%  # condition in hypothesis
    arrange(participant.id) %>%           # ensure matching order
    pull(AUCi.cortisol)-
    study2.final.df %>% 
    subset(resource == "gain")      %>%  #
    subset(condition == "Digital")     %>%  # condition in hypothesis
    arrange(participant.id) %>%           # ensure matching order
    pull(AUCi.cortisol)
  ,
  study2.final.df %>% 
    subset(resource == "loss")      %>%  # Since gain is already -ve, that is why loss-(-gain)
    subset(condition == "Traditional")     %>%  # condition in hypothesis
    pull(AUCi.cortisol)-
    study2.final.df %>% 
    subset(resource == "gain")      %>%  #
    subset(condition == "Traditional")     %>%  # condition in hypothesis
    pull(AUCi.cortisol),
)$p.value #%>%
  #print.t.test

#perceived (not supported)
pvals.study2$h4.perceived <- t.test(
  study2.final.df %>% 
    subset(resource == "loss")      %>%  # 
    subset(condition == "Digital")     %>%  # condition in hypothesis
    pull(AUCi.perceived)-
    study2.final.df %>% 
    subset(resource == "gain")      %>%  #
    subset(condition == "Digital")     %>%  # condition in hypothesis
    pull(AUCi.perceived)
  ,
  study2.final.df %>% 
    subset(resource == "loss")      %>%  #
    subset(condition == "Traditional")     %>%  # condition in hypothesis
    pull(AUCi.perceived) -
    study2.final.df %>% 
    subset(resource == "gain")      %>%  #
    subset(condition == "Traditional")     %>%  # condition in hypothesis
    pull(AUCi.perceived),
)$p.value #%>%
  #print.t.test

############################
#
# Hypotheses tests - Study 3
#
############################

# Save the p-values to a file
save(pvals.study2, file = "./02 RData/pvals_study2.RData")
