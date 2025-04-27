library(modelsummary)


#################
#
# Adapt MSUMMARY for my latex style
# I copied this latex-functions.R
#
#################

msummary.tex <- function(m_list,SEQUENCE.MODEL,TERM.AFTER = NULL,EXTRA.TERMS = NULL){
  
  if(!is.null(TERM.AFTER)){
    if(!is.null(EXTRA.TERMS)){
      term.sequence = c(term.sequence,EXTRA.TERMS)
    }
    elements.to.move <- term.sequence[grepl("\\\\times", term.sequence)]
    term.sequence <- term.sequence[!term.sequence %in% elements.to.move]
    index.to.insert <- which(term.sequence == TERM.AFTER) + 1
    term.sequence <- append(term.sequence, elements.to.move, after = index.to.insert - 1)
  }
  
  term.sequence = broom::tidy(SEQUENCE.MODEL)$term
 
  #print(term.sequence)
  
  first = T
  MSUMMARY <- msummary(m_list, stars=TRUE,output="data.frame",coef_map = term.sequence) %>%
    mutate(across(where(is.numeric), ~ sprintf("%.3f", .)))
  
  MSUMMARY <- data.frame(lapply(MSUMMARY, function(x) {gsub(".fixed.", " fixed ", x)}))
  MSUMMARY <- data.frame(lapply(MSUMMARY, function(x) {gsub("\\+", "$^{+}$", x)}))
  MSUMMARY <- data.frame(lapply(MSUMMARY, function(x) {gsub("Rsq", "$R^2$", x)}))
  MSUMMARY <- data.frame(lapply(MSUMMARY, function(x) {gsub("Num..obs.", "$N$", x)}))
  MSUMMARY <- data.frame(lapply(MSUMMARY, function(x) {gsub("\\*\\*\\*", "$^{xxx}$", x)}))
  MSUMMARY <- data.frame(lapply(MSUMMARY, function(x) {gsub("\\*\\*", "$^{xx}$", x)}))
  MSUMMARY <- data.frame(lapply(MSUMMARY, function(x) {gsub("\\*", "$^{x}$", x)}))
  MSUMMARY <- data.frame(lapply(MSUMMARY, function(x) {gsub("\\{xxx\\}", "{***}", x)}))
  MSUMMARY <- data.frame(lapply(MSUMMARY, function(x) {gsub("\\{xx\\}", "{**}", x)}))
  MSUMMARY <- data.frame(lapply(MSUMMARY, function(x) {gsub("\\{x\\}", "{*}", x)}))
  
  N.models <- dim(MSUMMARY)[2]-3
  N.all.rows <- dim(MSUMMARY)[1]
  N.rows <- sum(MSUMMARY$part=="estimates")
  out = ""
  
  for(i in 1:N.models)
    #out = paste(out,"&& \\muc{1.5cm}{(",i,")}", sep="")
    out = paste(out,"&& \\suc{(",i,")}", sep="")
  
  out = paste(out,"\\\\\n",sep="")
  
  for(i in 1:N.models)
    out = paste(out,"\\cline{",2*i+1,"-",2*i+1,"}", sep="")
  
  out = paste(out,"\n",sep="")
  
  for(i in 1:N.rows){
    if(grepl("2011",MSUMMARY[i,"term"])&&first){
      out = paste(out,"adoption year\\\\\n")
      first=F
    }
    if(MSUMMARY[i,"statistic"]=="estimate"){
      #if(MSUMMARY[i,"statistic"]=="modelsummary_tmp1"){
      out = paste(out,MSUMMARY[i,"term"], sep="")
      for(j in 4:(N.models+3)){
        out = paste(out,"&&", MSUMMARY[i,j], sep="")
      }
      out = paste(out,"\\\\\n", sep="")
    }
    if(MSUMMARY[i,"statistic"]=="std.error"){
      for(j in 4:(N.models+3)){
        out = paste(out,"&&\\suc{", MSUMMARY[i,j],"}", sep="")
      }
      out = paste(out,"\\\\\n", sep="")
    }
  }
  out = paste(out,"\\midrule\n", sep="")
  for(i in 1:N.all.rows){
    if(MSUMMARY[i,"part"]=="gof"){
      out = paste(out,MSUMMARY[i,"term"], sep="")
      for(j in 4:(N.models+3)){
        if(MSUMMARY[i,"term"]=="$N$"){
          out = paste(out,"&&\\suc{", MSUMMARY[i,j],"}", sep="")
        }
        else{
          out = paste(out,"&&", MSUMMARY[i,j], sep="")
        }
      }
      out = paste(out,"\\\\\n", sep="")
    }
  }
  
  cat(out)
  clipr::write_clip(out)
}




msummary.lmer.tex <- function(m_list){
  
  
  first = T
  MSUMMARY <- msummary(m_list, stars=TRUE,output="data.frame") %>%
    mutate(across(where(is.numeric), ~ sprintf("%.3f", .)))
  
  MSUMMARY <- data.frame(lapply(MSUMMARY, function(x) {gsub(".fixed.", " fixed ", x)}))
  MSUMMARY <- data.frame(lapply(MSUMMARY, function(x) {gsub("\\+", "$^{+}$", x)}))
  MSUMMARY <- data.frame(lapply(MSUMMARY, function(x) {gsub("Rsq", "$R^2$", x)}))
  MSUMMARY <- data.frame(lapply(MSUMMARY, function(x) {gsub("Num..obs.", "$N$", x)}))
  MSUMMARY <- data.frame(lapply(MSUMMARY, function(x) {gsub("\\*\\*\\*", "$^{xxx}$", x)}))
  MSUMMARY <- data.frame(lapply(MSUMMARY, function(x) {gsub("\\*\\*", "$^{xx}$", x)}))
  MSUMMARY <- data.frame(lapply(MSUMMARY, function(x) {gsub("\\*", "$^{x}$", x)}))
  MSUMMARY <- data.frame(lapply(MSUMMARY, function(x) {gsub("\\{xxx\\}", "{***}", x)}))
  MSUMMARY <- data.frame(lapply(MSUMMARY, function(x) {gsub("\\{xx\\}", "{**}", x)}))
  MSUMMARY <- data.frame(lapply(MSUMMARY, function(x) {gsub("\\{x\\}", "{*}", x)}))
  MSUMMARY <- data.frame(lapply(MSUMMARY, function(x) {gsub("TRUE", " (1=yes)", x)}))
  
  
  N.models <- dim(MSUMMARY)[2]-3
  N.all.rows <- dim(MSUMMARY)[1]
  N.rows <- sum(MSUMMARY$part=="estimates")
  out = ""
  
  for(i in 1:N.models)
    #out = paste(out,"&& \\muc{1.5cm}{(",i,")}", sep="")
    out = paste(out,"&& \\suc{(",i,")}", sep="")
  
  out = paste(out,"\\\\\n",sep="")
  
  for(i in 1:N.models)
    out = paste(out,"\\cline{",2*i+1,"-",2*i+1,"}", sep="")
  
  out = paste(out,"\n",sep="")
  
  for(i in 1:N.rows){
    if(grepl("2011",MSUMMARY[i,"term"])&&first){
      out = paste(out,"adoption year\\\\\n")
      first=F
    }
    if(MSUMMARY[i,"statistic"]=="estimate"){
      #if(MSUMMARY[i,"statistic"]=="modelsummary_tmp1"){
      out = paste(out,MSUMMARY[i,"term"], sep="")
      for(j in 4:(N.models+3)){
        out = paste(out,"&&", MSUMMARY[i,j], sep="")
      }
      out = paste(out,"\\\\\n", sep="")
    }
    if(MSUMMARY[i,"statistic"]=="std.error"){
      for(j in 4:(N.models+3)){
        out = paste(out,"&&\\suc{", MSUMMARY[i,j],"}", sep="")
      }
      out = paste(out,"\\\\\n", sep="")
    }
  }
  out = paste(out,"\\midrule\n", sep="")
  for(i in 1:N.all.rows){
    if(MSUMMARY[i,"part"]=="gof"){
      out = paste(out,MSUMMARY[i,"term"], sep="")
      for(j in 4:(N.models+3)){
        if(MSUMMARY[i,"term"]=="$N$"){
          out = paste(out,"&&\\suc{", MSUMMARY[i,j],"}", sep="")
        }
        else{
          out = paste(out,"&&", MSUMMARY[i,j], sep="")
        }
      }
      out = paste(out,"\\\\\n", sep="")
    }
  }
  
  cat(out)
  clipr::write_clip(out)
}



#################
#
# CoxPH for MSUMMARY
#
#################

CoxPH.to.list <- function(COXPH) {
  N.tasks <- COXPH %>% model.frame(.) %>% nrow()
  N.successes  <- COXPH %>% nobs(.) %>% as.numeric()
  AIC         <- COXPH %>% AIC(.) %>% round(3)  # Explicit rounding to 3 digits
  rmse        <- get_gof(COXPH)["rmse"] %>% round(3)  # Explicit rounding to 3 digits
  
  # Fixed effects remain as strings
#  supplier.industry.fixed.effect <- "\\suc{(included)}"
#  supplier.country.fixed.effect <- "\\suc{(included)}"
#  buyer.fixed.effect <- "\\suc{(included)}"
#  year.fixed.effect <- "\\suc{(included)}"
  
  # Process the model estimates
  fitted.df <- COXPH %>%
    broom::tidy(.) %>% 
    subset(!is.na(estimate))%>%
    mutate(term = stringr::str_replace_all(term, "ARFALSE", "ON_SITE")) %>%
    mutate(term = stringr::str_replace_all(term, "ARTRUE", "AR")) %>%
    mutate(term = stringr::str_replace_all(term, "dyad.formationdedicated", "DEDICATED")) %>%
    mutate(term = stringr::str_replace_all(term, "dyad.formationrandom", "RANDOM")) %>%
    mutate(term = stringr::str_replace_all(term, "&", "\\\\&")) %>%
    mutate(term = stringr::str_replace_all(term, "year", "")) %>%
    mutate(term = stringr::str_replace_all(term, "[.]", " ")) %>%
    mutate(term = stringr::str_replace_all(term, ":", "$\\\\times$")) %>%
    dplyr::select(-c("std.error", "statistic"))
  
  # Ensure numeric fields are explicitly rounded
  fit.df <- data.frame(
#    supplier.industry.fixed.effect,
#    supplier.country.fixed.effect,
#    buyer.fixed.effect,
#    year.fixed.effect,
    N.tasks = N.tasks,
    N.successes = N.successes,
    AIC = round(AIC, 3),
    rmse = round(rmse, 3),
    stringsAsFactors = FALSE  # Avoid coercing to factors
  )
  
  out.list <- list(estimates = fitted.df, fit = fit.df)
  class(out.list) <- c("custom_coxph", class(out.list))
  return(out.list)
}

#################
#
# LMER and GLMER for MSUMMARY
#
#################

glm.to.list <- function(GLM.FM) {
  N.successes <- sum(GLM.FM$y == 1, na.rm = TRUE)
  N.observations <- nobs(GLM.FM) %>% as.numeric()
  AIC            <- AIC(GLM.FM) %>% round(3)
  rmse           <- sqrt(mean(residuals(GLM.FM, type = "response")^2)) %>% round(3)
  
  # Process the model estimates
  fitted.df <- (lmtest::coeftest(GLM.FM, vcov = sandwich::vcovHC(GLM.FM, type = "HC0")))[] %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column(var="term") %>% 
    dplyr::mutate(
      term = stringr::str_replace_all(term, "year", ""),
      term = stringr::str_replace_all(term, "&", "\\\\&"),
      term = stringr::str_replace_all(term, "[.]", " "),
      term = stringr::str_replace_all(term, ":", "$\\\\times$")
    ) %>%
    mutate(term = stringr::str_replace_all(term, "ARFALSE", "ON_SITE")) %>%
    mutate(term = stringr::str_replace_all(term, "ARTRUE", "AR")) %>%
    mutate(term = stringr::str_replace_all(term, "dyad.formationdedicated", "DEDICATED")) %>%
    mutate(term = stringr::str_replace_all(term, "dyad.formationrandom", "RANDOM")) %>%
    dplyr::mutate(
      std.error = round(`Std. Error`, 3),
      statistic = round(`z value`, 3),
      p.value = round(`Pr(>|z|)`, 3),  # Compute p-value manually
      estimate = round(Estimate, 3)
    ) %>%
    dplyr::select(term, estimate, std.error, statistic, p.value)
  
  # Ensure numeric fields are explicitly rounded
  fit.df <- data.frame(
    N.observations = N.observations,
    N.successes = N.successes,
    AIC = AIC,
    rmse = rmse,
    stringsAsFactors = FALSE
  )
  
  out.list <- list(estimates = fitted.df, fit = fit.df)
  class(out.list) <- c("custom_glm", class(out.list))
  return(out.list)
}






#################
#
# CoxPH for MSUMMARY
#
#################


tidy.custom_coxph <- function(in.list, ...) {
  
  x <- in.list$estimates
  
  # Combine fixed effects with the model estimates
  combined <- rbind(
    data.frame(
      term = x$term,
      estimate = sprintf("%.3f",x$estimate),     
      std.error = round(x$robust.se,3),   
      p.value = x$p.value        
    )
  )
  
  return(combined)
}

glance.custom_coxph <-function(in.list,...){
  x <- in.list$fit
  data.frame(
    Num.observations=x$N.tasks%>% paste("\\suc{",.,"}",sep=""),
    Num.successes=x$N.successes%>% paste("\\suc{",.,"}",sep=""),
    AIC=round(x$AIC,2)%>% paste("\\suc{",.,"}",sep=""),
    model="\\suc{Cox PH}"
  )
}

tidy.custom_glm <- function(in.list, ...) {

  x <- in.list$estimates
  
  combined <- rbind(
    data.frame(
      term = x$term,
      estimate = sprintf("%.3f", x$estimate),     
      std.error = sprintf("%.3f", x$std.error),   
      p.value = sprintf("%.3f", x$p.value)        
    )
  )
  
  return(combined)
}

glance.custom_glm <- function(in.list, ...) {
  x <- in.list$fit
  data.frame(
    Num.observations = x$N.observations %>% paste("\\suc{", ., "}", sep = ""),
    Num.successes=x$N.successes%>% paste("\\suc{",.,"}",sep=""),
    AIC = round(x$AIC, 2) %>% paste("\\suc{", ., "}", sep = ""),
   # RMSE = round(x$rmse, 3) %>% paste("\\suc{", ., "}", sep = ""),
    model = "\\suc{Probit}"
  )
}
