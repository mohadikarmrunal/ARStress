#######################
#
#   KEEP
#
#
#######################

get.cnt <- function(var){
  tmp_cnt <- plyr::count(var)
  #isMode = (sum(grepl("discount",tmp_cnt$x)*1)>0) # to make Mode be sorted non alphabetically
 # if(!isMode){
 #   tmp_cnt<-tmp_cnt[order(as.character(tmp_cnt$x)),]
 # }
  return(tmp_cnt)
}

stackColumn<-function(col.in,names.in){
  for(i in 1:length(col.in)){
    cnt.df<-get.cnt(col.in[[i]])
    cnt.df$x<-gsub("&","\\\\&",as.character(cnt.df$x))
    m<-sum(cnt.df$freq)
    cnt.df$rel <- paste(sprintf("%.2f",round(cnt.df$freq/m*10000)/100),"\\%",sep="")
    cnt.df<-rbind(c(names.in[i],"{count}","{percentage}"),cnt.df,c("\\textbf{total}",m,"100\\%" ),c("","",""))
    if(i==1){
      col <- cnt.df
    }
    else{
      col<-rbind(col,cnt.df)
    }
  }
  return(col)
}

get.finish.lines<-function(col.in){
  line = 1
  line.vec = c()
  for(i in 1:length(col.in)){
    cnt = get.cnt(col.in[[i]])
    line = line + length(cnt$freq)
    line.vec = c(line.vec,line)
    line=line+3
  }
  return(line.vec)
}

get.start.lines<-function(col.in){
  line = 1
  line.vec = c(line)
  for(i in 1:(length(col.in))){
    if(i<length(col.in)){
      cnt = get.cnt(col.in[[i]])
      line = line + length(cnt$freq)+3
      line.vec = c(line.vec,line)
    }
  }
  return(line.vec)
}

createTable<-function(col.1.in,names.1.in,col.2.in,names.2.in){
  output = ""
  col.1 <- stackColumn(col.1.in,names.1.in)
  col.2 <- stackColumn(col.2.in,names.2.in)
  while(length(col.1[,1])<length(col.2[,1])){
    col.1<-rbind(col.1,c("","",""))
  }
  while(length(col.2[,1])<length(col.1[,1])){
    col.2<-rbind(col.2,c("","",""))
  }
  col.1.start.lines<-get.start.lines(col.1.in)
  col.2.start.lines<-get.start.lines(col.2.in)
  col.1.finish.lines<-get.finish.lines(col.1.in)
  col.2.finish.lines<-get.finish.lines(col.2.in)
  
  df<-(unname(cbind(col.1,col.2)))
  
  num.cols <- length(df) 
  num.rows <- length(df[,1])
  for(r in 1:num.rows){
    string=""
    for(c in 1:num.cols){
      if(c<num.cols){
        string = paste(string,df[r,c],"&&")
      }
      else{
        string= paste(string,df[r,c])
      }
    }
    if(r %in% col.1.start.lines | r %in% col.2.start.lines){
      string = paste(string,"\\\\")}
    else{
      string = paste(string,"\\\\[3pt]")
    }
    if(r %in% col.1.start.lines){
      string=paste(string,"\\cline{1-5}\\\\[-12pt]")
    }
    if(r %in% col.2.start.lines){
      string=paste(string,"\\cline{7-11}\\\\[-12pt]")
    }
    if(r %in% col.1.finish.lines){
      string=paste(string,"\\cline{1-5}\\\\[-12pt]")
    }
    if(r %in% col.2.finish.lines){
      string=paste(string,"\\cline{7-11}\\\\[-12pt]")
    }
    #cat(string,"\n")
    output = paste(output,string,"\n",sep="")
  }
  cat(output)
  clipr::write_clip(output)
}


print.latex.mean.row <- function(data.expr, proximity.label = NULL, complexity.label = NULL) {
  df <- eval(substitute(data.expr))
  
  if (!all(c("random.dyad", "value") %in% names(df))) {
    stop("Data must include 'random.dyad' and 'value' columns")
  }
  
  df <- df %>% mutate(value = as.numeric(as.character(value)))
  
  mean.dedicated <- df %>% filter(random.dyad == FALSE) %>% pull(value) %>% mean(na.rm = TRUE)
  mean.random    <- df %>% filter(random.dyad == TRUE)  %>% pull(value) %>% mean(na.rm = TRUE)
  
  diff <- round(mean.random - mean.dedicated, 2)
  
  if (is.null(proximity.label)) {
    proximity.label <- if ("AR" %in% names(df)) {
      if (isTRUE(unique(df$AR))) "AR" else "ONSITE"
    } else {
      "?"
    }
  }
  
  if (is.null(complexity.label)) {
    complexity.label <- if ("task.type" %in% names(df)) {
      if (unique(df$task.type) == "s") "simple" else "complex"
    } else {
      "?"
    }
  }
  
  line <- sprintf(
    "%s & %s  & %.2f & %.2f & %.2f \\\\",
    proximity.label,
    complexity.label,
    mean.dedicated,
    mean.random,
    diff
  )
  
  cat(line, "\n")
  clipr::write_clip(line)
  return(invisible(line))
}



print.tex.effectVar <- function(localVar, varName) {

  stats <- pastecs::stat.desc(localVar)[c("min", "max", "mean", "std.dev", "nbr.val")]
  
  return(paste(
    gsub("&", "\\\\&", varName), "&&",
    sprintf("%.3f", stats[1]), "&&",
    sprintf("%.3f", stats[2]), "&&",
    sprintf("%.3f", stats[3]), "&&",
    sprintf("%.3f", stats[4]), "&&",
    stats[5], "\\\\[3pt]\n"
  ))
}




print.corr.matrix <- function(MATRIX){
  output=""
  dim = sqrt(length(MATRIX$r))
  out.matrix <- matrix(rep("",dim*(dim-1)),ncol=dim-1,nrow=dim)
  dimnames(out.matrix)<-list(paste(1:dim,colnames(MATRIX$r),sep=". "), paste(1:(dim-1),".",sep=""))
  
  for (row in 1:dim){
    for (col in 1:dim-1){
      if(row>col)
        out.matrix[row,col]<-paste(sprintf("%.2f",round(MATRIX$r[row,col],2)),apply(MATRIX$P,2,star.symbol.corr)[row,col],sep="")
      if(row==col)
        out.matrix[row,col]<-""
    }
  }
  
  out.df <- data.frame(out.matrix)
  out.df$variables <- rownames(out.df)
  out.df<-unname(out.df)
  out.df <- out.df[c(length(out.df),1:length(out.df)-1)]
  
  for (row in 1:length(out.df[,1])){
    for (col in 1:length(out.df[row,])){
   #   cat(paste(as.matrix(out.df[row,col]),"&&"))
      output=paste(output,as.matrix(out.df[row,col]),"&&",sep="")
    }
   # cat("\\\\[2pt]\n")
    output=paste(output,"\\\\[2pt]\n",sep="")
  }
  cat(output)
  clipr::write_clip(output)
}



star.symbol.corr<-function(p){
  Stars <- vector(mode="character", length=length(p))
  Stars<-rep("",length(p))
  Stars <- ifelse(p <= 0.05, "*" , Stars)
  return(Stars)
}


winsor.fn <- function(value,p){
  MAX = quantile(value,p)
  MIN = quantile(value,1-p)
  value<-ifelse(value>MAX,MAX,value)
  value<-ifelse(value<MIN,MIN,value)
  return(value)
}

# functions
se <- function(x) sd(x)/sqrt(length(x))


# Print function for t-tests
print.t.test <- function(T.TEST) {
  paired <- T.TEST$method == "Paired t-test"
  tval <- as.numeric(T.TEST$statistic)
  df <- as.numeric(T.TEST$parameter)
  
  # Extract beta
  if (paired) {
    beta <- round(T.TEST$estimate, 2)
    n <- df + 1
    cohens.d <- round(tval / sqrt(n), 2)
  } else {
    beta <- round(T.TEST$estimate[1] - T.TEST$estimate[2], 2)
    cohens.d <- round(2 * tval / sqrt(df), 2)
  }
  
  # Format p-value
  p.value <- T.TEST$p.value
  p.text <- if (p.value < 0.001) {
    "p<0.001"
  } else if (p.value < 0.01) {
    paste0("p=", sprintf("%.3f", p.value))
  } else {
    paste0("p=", sprintf("%.2f", p.value))
  }
  
  # Output
  latex.output <- paste0("$(\beta = ", beta, ", d = ", cohens.d, ", ", p.text, ")$")
  clipr::write_clip(latex.output)
  return(latex.output)
}

# Copied msummary here
library(modelsummary)


#################
#
# Adapt MSUMMARY for my latex style
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

